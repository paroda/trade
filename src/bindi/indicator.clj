(ns bindi.indicator
  (:require [taoensso.timbre :as log]))

(defn- avg [xs]
  (if (seq xs)
    (/ (apply + xs) (count xs))))

(defn- smooth-avg
  ([a v] (smooth-avg a v 14))
  ([a v n]
   (if (number? a)
     ;; post n values, do smooth average
     (/ (+ (* (dec n) a) v) n)
     ;; prior to initial n values
     (let [a (conj a v)]
       (if (< (count a) n)
         ;; not n values yet
         a
         ;; got initial n values, do simple average
         (avg a))))))

(defn- exp-avg
  ([a v] (exp-avg a v 14 2))
  ([a v n] (exp-avg a v n 2))
  ([a v n f] ;; f = smoothing factor (default 2)
   (if (number? a)
     ;; post n values, do smooth average
     (let [m (/ f (inc n))]
       (+ (* v m) (* a (- 1 m))))
     ;; prior to initial n values
     (let [a (conj a v)]
       (if (< (count a) n)
         ;; not n values yet
         a
         ;; got initial n values, do simple average
         (avg a))))))

(defmacro evaluate-once [[state ind-key quote] & body]
  `(if (= (:t ~quote) (:t (~ind-key ~state)))
     [(get-in ~state [~ind-key :val]) ~state]
     (let [[v# s#] (do ~@body)]
       [v# (update s# ~ind-key assoc :t (:t ~quote) :val v#)])))

(defn- rsi* [state quote]
  (evaluate-once
   [state :rsi quote]
   (let [{{:keys [avg-gain avg-loss]} :rsi} state
         {:keys [o c t]} quote
         gain (/ (- c o) o 0.01)
         ag (smooth-avg avg-gain (if (pos? gain) gain 0))
         al (smooth-avg avg-loss (if (pos? gain) 0 (- gain)))
         rsi (if (number? ag)
               {:t t, :i (if (zero? al)
                           100
                           (- 100 (/ 100.0 (+ 1.0 (/ ag al)))))})]
     [rsi (assoc state :rsi {:avg-gain ag, :avg-loss al})])))

(defn- atr* [state quote]
  (evaluate-once
   [state :atr quote]
   (let [{{:keys [atr2], pc :c} :atr} state
         {:keys [t h l c]} quote
         [atr2 atr]
         (if-not pc
           ;; very first quote
           [() nil]
           ;; second quote onwards
           (let [tr (- (max h pc) (min l pc))
                 a (smooth-avg atr2 tr)]
             [a (if (number? a) {:t t, :i a})]))]
     [atr (assoc state :atr {:atr2 atr2, :c c})])))

(defn- pos-di* [state quote]
  (evaluate-once
   [state :pos-di quote]
   (let [[{atr :i} state] (atr* state quote)
         {{:keys [avg-dm], ph :h, pl :l} :pos-di} state
         {:keys [t h l]}  quote
         [avg-dm pos-di]
         (if-not ph
           ;; very first quote
           [() nil]
           ;; second quote onwards
           (let [+dm (- h ph), -dm (- pl l)
                 dm (if (and (pos? +dm) (> +dm -dm)) +dm 0)
                 a (smooth-avg avg-dm dm)
                 di (if (and atr (number? a)) (/ a atr 0.01))]
             [a (if di {:t t, :i di})]))]
     [pos-di (assoc state :pos-di {:avg-dm avg-dm, :h h, :l l})])))

(defn- neg-di* [state quote]
  (evaluate-once
   [state :neg-di quote]
   (let [[{atr :i} state] (atr* state quote)
         {{:keys [avg-dm], ph :h, pl :l} :neg-di} state
         {:keys [t h l]}  quote
         [avg-dm neg-di]
         (if-not ph
           ;; very first quote
           [() nil]
           ;; second quote onwards
           (let [+dm (- h ph), -dm (- pl l)
                 dm (if (and (pos? -dm) (< +dm -dm)) -dm 0)
                 a (smooth-avg avg-dm dm)
                 di (if (and atr (number? a)) (/ a atr 0.01))]
             [a (if di {:t t, :i di})]))]
     [neg-di (assoc state :neg-di {:avg-dm avg-dm, :h h, :l l})])))

(defn- adx* [state quote]
  (evaluate-once
   [state :adx quote]
   (let [[{+di :i} state] (pos-di* state quote)
         [{-di :i} state] (neg-di* state quote)
         {{:keys [adx2]} :adx} state
         {:keys [t]} quote
         [adx2 adx]
         (if (and +di -di)
           (let [dx (/ (Math/abs (- +di -di)) (+ +di -di) 0.01)
                 a (smooth-avg adx2 dx)]
             [a (if (number? a) {:t t, :i a})]))]
     [adx (assoc state :adx {:adx2 adx2})])))

(defn- ema-12* [state quote]
  (evaluate-once
   [state :ema-12 quote]
   (let [{{:keys [ema-12-2]} :ema-12} state
         {:keys [t c]} quote
         [ema-12-2 ema-12]
         (let [a (exp-avg ema-12-2 c 12)]
           [a (if (number? a) {:t t, :i a})])]
     [ema-12 (assoc state :ema-12 {:ema-12-2 ema-12-2})])))

(defn- ema-26* [state quote]
  (evaluate-once
   [state :ema-26 quote]
   (let [{{:keys [ema-26-2]} :ema-26} state
         {:keys [t c]} quote
         [ema-26-2 ema-26]
         (let [a (exp-avg ema-26-2 c 26)]
           [a (if (number? a) {:t t, :i a})])]
     [ema-26 (assoc state :ema-26 {:ema-26-2 ema-26-2})])))

(defn- macd* [state quote]
  (evaluate-once
   [state :macd quote]
   (let [[{ema-12 :i} state] (ema-12* state quote)
         [{ema-26 :i} state] (ema-26* state quote)
         {:keys [t]} quote
         macd (if (and ema-12 ema-26)
                {:t t, :i (- ema-12 ema-26)})]
     [macd (assoc state :macd {})])))

(defn- macd-signal* [state quote]
  (evaluate-once
   [state :macd-signal quote]
   (let [[{macd :i} state] (macd* state quote)
         {{:keys [macd-signal-2]} :macd-signal} state
         {:keys [t]} quote
         [macd-signal-2 macd-signal]
         (let [a (if macd (exp-avg macd-signal-2 macd 9))]
           [a (if (number? a) {:t t, :i a})])]
     [macd-signal (assoc state :macd-signal {:macd-signal-2 macd-signal-2})])))

(defn- cci-20* [state quote]
  ;; 20 periods
  (evaluate-once
   [state :cci-20 quote]
   (let [{{:keys [pts]} :cci-20} state
         {:keys [t h l c]} quote
         pt (/ (+ h l c) 3)
         pts (take 20 (conj pts pt))
         cci-20 (if (= 20 (count pts))
                  (let [ma (avg pts)
                        md (->> pts (map #(Math/abs (- % ma))) avg)]
                    {:t t, :i (/ (- pt ma) 0.015 md)}))]
     [cci-20 (assoc state :cci-20 {:pts pts})])))

(defn- cci-200* [state quote]
  ;; 20 periods
  (evaluate-once
   [state :cci-200 quote]
   (let [{{:keys [pts]} :cci-200} state
         {:keys [t h l c]} quote
         pt (/ (+ h l c) 3)
         pts (take 200 (conj pts pt))
         cci-200 (if (= 200 (count pts))
                   (let [ma (avg pts)
                         md (->> pts (map #(Math/abs (- % ma))) avg)]
                     {:t t, :i (/ (- pt ma) 0.015 md)}))]
     [cci-200 (assoc state :cci-200 {:pts pts})])))

(defn- high-swing* [state quote]
  (evaluate-once
   [state :high-swing quote]
   (let [[{atr :i} state] (atr* state quote)
         {{:keys [top d p lh]} :high-swing} state
         {:keys [t h]} quote
         lh (or lh h)
         delta (if p (- h p))
         new-top (if (and atr d delta (< delta 0 d)
                          (or (not top) (> p top)
                              (> (- p lh) atr)))
                   p)
         [top lh] (if atr
                    (cond
                      (and (not top) new-top) [new-top h]
                      (and (not new-top) top) [top (min lh h)]
                      (and top new-top) (if (> new-top top)
                                          [new-top h]
                                          (if (> atr (- top new-top))
                                            [top (min lh h)]
                                            [new-top h]))))
         swing (if top {:t t, :i top})]
     [swing (assoc state :high-swing {:top top :d delta, :p h, :lh lh})])))

(defn- low-swing* [state quote]
  (evaluate-once
   [state :low-swing quote]
   (let [[{atr :i} state] (atr* state quote)
         {{:keys [bot d p hl]} :low-swing} state
         {:keys [t l]} quote
         hl (or hl l)
         delta (if p (- l p))
         new-bot (if (and atr d delta (< d 0 delta)
                          (or (not bot) (< p bot)
                              (> (- hl p) atr)))
                   p)
         [bot hl] (cond
                    (and (not bot) new-bot) [new-bot l]
                    (and (not new-bot) bot) [bot (max hl l)]
                    (and bot new-bot) (if (> bot new-bot)
                                        [new-bot l]
                                        (if (> atr (- new-bot bot))
                                          [bot (max hl l)]
                                          [new-bot l])))
         swing (if bot {:t t, :i bot})]
     [swing (assoc state :low-swing {:bot bot, :d delta, :p l, :hl hl})])))

(defn- ->indicator [evaluator]
  (fn [xf]
    (let [state (volatile! {})]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result quote]
         (let [[indicator new-state] (evaluator @state quote)]
           (vreset! state new-state)
           (if indicator
             (xf result indicator)
             result)))))))

;; make a combined evaluator for specified indicators
;; indicator data is returned only when all specified indicators have value
;; also includes the quote in result
(defn make-evaluator [ind-keys]
  (let [evaluators (->> ind-keys
                        (map #(list % (ns-resolve 'bindi.indicator
                                                  (symbol (str (name %) \*)))))
                        (filter second))]
    (fn [state {:keys [t] :as quote}]
      (reduce (fn [[result state] [ind-key evaluator]]
                (let [[{:keys [i]} state] (evaluator state quote)]
                  [(if (and result i)
                     (assoc result ind-key i))
                   state]))
              [{:t t, :quote quote}
               state]
              evaluators))))

;; indicator data is returned only when all specified indicators have value
;; also includes the quote in result
(defn indicators
  ([ind-keys]
   (->indicator (make-evaluator ind-keys)))
  ([ind-keys quotes] (sequence (indicators ind-keys) quotes)))

(defn rsi
  ([] (->indicator rsi*))
  ([quotes] (sequence (rsi) quotes)))

(defn atr
  ([] (->indicator atr*))
  ([quotes] (sequence (atr) quotes)))

(defn pos-di
  ([] (->indicator pos-di*))
  ([quotes] (sequence (pos-di) quotes)))

(defn neg-di
  ([] (->indicator neg-di*))
  ([quotes] (sequence (neg-di) quotes)))

(defn adx
  ([] (->indicator adx*))
  ([quotes] (sequence (adx) quotes)))

(defn ema-12
  ([] (->indicator ema-12*))
  ([quotes] (sequence (ema-12) quotes)))

(defn ema-26
  ([] (->indicator ema-26*))
  ([quotes] (sequence (ema-26) quotes)))

(defn macd
  ([] (->indicator macd*))
  ([quotes] (sequence (macd) quotes)))

(defn macd-signal
  ([] (->indicator macd-signal*))
  ([quotes] (sequence (macd-signal) quotes)))

(defn cci-20
  ([] (->indicator cci-20*))
  ([quotes] (sequence (cci-20) quotes)))

(defn cci-200
  ([] (->indicator cci-200*))
  ([quotes] (sequence (cci-200) quotes)))

(defn high-swing
  ([] (->indicator high-swing*))
  ([quotes] (sequence (high-swing) quotes)))

(defn low-swing
  ([] (->indicator low-swing*))
  ([quotes] (sequence (low-swing) quotes)))

(comment

  (let [quotes (map #(hash-map :t %, :o (rand), :h (rand), :l (rand), :c (rand))
                    (range 35))]
    ;; (rsi quotes)
    ;; (atr quotes)
    ;; (pos-di quotes)
    ;; (neg-di quotes)
    ;; (indicators [:rsi :atr] quotes)
    ;; (indicators [:pos-di :neg-di :atr] quotes)
    ;; (indicators [:pos-di :adx] quotes)
    ;; (adx quotes)
    ;; (indicators [:ema-12 :ema-26 :macd :macd-signal] quotes)
    (high-swing quotes)
    )

  )
