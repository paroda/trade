(ns bindi.analysis
  (:require [clojure.core.async :as a]
            [taoensso.timbre :as log]
            [bindi.indicator :as ind]))

;; {:data {:eur-usd {:tis <atom ({:t :rsi :atr ..} ..)> ;; reverse ordered
;;                   :ch <chan>}}
;;  :max-count 100
;;  :inited? false}
(defonce ^:private state (atom {:inited? false}))

(def lead-ti-count 200)

(def indicator-keys [:rsi :cci-20
                     :atr :adx :pos-di :neg-di
                     ;; :atr-30 :adx-30 :pos-di-30 :neg-di-30
                     :ema-12 :ema-26 :macd :macd-signal
                     :high-swing :low-swing])

(defn- dedupe-quote
  ([]
   (fn [xf]
     (let [prev (volatile! nil)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result quote]
          (let [prior @prev]
            (vreset! prev quote)
            (if (= (:t quote) (:t prior))
              result
              (xf result quote))))))))
  ([quotes] (sequence (dedupe-quote) quotes)))

(defn with-history
  ([n]
   (assert (and (number? n) (> n 2)) "item count must be > 2")
   (fn [xf]
     (let [prev (volatile! nil)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result item]
          (let [next (take n (conj @prev item))]
            (vreset! prev next)
            (if (seq next)
              (xf result next)
              result)))))))
  ([n items] (sequence (with-history n) items)))

(defn analyze
  ([strategy]
   (fn [xf]
     (let [state (volatile! nil)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result ti]
          (let [[ti s] (strategy @state ti)]
            (vreset! state s)
            (xf result ti)))))))
  ([strategy tis] (sequence (analyze strategy) tis)))

(defn strategy-adx-simple [state ti]
  (let [{:keys [adx pos-di neg-di]} ti
        {[adx-2 adx-1] :adx-history} (:adx-simple state)
        trade {:mode (if (and adx-1 adx-2 (> adx 25) (> adx adx-1 adx-2))
                       (cond
                         (> pos-di neg-di) :buy
                         (< pos-di neg-di) :sell))}]
    [(assoc ti :trade trade)
     (assoc state :adx-simple {:adx-history [adx-1 adx]})]))

;; 5/24 - 1000 - m30 => 282
(defn strategy-adx-01 [state ti]
  (let [{:keys [atr adx pos-di neg-di quote]} ti
        {:keys [o h l c]} quote
        {[adx-1 pos-di-1 neg-di-1] :history} (:adx-01 state)
        trade {:mode (if (and adx-1 (> adx 20)
                              (> 5 (/ (- h l) atr)))
                       (cond
                         (and (> (- pos-di neg-di) 5)
                              (> c o)
                              (> pos-di pos-di-1))
                         :buy
                         (and (> (- neg-di pos-di) 5)
                              (< c o)
                              (> neg-di neg-di-1))
                         :sell))}]
    [(assoc ti :trade trade)
     (assoc state :adx-01 {:history [adx pos-di neg-di]})]))

(defn strategy-adx-02 [state ti]
  (let [{:keys [atr-30 adx-30 pos-di-30 neg-di-30 quote]} ti
        {:keys [o h l c]} quote
        {[adx-1 pos-di-1 neg-di-1] :history} (:adx-02 state)
        trade {:mode (if (and adx-1 (> adx-30 20)
                              (> 3 (/ (- h l) atr-30)))
                       (cond
                         (and (> (- pos-di-30 neg-di-30) 5)
                              (> c o)
                              (> pos-di-30 pos-di-1))
                         :buy
                         (and (> (- neg-di-30 pos-di-30) 5)
                              (< c o)
                              (> neg-di-30 neg-di-1))
                         :sell))}]
    [(assoc ti :trade trade)
     (assoc state :adx-02 {:history [adx-30 pos-di-30 neg-di-30]})]))

(defn strategy-cci-01 [state ti]
  (let [{:keys [adx atr cci-20 quote]} ti
        {:keys [o h l c]} quote
        {[cci-20-1] :history} (:cci-01 state)
        trade {:mode (if (and cci-20-1 (< adx 20))
                       (cond
                         (< cci-20 -100)
                         :buy
                         (> cci-20 100)
                         :sell))}]
    [(assoc ti :trade trade)
     (assoc state :cci-01 {:history [cci-20]})]))

(defn strategy-adx-cci-01 [state ti]
  (let [{:keys [adx pos-di neg-di cci-20 quote]} ti
        {[cci-20-1 adx-1 pos-di-1 neg-di-1] :history
         :keys [last-mode]} (:adx-cci-01 state)
        mode (if (and adx-1 (> adx 20))
               (cond
                 (and (> cci-20 100)
                      ;; (> (:o quote) (:c quote))
                      (> (- pos-di neg-di) 10))
                 :buy
                 (and (< cci-20 -100)
                      ;; (< (:o quote) (:c quote))
                      (> (- neg-di pos-di) 10))
                 :sell))
        exit? (and last-mode (not mode)
                   (or (< adx 20)
                       (case last-mode
                         :buy (< (- pos-di neg-di) 1)
                         :sell (< (- neg-di pos-di) 1))))
        trade {:mode mode, :exit? exit?}]
    [(assoc ti :trade trade)
     (assoc state :adx-cci-01 {:history [cci-20 adx pos-di neg-di]
                               :last-mode (or mode last-mode)})]))

(defn strategy-adx-cci-02 [state ti]
  (let [{:keys [adx pos-di neg-di cci-20 quote]} ti
        {[adx-1 pos-di-1 neg-di-1] :history} (:adx-cci-02 state)
        trade {:mode (if (and adx-1 (> adx 20))
                       (cond
                         (and (< cci-20 100)
                              (> (- pos-di neg-di) 5)
                              (> (:c quote) (:o quote))
                              (> pos-di pos-di-1))
                         :buy
                         (and (> cci-20 -100)
                              (> (- neg-di pos-di) 5)
                              (< (:c quote) (:o quote))
                              (> neg-di neg-di-1))
                         :sell))}]
    [(assoc ti :trade trade)
     (assoc state :adx-cci-02 {:history [adx pos-di neg-di]})]))

(defn- setup-instrument [state ikey max-count]
  (let [ind-keys indicator-keys
        strategy strategy-adx-01
        ch (a/chan 100 (comp (dedupe-quote)
                             (ind/indicators ind-keys)
                             (analyze strategy))
                   #(log/error % "indicator transducer error"))
        tis (atom nil)]
    (some-> (get-in state [:data ikey :ch])
            (a/close!))
    (a/go-loop []
      (when-let [ti (a/<! ch)]
        (swap! tis #(take max-count (conj % ti)))
        (recur)))
    (assoc-in state [:data ikey] {:tis tis, :ch ch})))

(defn init [inst-keys max-count]
  (when-not (:inited? @state)
    (let [init-state (reduce (fn [state ikey]
                               (setup-instrument state ikey max-count))
                             {:inited? true, :max-count max-count}
                             inst-keys)]
      (reset! state init-state))))

(defn put-quotes [ikey quotes]
  (if-let [ch (get-in @state [:data ikey :ch])]
    (a/onto-chan!! ch quotes false)))

(defn get-indicators
  "returns indicators order recent first to oldest last"
  [ikey]
  (some-> @state
          (get-in [:data ikey :tis])
          (deref)))

(comment

  @state

  (swap! state setup-instrument :eur-usd 100)

  (get-indicators :eur-usd)

  (with-history 3 (range 10)))
