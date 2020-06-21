(ns bindi.backtest
  (:require [clojure.core.async :as a]
            [bindi.config :as cfg]
            [bindi.analysis :as ana]
            [bindi.fx-broker :as fxb]
            [bindi.indicator :as ind])
  (:import java.util.Date))

(defonce instruments (atom {}))

(defn update-instrument [ikey attr value]
  {:pre [(case attr
           (:limit :stop-max :stop-min :stop-buff) (or (nil? value) (number? value))
           :atr-limit (or (nil? value)
                          (and (coll? value)
                               (every? #(or (nil? %) (number? %)) value)))
           false)]}
  (swap! instruments assoc-in [ikey attr] value)
  (get @instruments ikey))

;; return new trade
(defn- evaluate-open-trade [ti state quote]
  (let [{{:keys [mode]} :trade
         :keys [atr high-swing low-swing]} ti
        {:keys [pip limit stop-max stop-min stop-buff spread]
         [atr-low atr-high] :atr-limit} (:settings state)
        atr-pips (/ atr pip)
        limit (* limit atr)]
    (if (and mode
             (or (not atr-high) (> atr-high atr-pips))
             (or (not atr-low) (< atr-low atr-pips))
             (> limit (* 3 spread pip)))
      (let [{:keys [t o h l c]} quote
            open-price (case mode
                         :buy (+ (* pip spread) c)
                         :sell c)
            limit-price (case mode
                          :buy (+ open-price limit)
                          :sell (- open-price limit))
            stop-max (if stop-max (* limit stop-max))
            stop-min (if stop-min (* limit stop-min))
            stop-buff (if stop-buff (* stop-buff pip))
            stop-price (case mode
                         :buy (cond-> low-swing
                                stop-max (max (- open-price stop-max))
                                stop-min (min (- open-price stop-min))
                                stop-buff (- stop-buff))
                         :sell (cond-> high-swing
                                 stop-max (min (+ open-price stop-max))
                                 stop-min (max (+ open-price stop-min))
                                 stop-buff (+ stop-buff)))]
        (if (case mode ;; this check is needed if stop-min is not specified
              :buy (< stop-price open-price)
              :sell (> stop-price open-price))
          {:mode mode
           :open-time t
           :open-price open-price
           :limit-price limit-price
           :stop-price stop-price})))))

;; return closed-trade
(defn- evaluate-close-trade [ti state quote]
  (let [{:keys [limit-price stop-price open-price mode]} (:trade state)
        {:keys [pip pip-cost spread]} (:settings state)
        {:keys [t o h l c]} quote
        s (* pip spread)]
    (if-let [close-price
             (case mode
               :buy (if (> h limit-price) limit-price
                        (if (< l stop-price) stop-price))
               :sell (if (< (+ s l) limit-price) limit-price
                         (if (> (+ s h) stop-price) stop-price)))]
      (let [gain (* pip-cost (/ (- close-price open-price) pip))]
        (assoc (:trade state)
               :closed? true
               :close-price close-price
               :close-time t
               :profit (case mode :buy gain (- gain)))))))

;; update trade/closed-trades and balance
(defn- update-state [state trade]
  (if (:closed? trade)
    (let [b (+ (:balance state) (:profit trade))]
      (-> (dissoc state :trade)
          (update :closed-trades conj trade)
          (assoc :balance b)
          (update :min-bal min b)
          (update :max-bal max b)))
    (assoc state :trade trade)))

(defn- simulate [ikey ti state quote]
  (let [state (or state {:balance 0.0
                         :max-bal 0.0
                         :min-bal 0.0
                         :settings (get @instruments ikey)
                         :trade nil
                         :closed-trades ()})]
    (if (:trade state)
      (if-let [closed-trade (evaluate-close-trade ti state quote)]
        (cond-> (update-state state closed-trade)
          (neg? (:profit closed-trade)) (assoc :bad-ti ti))
        ;; no action
        state)
      (if-let [open-trade (if-not (identical? ti (:bad-ti state))
                            (evaluate-open-trade ti state quote))]
        (update-state state open-trade)
        ;; no action
        state))))

(defn test-strategy [strategy ind-keys ikey time-frame date-to n]
  (let [;; candles for analysis
        qs (fxb/get-hist-prices ikey time-frame date-to
                                (+ n ana/lead-ti-count))
        xf (comp (ind/indicators ind-keys)
                 (ana/analyze strategy))
        tis (reverse (take n (reverse (sequence xf qs))))
        ;; 1m candles for actual trade simulation
        qs (if-let [t (:t (second tis))]
             (fxb/get-hist-prices ikey "m1" date-to t))]
    [(butlast tis)
     (loop [qs qs
            tis tis
            ti nil
            state nil]
       (if-let [t2 (some-> tis second :t .getTime)]
         (let [qs-p (take-while #(> t2 (.getTime (:t %))) qs)]
           (recur (drop (count qs-p) qs)
                  (rest tis)
                  (first tis)
                  (if ti
                    (reduce (partial simulate ikey ti) state qs-p)
                    state)))
         state))]))

(defn init []
  (->> (fxb/get-offers)
       (map (fn [[ikey {:keys [pip]}]]
              [ikey (merge (select-keys (:trade @cfg/config)
                                        [:limit :stop-max :stop-min :stop-buff])
                           (-> (get-in @cfg/config [:instruments ikey])
                               (select-keys [:spread :pip-cost
                                             :limit :stop-max :stop-min :stop-buff
                                             :atr-limit])
                               (assoc :pip pip)))]))
       (into {})
       (reset! instruments)))

(comment

  (fxb/session-connected?)

  instruments
  (update-instrument :usoil :atr-limit [30 40])
  (swap! instruments update :usoil assoc
         :limit 1, :stop-max 5, :stop-min 1, :atr-limit [20 100])

  (let [res (test-strategy ana/strategy-adx-01
                           ana/indicator-keys
                           :usoil
                           "m30"
                           #inst "2020-06-21T00:00:00.000-00:00"
                           20000)]
    [(dissoc (second res) :closed-trades)
     (->> (:closed-trades (second res))
          ;; (map (juxt :profit :open-time))
          count)
     (first (:closed-trades (second res)))])

  ;; adx-01 :eur-usd m30/1000 5/24 => 24
  ;;                     10000     => 13

  (-> nil
      (as-> $ (simulate {:trade {:mode :buy}} $
                        {:t 1 :o 1.0001 :h 1.0003 :l 1.0000 :c 1.0002}))
      (as-> $ (simulate {:trade {:mode nil}} $
                        {:t 2 :o 1.0000 :h 1.0005 :l 0.9980 :c 0.9990}))
      (as-> $ (simulate {:trade {:mode :buy}} $
                        {:t 3 :o 1.0008 :h 1.0020 :l 1.0006 :c 1.0007})))

  )
