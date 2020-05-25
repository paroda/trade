(ns bindi.backtest
  (:require [clojure.core.async :as a]
            [bindi.analysis :as ana]
            [bindi.fx-broker :as fxb]
            [bindi.indicator :as ind])
  (:import java.util.Date))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return new trade
(defn- evaluate-open-trade [ti state quote]
  (let [{:keys [pip lot size limit stop]} (:settings state)
        {:keys [t o h l c]} quote
        {{:keys [mode]} :trade
         :keys [atr high-swing low-swing]} ti
        open-price c
        limit-price (case mode
                      :buy (+ open-price atr)
                      :sell (- open-price atr)
                      nil)
        stop-price (case mode
                     :buy (- low-swing (* 3 pip))
                     :sell (+ high-swing (* 3 pip))
                     nil)]
    (if (and mode limit-price stop-price)
      {:mode mode
       :quantity (* lot size)
       :open-time t
       :open-price open-price
       :limit-price limit-price
       :stop-price stop-price})))

;; return closed-trade
(defn- evaluate-close-trade [ti state quote]
  (let [{:keys [quantity limit-price stop-price open-price mode]} (:trade state)
        {:keys [t o h l c]} quote]
    (if-let [close-price
             (case mode
               :buy (if (> h limit-price) limit-price
                        (if (< l stop-price) stop-price))
               :sell (if (< l limit-price) limit-price
                         (if (> h stop-price) stop-price)))]
      (let [gain (* quantity (- close-price open-price))]
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

(defn- simulate [ti state quote]
  (let [state (or state {:balance 0.0
                         :max-bal 0.0
                         :min-bal 0.0
                         :settings {:pip 0.0001
                                    :lot 10
                                    :size 1000
                                    :limit 10
                                    :stop 10}
                         :trade nil
                         :closed-trades ()})]
    (if (:trade state)
      (if-let [closed-trade (evaluate-close-trade ti state quote)]
        (update-state state closed-trade)
        ;; no action
        state)
      (if-let [open-trade (evaluate-open-trade ti state quote)]
        (update-state state open-trade)
        ;; no action
        state))))

(defn test-strategy [strategy ind-keys ikey time-frame date-to n]
  (let [;; candles for analysis
        qs (fxb/get-hist-prices ikey time-frame date-to n)
        xf (comp (ind/indicators ind-keys)
                 (ana/analyze strategy))
        tis (sequence xf qs)
        ;; 1m candles for actual trade simulation
        qs (if-let [t (:t (second tis))]
             (fxb/get-hist-prices ikey "m1" date-to t))]
    [(butlast tis)
     (loop [qs qs
            tis tis
            ti nil
            state nil]
       (if-let [t2 (some-> tis second :t .getTime)]
         (let [qs-p (filter #(> t2 (.getTime (:t %))) qs)]
           (recur (drop (count qs-p) qs)
                  (rest tis)
                  (first tis)
                  (if ti
                    (reduce (partial simulate ti) state qs-p)
                    state)))
         state))]))

(comment

  (fxb/session-connected?)

  (let [res (test-strategy ana/strategy-adx-01
                           ana/indicator-keys
                           :eur-usd
                           "m30"
                           nil ;#inst "2020-05-25T00:00:00.000-00:00"
                           1000)]
    (dissoc (second res) :closed-trades))

  ;; adx-01 5/26 1000 m30 => 496

  (-> nil
      (as-> $ (simulate {:trade {:mode :buy}} $
                        {:t 1 :o 1.0001 :h 1.0003 :l 1.0000 :c 1.0002}))
      (as-> $ (simulate {:trade {:mode nil}} $
                        {:t 2 :o 1.0000 :h 1.0005 :l 0.9980 :c 0.9990}))
      (as-> $ (simulate {:trade {:mode :buy}} $
                        {:t 3 :o 1.0008 :h 1.0020 :l 1.0006 :c 1.0007})))

  )
