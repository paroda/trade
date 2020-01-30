(ns example
  (:require [bindi.broker :as br]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example scout function

(defn make-daily-scout [scout-id mode quantity
                        symbols-data
                        stop-loss-margin-pips target-margin-pips]
  (fn [current-wealth prices]
    (->>
     (for [[symbol {:keys [t c]}] prices]
       (let [d (:data current-wealth)
             {:keys [pip-value]} (get symbols-data symbol)
             stop-loss-margin (* pip-value stop-loss-margin-pips)
             target-margin (* pip-value  target-margin-pips)]
         (when (and t c d (< (* 24 3600e3)
                             (- (.getTime t)
                                (get-in @d [scout-id :lts symbol] 0))))
           (swap! d assoc-in [scout-id :lts symbol] (.getTime t))
           {:chance 1
            :mode mode, :quantity quantity
            :stop-loss-price ((case mode :buy - :sell +) c stop-loss-margin)
            :target-price ((case mode :buy + :sell -) c target-margin)
            :symbol symbol, :scout-id scout-id})))
     (remove nil?))))

(def a1 (make-daily-scout :a1 :buy 10
                          br/symbols-data-simulated
                          10 100))

(def a2 (make-daily-scout :a2 :sell 10
                          br/symbols-data-simulated
                          10 100))
