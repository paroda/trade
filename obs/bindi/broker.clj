(ns bindi.broker
  (:require [bindi.util :as u]))

(def symbols-data-simulated
  {:eur-usd {:type :fx, :mmr 3.25, :pip-cost 0.1, :pip-value 0.0001, :spread 1}
   :gbp-usd {:type :fx, :mmr 3.75, :pip-cost 0.1, :pip-value 0.0001, :spread 1.5}
   :eur-gbp {:type :fx, :mmr 3.25, :pip-cost 0.13, :pip-value 0.0001, :spread 2}})

(defn- open-close-symbol-type [symbols-data {:keys [symbol]}]
  (get-in symbols-data [symbol :type]))

(defmulti open-simulated open-close-symbol-type)
(defmulti close-simulated open-close-symbol-type)

(defmethod open-simulated :fx
  [symbols-data prospect]
  (let [{:keys [symbol quantity]} prospect
        {:keys [mmr]} (get symbols-data symbol)
        ;; cost is margin maintenance requirement
        cost (* quantity mmr)]
    ;; update prospect to venture with opening info
    (assoc prospect
           :status :venture
           :cost cost)))

(defmethod close-simulated :fx
  [symbols-data venture]
  (let [{:keys [symbol mode quantity cost open-price close-price]} venture
        {:keys [pip-cost pip-value spread]} (get symbols-data symbol)
        pip-gain (/ (case mode
                      :buy (- close-price open-price)
                      :sell (- open-price close-price))
                    pip-value)
        ;; net profit
        profit (u/round (* quantity pip-cost (- pip-gain spread)) 2)
        ;; net credit from closing the venture
        credit (+ profit cost)]
    ;; update venture to terminated with closing info
    (assoc venture
           :status :terminated
           :credit credit
           :profit profit)))

(defn order-simulated
  "Order the prospect and register cost.
=prospects= prospect collection
A prospect has :mode, :quantity, :stop-loss-price, :target-price and :open-price."
  [symbols-data prospects]
  (->> prospects
       (filter #(and (= :prospect (:status %))
                     (:open-price %)))
       (map (partial open-simulated symbols-data))))

(defn terminate-simulated
  "Terminate the venture bad to continue and register credit and net profit.
=bad-venture= bad venture collection
A venture has :mode, :quantity, :cost, :close-price."
  [symbols-data bad-ventures]
  (->> bad-ventures
       (filter #(and (= :venture (:status %))
                     (:close-price %)))
       (map (partial close-simulated symbols-data))))

(defn get-simulated-broker [symbols-data]
  {:order (partial order-simulated symbols-data)
   :terminate (partial terminate-simulated symbols-data)
   :check (constantly nil)})