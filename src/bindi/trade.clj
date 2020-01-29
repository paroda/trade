(ns bindi.trade
  (:require [bindi.util :as u]))

(defn venture
  "Scout for promising prospects and order them.
Returns updated wealth."
  [current-wealth prices]
  (let [{:keys [scout]
         {:keys [order]} :broker} current-wealth
        ;; scout for promising prospects
        prospects (scout current-wealth prices)
        ;; order prospects to obtain new ventures
        new-ventures (order prospects)]
    (-> current-wealth
        ;; add new ventures
        (update :ventures into new-ventures)
        ;; update balance with costs of new ventures
        (update :balance - (->> new-ventures
                                (map :cost)
                                (reduce +))))))

(defn- update-terminated-ventures [current-wealth terminated-ventures]
  (let [[credit profit] (->> terminated-ventures
                             (map (juxt :credit :profit))
                             (reduce #(map + %1 %2) [0 0]))]
    (-> current-wealth
        (update :terminated into 
                (->> terminated-ventures
                     (map #(select-keys % [:mode :symbol :cost :profit
                                           :open-time, :close-time
                                           :open-price, :close-price]))))
        (update :ventures u/remove-items terminated-ventures)
        (update :balance + credit)
        (update :profit + profit))))

(defn register-broker-terminations
  "Get terminated ventures from broker.
Returns updated wealth."
  [current-wealth]
  (let [{:keys [ventures]
         {:keys [check]} :broker} current-wealth
        terminated-ventures (check ventures)]
    (cond-> current-wealth
      (seq terminated-ventures)
      (update-terminated-ventures terminated-ventures))))

(defn evaluate-and-terminate
  "Evaluate ventures and terminate bad ventures.
Returns updated wealth."
  [current-wealth prices]
  (let [{:keys [evaluate]
         {:keys [terminate]} :broker} current-wealth
        bad-ventures (evaluate current-wealth prices)
        terminated-ventures (terminate bad-ventures)]
    (cond-> current-wealth
      (seq terminated-ventures)
      (update-terminated-ventures terminated-ventures))))

(defn track-wealth
  "Check current balance and update historical data.
Returns updated wealth."
  [wealth]
  (let [{:keys [balance max-balance min-balance]
         :or {max-balance 0, min-balance 0}} wealth]
    (assoc wealth
           :max-balance (max balance max-balance)
           :min-balance (min balance min-balance))))

(defn trade
  "Process latest price quote and update my wealth.

*wealth*
:scout = scout to find prospects
:evaluate = evaluate to find bad ventures
:broker = agent representing broker
:balance = balance
:min-balance, :max-balance = minimum and maximum balance in the past
:ventures = active ventures
  [{:mode = buy/sell :buy or :sell
    :id, :scout-id, :symbol
    :status = :prospect, :venture, :terminated
    :chance = chances of success (0 to 1), applies to prospect
    :quantity, :stop-loss-price, :target-price
    :open-time, :open-price, :cost
    :close-time, :close-price, :credit, :profit}]
:terminated = terminated ventures
:data = general purpose atom for use by stateful methods

*prices* map of symbol to price
:t = time
:o, :h, :l, :c = price variants"
  [wealth prices]
  (-> wealth
      (register-broker-terminations)
      (track-wealth)
      (evaluate-and-terminate prices)
      (track-wealth)
      (venture prices)
      (track-wealth)))

(defn new-wealth []
  {:scout nil
   :evaluate nil
   :broker nil
   :balance 0, :profit 0
   :max-balance 0, :min-balance 0
   :ventures []
   :terminated []
   :data (atom {})})