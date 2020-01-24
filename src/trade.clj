(ns trade)

(defn gather-promising-prospects [{:keys [scouts] :as current-wealth} price]
  (->> scouts
       (mapcat #(% current-wealth price))
       (assoc current-wealth :prospects)))

(defn try-deal [{:keys [mode quantity stop-loss-price target-price]
                 {:keys [o b] :as price} :price
                 :as prospect}]
  ;; deal if opening price is withing stop-loss-price and target-price
  (if (case mode
        :buy (and (< stop-loss-price o) (> target-price o))
        :sell (and (> stop-loss-price o) (< target-price o)))
    (let [cost (* quantity (case mode
                             ;; buy mode, cost of buy
                             :buy (+ o b)
                             ;; sell mode, negative cost of sell
                             :sell (- (- o b))))]
      (-> prospect
          (dissoc :price)
          (assoc :got-deal? true
                 :order-price price
                 :cost cost)))
    ;; un-dealed prospects
    prospect))

(defn venture [current-wealth price]
  ;; try new ventures and return wealth after considering the cost
  (-> current-wealth
      ;; find possible promising prospects
      (gather-promising-prospects price)
      ;; try dealing the prospect
      (update :prospects #(map try-deal %))
      ;; record new successfully dealed prospects
      (update (fn record-new-ventures [{:keys [prospects] :as current-wealth}]
                ;; new ventures are those prospects for which you got the deal
                (let [new-ventures (->> prospects (filter :got-deal?))]
                  (-> current-wealth
                      ;; add new ventures
                      (update :ventures into new-ventures)
                      ;; update balance with costs of new ventures
                      (update :balance - (->> new-ventures
                                              (map :cost)
                                              (reduce +)))))))))

(defn track-wealth [wealth]
  (let [{:keys [balance max-balance min-balance]
         :or {max-balance 0, min-balance 0}} wealth]
    (assoc wealth
           :max-balance (max balance max-balance)
           :min-balance (min balance min-balance))))

(defn get-close-price [{:keys [mode target-price stop-loss-price] :as venture}
                       {:keys [o h l] :as price}]
  (case mode
    ;; buy order, sell out to close
    :buy (if (>= h target-price)
           (max o target-price)
           (if (<= l stop-loss-price)
             (min o stop-loss-price)))
    ;; sell order, buy back to close
    :sell (if (<= l target-price)
            (min o target-price)
            (if (>= h stop-loss-price)
              (max o stop-loss-price)))))

(defn close-venture-if-required [{:keys [mode quantity cost] :as venture}
                                 {:keys [b] :as price}]
  (if-let [;; obtain closing price if it should be closed
           close-price (if (not (:close-price venture))
                         (get-close-price venture price))]
    ;; close if got closing price
    (let [;; credit from closing the venture
          credit (* quantity (case mode
                               ;; buy order, credit from sell
                               :buy (- close-price b)
                               ;; sell mode, negative credit from buy
                               :sell (- (+ close-price b))))
          ;; net profit
          profit (- credit cost)]
      ;; update ventur with closing info
      (assoc venture
             :close-price price
             :credit credit
             :profit profit))
    ;; do not close, return unchanged
    venture))

(defn close-required-ventures [current-wealth price]
  (-> current-wealth
      (update :ventures #(map (partial close-venture-if-required price) %))
      (update (fn record-closed-ventures [{:keys [ventures balance profit] :as current-wealth}]
                (let [vs (filter :record? ventures)
                      balance (->> vs (map :credit) (reduce + balance))
                      profit (->> vs (map :profit) (reduce + profit))]
                  (assoc current-wealth
                         :ventures (->> ventures (map #(dissoc % :record?)))
                         :balance balance
                         :profit profit))))))

(defn note-closed-ventures [current-wealth]
  current-wealth)

(defn trade
"Process latest price quote and update my wealth.

*wealth*
:scouts = list of scouts to scout prospect
:balance = balance
:ventures = active ventures
  {:mode = buy/sell :buy or :sell
   :quantity
   :stop-loss-price
   :target-price
   :order-price
   :cost
   :close-price
   :credit
   :profit
   }

*price*
:t = time
:b = brokerage
:o, :h, :l, :c = price variants
" 
  [wealth price]
  (-> wealth
      (venture price)
      (track-wealth)
      (close-required-ventures price)
      (note-closed-ventures) ;; note ventures closed by bank
      (track-wealth)))
