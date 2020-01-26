(ns bindi.broker)

(defn order-simulated
  "Order the prospect and register cost.
=prospects= prospect collection
A prospect has :mode, :quantity, :stop-loss-price, :target-price and order-price.
The order-price has :open-price which includes brokerage."
  [prospects]
  (->> prospects
       (filter #(and (= :prospect (:status %))
                     (:open-price (:order-price %))))
       (map (fn order [{:keys [mode quantity]
                        {:keys [open-price]} :order-price
                        :as prospect}]
              (let [cost (* quantity (case mode
                                       ;; buy mode, cost of buy
                                       :buy open-price
                                       ;; sell mode, negative cost of sell
                                       :sell (- open-price)))]

                (assoc prospect
                       :status :venture
                       :cost cost))))))

(defn terminate-simulated
  "Terminate the venture bad to continue and register credit and net profit.
=bad-venture= bad venture collection
A venture has :mode, :quantity, :cost and terminate-price.
The terminate-price has :close-price which includes brokerage."
  [bad-ventures]
  (->> bad-ventures
       (filter #(and (= :venture (:status %))
                     (:close-price (:terminate-price %))))
       (map (fn terminate [{:keys [mode quantity cost]
                            {:keys [close-price]} :terminate-price
                            :as venture}]
              (let [;; credit from closing the venture
                    credit (* quantity (case mode
                                         ;; buy order, credit from sell
                                         :buy close-price
                                         ;; sell mode, negative credit from buy
                                         :sell (- close-price)))
                    ;; net profit
                    profit (- credit cost)]
                ;; update ventur with closing info
                (assoc venture
                       :status :terminated
                       :credit credit
                       :profit profit))))))

(defn check-simulated [ventures])

(defn get-simulated-broker []
  {:order order-simulated
   :terminate terminate-simulated
   :check check-simulated})