(ns bindi.evaluate)

(defn won-lost-simulator
  "buy order is won when price goes above target-price.
buy order is lost when price goes below stop-loss-price.
Actual closing price of buy order is either target price,
or the starting price if it starts higher than target-price.

sell order is won when price goes below target-price.
sell order is lost when price goes above stop-loss-price.
Actual closing price of sell order is either the target price,
or the starting price if it starts lower than target-price.

Brokerage is applied to the closing price."
  [{:keys [symbol mode target-price stop-loss-price] :as venture} prices]
  (let [{:keys [o h l b] :as price} (get prices symbol)]
    (if-let [p (case mode
                 ;; buy order, sell out to close
                 :buy (if (>= h target-price)
                        (max o target-price)
                        (if (<= l stop-loss-price)
                          (min o stop-loss-price)))
                 ;; sell order, buy back to close
                 :sell (if (<= l target-price)
                         (min o target-price)
                         (if (>= h stop-loss-price)
                           (max o stop-loss-price))))]
      (assoc venture :terminate-price
             (assoc price :close-price
                    (case mode
                      ;; buy order, effective sell price reduced by brokerage
                      :buy (- p b)
                      ;; sell order, effective buy price increased by brokerage
                      :sell (+ p b)))))))

;; evaluator returns ventures with terminate-price if deemed bad to continue
;; otherise returns nil, when all ventures considered good to continue further
(def evaluators [won-lost-simulator])

(defn make-evaluate
  "Returns an evaluate function based on given evaluators.
=evaluate= returned function
Evaluate ventures and add a termination-price to 
ventures bad for further continuation
so that those could be closed to optimize profit.
Returns bad ventures."
  [evaluators]
  (fn [current-wealth prices]
    (letfn [(bad-venture [venture prices]
              (some #(% venture prices) evaluators))]
      (->> (:ventures current-wealth)
           (map #(bad-venture % prices))
           (remove nil?)))))