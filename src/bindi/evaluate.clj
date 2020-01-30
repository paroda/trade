(ns bindi.evaluate)

(defn won-lost-simulator
  "buy order is won when price goes above target-price.
buy order is lost when price goes below stop-loss-price.
sell order is won when price goes below target-price.
sell order is lost when price goes above stop-loss-price."
  [{:keys [symbol mode target-price stop-loss-price] :as venture} prices]
  (if-let [{:keys [t o h l]} (get prices symbol)]
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
      (assoc venture :close-time t, :close-price p))))

;; evaluator returns ventures if deemed bad to continue
;; otherise returns nil, when all ventures considered good to continue further
(def evaluators [won-lost-simulator])

(defn make-evaluate
  "Returns an evaluate function based on given evaluators. It considers a
venture to be bad if at least one of the given evaluator considers it bad.
=evaluate= returned function
Evaluate ventures and returns those bad for further continuation
so that those could be closed to optimize profit.
Returns bad ventures."
  [evaluators]
  (fn [current-wealth prices]
    (letfn [(bad-venture [venture prices]
              (some #(% venture prices) evaluators))]
      (->> (:ventures current-wealth)
           (map #(bad-venture % prices))
           (remove nil?)))))