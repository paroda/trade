(ns bindi.scout)

(def ^:private counter (atom 0))
(defn new-prospect-id [] (str "p" (swap! counter inc)))

(defn- weigh
  "Weigh several prospects for a symbol and return a single prospect."
  [prospects price]
  (let [wfn #(->> %
                  (map (fn [{:keys [chance weight]}]
                         [(* chance weight) weight]))
                  (apply map list)
                  (map (partial reduce +))
                  (apply /)
                  (double))
        ;; buy prospects
        bs (filter #(= :buy (:mode %)) prospects)
        bw (if (seq bs) (wfn bs) 0)
        ;; sell-prospects
        ss (filter #(= :sell (:mode %)) prospects)
        sw (if (seq ss) (wfn ss) 0)
        ;; decide buy or sell
        [mode ps] (if (> bw sw) [:buy bs] [:sell ss])
        chance (max bw sw)]
    (if (> chance 0.5)
      (let [;; decide quantity, target-price, stop-price
            [ws qs tps slps] (->> ps
                                  (map (juxt :weight :quantity :target-price :stop-loss-price))
                                  (apply map list))
            tw (reduce + ws)
            q (Math/round (double (/ (reduce + (map * ws qs)) tw)))
            tp (double (/ (reduce + (map * ws tps)) tw))
            slp (double (/ (reduce + (map * ws slps)) tw))
            ;; check sanity
            ;; * price within target-price and stop-loss-price
            ;; * target margin at least thrice stop-loss margin
            {:keys [t c]} price
            sane? (case mode
                    :buy (and (> tp c slp)
                              (> (- tp c) (* 3 (- c slp))))
                    :sell (and (> slp c tp)
                               (> (- c tp) (* 3 (- slp c)))))]
        (if sane?
          {:status :prospect, :chance chance
           :mode mode, :quantity q
           :target-price tp, :stop-loss-price slp
           :open-time t, :open-price c})))))

(defn make-scout
  "Returns a scout function combining given scouts and their weights.
=scout= returned function
Analyze current-wealth and latest price to find out
prospects to venture in, which may return profit in future upon termination.
Returns prospects (only one prospect for each symbol)."
  [id scouts weights]
  ;; use prospect for whom weight is specified
  (fn [current-wealth prices]
    (->> scouts
         (mapcat #(% current-wealth prices))
         (filter #(pos? (:quantity %)))
         (map (fn [{:keys [symbol scout-id] :as prospect}]
                (assoc prospect :weight (get-in weights [symbol scout-id]))))
         (filter :weight)
         (group-by :symbol)
         (map (fn [[symbol prospects]]
                (some-> (weigh prospects (get prices symbol))
                        (assoc :symbol symbol
                               :scout-id id
                               :id (new-prospect-id)))))
         (remove nil?))))
