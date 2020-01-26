(ns bindi.scout)

(def ^:private counter (atom 0))
(defn new-prospect-id [] (str "p" (swap! counter inc)))

(defn- weigh
  "Weigh several prospects for a symbol and return a single prospect."
  [prospects price]
  (let [wfn #(->> % (map :weight) (reduce +))
        ;; buy prospects
        bs (filter #(= :buy (:mode %)) prospects)
        bw (wfn bs)
        ;; sell-prospects
        ss (filter #(= :sell (:mode %)) prospects)
        sw (wfn ss)
        ;; decide buy or sell
        mode (if (pos? (max bw sw))
               (if (> bw sw) :buy :sell))
        ps (case mode :buy bs, :sell ss, nil)]
    (if mode
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
            {:keys [c b]} price
            sane? (case mode
                    :buy (and (> tp c slp)
                              (> (- tp c) (* 3 (max b (- c slp)))))
                    :sell (and (> slp c tp)
                               (> (- c tp) (* 3 (max b (- slp c))))))]
        (if sane?
          (let [;; price with brokerage
                p (case mode
                    :buy (+ c b)
                    :sell (- c b))]
            {:status :prospect
             :mode mode, :quantity q
             :target-price tp, :stop-loss-price slp
             :order-price (assoc price :open-price p)}))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example scout function

(defn make-daily-scout [symbol scout-id mode quantity
                        stop-loss-margin target-margin]
  (fn [current-wealth prices]
    (let [{:keys [t c]} (get prices symbol)
          d (:data current-wealth)]
      (when (and t c d (< (* 24 3600e3)
                          (- (.getTime t)
                             (get-in @d [scout-id :lts] 0))))
        (swap! d assoc-in [scout-id :lts] (.getTime t))
        [{:mode mode, :quantity quantity
          :stop-loss-price ((case mode :buy - :sell +) c stop-loss-margin)
          :target-price ((case mode :buy + :sell -) c target-margin)
          :symbol symbol, :scout-id scout-id}]))))

(def a1 (make-daily-scout :eur-usd :a1 :buy 10 0.00500 0.05000))
(def a2 (make-daily-scout :eur-usd :a2 :sell 10 0.00500 0.05000))

;; scout returns prospects with order-price for probable profit
;; returns nil if nothing found profitable
;; A prospect has :mode, :quantity, :target-price, :stop-loss-price
;; The order-price has :open-price which includes brokerage
;; The prospect also has :id, :scout-id and :symbol for identification
(def scouts [a1 a2])

(def weights {:eur-usd {:a1 1}})
