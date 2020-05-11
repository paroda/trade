(ns bindi.worker
  (:require [clojure.core.async :as a]
            [taoensso.timbre :as log]
            [bindi.config :as cfg]
            [bindi.fx-broker :as fxb])
  (:import [java.util Date]))

(defonce ^:private state (atom {:last-order {}
                                :stop-chan nil}))

(defn- trade-instrument [ikey]
  (let [{oid :id, t :at, last-mode :mode, cancelled? :cancelled?}
        (get-in @state [:last-order ikey])
        odt (if t (int (/ (- (.getTime (Date.)) (.getTime t)) 60000)))
        {:keys [offer order trade closed-trade prices]} (fxb/get-trade-status ikey)
        hl-range (if (seq prices)
                   (let [[hs ls] (->> (take 5 prices)
                                      (map (juxt :h :l))
                                      (apply map list))]
                     (/ (- (apply max hs) (apply min ls)) (:pip offer))))
        open-order (some #(if (= oid (:id %)) %) order)
        open-trade (some #(if (= oid (:open-order-id %)) %) trade)
        tdt (if-let [t (:open-time open-trade)]
              (int (/ (- (.getTime (Date.)) (.getTime t)) 60000)))
        dp (if-let [p (:open-price open-trade)]
             (case last-mode
               :buy (/ (- (:b offer) p) (:pip offer))
               :sell (/ (- p (:a offer)) (:pip offer))))
        ;; wait time in minutes
        order-wait 2
        lots 10, entry 1
        limit 6, limit-2h 5, limit-2l 3, limit-wait 15
        stop 10, stop-2h 6, stop-2l 3, stop-wait 45]
    (cond
      ;; new trade if no trade pending and no high fluctuations
      (not (or open-order open-trade (> hl-range stop)))
      (let [{:keys [profit mode]} (->> closed-trade
                                       (sort-by #(.getTime (:open-time %)) >)
                                       first)
            buy-sell (if cancelled?
                       (case last-mode :buy :sell :buy)
                       (if (and profit mode)
                         (if (pos? profit)
                           ;; profit => repeat last mode
                           mode
                           ;; loss => try other mode
                           (case mode :buy :sell :buy))
                         ;; no previous record, start with buy
                         :buy))
            oid (fxb/create-order ikey buy-sell lots entry limit stop)]
        (swap! state assoc-in [:last-order ikey]
               {:at (Date.)
                :id oid, :mode buy-sell, :lots lots
                :entry entry, :limit limit, :stop stop})
        (log/debug "ordered" ikey buy-sell lots entry limit stop))
      ;; cancel order if waited too long
      (and open-order (< order-wait odt))
      (try
        (log/info "order pending too long. canceling:" oid)
        (fxb/remove-order oid)
        (swap! state assoc-in [:last-order ikey :cancelled?] true)
        (catch Exception ex
          (log/warn "failed to cancel order, error:"
                    (.getName (type ex)) (.getMessage ex))))
      ;; pre-close open trade if waited too long and price is ok
      (and false open-trade
           ;; TODO: currently disabled
           ;; think some other strategy to escape
           ;; like if there is a strong trend in opposite direction
           ;; then break and place order in other direction
           ;; but, beware it may be at peak and do a u-turn
           (or
            ;; waited too long and currently good profit
            (and (< limit-wait tdt) (< limit-2l dp limit-2h))
            ;; waited too long and currently low loss
            (and (< stop-wait tdt) (< stop-2l (- dp) stop-2h))))
      (do
        (log/info "aborting trade" ikey dp tdt)
        (fxb/close-trade (:id open-trade))))))

(defn- trade []
  #_(doseq [ikey (fxb/get-instruments)]
      (trade-instrument ikey))
  (if (let [now (.getTime (Date.))]
        (->> (vals (fxb/get-offers))
             (every? #(< 60000 (- now (.getTime (:t %)))))))
    ;; stale offers data, refresh
    (fxb/refresh-offers)
    ;; ok to trade
    (trade-instrument :eur-usd)))

(defn stop []
  (if-let [stop-chan (:stop-chan @state)]
    (a/put! stop-chan :stop)))

(defn start []
  (if-not (:stop-chan @state)
    (let [stop-chan (a/chan)
          ms1m 60000
          ms1h (* 60 ms1m)
          ms-p 5000]
      (a/go-loop [ms ms-p]
        (let [[_ ch] (a/alts! [stop-chan (a/timeout ms)]
                              :priority true)]
          (if (= ch stop-chan)
            ;; abort
            (do
              (log/info "aborted worker!")
              (swap! state dissoc :stop-chan))
            ;; carry on
            (if (fxb/market-open?)
              ;; trade
              (do
                (if (fxb/session-inited?) (trade))
                (recur ms-p))
              ;; snooze 1 hour
              (recur ms1h)))))
      (swap! state assoc :stop-chan stop-chan)
      (log/info "started worker.."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (cfg/init)
  (fxb/init-session)
  (fxb/init-session-data)

  (fxb/end-session)

  (start)

  @state

  [[[(Date.)]]
   (->> (fxb/get-trade-status :eur-usd)
        :closed-trade
        (sort-by #(.getTime (:open-time %)) >)
        (map (juxt :close-time :profit #_ :open-time)))]

  (->> (fxb/get-trade-status :eur-usd)
       :closed-trade
       (sort-by #(.getTime (:close-time %)) >)
       (partition-by #(.getDate (:close-time %)))
       (map #(list (.getDate (:close-time (first %)))
                   (count %)
                   (reduce + (map :profit %)))))

  (->> (fxb/get-trade-status :eur-usd)
       :closed-trade
       (sort-by #(.getTime (:open-time %)) >)
       (map (juxt :profit
                  (fn [{:keys [close-time open-time]}]
                    (int (/ (- (.getTime close-time)
                               (.getTime open-time))
                            60000)))))
       (map #(apply format " %6.2f %5d " %)))

  (take 5 (:prices (fxb/get-trade-status :eur-usd)))

  (stop)

  )
