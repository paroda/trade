(ns bindi.worker
  (:require [clojure.core.async :as a]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [bindi.config :as cfg]
            [bindi.log :as log-cfg]
            [bindi.fx-broker :as fxb]
            [bindi.analysis :as ana]
            [bindi.indicator :as ind])
  (:import [java.util Date]))

(defonce ^:private state (atom {:stop-chan nil}))

(def ana-max-count 100)

(def ana-time-frame "H1")

(defn- trade-instrument [ikey]
  (let [{:keys [atr high-swing low-swing]
         {:keys [mode]} :trade} (first (ana/get-indicators ikey))
        {:keys [offer order trade]} (fxb/get-trade-status ikey)
        open-order (some #(if (false? (:closing? %)) %) order)
        {oid :id} open-order
        t (if oid (or (get-in @state [:last-order ikey :at]) (Date.)))
        odt (if t (int (/ (- (.getTime (Date.)) (.getTime t)) 60000)))
        ;; wait time in minutes
        order-wait 3
        lots 10, entry 1]
    (cond
      ;; trade placed clear state of last-order
      (first trade)
      (swap! state update :last-order dissoc ikey)
      ;; cancel order if waited too long
      (and open-order (< order-wait odt))
      (try
        (log/info "order pending too long. canceling:" oid)
        (fxb/remove-order oid)
        (swap! state update :last-order dissoc ikey)
        (catch Exception ex
          (log/warn "failed to cancel order, error:"
                    (.getName (type ex)) (.getMessage ex))))
      ;; new trade if no trade pending, and trade signal present
      (and mode (not open-order))
      (let [limit (int (/ atr (:pip offer)))
            stop (+ 3 (int (/ (case mode
                                :buy (- (:a offer) low-swing)
                                :sell (- high-swing (:b offer)))
                              (:pip offer))))
            oid (fxb/create-order ikey mode lots entry limit stop)]
        (when oid
          (swap! state assoc-in [:last-order ikey]
                 {:at (Date.), :id oid, :limit limit, :stop stop})
          (log/debug "ordered:" ikey mode lots entry limit stop))))))

(defn- trade []
  ;; ignore the latest one as incomplete
  (let [quotes (take 1 (fxb/get-hist-prices :eur-usd ana-time-frame nil 2))]
    (ana/put-quotes :eur-usd quotes))
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
          ms-idle (* 60 60 1000)
          ms-active (* 15 1000)]
      (a/go-loop [ms ms-active]
        (let [[_ ch] (a/alts! [stop-chan (a/timeout ms)]
                              :priority true)]
          (if (= ch stop-chan)
            ;; abort
            (do
              (log/info "aborted worker!")
              (swap! state dissoc :stop-chan))
            ;; carry on
            (do
              ;; ensure connection
              (if-not (fxb/session-connected?)
                (fxb/connect-session))
              ;; trade when market open
              (if (fxb/market-open?)
                ;; trade
                (do
                  (trade)
                  (recur ms-active))
                ;; snooze 1 hour
                (recur ms-idle))))))
      (swap! state assoc :stop-chan stop-chan)
      (log/info "started worker.."))))

(defn init-ind-hist []
  ;; ignore the latest one as incomplete
  (let [quotes (butlast
                (fxb/get-hist-prices :eur-usd ana-time-frame nil
                                     (+ ana-max-count ana/lead-ti-count)))]
    (ana/put-quotes :eur-usd quotes)))

(defn init []
  (fxb/connect-session)
  (fxb/init-session-data)
  (ana/init ana-max-count)
  (init-ind-hist)
  (start))

(defn exit []
  (stop)
  (fxb/end-session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (fxb/end-session)

  (start)

  @state
  (reset! state nil)
  (stop)

  [(:last-order @state)
   (:trade (fxb/get-trade-status :eur-usd))
   [[(Date.)]]
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

  (count (ana/get-indicators :eur-usd))

  )
