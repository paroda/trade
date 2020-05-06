(ns bindi.worker
  (:require [clojure.core.async :as a]
            [taoensso.timbre :as log]
            [bindi.config :as cfg]
            [bindi.fx-broker :as fxb])
  (:import [java.util Date]))

(defonce ^:private state (atom {:last-order-id {}
                                :stop-chan nil}))

(defn- trade-instrument [ikey]
  (let [{:keys [order trade closed-trade]} (fxb/get-trade-status ikey)]
    (if-not (or (seq order) (seq trade))
      ;; new trade if no trade pending
      (let [{:keys [profit mode]} (->> closed-trade
                                       (sort-by #(.getTime (:open-time %)) >)
                                       first)
            buy-sell (if (and profit mode)
                       (if (pos? profit)
                         ;; profit => repeat last mode
                         mode
                         ;; loss => try other mode
                         (if (= :buy mode) :sell :buy))
                       ;; no previous record, start with buy
                       :buy)
            lots 10
            entry 1
            limit 10
            stop 5
            oid (fxb/create-order ikey buy-sell lots entry limit stop)]
        (swap! state assoc-in [:last-order-id ikey] oid)
        (log/debug "ordered" ikey buy-sell lots entry limit stop)))))

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
          ms1h (* 60 ms1m)]
      (a/go-loop [ms ms1m]
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
                (recur ms1m))
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

  (->> (fxb/get-trade-status :eur-usd)
       :closed-trade
       (sort-by #(.getTime (:open-time %)) >)
       (map (juxt :open-time :profit)))

  (stop)

  )
