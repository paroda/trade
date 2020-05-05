(ns bindi.worker
  (:require [clojure.core.async :as a]
            [taoensso.timbre :as log]
            [bindi.config :as cfg]
            [bindi.fx-broker :as fxb]))

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
            stop 3
            oid (fxb/create-order ikey buy-sell lots entry limit stop)]
        (swap! state assoc-in [:last-order-id ikey] oid)
        (log/debug "ordered" ikey buy-sell lots entry limit stop)))))

(defn- trade []
  (doseq [ikey (fxb/get-instruments)]
    (trade-instrument ikey)))

(defn stop []
  (if-let [stop-chan (:stop-chan @state)]
    (a/put! stop-chan :stop)))

(defn start []
  (if-not (:stop-chan @state)
    (let [stop-chan (a/chan)
          ms 60000]
      (a/go-loop []
        (let [[_ ch] (a/alts! [stop-chan (a/timeout ms)]
                              :priority true)]
          (if (= ch stop-chan)
            ;; abort
            (do
              (log/info "aborted worker!")
              (swap! state dissoc :stop-chan))
            ;; trade
            (do
              (trade-instrument :eur-usd)
              (recur)))))
      (swap! state assoc :stop-chan stop-chan)
      (log/info "started worker.."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (cfg/init)
  (fxb/init-session)
  (fxb/init-session-data)

  (start)

  @state

  (fxb/get-trade-status :eur-usd)

  (stop)

  )
