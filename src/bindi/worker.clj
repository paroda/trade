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

(defonce ^:private state (atom {:stop-chan nil
                                :instruments {}}))

(def ana-max-count 100)

(def ana-time-frame "m30")

(defn pause-instrument [ikey]
  (swap! state update :instruments
         #(cond-> %
            (get-in % [ikey :enabled?]) (assoc-in [ikey :order?] false)))
  (get-in @state [:instruments ikey]))

(defn resume-instrument [ikey]
  (swap! state update :instruments
         #(cond-> %
            (get-in % [ikey :enabled?]) (assoc-in [ikey :order?] true)))
  (get-in @state [:instruments ikey]))

(defn- trade-instrument [ikey]
  (let [{:keys [atr high-swing low-swing]
         tit :t
         {:keys [mode]} :trade} (first (ana/get-indicators ikey))
        {:keys [offer order trade closed-trade]} (fxb/get-trade-status ikey)
        open-order (some #(if (false? (:closing? %)) %) order)
        {oid :id} open-order
        ot (if oid (or (get-in @state [:last-order ikey :at]) (Date.)))
        odt (if ot (int (/ (- (.getTime (Date.)) (.getTime ot)) 60000)))
        {:keys [order-wait lots entry]} (:trade @cfg/config)
        ;; if already lost a trade with current trend indicator
        ;; then consider this ti bad and wait for next
        bad-ti? (some (fn [ct]
                        (and (neg? (:profit ct))
                             (> (.getTime ^Date (:open-time ct))
                                (.getTime ^Date tit))))
                      closed-trade)]
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
      (and mode (not open-order)
           (get-in @state [:instruments ikey :order?]))
      (let [limit (int (/ atr (:pip offer)))
            stop (+ 3 (int (/ (max atr
                                   (case mode
                                     :buy (- (:a offer) low-swing)
                                     :sell (- high-swing (:b offer))))
                              (:pip offer))))
            oid (if (and (> stop limit 3) (not bad-ti?))
                  (fxb/create-order ikey mode lots entry limit stop))]
        (when oid
          (swap! state assoc-in [:last-order ikey]
                 {:at (Date.), :id oid, :limit limit, :stop stop})
          (log/debug "ordered:" ikey mode lots entry limit stop))))))

(defn- trade []
  (let [inst-keys (->> (:instruments @state)
                       (filter (comp :enabled? val))
                       (map key))
        offers (select-keys (fxb/get-offers) inst-keys)
        now (.getTime (Date.))]
    ;; ensure offers are recent, within last 1 minute
    (if (some #(< 60000 (- now (.getTime (:t %))))
              (vals offers))
      ;; offers are stale, refresh
      (fxb/refresh-offers)
      ;; ok to trade
      (doseq [ikey inst-keys]
        ;; update tech indicators with latest full candle
        ;; ignore the latest candle being incomplete, take the one before it
        (->> 2
             (fxb/get-hist-prices ikey ana-time-frame nil)
             (take 1)
             (ana/put-quotes ikey))
        (trade-instrument ikey)))))

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

(defn init []
  ;; setup trade session
  (fxb/connect-session)
  (fxb/init-session-data)
  (Thread/sleep 1000) ;; give 1 sec for session data agent to be ready
  (if (not (fxb/session-data-inited?))
    (throw (Exception. "Session data not initialized!")))
  ;; initialize analysis
  (let [inst-keys (fxb/get-instruments)]
    (ana/init inst-keys ana-max-count)
    (doseq [ikey inst-keys]
      ;; initialize tech indicators using full candles only
      ;; ignore the latest one as it is incomplete, use all before it
      (->> (+ 1 ana-max-count ana/lead-ti-count)
           (fxb/get-hist-prices ikey ana-time-frame nil)
           butlast
           (ana/put-quotes ikey)))
    (swap! state assoc :instruments
           (zipmap inst-keys (repeat {:enabled? true
                                      :order? true}))))
  ;; start the worker
  (start))

(defn exit []
  (stop)
  (fxb/end-session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn my-summary [ikey k]
  (case k
    :ct1
    (let [ts (fxb/get-trade-status ikey)]
      [(:last-order @state)
       (:offer ts)
       (:trade ts)
       [[(Date.)]]
       (->> ts
            :closed-trade
            (sort-by #(.getTime (:open-time %)) >)
            (map (juxt :close-time :profit #_ :open-time)))])
    :p1
    (->> (fxb/get-trade-status ikey)
         :closed-trade
         (sort-by #(.getTime (:close-time %)) >)
         (partition-by #(.getDate (:close-time %)))
         (map #(list (.getDate (:close-time (first %)))
                     (count %)
                     (reduce + (map :profit %)))))
    :p2
    (->> (fxb/get-trade-status ikey)
         :closed-trade
         (sort-by #(.getTime (:open-time %)) >)
         (map (juxt :profit
                    (fn [{:keys [close-time open-time]}]
                      (int (/ (- (.getTime close-time)
                                 (.getTime open-time))
                              60000)))))
         (map #(apply format " %6.2f %5d " %)))
    ;; else
    ["invalid key! should be one of:" ]))

(comment

  (fxb/end-session)

  (start)

  @state
  (reset! state nil)
  (stop)

  (dissoc (fxb/get-trade-status :eur-usd) :closed-trade :prices)

  (my-summary :eur-usd :ct1)

  (my-summary :eur-usd :p1)

  (take 5 (:prices (fxb/get-trade-status :eur-usd)))

  (stop)

  (count (ana/get-indicators :eur-usd))

  )
