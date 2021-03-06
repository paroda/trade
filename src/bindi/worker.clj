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
(def ana-time-frame-ms (* 30 60 1000))

(defn update-instrument [ikey attr value]
  {:pre [(case attr
           (:order-wait :lots :entry) (int? value)
           (:limit :stop-max :stop-min :stop-buff) (or (nil? value) (number? value))
           :atr-limit (or (nil? value)
                          (and (coll? value)
                               (every? #(or (nil? %) (number? %)) value)))
           false)]}
  (swap! state assoc-in [:instruments ikey attr] value)
  (get-in @state [:instruments ikey]))

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
        ;; trade attributes, with instrument specific overrides
        {:keys [order-wait lots entry]} (get-in @state [:instruments ikey])
        ;; if already lost a trade with current trend indicator
        ;; then consider this ti bad and wait for next
        tit-eff (+ ana-time-frame-ms (.getTime ^Date tit))
        bad-ti? (some (fn [ct]
                        (and (neg? (:profit ct))
                             (> (.getTime ^Date (:open-time ct)) tit-eff)))
                      closed-trade)
        ;; trade settings for this instrument
        {:keys [order? limit stop-max stop-min stop-buff spread]
         [atr-low atr-high] :atr-limit}
        (get-in @state [:instruments ikey])]
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
      (and mode (not open-order) order? (not bad-ti?))
      (let [pip (:pip offer)
            atr-pips (/ atr pip)
            limit (int (* limit atr-pips))]
        (if (and (or (not atr-high) (> atr-high atr-pips))
                 (or (not atr-low) (< atr-low atr-pips))
                 (> limit (* 3 spread)))
          (let [stop-max (if stop-max (int (* limit stop-max)))
                stop-min (if stop-min (int (* limit stop-min)))
                stop (cond->
                         (int (/ (case mode
                                   :buy (- (:b offer) low-swing)
                                   :sell (- high-swing (:b offer)))
                                 pip))
                       stop-max (min stop-max)
                       stop-min (max stop-min)
                       stop-buff (+ stop-buff))
                oid (if (pos? stop) ;; this check is needed if stop-min is not specified
                      (fxb/create-order ikey mode lots entry limit stop))]
            (when oid
              (swap! state assoc-in [:last-order ikey]
                     {:at (Date.), :id oid, :limit limit, :stop stop})
              (log/debug "ordered:" ikey mode lots entry limit stop))))))))

(defn- trade []
  (let [inst-keys (->> (:instruments @state)
                       (filter (comp :enabled? val))
                       (map key))
        offers (fxb/get-offers)]
    (doseq [ikey inst-keys]
      ;; update tech indicators with latest full candle
      ;; ignore the latest candle being incomplete, take the one before it
      (->> 2
           (fxb/get-hist-prices ikey ana-time-frame nil)
           (take 1)
           (ana/put-quotes ikey))
      ;; trade only when offers are fresh (not older than 15 sec)
      (if (> 15000 (- (.getTime (Date.))
                      (.getTime ^Date (get-in offers [ikey :t]))))
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
           (merge-with merge
                       (zipmap inst-keys (repeat (merge {:enabled? true
                                                         :order? true}
                                                        (:trade @cfg/config))))
                       (select-keys (:instruments @cfg/config) inst-keys))))
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
