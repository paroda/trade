(ns bindi.fxcm
  (:import [java.text SimpleDateFormat]
           [com.fxcore2 O2GSession O2GTransport IO2GSessionStatus
            O2GRequest O2GResponse IO2GResponseListener
            O2GCandleOpenPriceMode]
           [java.util Calendar Date SimpleTimeZone])
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))

(defprotocol session-protocol
  "*login-params* {:login \"\"
                   :password \"\"
                   :url \"http://www.fxcorporate.com/Hosts.jsp\"
                   :connection \"\" ;; \"Demo\" or \"Real\"
                   :session-id nil
                   :pin nil}"
  (login [this login-params])
  (logout [this])
  ;; returns the underlying fxcore2 session
  (base [this]))

(defn create-session []
  (let [session ^O2GSession (O2GTransport/createSession)
        state (atom {:connected? false, :promise nil})
        me (reify
             ;; make stateful
             clojure.lang.IDeref
             (deref [this] (dissoc @state :promise))
             ;; implement session protocol
             session-protocol
             (login [this login-params]
               (if (:connected? @state)
                 true ;; already logged in
                 (let [p (promise)
                       {:keys [login password url connection]} login-params]
                   (swap! state merge login-params {:promise p})
                   (.login session login password url connection)
                   @p)))
             (logout [this]
               (if (:connected? @state)
                 (let [p (promise)]
                   (swap! state assoc :promise p)
                   (.logout session)
                   @p)
                 ;; already logged out
                 false))
             (base [this] session)
             ;; implement status listener
             IO2GSessionStatus
             (onSessionStatusChanged [this status]
               (log/info "Status:" (str status))
               (case (str status)
                 "TRADING_SESSION_REQUESTED"
                 (let [{:keys [session-id pin]
                        :or {session-id "", pin ""}} @state]
                   (.setTradingSession session session-id pin))
                 "CONNECTED"
                 (-> state
                     (swap! assoc :connected? true)
                     :promise
                     (deliver true))
                 "DISCONNECTED"
                 (-> state
                     (swap! assoc :connected? false)
                     :promise
                     (deliver false))
                 ;; other status changes, ignore
                 nil)
               nil)
             (onLoginFailed [this err]
               (log/error "Login error:" err)))]
    (.subscribeSessionStatus session me)
    me))

(defprotocol listener-protocol
  (request [this req])
  (stop [this]))

(defn create-listener [session]
  (let [session ^O2GSession (base session)
        state (atom {})
        me (reify
             listener-protocol
             (request [this req]
               (let [p (promise)]
                 (swap! state assoc
                        :req-id (.getRequestId ^O2GRequest req)
                        :promise p)
                 (.sendRequest session ^O2GRequest req)
                 @p))
             (stop [this] (.unsubscribeResponse session ^IO2GResponseListener this))
             IO2GResponseListener
             (^void onRequestCompleted [this ^String req-id ^O2GResponse res]
              (when (= (:req-id @state) req-id)
                (deliver (:promise @state) res))
              nil)
             (^void onRequestFailed [this ^String req-id ^String err]
              (when (= (:req-id @state) req-id)
                (if (str/blank? err)
                  (log/info "There is no more data for req:" req-id)
                  (log/warn "Failed req:" req-id ", err:" err))
                (deliver (:promise @state) false))
              nil)
             (^void onTablesUpdates [this ^O2GResponse res] nil))]
    (.subscribeResponse session me)
    me))

(defn collect-candles [reader]
  (if (.isBar reader)
    (vec (for [i (range (.size reader))]
           {:t (.getTime (.getDate reader i)), :v (.getVolume reader i)
            :o (.getBidOpen reader i), :c (.getBidClose reader i)
            :h (.getBidHigh reader i), :l (.getBidLow reader i)}))))

(defn ->Calendar [^Date date-time]
  (doto (Calendar/getInstance
         (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
    (.setTime date-time)))

(defn get-hist-prices
  "Get historical bid prices {:t, :v, :o, :h, :l, :c}
  *instrument*: like EUR/USD
  *timeframe*: m1: 1 minute, H1: 1 hour, D1: 1 day"
  [session instrument timeframe date-from date-to]
  (let [bs ^O2GSession (base session)
        f (.getRequestFactory bs)
        tfrm (-> f .getTimeFrameCollection (.get timeframe))
        req (.createMarketDataSnapshotRequestInstrument f instrument tfrm 300)
        dto (->Calendar date-to)
        dfrom (->Calendar date-from)
        listener (create-listener session)]
    (loop [dto dto
           ps []]
      (.fillMarketDataSnapshotRequestTime f req dfrom dto false
                                          O2GCandleOpenPriceMode/PREVIOUS_CLOSE)
      (if-let [res (request listener req)]
        ;; request complete
        (let [f (.getResponseReaderFactory bs)
              r (.createMarketDataSnapshotReader f res)]
          (if (> (.size r) 0)
            ;; has some rows
            (let  [;; earliest date in retured response would be next To date
                   dto (.getDate r 0)
                   ps1 (collect-candles r)
                   o? (= (:t (last ps1)) (:t (first ps)))
                   ps1 (if o? (butlast ps1) ps1)
                   ps (concat ps1 ps)]
              (log/debug "got hist prices: done" (.size r) (count ps1) (pr-str dto))
              (if (and (seq ps1) (.after dto dfrom))
                ;; partial result, repeat
                (recur dto ps)
                ;; done!
                (do
                  (stop listener)
                  (vec ps))))
            ;; no rows
            (do
              (stop listener)
              (vec ps))))
        ;; request failure
        (do
          (stop listener)
          (vec ps))))))

(comment

  (require 'bindi.config)
  (require 'clojure.edn)
  (bindi.config/init)
  (def session (create-session))
  (let [p (merge (:fxcm @bindi.config/config)
                 (:demo (->> "workspace/fxcm.edn" slurp clojure.edn/read-string)))]
    (login session p))

  (let [y 2020
        inst "GBP/USD"
        sym (.toLowerCase (clojure.string/replace inst "/" "-"))
        out (format "workspace/data/%s-%d.edn" sym y)
        dfrom (.getTime (doto (Calendar/getInstance
                               (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
                          (.clear) (.set y 0 1)))
        dto (.getTime (doto (Calendar/getInstance
                             (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
                        (.clear) (.set (inc y) 0 1)))
        ps (get-hist-prices session inst "m1" dfrom dto)]
    (log/info "total count" (count ps) "from:" (first ps) "to:" (last ps))
    (spit out ps)
    (log/info "wrote to file:" out))

  (logout session)

  )
