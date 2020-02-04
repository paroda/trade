(ns bindi.fxcm
  (:import [java.text SimpleDateFormat]
           [com.fxcore2
            O2GSession O2GTransport IO2GSessionStatus
            O2GRequest O2GResponse IO2GResponseListener
            O2GRequestFactory O2GResponseReaderFactory
            O2GOrdersTableResponseReader O2GTradesTableResponseReader
            O2GAccountsTableResponseReader O2GMarketDataSnapshotResponseReader
            O2GClosedTradesTableResponseReader
            O2GAccountRow O2GOrderRow O2GTradeRow O2GClosedTradeRow
            O2GTimeframe O2GCandleOpenPriceMode
            O2GTableType O2GHtmlContentUtils]
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
  ;; returns the underlying fxcore2 session, used for creating request objects
  (base [this])
  (request [this req]))

(defn create-session []
  (let [session ^O2GSession (O2GTransport/createSession)
        state (atom {:connected? false})
        promises (atom {:status nil, :request {}})
        listener (reify
                   IO2GResponseListener
                   (^void onRequestCompleted [this ^String req-id ^O2GResponse res]
                    (when-let [p (get-in @promises [:request req-id])]
                      (swap! promises update :request dissoc req-id)
                      (deliver p res))
                    nil)
                   (^void onRequestFailed [this ^String req-id ^String err]
                    (when-let [p (get-in @promises [:request req-id])]
                      (if (str/blank? err)
                        (log/info "There is no more data for req:" req-id)
                        (log/warn "Failed req:" req-id ", err:" err))
                      (swap! promises update :request dissoc req-id)
                      (deliver p false))
                    nil)
                   (^void onTablesUpdates [this ^O2GResponse res] nil))
        me (reify
             ;; make stateful
             clojure.lang.IDeref
             (deref [this] @state)
             ;;
             ;; implement session protocol
             ;;
             session-protocol
             (login [this login-params]
               (if (:connected? @state)
                 true ;; already logged in
                 ;; try login if not busy
                 (let [p (promise)]
                   (if (= p (-> promises
                                (swap! #(if (:status %) % (assoc % :status p)))
                                :status))
                     (let [{:keys [login password url connection]} login-params]
                       (swap! state merge login-params)
                       (.login session login password url connection))
                     ;; already busy!
                     (throw (Exception. "Already pending with previous operation!")))
                   @p)))
             (logout [this]
               (if-not (:connected? @state)
                 ;; already logged out
                 false
                 ;; try logout if not busy
                 (let [p (promise)]
                   (if (= p (-> promises
                                (swap! #(if (:status %) % (assoc % :status p)))
                                :status))
                     (do
                       (swap! state assoc :connected? false)
                       (.unsubscribeResponse session ^IO2GResponseListener listener)
                       (.logout session))
                     ;; already busy!
                     (throw (Exception. "Already pending with previous operation!")))
                   @p)))
             (base [this] session)
             (request [this req]
               (if (:connected? @state)
                 (let [p (promise)]
                   (swap! promises assoc-in
                          [:request (.getRequestId ^O2GRequest req)]
                          p)
                   (.sendRequest session ^O2GRequest req)
                   @p)
                 (throw (Exception. "Not connected!"))))
             ;;
             ;; implement status listener
             ;;
             IO2GSessionStatus
             (onSessionStatusChanged [this status]
               (log/info "FXCM session status:" (str status))
               (case (str status)
                 "TRADING_SESSION_REQUESTED"
                 (let [{:keys [session-id pin]
                        :or {session-id "", pin ""}} @state]
                   (.setTradingSession session session-id pin))
                 "CONNECTED"
                 (do
                   (.subscribeResponse session listener)
                   (swap! state assoc :connected? true)
                   (deliver (:status @promises) true)
                   (swap! promises dissoc :status))
                 "DISCONNECTED"
                 (do
                   (deliver (:status @promises) false)
                   (doseq [p (vals (:request @promises))]
                     (deliver p false))
                   (reset! promises {}))
                 ;; other status changes, ignore
                 nil)
               nil)
             (onLoginFailed [this err]
               (log/error "Login error:" err)))]
    (.subscribeSessionStatus session me)
    me))

(defn ->Calendar [^Date date-time]
  (doto (Calendar/getInstance
         (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
    (.setTime date-time)))

(defn collect-candles [reader]
  (if (.isBar reader)
    (vec (for [i (range (.size reader))]
           {:t (.getTime (.getDate reader i)), :v (.getVolume reader i)
            :o (.getBidOpen reader i), :c (.getBidClose reader i)
            :h (.getBidHigh reader i), :l (.getBidLow reader i)}))))

(defn get-hist-prices
  "Get historical bid prices {:t, :v, :o, :h, :l, :c}
  *instrument*: like EUR/USD
  *timeframe*: m1: 1 minute, H1: 1 hour, D1: 1 day"
  [session instrument timeframe date-from date-to]
  (assert (:connected? @session) "Session Disconnected")
  (let [bs ^O2GSession (base session)
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        tfrm ^O2GTimeframe (-> req-fct .getTimeFrameCollection (.get timeframe))
        req ^O2GRequest (.createMarketDataSnapshotRequestInstrument
                         req-fct instrument tfrm 300)
        dto ^Calendar (->Calendar date-to)
        dfrom ^Calendar (->Calendar date-from)]
    (loop [dto dto
           ps []]
      (.fillMarketDataSnapshotRequestTime req-fct req dfrom dto false
                                          O2GCandleOpenPriceMode/PREVIOUS_CLOSE)
      (if-let [res ^O2GResponse (request session req)]
        ;; request complete
        (let [rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory bs)
              rdr ^O2GMarketDataSnapshotResponseReader
              (.createMarketDataSnapshotReader rdr-fct res)]
          (if (> (.size rdr) 0)
            ;; has some rows
            (let  [;; earliest date in retured response would be next To date
                   dto (.getDate rdr 0)
                   ps1 (collect-candles rdr)
                   o? (= (:t (last ps1)) (:t (first ps)))
                   ps1 (if o? (butlast ps1) ps1)
                   ps (concat ps1 ps)]
              (log/debug "got hist prices: done"
                         ", count:" (count ps1)
                         ", starting at:" (pr-str dto))
              (if (and (seq ps1) (.after dto dfrom))
                ;; partial result, repeat
                (recur dto ps)
                ;; done!
                (vec ps)))
            ;; no rows
            (vec ps)))
        ;; request failure
        (vec ps)))))

(defn get-accounts
  [session]
  (assert (:connected? @session) "Session Disconnected")
  (let [bs ^O2GSession (base session)
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        req ^O2GRequest (.createRefreshTableRequest req-fct O2GTableType/ACCOUNTS)
        res ^O2GResponse (request session req)]
    (if res
      (let [rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory bs)
            rdr ^O2GAccountsTableResponseReader (.createAccountsTableReader
                                                 rdr-fct res)]
        (vec
         (for [i (range (.size rdr))]
           (let [a ^O2GAccountRow (.getRow rdr i)]
             {:id (.getAccountID a)
              :limit (.getAmountLimit a)
              :balance (.getBalance a)})))))))

(defn get-orders
  "Get pending orders"
  [session account-id]
  (assert (:connected? @session) "Session Disconnected")
  (let [bs ^O2GSession (base session)
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        req ^O2GRequest (.createRefreshTableRequestByAccount
                         req-fct O2GTableType/ORDERS account-id)
        res ^O2GResponse (request session req)]
    (if res
      (let [rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory bs)
            rdr ^O2GOrdersTableResponseReader (.createOrdersTableReader
                                               rdr-fct res)]
        (vec
         (for [i (range (.size rdr))]
           (let [o ^O2GOrderRow (.getRow rdr i)]
             {:id (.getOrderID o)
              :status (.getStatus o)
              :action (.getBuySell o)
              :quantity (.getOriginAmount o)
              :peg-offset (.getPegOffset o)
              :rate (.getRate o)
              :stage (.getStage o) ;; order is placed to open (O) or close (C)
              :trade-id (.getTradeID o)
              :type (.getType o)})))))))

(defn get-trades
  "Get open positions"
  [session account-id]
  (assert (:connected? @session) "Session Disconnected")
  (let [bs ^O2GSession (base session)
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        req ^O2GRequest (.createRefreshTableRequestByAccount
                         req-fct O2GTableType/TRADES account-id)
        res ^O2GResponse (request session req)]
    (if res
      (let [rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory bs)
            rdr ^O2GTradesTableResponseReader (.createTradesTableReader
                                               rdr-fct res)]
        (vec
         (for [i (range (.size rdr))]
           (let [t ^O2GTradeRow (.getRow rdr i)]
             {:id (.getTradeID t)
              :quantity (.getAmount t)
              :mode (.getBuySell t)
              :fee (.getCommission t)
              :open-order-id (.getOpenOrderID t)
              :open-price (.getOpenRate t)
              :open-time (some-> t .getOpenTime .getTime)})))))))

(defn get-closed-trades
  "Get closed positions in current day"
  [session account-id]
  (assert (:connected? @session) "Session Disconnected")
  (let [bs ^O2GSession (base session)
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        req ^O2GRequest (.createRefreshTableRequestByAccount
                         req-fct O2GTableType/CLOSED_TRADES account-id)
        res ^O2GResponse (request session req)]
    (if res
      (let [rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory bs)
            rdr ^O2GClosedTradesTableResponseReader (.createClosedTradesTableReader
                                                     rdr-fct res)]
        (vec
         (for [i (range (.size rdr))]
           (let [t ^O2GClosedTradeRow (.getRow rdr i)]
             {:id (.getTradeID t)
              :quantity (.getAmount t)
              :mode (.getBuySell t)
              :fee (.getCommission t)
              :profit (.getGrossPL t)
              :open-order-id (.getOpenOrderID t)
              :open-price (.getOpenRate t)
              :open-time (some-> t .getOpenTime .getTime)
              :close-order-id (.getCloseOrderID t)
              :close-price (.getCloseRate t)
              :close-time (some-> t .getCloseTime .getTime)})))))))

(defn get-content [^java.net.URL url]
  (let [conn (.openConnection url)
        in (java.io.BufferedReader.
            (java.io.InputStreamReader.
             (.getInputStream conn)))
        res (java.lang.StringBuilder.)]
    (loop [l ""]
      (-> res
          (.append l)
          (.append "\n"))
      (if l
        (recur (.readLine in))
        (.toString res)))))

(defn download-reports [session out-dir]
  (assert @(:connected? session) "Session Disconnected")
  (let [bs (base session)
        lr (.getLoginRules bs)
        rf (.getResponseReaderFactory bs)
        ares (.getTableRefreshResponse lr O2GTableType/ACCOUNTS)
        ardr (.createAccountsTableReader rf ares)]
    (doseq [i (range (.size ardr))]
      (let [a (.getRow ardr i)
            dfrom (doto (Calendar/getInstance)
                    (.add Calendar/MONTH -1))
            url (.getReportURL bs
                               (.getAccountID a)
                               dfrom
                               (Calendar/getInstance)
                               "html"
                               nil)
            fname (str (.getAccountID a) ".html")
            fname (str out-dir "/" fname)
            _ (log/info "AccountID:" (.getAccountID a)
                        ", Balance:" (.getBalance a)
                        ", Report URL:" url)
            url (java.net.URL. url)
            content (get-content url)
            prefix (format "%s://%s/" (.getProtocol url) (.getHost url))
            report (O2GHtmlContentUtils/replaceRelativePathWithAbsolute content prefix)]
        (spit fname report)
        (log/info "Report is saved to" fname)))
    (.size ardr)))

(comment

  (require 'bindi.config)
  (require 'clojure.edn)
  (bindi.config/init)
  (def my-session (create-session))
  (let [p (merge (:fxcm @bindi.config/config)
                 (:demo (->> "workspace/fxcm.edn" slurp clojure.edn/read-string)))]
    (login my-session p))

  (let [y 2020
        inst "EUR/USD"
        sym (.toLowerCase (clojure.string/replace inst "/" "-"))
        out (format "workspace/data/%s-%d.edn" sym y)
        dfrom (.getTime (doto (Calendar/getInstance
                               (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
                          (.clear) (.set y 0 1)))
        dto (.getTime (doto (Calendar/getInstance
                             (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
                        (.clear) (.set (inc y) 0 1)))
        ps (get-hist-prices my-session inst "m1" dfrom dto)]
    (log/info "total count" (count ps) "from:" (first ps) "to:" (last ps))
    (spit out ps)
    (log/info "wrote to file:" out))

  (download-reports my-session "workspace/report")

  (get-accounts my-session)

  (if-let [aid (:id (first (get-accounts my-session)))]
    (get-orders my-session aid))

  (if-let [aid (:id (first (get-accounts my-session)))]
    (get-trades my-session aid))

  (if-let [aid (:id (first (get-accounts my-session)))]
    (get-closed-trades my-session aid))

  (logout my-session)

  )
