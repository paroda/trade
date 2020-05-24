(ns bindi.fxcm
  (:import [java.text SimpleDateFormat]
           [com.fxcore2
            Constants$Commands Constants$Orders
            O2GSession O2GTransport IO2GSessionStatus O2GSessionStatusCode
            O2GRequest O2GResponse O2GResponseType IO2GResponseListener
            O2GRequestFactory O2GResponseReaderFactory
            O2GValueMap O2GRequestParamsEnum
            O2GPermissionChecker O2GPermissionStatus
            O2GAccountsTableResponseReader O2GLoginRules
            O2GOrderResponseReader O2GCommandResponseReader
            O2GOrdersTableResponseReader O2GOffersTableResponseReader
            O2GClosedTradesTableResponseReader O2GTradesTableResponseReader
            O2GTablesUpdatesReader O2GMarketDataSnapshotResponseReader
            O2GAccountRow O2GOrderRow O2GTradeRow O2GClosedTradeRow O2GOfferRow
            O2GTimeframe O2GCandleOpenPriceMode O2GTableType O2GTableUpdateType
            O2GTradingSettingsProvider O2GMarketStatus O2GHtmlContentUtils]
           [com.candleworks.pricehistorymgr
            IPriceHistoryCommunicator PriceHistoryError
            IPriceHistoryCommunicatorRequest IPriceHistoryCommunicatorResponse
            IPriceHistoryCommunicatorListener IPriceHistoryCommunicatorStatusListener
            PriceHistoryCommunicatorFactory]
           [java.util Calendar Date SimpleTimeZone])
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))

;; you can deref session or communicator to obtain instrument mappings
;; like (:insturments @session) or (:instruments @communicator)
;; which will return a map of instrument id or key to instrument info
;; instrument info example: {:iid "1", :iname "EUR/USD", :ikey :eur-usd}

(defn- build-instrument-map [ikey-iname iid-iname]
  (let [m (zipmap (vals ikey-iname) (keys ikey-iname))]
    (->> iid-iname
         (mapcat (fn [[iid iname]]
                   (if-let [ikey (get m iname)]
                     (let [m {:iid iid, :iname iname, :ikey ikey}]
                       [[iid m] [ikey m]]))))
         (into {}))))

(comment
  (= (build-instrument-map {:eur-usd "EUR/USD", :eur-gbp "EUR/GBP"}
                           {"1" "EUR/USD", "2" "EUR/GBP"})
     {"1" {:iid "1", :iname "EUR/USD", :ikey :eur-usd}
      "2" {:iid "2", :iname "EUR/GBP", :ikey :eur-gbp}
      :eur-usd {:iid "1", :iname "EUR/USD", :ikey :eur-usd}
      :eur-gbp {:iid "2", :iname "EUR/GBP", :ikey :eur-gbp}})
  )

(defn- load-instrument-map [^O2GSession base-session ikey-iname]
  (let [lr ^O2GLoginRules (.getLoginRules base-session)
        rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory base-session)
        res ^O2GResponse (.getTableRefreshResponse lr O2GTableType/OFFERS)
        rdr ^O2GOffersTableResponseReader (.createOffersTableReader rdr-fct res)
        iid-iname (->> (for [i (range (.size rdr))]
                         (let [o ^O2GOfferRow (.getRow rdr i)
                               iid (if (.isOfferIDValid o) (.getOfferID o))
                               iname (if (.isInstrumentValid o) (.getInstrument o))]
                           (if (and iid iname)
                             [iid iname])))
                       (into {}))]
    (build-instrument-map ikey-iname iid-iname)))

(defn ->Calendar [^Date date-time]
  (doto (Calendar/getInstance
         (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
    (.setTime date-time)))

(defn- parse-offer-row
  "returns {:id <instrument keyword>
            :t <timestamp>
            :a <ask price>
            :b <bid price>
            :pip <pip size>
            :v <minute volume>}"
  [instrument-map ^O2GOfferRow o]
  (if-let [k (if (.isOfferIDValid o)
               (:ikey (get instrument-map (.getOfferID o))))]
    (->> [(if (.isTimeValid o) [:t (some-> o .getTime .getTime)])
          (if (.isAskValid o) [:a (.getAsk o)])
          (if (.isBidValid o) [:b (.getBid o)])
          (if (.isPointSizeValid o) [:pip (.getPointSize o)])
          ;; the volume is of the current minute
          (if (.isVolumeValid o) [:v (.getVolume o)])]
         (remove nil?)
         (into {:id k}))))

(defn- parse-account-row [^O2GAccountRow a]
  {:id (.getAccountID a)
   :limit (.getAmountLimit a)
   :balance (.getBalance a)})

(defn- parse-order-row [instrument-map ^O2GOrderRow o]
  {:id (.getOrderID o)
   :aid (.getAccountID o)
   :ikey (:ikey (get instrument-map (.getOfferID o)))
   :mode ({"S" :sell, "B" :buy} (.getBuySell o))
   :quantity (.getOriginAmount o)
   :closing? (= "C" (.getStage o))
   :status (.getStatus o)
   :peg-offset (.getPegOffset o)
   :rate (.getRate o)
   :trade-id (.getTradeID o)
   :type (.getType o)})

(defn- parse-trade-row [instrument-map ^O2GTradeRow t]
  {:id (.getTradeID t)
   :aid (.getAccountID t)
   :ikey (:ikey (get instrument-map (.getOfferID t)))
   :mode ({"S" :sell, "B" :buy} (.getBuySell t))
   :quantity (.getAmount t)
   :fee (.getCommission t)
   :open-order-id (.getOpenOrderID t)
   :open-price (.getOpenRate t)
   :open-time (some-> t .getOpenTime .getTime)})

(defn- parse-closed-trade-row [instrument-map ^O2GClosedTradeRow t]
  {:id (.getTradeID t)
   :aid (.getAccountID t)
   :ikey (:ikey (get instrument-map (.getOfferID t)))
   :mode ({"S" :sell, "B" :buy} (.getBuySell t))
   :quantity (.getAmount t)
   :fee (.getCommission t)
   :profit (.getGrossPL t)
   :open-order-id (.getOpenOrderID t)
   :open-price (.getOpenRate t)
   :open-time (some-> t .getOpenTime .getTime)
   :close-order-id (.getCloseOrderID t)
   :close-price (.getCloseRate t)
   :close-time (some-> t .getCloseTime .getTime)})

(defn- collect-candles [^O2GMarketDataSnapshotResponseReader reader]
  (if (.isBar reader)
    (vec (for [i (range (.size reader))]
           {:t (.getTime (.getDate reader i)), :v (.getVolume reader i)
            :o (.getBidOpen reader i), :c (.getBidClose reader i)
            :h (.getBidHigh reader i), :l (.getBidLow reader i)}))))

(defprotocol session-protocol
  "methods to be supported by the session
  *login-params* {:login \"\"
                  :password \"\"
                  :url \"http://www.fxcorporate.com/Hosts.jsp\"
                  :connection \"\" ;; \"Demo\" or \"Real\"
                  :session-id nil
                  :pin nil}"
  (login [this login-params])
  (logout [this])
  (dispose [this])
  ;; returns the underlying fxcore2 session, used for creating request objects
  (base [this])
  (request [this req]))

(defn create-session
  "*ikey-iname*: map of instrument keywords to names
   *handle-update* (fn [action type data])
   -- action = :insert, :update or :delete
   -- type = :account, :offer, :order, :trade, :closed-trade"
  ([ikey-iname handle-update]
   (create-session ikey-iname handle-update #(log/warn "Lost FXCM session")))
  ([ikey-iname handle-update on-session-lost]
   (let [session ^O2GSession (O2GTransport/createSession)
         state (atom {:connected? false, :instruments {}})
         promises (atom {:status nil, :request {}})
         listener (letfn [(res-fn [^String req-id ^O2GResponse res]
                            (when-let [p (get-in @promises [:request req-id])]
                              (swap! promises update :request dissoc req-id)
                              (deliver p res)))]
                    (reify
                      IO2GResponseListener
                      (^void onRequestCompleted [this ^String req-id ^O2GResponse res]
                       (res-fn req-id res))
                      (^void onRequestFailed [this ^String req-id ^String err]
                       (if (str/blank? err)
                         (log/info "There is no more data for req:" req-id)
                         (log/warn "Failed req:" req-id ", err:" err))
                       (res-fn req-id nil))
                      (^void onTablesUpdates [this ^O2GResponse res]
                       (if (= (.getType res) O2GResponseType/TABLES_UPDATES)
                         (let [rdr-fct ^O2GResponseReaderFactory
                               (.getResponseReaderFactory session)
                               rdr ^O2GTablesUpdatesReader
                               (.createTablesUpdatesReader rdr-fct res)]
                           (doseq [i (range (.size rdr))]
                             (when-let [op (case (str (.getUpdateType rdr i))
                                             "INSERT" :insert
                                             "UPDATE" :update
                                             "DELETE" :delete
                                             ;; unlikely
                                             nil)]
                               (case (str (.getUpdateTable rdr i))
                                 "OFFERS"
                                 (some->> (.getOfferRow rdr i)
                                          (parse-offer-row (:instruments @state))
                                          (handle-update op :offer))
                                 "ACCOUNTS"
                                 (->> (.getAccountRow rdr i)
                                      parse-account-row
                                      (handle-update op :account))
                                 "ORDERS"
                                 (->> (.getOrderRow rdr i)
                                      (parse-order-row (:instruments @state))
                                      (handle-update op :order))
                                 "TRADES"
                                 (->> (.getTradeRow rdr i)
                                      (parse-trade-row (:instruments @state))
                                      (handle-update op :trade))
                                 "CLOSED_TRADES"
                                 (->> (.getClosedTradeRow rdr i)
                                      (parse-closed-trade-row (:instruments @state))
                                      (handle-update op :closed-trade))
                                 ;; ignore others
                                 nil))))))))
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
                    (if (deref p 60000 nil)
                      ;; login success - load instruments mappings
                      (let [insts (load-instrument-map session ikey-iname)]
                        (swap! state assoc :instruments insts)
                        true)
                      ;; login failed
                      false))))
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
                    (deref p 60000 nil))))
              (dispose [this]
                (.unsubscribeSessionStatus session ^IO2GSessionStatus this)
                (.dispose session))
              (base [this] session)
              (request [this req]
                (if (:connected? @state)
                  (let [p (promise)]
                    (swap! promises assoc-in
                           [:request (.getRequestId ^O2GRequest req)]
                           p)
                    (.sendRequest session ^O2GRequest req)
                    (deref p 60000 nil))
                  (throw (Exception. "Not connected!"))))
              ;;
              ;; implement status listener
              ;;
              IO2GSessionStatus
              (^void onSessionStatusChanged [this ^O2GSessionStatusCode status]
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
                   (some-> (:status @promises) (deliver true))
                   (swap! promises dissoc :status))
                 "DISCONNECTED"
                 (do
                   (some-> (:status @promises) (deliver false))
                   (doseq [p (vals (:request @promises))]
                     (deliver p false))
                   (reset! promises {}))
                 "SESSION_LOST"
                 (do
                   (swap! state assoc :connected? false)
                   (.unsubscribeResponse session ^IO2GResponseListener listener)
                   (doseq [p (vals (:request @promises))]
                     (deliver p false))
                   (reset! promises {})
                   (on-session-lost))
                 ;; other status changes, ignore
                 nil))
              (^void onLoginFailed [this ^String err]
               (log/error "Login error:" err)))]
     (.subscribeSessionStatus session me)
     me)))

(defn create-price-history-communicator [session]
  (let [communicator ^IPriceHistoryCommunicator
        (PriceHistoryCommunicatorFactory/createCommunicator (base session) "History")
        state (atom {:ready? (.isReady communicator), :instruments {}})
        promises (atom {:request {}})
        listener (letfn [(res-fn [^IPriceHistoryCommunicatorRequest req
                                  ^IPriceHistoryCommunicatorResponse res]
                           (when-let [p (get-in @promises [:request (hash req)])]
                             (swap! promises update request dissoc (hash req))
                             (deliver p res)))]
                   (reify
                     IPriceHistoryCommunicatorListener
                     (^void onRequestCompleted
                      [this
                       ^IPriceHistoryCommunicatorRequest req
                       ^IPriceHistoryCommunicatorResponse res]
                      (log/trace "req complete id" (hash req))
                      (res-fn req res))
                     (^void onRequestFailed
                      [this
                       ^IPriceHistoryCommunicatorRequest req
                       ^PriceHistoryError err]
                      (log/error err
                                 "PriceHistoryCommunicator request failed!"
                                 (hash req))
                      (res-fn req nil))
                     (^void onRequestCancelled
                      [this ^IPriceHistoryCommunicatorRequest req]
                      (log/info "PriceHistoryCommunicator request cancelled!"
                                (hash req))
                      (res-fn req nil))))
        me (reify
             clojure.lang.IDeref
             (deref [this] (assoc @state :instruments (:instruments @session)))
             session-protocol
             (dispose [this]
               (.removeListener
                communicator ^IPriceHistoryCommunicatorListener listener)
               (.removeStatusListener
                communicator ^IPriceHistoryCommunicatorStatusListener this)
               (.dispose communicator)
               nil)
             (base [this] communicator)
             (request [this req]
               (log/trace "req id" (hash req))
               (if (:ready? @state)
                 (let [p (promise)]
                   (swap! promises assoc-in [:request (hash req)] p)
                   (.sendRequest communicator req)
                   (deref p 60000 nil))
                 (throw (Exception. "PriceHistoryCommunicator not ready!"))))
             IPriceHistoryCommunicatorStatusListener
             (^void onCommunicatorStatusChanged [this ^boolean ready?]
              (log/info "PriceHistoryCommunicator ready?" ready?)
              (swap! state assoc :ready? ready?))
             (^void onCommunicatorInitFailed [this ^PriceHistoryError err]
              (log/error err "PriceHistoryCommunicator initialization error!")))]
    (.addStatusListener communicator me)
    (.addListener communicator listener)
    me))

(defn get-trading-settings [session account-id ikey-iname]
  (let [bs ^O2GSession (base session)
        lr ^O2GLoginRules (.getLoginRules bs)
        rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory bs)
        res ^O2GResponse (.getTableRefreshResponse lr O2GTableType/ACCOUNTS)
        rdr ^O2GAccountsTableResponseReader (.createAccountsTableReader rdr-fct res)
        a ^O2GAccountRow (->> (range (.size rdr))
                              (some #(let [a ^O2GAccountRow (.getRow rdr %)]
                                       (if (= (.getAccountID a) account-id) a))))
        tsp ^O2GTradingSettingsProvider (.getTradingSettingsProvider lr)]
    (->> ikey-iname
         (map (fn [[ikey iname]]
                (let [ms ^O2GMarketStatus (.getMarketStatus tsp iname)
                      o? (= ms O2GMarketStatus/MARKET_STATUS_OPEN)]
                  [ikey
                   {:ikey ikey
                    :iname iname
                    :stop-tr (.getCondDistStopForTrade tsp iname)
                    :limit-tr (.getCondDistLimitForTrade tsp iname)
                    :stop (.getCondDistEntryStop tsp iname)
                    :limit (.getCondDistEntryLimit tsp iname)
                    :q-min (.getMinQuantity tsp iname a)
                    :q-max (.getMaxQuantity tsp iname a)
                    :size (.getBaseUnitSize tsp iname a)
                    :mmr (.getMMR tsp iname a)
                    :open? o?}])))
         (into {}))))

(defn get-recent-prices
  "Get historical bid prices {:t, :v, :o, :h, :l, :c}
  *ikey*: instrument key like :eur-usd
  *timeframe*: m1: 1 minute, H1: 1 hour, D1: 1 day
  NOTE: Do NOT use for big data!"
  ([session ikey timeframe date-from date-to]
   (get-recent-prices session ikey timeframe date-from date-to nil))
  ([session ikey timeframe date-from date-to max-count]
   (assert (:connected? @session) "Session Disconnected")
   (let [iname (get-in @session [:instruments ikey :iname])
         bs ^O2GSession (base session)
         req-fct ^O2GRequestFactory (.getRequestFactory bs)
         tfrm ^O2GTimeframe (-> req-fct .getTimeFrameCollection (.get timeframe))
         req ^O2GRequest (.createMarketDataSnapshotRequestInstrument
                          req-fct iname tfrm (min 300 (or max-count 300)))
         dto ^Calendar (->Calendar (or  date-to (Date.)))
         d0 ^Calendar (->Calendar (Date. 0))
         dfrom ^Calendar (if date-from (->Calendar date-from))]
     (loop [dto dto
            ps []]
       (.fillMarketDataSnapshotRequestTime req-fct req (or dfrom d0) dto false
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
                    ps (cond->> (concat ps1 ps)
                         max-count (take max-count))]
               (log/trace "got recent prices: done"
                          ", count:" (count ps1)
                          ", starting at:" (pr-str dto))
               (if (and (seq ps1)
                        (or dfrom max-count)
                        (if dfrom (.after dto dfrom) true)
                        (if max-count (< (count ps) max-count) true))
                 ;; partial result, repeat
                 (recur dto ps)
                 ;; done!
                 (vec ps)))
             ;; no rows
             (vec ps)))
         ;; request failure
         (vec ps))))))

(defn get-offers
  "Get latest price offers"
  [session]
  (assert (:connected? @session) "Session Disconnected")
  (let [bs ^O2GSession (base session)
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        req ^O2GRequest (.createRefreshTableRequest req-fct O2GTableType/OFFERS)
        res ^O2GResponse (request session req)]
    (if res
      (let [rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory bs)
            rdr ^O2GOffersTableResponseReader (.createOffersTableReader
                                               rdr-fct res)]
        (->> (for [i (range (.size rdr))]
               (parse-offer-row (:instruments @session) (.getRow rdr i)))
             (remove nil?)
             vec)))))

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
           (parse-account-row (.getRow rdr i))))))))

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
           (parse-order-row (:instruments @session) (.getRow rdr i))))))))

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
           (parse-trade-row (:instruments @session) (.getRow rdr i))))))))

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
           (parse-closed-trade-row (:instruments @session) (.getRow rdr i))))))))

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
  (assert (:connected? @session) "Session Disconnected")
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

(defn get-hist-prices
  "*timeframe*: m1: 1 minute, m5: 5 minute, H1: 1 hour, D1: 1 day
  - candle day closes at 22:00 UTC or 21:00 UTC depending on daylight saving
  - use 22:00 for from-date to include
  - use 20:00 for to-date to exclude"
  [communicator ikey timeframe date-from date-to max-count]
  (assert (:ready? @communicator) "PriceHistoryCommunicator not ready!")
  (assert (get-in @communicator [:instruments ikey]) "Invalid instrument")
  (assert (#{"m1" "m5" "m30" "H1" "D1"} timeframe) "Invalid timeframe")
  (assert (or date-from max-count) "at least date-from or max-count required")
  (let [max-count (or max-count -1)
        iname (get-in @communicator [:instruments ikey :iname])
        comm ^IPriceHistoryCommunicator (base communicator)
        tfrm ^O2GTimeframe (-> (.getTimeframeFactory comm)
                               (.create timeframe))
        dfrom ^Calendar (if date-from (->Calendar date-from))
        dto ^Calendar (if date-to (->Calendar date-to))]
    (let [req ^IPriceHistoryCommunicatorRequest
          (.createRequest comm iname tfrm dfrom dto max-count)
          res ^IPriceHistoryCommunicatorResponse (request communicator req)]
      (if res
        (let [rdr ^O2GMarketDataSnapshotResponseReader
              (.createResponseReader comm res)]
          (collect-candles rdr))))))

(defn create-order-ELS
  "Order an instrument with Limit and Stop traling entry.
  Returns the order id on success, else nil
  *offer*, *tr-settings*: for the desired instrument
  *order-type*: LE or SE
  *op*: :buy or :sell
  *lots*: lots to order
  *entry*: rate to enter, in terms of pips from market price
  *limit*, *stop*: rate in terms of pips from entry rate"
  [session account-id offer tr-settings
   order-type op lots entry limit stop]
  (assert (#{"LE" "SE"} order-type) "order-type must be LE or SE")
  (assert (#{:buy :sell} op) "op must be :buy or :sell")
  (assert (every? pos? [lots entry limit stop])
          "lots entry limit stop must be positive integers")
  (let [iid (get-in @session [:instruments (:id offer) :iid])
        amount (* (:size tr-settings) lots)
        {:keys [a b pip]} offer
        [r l s] (case op
                  :buy (let [e ((case order-type "LE" - +) a (* entry pip))]
                         [e
                          (+ e (* limit pip))
                          (- e (* stop pip))])
                  :sell (let [e ((case order-type "LE" + -) b (* entry pip))]
                          [e
                           (- e (* limit pip))
                           (+ e (* stop pip))]))
        bs ^O2GSession (base session)
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        value-map ^O2GValueMap
        (doto (.createValueMap req-fct)
          (.setString O2GRequestParamsEnum/COMMAND Constants$Commands/CreateOrder)
          (.setString O2GRequestParamsEnum/ORDER_TYPE order-type)
          (.setString O2GRequestParamsEnum/ACCOUNT_ID account-id)
          (.setString O2GRequestParamsEnum/OFFER_ID iid)
          (.setString O2GRequestParamsEnum/BUY_SELL (case op :buy "B" "S"))
          (.setInt O2GRequestParamsEnum/AMOUNT amount)
          (.setDouble O2GRequestParamsEnum/RATE r)
          (.setDouble O2GRequestParamsEnum/RATE_LIMIT l)
          (.setDouble O2GRequestParamsEnum/RATE_STOP s)
          (.setString O2GRequestParamsEnum/CUSTOM_ID "create-order-ELS"))
        req ^O2GRequest (.createOrderRequest req-fct value-map)]
    (if req
      (if-let [res ^O2GResponse (request session req)]
        (let [rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory bs)
              rdr ^O2GOrderResponseReader (.createOrderResponseReader rdr-fct res)]
          (.getOrderID rdr))
        (log/error "order creation failed!"))
      (log/error "can not create order request:" (.getLastError req-fct)))))

(defn remove-order [session account-id order-id]
  (let [bs (base session)
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        value-map ^O2GValueMap
        (doto (.createValueMap req-fct)
          (.setString O2GRequestParamsEnum/COMMAND Constants$Commands/DeleteOrder)
          (.setString O2GRequestParamsEnum/ACCOUNT_ID account-id)
          (.setString O2GRequestParamsEnum/ORDER_ID order-id)
          (.setString O2GRequestParamsEnum/CUSTOM_ID "remove-oder"))
        req ^O2GRequest (.createOrderRequest req-fct value-map)]
    (if req
      (request session req)
      (log/error "can not create remove order request:" (.getLastError req-fct)))))

(defn close-trade-at-market [session trade]
  (let [bs (base session)
        {:keys [ikey id aid mode quantity]} trade
        {:keys [iname iid]} (get-in @session [:instruments ikey])
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        lr ^O2GLoginRules (.getLoginRules bs)
        pchkr ^O2GPermissionChecker (.getPermissionChecker lr)
        ce? (= O2GPermissionStatus/PERMISSION_ENABLED
               (.canCreateMarketCloseOrder pchkr iname))
        ;; _ (log/info "ce?:" ce?)
        value-map ^O2GValueMap
        (doto (.createValueMap req-fct)
          (.setString O2GRequestParamsEnum/COMMAND Constants$Commands/CreateOrder)
          (.setString O2GRequestParamsEnum/ORDER_TYPE (if ce? "CM" "OM"))
          ;; (.setString O2GRequestParamsEnum/ORDER_TYPE "CM")
          ;; (.setString O2GRequestParamsEnum/TRADE_ID id)
          (.setString O2GRequestParamsEnum/ACCOUNT_ID aid)
          (.setString O2GRequestParamsEnum/OFFER_ID iid)
          (.setString O2GRequestParamsEnum/BUY_SELL (case mode :buy "S" "B"))
          (.setInt O2GRequestParamsEnum/AMOUNT quantity)
          (.setString O2GRequestParamsEnum/CUSTOM_ID "close-trade-at-market"))
        _ (if ce? (.setString value-map O2GRequestParamsEnum/TRADE_ID id))
        req ^O2GRequest (.createOrderRequest req-fct value-map)]
    (if req
      (request session req)
      (log/error "can not create close trade request:" (.getLastError req-fct)))))

(comment

  (require 'bindi.config)
  (require 'clojure.edn)
  (bindi.config/init)

  (def my-insts {:eur-usd "EUR/USD"
                 ;; :eur-gbp "EUR/GBP"
                 :gbp-usd "GBP/USD"})

  (def my-config (:demo (->> "workspace/fxcm.edn" slurp clojure.edn/read-string)))

  (def my-session (create-session my-insts (constantly nil)))

  (let [p (merge (:fxcm @bindi.config/config) my-config)]
    (if (login my-session p)
      (log/info "login success")
      (log/info "failed to login")))

  @my-session

  (log/info "server time:" (-> my-session base .getServerTime .getTime pr-str)
            "local time:" (pr-str (Date.)))

  (let [dto (Date.)
        dfrom (.getTime (doto (Calendar/getInstance)
                          (.add Calendar/DAY_OF_MONTH -1)))]
    (get-recent-prices my-session :eur-usd "D1" dfrom dto))

  (let [y 2020, m 0, d0 8, d1 9
        ikey :eur-usd
        out (format "workspace/misc/%s-%d-%d-%d_%d.edn" (name ikey) y m d0 d1)
        dfrom (.getTime (doto (Calendar/getInstance
                               (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
                          (.clear) (.set y m d0)))
        dto (.getTime (doto (Calendar/getInstance
                             (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
                        (.clear) (.set y m d1)))
        ps (get-recent-prices my-session ikey "m1" dfrom dto)]
    (log/info "total count" (count ps) "from:" (first ps) "to:" (last ps))
    (spit out ps)
    (log/info "wrote to file:" out))

  (download-reports my-session "workspace/reports")

  (get-offers my-session)

  (get-accounts my-session)

  (get-trading-settings my-session (:account-id my-config) my-insts)

  (get-orders my-session (:account-id my-config))

  (get-trades my-session (:account-id my-config))

  (get-closed-trades my-session (:account-id my-config))

  (def my-communicator (create-price-history-communicator my-session))

  (def d
    (get-hist-prices my-communicator :eur-usd "m1"
                     #inst "2019-01-01T00:00:00.000+00:00"
                     #inst "2020-01-01T00:00:00.000+00:00"
                     -1))

  (logout my-session)

  (dispose my-communicator)
  (dispose my-session)

  )
