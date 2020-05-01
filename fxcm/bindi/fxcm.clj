(ns bindi.fxcm
  (:import [java.text SimpleDateFormat]
           [com.fxcore2
            O2GSession O2GTransport IO2GSessionStatus O2GSessionStatusCode
            O2GRequest O2GResponse O2GResponseType IO2GResponseListener
            O2GRequestFactory O2GResponseReaderFactory
            O2GAccountsTableResponseReader O2GLoginRules
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

;; map of instrument id or key to instrument info
;; instrument info example: {:iid "1", :iname "EUR/USD", :ikey :eur-usd}
(defonce ^:private instrument-map (atom nil))

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

(defn ->Calendar [^Date date-time]
  (doto (Calendar/getInstance
         (SimpleTimeZone. SimpleTimeZone/UTC_TIME "UTC"))
    (.setTime date-time)))

(defn- parse-offer-row [^O2GOfferRow o]
  (if-let [k (if (.isOfferIDValid o)
               (:ikey (get @instrument-map (.getOfferID o))))]
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

(defn- parse-order-row [^O2GOrderRow o]
  {:id (.getOrderID o)
   :aid (.getAccountID o)
   :ikey (:ikey (get @instrument-map (.getOfferID o)))
   :mode ({"S" :sell, "B" :buy} (.getBuySell o))
   :quantity (.getOriginAmount o)
   :closing? (= "C" (.getStage o))
   :status (.getStatus o)
   :peg-offset (.getPegOffset o)
   :rate (.getRate o)
   :trade-id (.getTradeID o)
   :type (.getType o)})

(defn- parse-trade-row [^O2GTradeRow t]
  {:id (.getTradeID t)
   :aid (.getAccountID t)
   :ikey (:ikey (get @instrument-map (.getOfferID t)))
   :mode ({"S" :sell, "B" :buy} (.getBuySell t))
   :quantity (.getAmount t)
   :fee (.getCommission t)
   :open-order-id (.getOpenOrderID t)
   :open-price (.getOpenRate t)
   :open-time (some-> t .getOpenTime .getTime)})

(defn- parse-closed-trade-row [^O2GClosedTradeRow t]
  {:id (.getTradeID t)
   :aid (.getAccountID t)
   :ikey (:ikey (get @instrument-map (.getOfferID t)))
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
  "*login-params* {:login \"\"
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
  "*handle-update* (fn [action type data])
  -- action = :insert, :update or :delete
  -- type = :account, :offer, :order, :trade, :closed-trade"
  [handle-update]
  (let [session ^O2GSession (O2GTransport/createSession)
        state (atom {:connected? false})
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
                            (when-let [t (case (str (.getUpdateType rdr i))
                                           "INSERT" :insert
                                           "UPDATE" :update
                                           "DELETE" :delete
                                           ;; unlikely
                                           nil)]
                              (case (str (.getUpdateTable rdr i))
                                "OFFERS"
                                (some->> (.getOfferRow rdr i)
                                         parse-offer-row
                                         (handle-update t :offer))
                                "ACCOUNTS"
                                (->> (.getAccountRow rdr i)
                                     parse-account-row
                                     (handle-update t :account))
                                "ORDERS"
                                (->> (.getOrderRow rdr i)
                                     parse-order-row
                                     (handle-update t :order))
                                "TRADES"
                                (->> (.getTradeRow rdr i)
                                     parse-trade-row
                                     (handle-update t :trade))
                                "CLOSED_TRADES"
                                (->> (.getClosedTradeRow rdr i)
                                     parse-closed-trade-row
                                     (handle-update t :closed-trade))
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
                   @p)
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
                  (deliver (:status @promises) true)
                  (swap! promises dissoc :status))
                "DISCONNECTED"
                (do
                  (deliver (:status @promises) false)
                  (doseq [p (vals (:request @promises))]
                    (deliver p false))
                  (reset! promises {}))
                ;; other status changes, ignore
                nil))
             (^void onLoginFailed [this ^String err]
              (log/error "Login error:" err)))]
    (.subscribeSessionStatus session me)
    me))

(defn create-price-history-communicator [session]
  (let [communicator ^IPriceHistoryCommunicator
        (PriceHistoryCommunicatorFactory/createCommunicator (base session) "History")
        state (atom {:ready? (.isReady communicator)})
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
                      (log/debug "req complete id" (hash req))
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
             (deref [this] @state)
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
               (log/debug "req id" (hash req))
               (if (:ready? @state)
                 (let [p (promise)]
                   (swap! promises assoc-in [:request (hash req)] p)
                   (.sendRequest communicator req)
                   @p)
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

(defn update-instrument-map [session ikey-iname]
  (let [bs ^O2GSession (base session)
        lr ^O2GLoginRules (.getLoginRules bs)
        rdr-fct ^O2GResponseReaderFactory (.getResponseReaderFactory bs)
        res ^O2GResponse (.getTableRefreshResponse lr O2GTableType/OFFERS)
        rdr ^O2GOffersTableResponseReader (.createOffersTableReader rdr-fct res)
        iid-iname (->> (for [i (range (.size rdr))]
                         (let [o ^O2GOfferRow (.getRow rdr i)
                               iid (if (.isOfferIDValid o) (.getOfferID o))
                               iname (if (.isInstrumentValid o) (.getInstrument o))]
                           (if (and iid iname)
                             [iid iname])))
                       (into {}))
        inst-map (build-instrument-map ikey-iname iid-iname)]
    (reset! instrument-map inst-map)))

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
  [session ikey timeframe date-from date-to]
  (assert (:connected? @session) "Session Disconnected")
  (let [iname (:iname (get @instrument-map ikey))
        bs ^O2GSession (base session)
        req-fct ^O2GRequestFactory (.getRequestFactory bs)
        tfrm ^O2GTimeframe (-> req-fct .getTimeFrameCollection (.get timeframe))
        req ^O2GRequest (.createMarketDataSnapshotRequestInstrument
                         req-fct iname tfrm 300)
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
               (parse-offer-row (.getRow rdr i)))
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
           (parse-order-row (.getRow rdr i))))))))

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
           (parse-trade-row (.getRow rdr i))))))))

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
           (parse-closed-trade-row (.getRow rdr i))))))))

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

(defn get-hist-prices
  "*timeframe*: m1: 1 minute, H1: 1 hour, D1: 1 day
  - candle day closes at 22:00 UTC or 21:00 UTC depending on daylight saving
  - use 22:00 for from-date to include
  - use 20:00 for to-date to exclude"
  [communicator ikey timeframe date-from date-to]
  (assert (:ready? @communicator) "PriceHistoryCommunicator not ready!")
  (assert (not (get @instrument-map ikey)) "Invalid symbol")
  (assert (#{"m1" "H1" "D1"} timeframe) "Invalid timeframe")
  (assert date-from "Missing date-from")
  (assert date-to "Missing date-to")
  (let [iname (:iname (get @instrument-map ikey))
        comm ^IPriceHistoryCommunicator (base communicator)
        tfrm ^O2GTimeframe (-> (.getTimeframeFactory comm)
                               (.create timeframe))
        dfrom ^Calendar (->Calendar date-from)
        dto ^Calendar (->Calendar date-to)]
    (let [req ^IPriceHistoryCommunicatorRequest
          (.createRequest comm iname tfrm dfrom dto -1)
          res ^IPriceHistoryCommunicatorResponse (request communicator req)]
      (if res
        (let [rdr ^O2GMarketDataSnapshotResponseReader
              (.createResponseReader comm res)]
          (collect-candles rdr))))))

(defn- update-minute-price-history [prices offer size])

(defn handle-row-update
  "*a*: atom to keep all row data
  *op*: operation = :insert, :update or :delete
  *t*: type of row = :account, :offer, :order, :trade, :closed-trade
  *d*: row data"
  [a op t d]
  (when (and (:inited? @a) @instrument-map)
    (case op
      (:insert :update) (swap! a update-in [t (:id d)] merge d)
      :delete (if (not= t :closed-trade)
                (swap! a update t dissoc (:id d))))
    (if (= t :offer)
      (swap! a update-in [:price-history (:id d)]
             update-minute-price-history d (:price-history-size @a)))))

(comment

  (require 'bindi.config)
  (require 'clojure.edn)
  (bindi.config/init)

  (def my-insts {:eur-usd "EUR/USD"
                 :eur-gbp "EUR/GBP"
                 :gbp-usd "GBP/USD"})

  (def my-data (atom {;; maps of id to entity
                      :account {}
                      :offer {}
                      :order {}
                      :trade {}
                      :closed-trade {}
                      ;; recent historical prices
                      :price-history-size 1000
                      :price-history {}}))

  (def my-config (:demo (->> "workspace/fxcm.edn" slurp clojure.edn/read-string)))

  (def my-session (create-session (partial handle-row-update my-data)))

  (let [p (merge (:fxcm @bindi.config/config) my-config)]
    (if (login my-session p)
      (update-instrument-map my-session my-insts)
      (log/info "failed to login")))

  @instrument-map

  (log/info "server time:" (-> my-session base .getServerTime .getTime pr-str)
            "local time:" (pr-str (Date.)))

  (let [aid (:account-id my-config)
        f #(->> % (map (fn [d] [(:id d) d])) (into {}))
        acs (f (get-accounts my-session))
        offs (f (get-offers my-session))
        ords (f (get-orders my-session aid))
        trs (f (get-trades my-session aid))
        ctrs (f (get-closed-trades my-session aid))]
    (swap! my-data update :account merge acs)
    (swap! my-data update :offer merge offs)
    (swap! my-data update :order merge ords)
    (swap! my-data update :trade merge trs)
    (swap! my-data update :closed-trade merge ctrs)
    (swap! my-data assoc :inited? true))

  @my-data

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

  (download-reports my-session "workspace/report")

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
                     #inst "2020-01-01T00:00:00.000+00:00"))

  (logout my-session)

  (dispose my-communicator)
  (dispose my-session)

  )
