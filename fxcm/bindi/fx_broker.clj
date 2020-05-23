(ns bindi.fx-broker
  (:require [clojure.edn :as edn]
            [taoensso.timbre :as log]
            [bindi.config :as cfg]
            [bindi.fxcm :as fxcm])
  (:import [java.util Calendar Date]))

(defonce ^:private session-data
  (agent {:inited? false
          :account {}
          ;; maps of id to entity
          :order {}
          :trade {}
          :closed-trade {}
          ;; offers, trade-settings, price-history by ikey
          :offer {} ;; offer id is ikey
          :trade-settings {}
          :price-history {}
          ;; max count of historical prices
          :price-history-size 1000}))

(defonce ^:private state (atom {:instruments nil
                                :config nil
                                :session nil}))

(defn- update-minute-price-history
  "returns updated prices list
  - new completed 1-minute period would be added to front of list
  - pending 1-minute period would be updated in the metadata
   *prices*: 1 minute bid candles ({:t, :v, :o, :h, :l, :c}, ..)
             ordered recent one first
   *offer*: {:id, :t, :a, :b, :pip, :v}
   *size*: max count of candles in prices"
  [prices offer size]
  ;; (log/info "update-minute-price-history" offer)
  (let [{cp :current, cm :minute} (meta prices)
        {:keys [b v t]} offer
        om (if t (int (/ (.getTime t) 60000)))]
    (if (or (not om) (if cm
                       (< om cm)
                       (if-let [lt (:t (first prices))]
                         (<= om (/ (.getTime lt) 60000)))))
      prices ;; ignore old, return unchanged
      (if (= om cm)
        ;; same minute
        (let [p (-> cp
                    (assoc :v v, :c b)
                    (update :h max b)
                    (update :l min b))]
          (with-meta prices {:current p, :minute cm}))
        ;; new minute or the very first tick
        (let [t (Date. (* om 60000))
              p {:t t, :v v, :o b, :h b, :l b, :c b}
              prices (if cp (take size (conj prices cp)) prices)]
          (with-meta prices {:current p, :minute om}))))))

(defn- update-session-data [session-data op t d]
  (if-not (:inited? session-data)
    ;; skip
    session-data
    ;; update session-data
    (let [add? (#{:insert :update} op)
          ph-size (:price-history-size session-data)
          {:keys [id]} d]
      (cond-> session-data
        ;; update account
        (and add? (= :account t)) (update :account merge d)
        ;; update order, only if not converted to trade or closed-trade
        (and add? (= :order t)
             (not (get-in session-data [:trade (:trade-id d)]))
             (not (get-in session-data [:closed-trade (:trade-id d)])))
        (update-in [:order id] merge d)
        ;; update trade, only if not already closed
        (and add? (= :trade t)
             (not (get-in session-data [:closed-trade id])))
        (-> (update-in [:trade id] merge d)
            ;; remove associated opening order
            (update :order dissoc (:open-order-id d)))
        ;; update closed-trade
        (and add? (= :closed-trade t))
        (-> (update-in [:closed-trade id] merge d)
            ;; remove associated trade
            (update :trade dissoc id)
            ;; remove associated closing order
            (update :order dissoc (:close-order-id d)))
        ;; update offer (must have :t)
        (and add? (= :offer t) (:t d))
        (-> (update-in [:offer id] merge d)
            ;; update price-history for new tick
            (update-in [:price-history id] update-minute-price-history d ph-size))
        ;; delete order, trade
        (and (= :delete op) (#{:order :trade} t)) (update t dissoc id)))))

(defn- handle-row-update
  "*a*: atom to keep all row data
      {:account {}, :offer {}, :order {}, :trade {}, :closed-trade {}
       :price-history-size 1000
       :price-history {<inst-kw> (<bid-candle-1-minute>,..)}}
       price-history is a list with recent one first!
  *op*: operation = :insert, :update or :delete
  *t*: type of row = :account, :offer, :order, :trade, :closed-trade
  *d*: row data"
  [a op t d]
  (when (:inited? @a)
    (send a update-session-data op t d)))

(defn- on-session-lost []
  (when-let [session (:session @state)]
    (fxcm/dispose session)
    (fxcm/dispose (:price-history-communicator @state))
    (swap! state dissoc :session :price-history-communicator)))

(defn- create-session []
  (let [my-config (:fxcm @cfg/config)
        my-insts (->> (:instruments @cfg/config)
                      (map (fn [[ikey {:keys [iname]}]] [ikey iname]))
                      (into {}))
        my-session (fxcm/create-session
                    my-insts
                    (partial handle-row-update session-data)
                    on-session-lost)
        my-ph-communicator (fxcm/create-price-history-communicator my-session)]
    (swap! state assoc
           :config my-config
           :instruments my-insts
           :session my-session
           :price-history-communicator my-ph-communicator)
    my-session))

(defn connect-session []
  (let [session (or (:session @state)
                    (create-session))]
    (if-not (:connected? @session)
      (if (fxcm/login session (:config @state))
        (log/info "login success!")
        (log/error "failed to login!")))
    (:connected? @session)))

(defn end-session []
  (when-let [session (:session @state)]
    (fxcm/logout session)
    (fxcm/dispose session)
    (fxcm/dispose (:price-history-communicator @state))
    (swap! state dissoc :session :price-history-communicator)))

(defn init-session-data []
  (assert (:session @state) "no session")
  (let [{:keys [config session instruments]
         ph-comm :price-history-communicator} @state
        aid (:account-id config)
        f #(->> % (map (fn [d] [(:id d) d])) (into {}))
        acc (get (f (fxcm/get-accounts session)) aid)
        tr-settings (fxcm/get-trading-settings session aid instruments)
        offs (f (fxcm/get-offers session))
        ords (f (fxcm/get-orders session aid))
        trs (f (fxcm/get-trades session aid))
        ctrs (f (fxcm/get-closed-trades session aid))
        phsize (:price-history-size @session-data)
        ph (->> (keys instruments)
                (filter #(get-in @session [:instruments %]))
                (map #(vector % (reverse (fxcm/get-hist-prices
                                          ph-comm % "m1" nil nil phsize))))
                (into {}))]
    (send session-data assoc
          :account acc
          :trade-settings tr-settings
          :offer offs
          :order ords
          :trade trs
          :closed-trade ctrs
          :price-history ph
          :inited? true)))

(defn session-connected? []
  (some->  @state :session deref :connected?))

(defn session-data-inited? []
  (:inited? @session-data))

(defn refresh-offers []
  (assert (session-connected?) "session not connected?")
  (assert (session-data-inited?) "session-data not inited")
  (log/debug "refreshing offers..")
  (let [{:keys [session]} @state
        f #(->> % (map (fn [d] [(:id d) d])) (into {}))
        offs (f (fxcm/get-offers session))]
    (send session-data assoc :offer offs)))

(defn create-order [ikey buy-sell lots entry limit stop]
  (assert (session-connected?) "session not connected?")
  (assert (session-data-inited?) "session-data not inited")
  (let [{:keys [config session]} @state
        aid (:account-id config)
        offer (get-in @session-data [:offer ikey])
        tr-settings (get-in @session-data [:trade-settings ikey])
        otype "SE"]
    (fxcm/create-order-ELS session aid offer tr-settings otype
                           buy-sell lots entry limit stop)))

(defn remove-order [order-id]
  (assert (session-connected?) "session not connected?")
  (assert (session-data-inited?) "session-data not inited")
  (let [{:keys [config session]} @state
        aid (:account-id config)]
    (fxcm/remove-order session aid order-id)))

(defn close-trade [trade-id]
  (assert (session-connected?) "session not connected?")
  (assert (session-data-inited?) "session-data not inited")
  (if-let [trade (get-in @session-data [:trade trade-id])]
    (fxcm/close-trade-at-market (:session @state) trade)))

(defn get-instruments []
  (assert (session-data-inited?) "session-data not inited")
  (keys (:instruments @state)))

(defn get-offers []
  (assert (session-data-inited?) "session-data not inited")
  (:offer @session-data))

(defn get-closed-trades [ikey]
  (assert (session-data-inited?) "session-data not inited")
  (->> @session-data :closed-trade vals
       (filter #(= ikey (:ikey %)))))

(defn get-prices
  "quickly get recent m1 bid candles.
  for current minute candle (incomplete) pick :current from metadata.
  prices are arranged recent first to oldest last."
  [ikey]
  (assert (session-data-inited?) "session-data not inited")
  (get-in @session-data [:price-history ikey]))

(defn get-recent-prices
  "prices are ordered oldest first to recent last.
  always get from server (no caching).
  NOTE: use for small amount (< 300) of candles.
  for larger dataset use (get-hist-prices)"
  [ikey time-frame date-to max-count]
  (fxcm/get-recent-prices (:session @state)
                          ikey time-frame nil date-to max-count))

(defn get-hist-prices
  "prices are ordered oldest first to recent last.
  can be called as many time for large dataset (uses local cache)."
  [ikey time-frame date-to max-count]
  (fxcm/get-hist-prices (:price-history-communicator @state)
                        ikey time-frame nil date-to max-count))

(defn get-trade-status
  "returns {:account, :offer, :order, :trade, :closed-trade, :prices}"
  [ikey]
  (assert (session-data-inited?) "session-data not inited")
  (let [{:keys [account offer order trade closed-trade]} @session-data
        ph (get-in @session-data [:price-history ikey])]
    (letfn [(filter-by-ikey [data]
              (->> (vals data)
                   (filter #(= ikey (:ikey %)))))]
      {:account account
       :offer (get offer ikey)
       :order (filter-by-ikey order)
       :trade (filter-by-ikey trade)
       :closed-trade (filter-by-ikey closed-trade)
       :prices (remove nil? (take 30 (conj ph (:current (meta ph)))))})))

(defn market-open?
  "market is open from Sun 9pm/10pm to Fri 9pm/10pm GMT
   depending on Daylight saving. check the current time."
  []
  (let [now (Calendar/getInstance (java.util.TimeZone/getTimeZone "GMT"))
        h (.get now Calendar/HOUR_OF_DAY)
        d (.get now Calendar/DAY_OF_WEEK)]
    ;; Sun 9pm: [d h] = [1 21]
    ;; Fri 9pm: [d h] = [6 21]
    (or
     ;; Mon - Thu
     (< 1 d 6)
     ;; after Sun 11pm, 1 hour margin
     (and (= d 1) (> h 23))
     ;; before Fri 8pm, 1 hour margin
     (and (= d 6) (< h 20)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment

  (cfg/init)
  (connect-session)
  (init-session-data)

  (end-session)

  @state
  @session-data

  (let [ps (get-in @session-data [:price-history :eur-usd])]
    [(:current (meta ps)) (first ps)])

  (create-order :eur-usd :sell 20 1 30 10)

  (remove-order "117339822")

  (keys (:trade @session-data))
  (close-trade "67310766")

  [(select-keys @session-data [:account :order :trade :closed-trade])
   (:current (meta (get-in @session-data [:price-history :eur-usd])))]

  (:offer @session-data)

  (send session-data assoc :order {})
  (:order @session-data)

  (get-trade-status :eur-usd)

  (:iname (:eur-usd (:instruments @(:session @state))))

  (let [{:keys [config session]} @state
        aid (:account-id config)]
    (fxcm/get-offers (:session @state)))

  (spit (str (:workspace @cfg/config) "/closed-trade.edn")
        (:closed-trade @session-data))

  (get-recent-prices :eur-usd "m5" #inst "2020-05-15T00:00:00.000-00:00" 5)

  (get-hist-prices :eur-usd "m5" #inst "2020-05-15T00:00:00.000-00:00" 5)

  (.. (fxcm/base (:session @state))
      getSessionStatus)

  )
