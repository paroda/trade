(ns bindi.worker
  (:require [clojure.core.async :as a]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [uncomplicate.commons.core :as ucc]
            [uncomplicate.neanderthal.core :as unc]
            [uncomplicate.neanderthal.native :as unn]
            [bindi.config :as cfg]
            [bindi.log :as log-cfg]
            [bindi.nn :as nn]
            [bindi.fx-broker :as fxb]
            [bindi.analysis :as ana]
            [bindi.indicator :as ind])
  (:import [java.util Date]))

(defonce ^:private state (atom {:stop-chan nil
                                :instruments {}}))

(comment

  (with-open [w (io/writer "workspace/cts-eur-usd.edn" :append true)]
    (binding [*out* w]
      (doseq [ct (fxb/get-closed-trades :eur-usd)]
        (prn ct))))

  ;; nn-data
  ;; {:keys [inference training last-infered last-trained
  ;;         re-train-after-hours re-infer-after-minutes
  ;;         next-col ;; x,y column index to be replaced with new training data
  ;;         time-frames
  ;;         x-train y-train epochs eta lambda mu]}

  (let [ikey :eur-usd
        tframes [["m5" 30]]
        yprofit {:buy [1 0], :sell [0 1]}
        yloss {:buy [0 1], :sell [1 0]}]
    (spit "workspace/demo/train.edn"
          (vec
           (for [{:keys [profit mode open-time]}
                 (->> (fxb/get-trade-status ikey) :closed-trade)]
             [(->> tframes
                   (mapcat (fn [[tf n]]
                             (fxb/get-recent-prices ikey tf open-time n)))
                   (mapcat (juxt :o :h :l :c))
                   vec)
              (get (if (pos? profit) yprofit yloss) mode)]))))

  (->> "workspace/demo/train.edn" slurp clojure.edn/read-string first)

  (let [[xs ys] (->> "workspace/demo/train.edn" slurp clojure.edn/read-string
                     (apply map list))
        in-dim (count (first xs))
        out-dim (count (first ys))
        n (count ys)]
    (ucc/with-release [x (unc/ge unn/native-float in-dim n xs)
                       y (unc/ge unn/native-float out-dim n ys)
                       inference (nn/inference-network
                                  unn/native-float in-dim
                                  [(nn/fully-connected 200 nn/tanh)
                                   (nn/fully-connected 400 nn/tanh)
                                   (nn/fully-connected 800 nn/tanh)
                                   (nn/fully-connected 200 nn/tanh)
                                   (nn/fully-connected 50 nn/tanh)
                                   (nn/fully-connected 2 nn/sigmoid)])
                       training (nn/training-network inference x)]
      (nn/init! inference)
      [[in-dim out-dim n]
       {:untrained (unc/transfer (inference x))
        ;; :cost0 (nn/quadratic-cost! (unc/axpy -1.0 y (inference x)))
        :cost (vec (nn/sgd training y nn/quadratic-cost!
                           [[1000 [0.05 0.1 0.9]]
                            [1000 [0.05 0.1 0.9]]
                            [1000 [0.05 0.1 0.9]]]))
        :trained (mapv list (flatten ys)
                       (flatten (seq (unc/transfer (inference x)))))}]))

  )

(defn- init-instrument-nn [ikey]
  (let [nn-file (get-in @cfg/config [:instruments ikey :nn-file])])
  ;; initialize :nn in state atom for this instrument
  ;; - load inference model from file
  ;;   model info includes in-dim, layer-wise out-dim and activ-fn
  ;; - load x-train and y-train along with epochs eta lambda mu
  ;; - setup training
  ;; - next-col = 0
  )

(defn- save-instrument-nn [ikey nn-data]
  ;;  * get x data from last-infered and build y based on profit/loss and mode
  ;;  * also replace one column from x-train and y-train with this
  ;;  * save the model data to file
  ;; return updated nn-data
  (let [nn-file (get-in @cfg/config [:instruments ikey :nn-file])]
    nn-data))

(defn- analyze-instrument-nn [ikey nn-data]
  ;;  * analyze past prices with nn to predict next mode of trade
  ;;    obtain prices needed for inference, keep it in last-infered
  ;;  * re-analyze after expiry (3 min)
  ;; return updated nn-data
  (let []
    nn-data))

(defn- order-instrument-nn [ikey idata]
  ;; return updated idata
  idata)

;; nn-data
;; {:keys [inference training last-infered last-trained
;;         re-train-after-hours re-infer-after-minutes
;;         next-col ;; x,y column index to be replaced with new training data
;;         time-frames
;;         x-train y-train epochs eta lambda mu]}

(defn- trade-instrument-nn
  "returns updated idata or nil if no changes"
  [ikey {nn-data :nn, {ot :t, oid :id} :last-order, :as idata}]
  (let [now (.getTime (Date.))
        odt (if ot (int (/ (- now (.getTime ot)) 60000)))
        {:keys [order trade closed-trade]} (fxb/get-trade-status ikey)
        order (some #(if (= oid (:id %)) %) order)
        trade (some #(if (= oid (:open-order-id %)) %) trade)
        closed-trade (some #(if (= oid (:open-order-id %)) %) closed-trade)]
    (if (or order trade)
      ;; cancel order if waited too long (5 min)
      (when (< 5 odt)
        (try
          (log/info "order pending too long. canceling:" oid)
          (fxb/remove-order oid)
          (dissoc idata :last-order)
          (catch Exception ex
            (log/warn "failed to cancel order, error:"
                      (.getName (type ex)) (.getMessage ex)))))
      ;; no order/trade pending
      (if closed-trade
        ;; an order was just closed, update training data and save
        (assoc idata :last-order nil
               :nn (save-instrument-nn ikey nn-data))
        ;; ready for new venture, analyze and order
        (->> (assoc idata :nn (analyze-instrument-nn ikey nn-data))
             (order-instrument-nn ikey))))))

(defn- trade-instrument-01 [ikey]
  (let [{oid :id, t :at, last-mode :mode, cancelled? :cancelled?}
        (get-in @state [:last-order ikey])
        odt (if t (int (/ (- (.getTime (Date.)) (.getTime t)) 60000)))
        {:keys [offer order trade closed-trade prices]} (fxb/get-trade-status ikey)
        hl-range (if (seq prices)
                   (let [[hs ls] (->> (take 5 prices)
                                      (map (juxt :h :l))
                                      (apply map list))]
                     (/ (- (apply max hs) (apply min ls)) (:pip offer))))
        open-order (some #(if (= oid (:id %)) %) order)
        open-trade (some #(if (= oid (:open-order-id %)) %) trade)
        tdt (if-let [t (:open-time open-trade)]
              (int (/ (- (.getTime (Date.)) (.getTime t)) 60000)))
        dp (if-let [p (:open-price open-trade)]
             (case last-mode
               :buy (/ (- (:b offer) p) (:pip offer))
               :sell (/ (- p (:a offer)) (:pip offer))))
        ;; wait time in minutes
        order-wait 2
        lots 10, entry 1
        limit 6, limit-2h 5, limit-2l 3, limit-wait 15
        stop 6, stop-2h 6, stop-2l 3, stop-wait 45]
    (cond
      ;; new trade if no trade pending and no high fluctuations
      (not (or open-order open-trade (> hl-range stop)))
      (let [{:keys [profit mode]} (->> closed-trade
                                       (sort-by #(.getTime (:open-time %)) >)
                                       first)
            buy-sell (if cancelled?
                       (case last-mode :buy :sell :buy)
                       (if (and profit mode)
                         (if (pos? profit)
                           ;; profit => repeat last mode
                           mode
                           ;; loss => try other mode
                           (case mode :buy :sell :buy))
                         ;; no previous record, start with buy
                         :buy))
            oid (fxb/create-order ikey buy-sell lots entry limit stop)]
        (swap! state assoc-in [:last-order ikey]
               {:at (Date.)
                :id oid, :mode buy-sell, :lots lots
                :entry entry, :limit limit, :stop stop})
        (log/debug "ordered" ikey buy-sell lots entry limit stop))
      ;; cancel order if waited too long
      (and open-order (< order-wait odt))
      (try
        (log/info "order pending too long. canceling:" oid)
        (fxb/remove-order oid)
        (swap! state assoc-in [:last-order ikey :cancelled?] true)
        (catch Exception ex
          (log/warn "failed to cancel order, error:"
                    (.getName (type ex)) (.getMessage ex))))
      ;; pre-close open trade if waited too long and price is ok
      (and false open-trade
           ;; TODO: currently disabled
           ;; think some other strategy to escape
           ;; like if there is a strong trend in opposite direction
           ;; then break and place order in other direction
           ;; but, beware it may be at peak and do a u-turn
           (or
            ;; waited too long and currently good profit
            (and (< limit-wait tdt) (< limit-2l dp limit-2h))
            ;; waited too long and currently low loss
            (and (< stop-wait tdt) (< stop-2l (- dp) stop-2h))))
      (do
        (log/info "aborting trade" ikey dp tdt)
        (fxb/close-trade (:id open-trade))))))

(defn- trade-instrument-02 [ikey]
  (let [{oid :id, t :at} (get-in @state [:last-order ikey])
        odt (if t (int (/ (- (.getTime (Date.)) (.getTime t)) 60000)))
        {:keys [offer order trade closed-trade prices]} (fxb/get-trade-status ikey)
        open-order (some #(if (= oid (:id %)) %) order)
        open-trade (some #(if (= oid (:open-order-id %)) %) trade)
        ;; wait time in minutes
        order-wait 5
        lots 10, entry 1, limit 6, stop 6]
    (cond
      ;; new trade if no trade pending
      (not (or open-order open-trade))
      (let [{s :sell, b :buy} (->> closed-trade
                                   (sort-by #(.getTime (:open-time %)) >)
                                   (take 7)
                                   (map (fn [{:keys [profit mode]}]
                                          (if (pos? profit)
                                            mode
                                            (case mode :buy :sell :buy))))
                                   frequencies)
            buy-sell (cond
                       (nil? s) :buy
                       (nil? b) :sell
                       (> b s) :buy
                       :else :sell)
            oid (fxb/create-order ikey buy-sell lots entry limit stop)]
        (swap! state assoc-in [:last-order ikey]
               {:at (Date.)
                :id oid, :mode buy-sell, :lots lots
                :entry entry, :limit limit, :stop stop})
        (log/debug "ordered:" ikey buy-sell lots entry limit stop))
      ;; cancel order if waited too long
      (and open-order (< order-wait odt))
      (try
        (log/info "order pending too long. canceling:" oid)
        (fxb/remove-order oid)
        (swap! state assoc-in [:last-order ikey :cancelled?] true)
        (catch Exception ex
          (log/warn "failed to cancel order, error:"
                    (.getName (type ex)) (.getMessage ex)))))))

(defn- trade-instrument [ikey]
  (let [{:keys [atr adx pos-di neg-di]} (first (ana/get-indicators ikey))
        {:keys [offer order trade]} (fxb/get-trade-status ikey)
        open-order (some #(if (false? (:closing? %)) %) order)
        {oid :id} open-order
        t (if oid (or (get-in @state [:last-order ikey :at]) (Date.)))
        odt (if t (int (/ (- (.getTime (Date.)) (.getTime t)) 60000)))
        ;; wait time in minutes
        order-wait 5
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
        (swap! state assoc-in [:last-order ikey :cancelled?] true)
        (catch Exception ex
          (log/warn "failed to cancel order, error:"
                    (.getName (type ex)) (.getMessage ex))))
      ;; new trade if no trade pending
      (not open-order)
      (let [limit (let [limit (int (/ atr (:pip offer)))]
                    (if (< 10 limit 20) limit))
            stop (* 2 limit)
            buy-sell (if (> adx 25)
                       (if (> pos-di neg-di) :buy :sell))
            oid (if (and buy-sell limit)
                  (fxb/create-order ikey buy-sell lots entry limit stop))]
        (swap! state assoc-in [:last-order ikey] {:at (Date.), :id oid})
        (log/debug "ordered:" ikey buy-sell lots entry limit stop)))))

(defn- trade []
  ;; ignore the latest one as incomplete
  (let [quotes (take 1 (fxb/get-hist-prices :eur-usd "H1" nil 2))]
    (ana/put-quotes :eur-usd quotes))
  (if (let [now (.getTime (Date.))]
        (->> (vals (fxb/get-offers))
             (every? #(< 60000 (- now (.getTime (:t %)))))))
    ;; stale offers data, refresh
    (fxb/refresh-offers)
    ;; ok to trade
    (trade-instrument :eur-usd)
    #_(doseq [ikey (fxb/get-instruments)]
        (if-let [idata (get-in @state [:instruments ikey])]
          (if-let [idata (trade-instrument ikey idata)]
            (swap! state assoc-in [:instruments ikey] idata))))))

(defn stop []
  (if-let [stop-chan (:stop-chan @state)]
    (a/put! stop-chan :stop)))

(defn start []
  (if-not (:stop-chan @state)
    (let [stop-chan (a/chan)
          ms1m 60000
          ms1h (* 60 ms1m)
          ms-p 30000]
      (a/go-loop [ms ms-p]
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
                (if (fxb/session-connected?)
                  (trade)
                  (fxb/connect-session))
                (recur ms-p))
              ;; snooze 1 hour
              (recur ms1h)))))
      (swap! state assoc :stop-chan stop-chan)
      (log/info "started worker.."))))

(defn init-ind-hist []
  ;; ignore the latest one as incomplete
  (let [quotes (butlast (fxb/get-hist-prices :eur-usd "H1" nil 300))]
    (ana/put-quotes :eur-usd quotes)))

(defn init []
  (fxb/connect-session)
  (fxb/init-session-data)
  (ana/init)
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
