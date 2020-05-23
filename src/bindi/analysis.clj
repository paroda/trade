(ns bindi.analysis
  (:require [clojure.core.async :as a]
            [taoensso.timbre :as log]
            [bindi.indicator :as ind]))

;; {:data {:eur-usd {:tis <atom ({:t :rsi :atr ..} ..)> ;; reverse ordered
;;                   :ch <chan>}}
;;  :max-count 100
;;  :inited? false}
(defonce ^:private state (atom {:inited? false}))

(defn- dedupe-quote
  ([]
   (fn [xf]
     (let [prev (volatile! nil)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result quote]
          (let [prior @prev]
            (vreset! prev quote)
            (if (= (:t quote) (:t prior))
              result
              (xf result quote))))))))
  ([quotes] (sequence (dedupe-quote) quotes)))

(defn with-history
  ([n]
   (assert (and (number? n) (> n 2)) "item count must be > 2")
   (fn [xf]
     (let [prev (volatile! nil)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result item]
          (let [next (take n (conj @prev item))]
            (vreset! prev next)
            (if (seq next)
              (xf result next)
              result)))))))
  ([n items] (sequence (with-history n) items)))

(defn analyze
  ([strategy]
   (fn [xf]
     (let [state (volatile! nil)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result ti]
          (let [[ti s] (strategy @state ti)]
            (vreset! state s)
            (xf result ti)))))))
  ([strategy tis] (sequence (analyze strategy) tis)))

(defn strategy-adx-simple [state ti]
  (let [{:keys [adx pos-di neg-di]} ti
        {[adx-2 adx-1] :adx-history} (:adx-simple state)
        trade {:mode (if (and adx-1 adx-2 (> adx 25))
                       (cond
                         (and (> adx adx-1 adx-2) (> pos-di neg-di)) :buy
                         (and (< adx adx-1 adx-2) (< pos-di neg-di)) :sell))}]
    [(assoc ti :trade trade)
     (assoc state :adx-simple {:adx-history [adx-1 adx]})]))

(defn- setup-instrument [state ikey max-count]
  (let [ind-keys [:rsi :atr :adx :pos-di :neg-di :cci-20 :cci-200
                  :ema-12 :ema-26 :macd :macd-signal
                  :high-swing :low-swing]
        strategy strategy-adx-simple
        ch (a/chan 100 (comp (dedupe-quote)
                             (ind/indicators ind-keys)
                             (analyze strategy))
                   #(log/error % "indicator transducer error"))
        tis (atom nil)]
    (some-> (get-in state [:data ikey :ch])
            (a/close!))
    (a/go-loop []
      (when-let [ti (a/<! ch)]
        (swap! tis #(take max-count (conj % ti)))
        (recur)))
    (assoc-in state [:data ikey] {:tis tis, :ch ch})))

(defn init []
  (when-not (:inited? @state)
    (let [max-count 100
          init-state (-> @state
                         (assoc :inited? true, :max-count max-count)
                         (setup-instrument :eur-usd max-count))]
      (reset! state init-state))))

(defn put-quotes [ikey quotes]
  (if-let [ch (get-in @state [:data ikey :ch])]
    (a/onto-chan!! ch quotes false)))

(defn get-indicators
  "returns indicators order recent first to oldest last"
  [ikey]
  (some-> @state
          (get-in [:data ikey :tis])
          (deref)))

(comment

  @state

  (swap! state setup-instrument :eur-usd 100)

  (get-indicators :eur-usd)

  (with-history 3 (range 10)))
