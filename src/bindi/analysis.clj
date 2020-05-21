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

(defn- setup-instrument [state ikey max-count]
  (let [ind-keys [:rsi :atr :adx :pos-di :neg-di :cci-20 :cci-200
                  :ema-12 :ema-26 :macd :macd-signal]
        ch (a/chan 100 (comp (dedupe-quote)
                             (ind/indicators ind-keys))
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

  )
