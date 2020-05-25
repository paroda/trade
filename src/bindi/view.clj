(ns bindi.view
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]
            [hiccup.core :as h]
            [bindi.util :as u]
            [bindi.fx-broker :as fxb]
            [bindi.indicator :as ind]
            [bindi.analysis :as ana]
            [bindi.backtest :as bt]
            [bindi.worker :as wkr])
  (:import [java.util Date Calendar]
           java.text.SimpleDateFormat))

(def date-format (SimpleDateFormat. "yyMMdd.HHmm"))

(defn- date-str [^Date d]
  (.format date-format d))

(defn- ticks
  ([x1 x2] (ticks x1 x2 nil))
  ([x1 x2 dx]
   (let [[x1 x2] (if (> x1 x2) [x2 x1] [x1 x2])
         dx (or dx
                (let [dx (Math/pow 10 (Math/floor (Math/log10 (- x2 x1))))
                      r (/ (- x2 x1) dx)]
                  (cond
                    (> r 8) (* 2 dx)
                    (> r 3) dx
                    (> r 1.5) (/ dx 2)
                    :else (/ dx 5))))
         p1 (Math/floor (/ x1 dx))
         p2 (inc (Math/ceil (/ x2 dx)))]
     (map #(* % dx) (range p1 p2)))))

(defn- create-scale [xi xe yi ye margin-%]
  (let [d (- xe xi)
        xe (+ xe (* d margin-% 0.01))
        xi (- xi (* d margin-% 0.01))
        m (/ (- ye yi) (- xe xi))]
    (with-meta (fn [x]
                 (+ yi (* m (- x xi))))
      {:range [yi ye]
       :domain [xi xe]})))

(defn- g-grid [x-scale y-scale xs ys xfmt yfmt]
  (let [[xi xe] (:range (meta x-scale))
        [yi ye] (:range (meta y-scale))]
    (into
     [:g {:style "stroke:#442;stroke-width:0.5;font-size:8px;"}]
     (concat
      (mapcat (fn [x]
                (let [xx (x-scale x)]
                  (list
                   [:line {:x1 xx, :x2 xx, :y1 yi, :y2 ye}]
                   (if (fn? xfmt)
                     [:text {:x (+ xx 3), :y (- yi 3)
                             :style "stroke-width:0;fill:#774"
                             :transform (str "rotate(270," xx "," yi ")")}
                      (xfmt x)]))))
              xs)
      (mapcat (fn [y]
                (let [yy (y-scale y)]
                  (list
                   [:line {:x1 xi, :x2 xe, :y1 yy, :y2 yy}]
                   (if (fn? yfmt)
                     [:text {:x (+ xi 10), :y (- yy 3)
                             :style "stroke-width:0;fill:#774"}
                      (yfmt y)]))))
              ys)))))

(defn- path-candle [x-scale y-scale p]
  (let [x (x-scale (.getTime (:t p)))
        h (y-scale (:h p))
        l (y-scale (:l p))
        o (y-scale (:o p))
        c (y-scale (:c p))]
    [:path {:d (str/join " " ["M" (- x 2) o "h" 2 "V" h "V" l "V" c "h" 2])
            :style (if-not (> c o)
                     "stroke:#33a;stroke-width:1;fill:none"
                     "stroke:#a33;stroke-width:1;fill:none")}]))

(defn- g-trade [x-scale y-scale ct]
  (let [ti (x-scale (.getTime (:open-time ct)))
        te (x-scale (.getTime (:close-time ct)))
        pi (y-scale (:open-price ct))
        pe (y-scale (:close-price ct))
        [l h] (:range (meta y-scale))
        blue "fill:#0f6;stroke:none;opacity:0.3"
        red "fill:#f00;stroke:none;opacity:0.3"
        mode-style (case (:mode ct) :buy blue red)
        profit-style (if (pos? (:profit ct)) blue red)]
    [:g
     [:polygon {:points (str ti "," pi " " te "," pe " " ti "," pe)
                :style profit-style}]
     [:rect {:x ti, :y (- l 10), :width (- te ti), :height 4, :style profit-style}]
     [:rect {:x ti, :y (- l 5), :width (- te ti), :height 4, :style mode-style}]]))

(defn- svg-closed-trades [closed-trades prices]
  (let [h (apply max (map :h prices))
        l (apply min (map :l prices))
        y-scale (create-scale l h 300 0 5)
        xi (.getTime (:t (last prices)))
        xe (.getTime (:t (first prices)))
        x-scale (create-scale xi xe 0 1000 5)
        p (first prices)]
    (into
     [:svg {:flex 1, :viewBox (str/join "0 0 1000 300")
            :style "background-color:#ccc"}]
     (concat
      (map (partial path-candle x-scale y-scale) prices)
      (map (partial g-trade x-scale y-scale)
           (filter #(> (.getTime (:open-time %)) xi) closed-trades))))))

(defn closed-trades [ikey]
  (let [cts (fxb/get-closed-trades ikey)
        ps (fxb/get-prices ikey)
        svg (svg-closed-trades cts ps)]
    (h/html
     [:div
      [:h2 "Closed trades"]
      [:div {:style "padding:10px;display:flex-box;height:400px"}
       svg]])))

(defn- path-d [tis ind-key x-scale y-scale]
  (->> tis
       (map (juxt :t ind-key))
       (map-indexed
        (fn [i [t v]]
          (format (if (zero? i) "M%f,%f" "L%f,%f")
                  (x-scale (.getTime t))
                  (y-scale v))))
       (str/join " ")))

(defn- path-ema-12 [tis {:keys [x-scale y-scale]}]
  (if-let [tis (seq (filter :ema-12 tis))]
    [:path {:d (path-d tis :ema-12 x-scale y-scale)
            :style "stroke:#693;stroke-width:0.5;fill:none"}]))

(defn- path-ema-26 [tis {:keys [x-scale y-scale]}]
  (if-let [tis (seq (filter :ema-26 tis))]
    [:path {:d (path-d tis :ema-26 x-scale y-scale)
            :style "stroke:#963;stroke-width:0.5;fill:none"}]))

(defn- svg-rsi [tis {:keys [x-scale x-ticks y-ticks y-range]}]
  (if-let [tis (seq (filter :rsi tis))]
    (let [[yi ye] y-range
          y-scale (create-scale yi ye 100 0 5)]
      [:svg {:viewBox "0 0 1000 100"
             :style "background-color:#111"}
       (g-grid x-scale y-scale x-ticks y-ticks nil identity)
       [:text {:x 40, :y 20, :style "fill:#aa9;font-size:10"} "RSI"]
       [:path {:d (path-d tis :rsi x-scale y-scale)
               :style "stroke:#aa9;stroke-width:0.5;fill:none"}]])))

(defn- svg-atr [tis {:keys [x-scale x-ticks]}]
  (if-let [tis (seq (filter :atr tis))]
    (let [[[yi ye] y-ticks] (let [ys (->> tis
                                          (map :atr)
                                          (remove nil?))
                                  yi (apply min ys)
                                  ye (apply max ys)
                                  y-ticks (ticks yi ye)]
                              [[yi ye] y-ticks])
          y-scale (create-scale yi ye 100 0 5)]
      [:svg {:viewBox "0 0 1000 100"
             :style "background-color:#111"}
       (g-grid x-scale y-scale x-ticks y-ticks
               nil  #(format "%6.4f" %))
       [:text {:x 40, :y 20, :style "fill:#aa9;font-size:10"} "ATR"]
       [:path {:d (path-d tis :atr x-scale y-scale)
               :style "stroke:#aa9;stroke-width:0.5;fill:none"}]])))

(defn- svg-adx [tis {:keys [x-scale x-ticks]}]
  (if-let [tis (seq (filter :adx tis))]
    (let [[[yi ye] y-ticks] (let [ys (->> tis
                                          (mapcat (juxt :adx :pos-di :neg-di))
                                          (remove nil?))
                                  yi (apply min ys)
                                  ye (apply max ys)
                                  y-ticks (ticks yi ye)]
                              [[yi ye] y-ticks])
          y-scale (create-scale yi ye 100 0 5)]
      (->> [[:adx "stroke:#339;stroke-width:1;fill:none"]
            [:pos-di "stroke:#191;stroke-width:0.5;fill:none"]
            [:neg-di "stroke:#911;stroke-width:0.5;fill:none"]]
           (map (fn [[k style]]
                  [:path {:d (path-d tis k x-scale y-scale)
                          :style style}]))
           (into
            [:svg {:viewBox "0 0 1000 100"
                   :style "background-color:#111"}
             (g-grid x-scale y-scale x-ticks y-ticks nil identity)
             [:text {:x 40, :y 20, :style "fill:#aa9;font-size:10"} "ADX"]])))))

(defn- svg-macd [tis {:keys [x-scale x-ticks]}]
  (if-let [tis (seq (filter :macd-signal tis))]
    (let [tis (map (fn [ti]
                     (assoc ti :macd-diff (- (:macd ti) (:macd-signal ti))))
                   tis)
          [[yi ye] y-ticks] (let [ys (->> tis
                                          (mapcat (juxt :macd :macd-signal
                                                        :macd-diff))
                                          (remove nil?))
                                  yi (min 0 (apply min ys))
                                  ye (max 0 (apply max ys))
                                  y-ticks (ticks yi ye)]
                              [[yi ye] y-ticks])
          y-scale (create-scale yi ye 100 0 10)]
      (->> [[:macd "stroke:#339;stroke-width:1;fill:none"]
            [:macd-signal "stroke:#933;stroke-dasharray:2;stroke-width:0.5;fill:none"]
            [:macd-diff "stroke:#966;stroke-width:0.5;fill:none"]]
           (map (fn [[k style]]
                  [:path {:d (path-d tis k x-scale y-scale)
                          :style style}]))
           (into
            [:svg {:viewBox "0 0 1000 100"
                   :style "background-color:#111"}
             (g-grid x-scale y-scale x-ticks y-ticks
                     nil #(format "%6.4f" %))
             [:text {:x 40, :y 20, :style "fill:#aa9;font-size:10"} "MACD"]])))))

(defn- svg-cci [tis {:keys [x-scale x-ticks y-ticks y-range]}]
  (if-let [tis (seq (filter :cci-20 tis))]
    (let [[yi ye] y-range
          y-scale (create-scale yi ye 100 0 5)]
      [:svg {:viewBox "0 0 1000 100"
             :style "background-color:#111"}
       (g-grid x-scale y-scale x-ticks y-ticks nil identity)
       [:text {:x 40, :y 20, :style "fill:#aa9;font-size:10"} "CCI"]
       [:path {:d (path-d tis :cci-20 x-scale y-scale)
               :style "stroke:#aa9;stroke-width:0.5;fill:none"}]
       (if-let [tis (seq (filter :cci-200 tis))]
         [:path {:d (path-d tis :cci-200 x-scale y-scale)
                 :style "stroke:#a33;stroke-width:0.5;fill:none"}])])))

(defn- path-high-swing [tis {:keys [x-scale y-scale]}]
  (if-let [tis (seq (filter :high-swing tis))]
    [:path {:d (path-d tis :high-swing x-scale y-scale)
            :style "stroke:#aa3;stroke-dasharray:5;stroke-width:0.5;fill:none"}]))

(defn- path-low-swing [tis {:keys [x-scale y-scale]}]
  (if-let [tis (seq (filter :low-swing tis))]
    [:path {:d (path-d tis :low-swing x-scale y-scale)
            :style "stroke:#a3a;stroke-dasharray:5;stroke-width:0.5;fill:none"}]))

(defn- g-trade-signal [tis {:keys [x-scale y-scale]}]
  (if-let [tis (seq (filter (comp :mode :trade) tis))]
    (->> tis
         (map (fn [{:keys [t], {:keys [mode]} :trade}]
                (if mode
                  (let [x (x-scale (.getTime t))
                        [y _] (:range (meta y-scale))]
                    [:circle {:cx x, :cy (- y 20), :r 2
                              :style (case mode
                                       :buy "stroke:none;fill:#33a"
                                       :sell "stroke:none;fill:#a33")}]))))
         (into [:g]))))

(defn- price-indicators [prices dt indicators closed-trades]
  (let [h (apply max (map :h prices))
        l (apply min (map :l prices))
        y-scale (create-scale l h 200 0 5)
        xi (.getTime (:t (first prices)))
        xe (.getTime (:t (last prices)))
        x-scale (create-scale xi xe 0 1000 5)
        x-ticks (ticks xi xe dt)
        y-ticks (ticks h l)]
    [:div {:style "display:flex;flex-direction:column"}
     ;; price history chart
     [:svg {:viewBox "0 0 1000 200"
            :style "background-color:#111"}
      (g-grid x-scale y-scale x-ticks y-ticks
              #(date-str (Date. (long %)))
              #(format "%6.4f" %))
      ;; price candles
      (->> prices
           (map (partial path-candle x-scale y-scale))
           (into [:g]))
      ;; ema-12 & ema-26
      (path-ema-12 indicators {:x-scale x-scale, :y-scale y-scale})
      (path-ema-26 indicators {:x-scale x-scale, :y-scale y-scale})
      ;; high-swing & low-swing
      (path-high-swing indicators {:x-scale x-scale, :y-scale y-scale})
      (path-low-swing indicators {:x-scale x-scale, :y-scale y-scale})
      ;; trade signals
      (g-trade-signal indicators {:x-scale x-scale, :y-scale y-scale})
      ;; closed trades
      (->> closed-trades
           (map (partial g-trade x-scale y-scale))
           (into [:g]))]
     ;; indicator charts
     [:div {:style "height:5px"}]
     (svg-rsi indicators {:x-scale x-scale, :x-ticks x-ticks
                          :y-ticks [30 70], :y-range [0 100]})
     [:div {:style "height:5px"}]
     (svg-adx indicators {:x-scale x-scale, :x-ticks x-ticks})
     [:div {:style "height:5px"}]
     (svg-macd indicators {:x-scale x-scale, :x-ticks x-ticks})
     [:div {:style "height:5px"}]
     (svg-cci indicators {:x-scale x-scale, :x-ticks x-ticks
                          :y-ticks [-200 -100 0 100 200], :y-range [-200 200]})
     [:div {:style "height:5px"}]
     (svg-atr indicators {:x-scale x-scale, :x-ticks x-ticks})]))

(defn chart-price-indicators [ikey tfrm n]
  (if (not (fxb/session-connected?))
    (h/html [:div "Session not connected!"])
    (let [dto nil ;; #inst "2020-05-10T00:00:00.000-00:00"
          strategy ana/strategy-adx-cci-02
          ind-keys ana/indicator-keys
          dt (case tfrm
               "m1" 900e3
               "m5" (* 3600e3)
               "m30" (* 24 3600e3)
               "H1" (* 24 3600e3)
               "D1" (* 10 24 3600e3)
               (throw (Exception. "Invalid timeframe. Must be one of m1,m5,m30,H1,D1")))
          ps (fxb/get-hist-prices ikey tfrm dto (+ n ana/lead-ti-count))
          tis (->> ps
                   (ind/indicators ind-keys)
                   (ana/analyze strategy))
          ps (map :quote tis)
          cts (->> (fxb/get-trade-status ikey)
                   :closed-trade
                   (filter #(< (.getTime (:t (first ps)))
                               (.getTime (:close-time %)))))
          cview (price-indicators ps dt tis cts)]
      (h/html
       [:body {:style "background:#333;color:#aa9;font-family:Tahoma"}
        [:div
         [:h2 "Chart - Price Indicators"]
         [:div {:style "padding:10px"}
          cview]]]))))

(defn active-chart-price-indicators [ikey]
  (if (not (fxb/session-connected?))
    (h/html [:div "Session not connected!"])
    (let [dt (* 24 3600e3)
          ps (fxb/get-hist-prices ikey wkr/ana-time-frame nil wkr/ana-max-count)
          tis (ana/get-indicators ikey)
          cview (price-indicators ps dt tis ())]
      (h/html
       [:body {:style "background:#333;color:#aa9;font-family:Tahoma"}
        [:div
         [:h2 "Active Chart - Price Indicators"]
         [:div {:style "padding:10px"}
          cview]]]))))

(defn backtest-strategy [ikey tfrm n]
  (if (not (fxb/session-connected?))
    (h/html [:div "Session not connected!"])
    (let [dto nil ;; #inst "2020-05-23T00:00:00.000-00:00"
          strategy ana/strategy-adx-cci-02
          ind-keys ana/indicator-keys
          dt (case tfrm
               "m5" (* 3600e3)
               "m30" (* 24 3600e3)
               "H1" (* 24 3600e3)
               (throw (Exception. "Invalid timeframe. Must be one of m5,H1")))
          [tis {cts :closed-trades, bal :balance, bmax :max-bal, bmin :min-bal}]
          (bt/test-strategy strategy ind-keys ikey tfrm dto
                            (+ n ana/lead-ti-count))
          ps (map :quote tis)
          bt-view (price-indicators ps dt tis cts)]
      (h/html
       [:body {:style "background:#333;color:#aa9;font-family:Tahoma"}
        [:div
         [:h2 (format "Chart - back-test - balance: %10.2f (%10.2f : %10.2f)"
                      bal bmax bmin)]
         [:div {:style "padding:10px"}
          bt-view]]]))))

(defn chart-1 [ikey]
  (let [dto (Date.) ;; #inst "2020-05-15T20:00:00.000-00:00"
        n 100
        ps (fxb/get-hist-prices ikey "D1" dto n)
        pis (price-indicators
             ps (* 10 24 3600e3)
             {:xf ind/rsi, :ticks [30 70], :range [0 100]})]
    (h/html
     [:div
      [:h2 "Chart 1"]
      [:div {:style "padding:10px"}
       pis]])))
