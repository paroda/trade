(ns bindi.view
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]
            [hiccup.core :as h]
            [bindi.util :as u]
            [bindi.fx-broker :as fxb]
            [bindi.indicator :as ind]
            [bindi.analysis :as ana])
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

(defn- svg-grid [x-scale y-scale xs ys xfmt yfmt]
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

(defn- svg-candle [x-scale y-scale p]
  (let [x (x-scale (.getTime (:t p)))
        h (y-scale (:h p))
        l (y-scale (:l p))
        o (y-scale (:o p))
        c (y-scale (:c p))]
    [:path {:d (str/join " " ["M" (- x 2) o "h" 2 "V" h "V" l "V" c "h" 2])
            :style (if-not (> c o)
                     "stroke:#33a;stroke-width:1;fill:none"
                     "stroke:#a33;stroke-width:1;fill:none")}]))

(defn- svg-trade [x-scale y-scale ct]
  (let [ti (x-scale (.getTime (:open-time ct)))
        te (x-scale (.getTime (:close-time ct)))
        pi (y-scale (:open-price ct))
        pe (y-scale (:close-price ct))
        [l h] (:range (meta y-scale))
        blue "fill:#00f;stroke:none;opacity:0.3"
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
      (map (partial svg-candle x-scale y-scale) prices)
      (map (partial svg-trade x-scale y-scale)
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

(defn- svg-rsi [tis {:keys [x-scale x-ticks y-ticks y-range]}]
  (if (seq tis)
    (let [[yi ye] y-range
          y-scale (create-scale yi ye 100 0 5)]
      [:svg {:viewBox "0 0 1000 100"
             :style "background-color:#111"}
       (svg-grid x-scale y-scale x-ticks y-ticks nil identity)
       [:text {:x 40, :y 20, :style "fill:#aa9;font-size:10"} "RSI"]
       [:path {:d (->> tis
                       (map (juxt :t :rsi))
                       (filter second)
                       (map-indexed
                        (fn [i [t v]]
                          (format (if (zero? i) "M%f,%f" "L%f,%f")
                                  (x-scale (.getTime t))
                                  (y-scale v))))
                       (str/join " "))
               :style "stroke:#aa9;stroke-width:0.5;fill:none"}]])))

(defn- svg-atr [tis {:keys [x-scale x-ticks]}]
  (if (seq tis)
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
       (svg-grid x-scale y-scale x-ticks y-ticks
                 nil  #(format "%6.4f" %))
       [:text {:x 40, :y 20, :style "fill:#aa9;font-size:10"} "ATR"]
       [:path {:d (->> tis
                       (map (juxt :t :atr))
                       (filter second)
                       (map-indexed
                        (fn [i [t v]]
                          (format (if (zero? i) "M%f,%f" "L%f,%f")
                                  (x-scale (.getTime t))
                                  (y-scale v))))
                       (str/join " "))
               :style "stroke:#aa9;stroke-width:0.5;fill:none"}]])))

(defn- svg-adx [tis {:keys [x-scale x-ticks]}]
  (if (seq tis)
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
                  [:path {:d (->> tis
                                  (map (juxt :t k))
                                  (filter second)
                                  (map-indexed
                                   (fn [i [t v]]
                                     (format (if (zero? i) "M%f,%f" "L%f,%f")
                                             (x-scale (.getTime t))
                                             (y-scale v))))
                                  (str/join " "))
                          :style style}]))
           (into
            [:svg {:viewBox "0 0 1000 100"
                   :style "background-color:#111"}
             (svg-grid x-scale y-scale x-ticks y-ticks nil identity)
             [:text {:x 40, :y 20, :style "fill:#aa9;font-size:10"} "ADX"]])))))

(defn- price-indicators [prices dt tis]
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
      (svg-grid x-scale y-scale x-ticks y-ticks
                #(date-str (Date. (long %)))
                #(format "%6.4f" %))
      ;; price candles
      (->> prices
           (map (partial svg-candle x-scale y-scale))
           (into [:g]))]
     ;; indicator charts
     [:div {:style "height:5px"}]
     (svg-rsi tis {:x-scale x-scale, :x-ticks x-ticks
                   :y-ticks [30 70], :y-range [0 100]})
     [:div {:style "height:5px"}]
     (svg-atr tis {:x-scale x-scale, :x-ticks x-ticks})
     [:div {:style "height:5px"}]
     (svg-adx tis {:x-scale x-scale, :x-ticks x-ticks})]))

(defn chart-price-indicators [ikey tfrm]
  (let [dt (case tfrm
             "m1" 900e3
             "m5" (* 3600e3)
             "H1" (* 24 3600e3)
             "D1" (* 10 24 3600e3)
             (throw (Exception. "Invalid timeframe. Must be one of m1,m5,H1,D1")))
        ps (fxb/get-hist-prices ikey tfrm nil 100)
        tis  (ind/indicators [:rsi :atr :adx :pos-di :neg-di] ps)
        pis (price-indicators ps dt tis)]
    (h/html
     [:body {:style "background:#333;color:#aa9;font-family:Tahoma"}
      [:div
       [:h2 "Chart - Price Indicators"]
       [:div {:style "padding:10px"}
        pis]]])))

(defn active-chart-price-indicators [ikey]
  (let [dt (* 24 3600e3)
        ps (fxb/get-hist-prices ikey "H1" nil 100)
        tis (ana/get-indicators ikey)
        pis (price-indicators ps dt tis)]
    (h/html
     [:body {:style "background:#333;color:#aa9;font-family:Tahoma"}
      [:div
       [:h2 "Active Chart - Price Indicators"]
       [:div {:style "padding:10px"}
        pis]]])))

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
