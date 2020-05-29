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

(defn- date-str [^Date d] (.format date-format d))

(defn- date-picker [dto]
  [:div {:style "position:absolute;right;top:10px;right:10px"}
   [:input {:type "date", :id "toDate"
            :value (if dto (.format (SimpleDateFormat. "yyyy-MM-dd") dto))}]
   [:button {:style "margin-left:10px"
             :onClick (str "javascript: "
                           "window.location = window.location.pathname+'?dto='+"
                           "document.getElementById('toDate').value")}
    "GO"]])

(defn- cross-hair []
  [:div {:style "position:absolute;top:0;height:100%;width:100%"
         :onmousemove "javascript:
  var x = event.pageX - this.offsetLeft -20;
  var y = event.pageY - this.offsetTop -70;
  document.getElementById('chv').style.width= ''+x+'px';
  document.getElementById('chh').style.height= ''+y+'px';"}
   [:div {:style "position:absolute;top:0;left:0;
height:100%;border-right:1px solid#aa55dd77;width:300px"
          :id "chv"}]
   [:div {:style "position:absolute;left:0;top:0;
width:100%;border-bottom:1px solid#aa55dd77;height:300px"
          :id "chh"}]])

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

(defn- svg-adx
  ([tis opts] (svg-adx tis opts :adx :pos-di :neg-di))
  ([tis {:keys [x-scale x-ticks]} adx-kw pos-di-kw neg-di-kw]
   (if-let [tis (seq (filter adx-kw tis))]
     (let [[[yi ye] y-ticks] (let [ys (->> tis
                                           (mapcat (juxt adx-kw pos-di-kw neg-di-kw))
                                           (remove nil?))
                                   yi (apply min ys)
                                   ye (apply max ys)
                                   y-ticks (ticks yi ye)]
                               [[yi ye] y-ticks])
           y-scale (create-scale yi ye 100 0 5)]
       (->> [[adx-kw "stroke:#339;stroke-width:1;fill:none"]
             [pos-di-kw "stroke:#191;stroke-width:0.5;fill:none"]
             [neg-di-kw "stroke:#911;stroke-width:0.5;fill:none"]]
            (map (fn [[k style]]
                   [:path {:d (path-d tis k x-scale y-scale)
                           :style style}]))
            (into
             [:svg {:viewBox "0 0 1000 100"
                    :style "background-color:#111"}
              (g-grid x-scale y-scale x-ticks y-ticks nil identity)
              [:text {:x 40, :y 20, :style "fill:#aa9;font-size:10"}
               (str/upper-case (name adx-kw))]]))))))

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
               :style "stroke:#aa9;stroke-width:0.5;fill:none"}]])))

(defn- path-high-swing [tis {:keys [x-scale y-scale]}]
  (if-let [tis (seq (filter :high-swing tis))]
    [:path {:d (path-d tis :high-swing x-scale y-scale)
            :style "stroke:#aa3;stroke-dasharray:5;stroke-width:0.5;fill:none"}]))

(defn- path-low-swing [tis {:keys [x-scale y-scale]}]
  (if-let [tis (seq (filter :low-swing tis))]
    [:path {:d (path-d tis :low-swing x-scale y-scale)
            :style "stroke:#a3a;stroke-dasharray:5;stroke-width:0.5;fill:none"}]))

(defn- g-trade-signal [tis {:keys [x-scale y-scale]}]
  (->> tis
       (mapcat (fn [{:keys [t], {:keys [mode exit?]} :trade}]
                 (let [x (x-scale (.getTime t))
                       [y _] (:range (meta y-scale))]
                   (list
                    (if mode
                      [:circle {:cx x, :cy (- y 20), :r 2
                                :style (case mode
                                         :buy "stroke:none;fill:#33a"
                                         :sell "stroke:none;fill:#a33")}])
                    (if exit?
                      [:circle {:cx x, :cy (- y 15), :r 1
                                :style "stroke:none;fill:#999"}])))))
       (remove nil?)
       (into [:g])))

(defn- g-closed-trades [closed-trades {:keys [x-scale y-scale]}]
  (let [blue "fill:#3366ff77;stroke:non"
        red "fill:#99333377;stroke:none"
        green "fill:#33993377;stroke:none"
        [l h] (:range (meta y-scale))]
    (->> closed-trades
         (mapcat (fn [ct]
                   (let [ti (x-scale (.getTime (:open-time ct)))
                         te (x-scale (.getTime (:close-time ct)))
                         pi (y-scale (:open-price ct))
                         pe (y-scale (:close-price ct))
                         mode-style (case (:mode ct) :buy blue red)
                         profit-style (if (pos? (:profit ct)) green red)]
                     (list
                      [:polygon {:points (str ti "," pi " " te "," pe " " ti "," pe)
                                 :style profit-style}]
                      [:rect {:x ti, :y (- l 10), :width (- te ti), :height 4
                              :style profit-style}]
                      [:rect {:x ti, :y (- l 5), :width (- te ti), :height 4
                              :style mode-style}]))))
         (into [:g]))))

(defn- g-active-trade [trade offer order {:keys [x-scale y-scale]}]
  (if (and offer trade)
    (let [op (:open-price trade)
          cp (case (:mode trade) :buy (:b offer) (:a offer))
          gain? (case (:mode trade) :buy (> cp op) (> op cp))
          x (x-scale (.getTime (:open-time trade)))
          y (y-scale op)
          ps (zipmap (map :type order) (map :rate order))
          ys (if-let [p (get ps "S")] (y-scale p))
          yl (if-let [p (get ps "L")] (y-scale p))
          {[_ xe] :range} (meta x-scale)
          {[yi _] :range} (meta y-scale)
          y2 (y-scale cp)
          [y1 y2] (if (> y y2) [y y2] [y2 y])
          dy (- y1 y2)]
      [:g
       [:rect {:x (- xe 20), :y y2, :width 15, :height dy
               :style (if gain?
                        "fill:#33993399;stroke:none"
                        "fill:#99333399;stroke:none")}]
       [:line {:x1 x, :x2 (- xe 5), :y1 y, :y2 y
               :style "stroke:#999933;stroke-width:0.5;stroke-dasharray:3"}]
       [:circle {:cx x, :cy y, :r 1, :style "fill:#999933;stroke:none"}]
       (if ys
         [:line {:x1 (- xe 20), :x2 (- xe 5), :y1 ys, :y2 ys
                 :style "stroke:#cc3333;stroke-width:0.5;stroke-dasharray:3"}])
       (if yl
         [:line {:x1 (- xe 20), :x2 (- xe 5), :y1 yl, :y2 yl
                 :style "stroke:#33cc33;stroke-width:0.5;stroke-dasharray:3"}])
       [:rect {:x (- xe 20), :y (- yi 10), :width 15, :height 5
               :style (case (:mode trade) :buy
                            "fill:#3366ff99;stroke:none"
                            "fill:#99333399;stroke:none")}]])))

(defn- price-indicators [prices dt indicators closed-trades & gfns]
  (let [h (apply max (map :h prices))
        l (apply min (map :l prices))
        y-scale (create-scale l h 200 0 5)
        xi (.getTime (:t (first prices)))
        xe (.getTime (:t (last prices)))
        x-scale (create-scale xi xe 0 1000 5)
        x-ticks (ticks xi xe dt)
        y-ticks (ticks h l)
        scales {:x-scale x-scale, :y-scale y-scale}]
    [:div {:style "display:flex;flex-direction:column"}
     ;; price history chart
     (into
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
       (path-ema-12 indicators scales)
       (path-ema-26 indicators scales)
       ;; high-swing & low-swing
       (path-high-swing indicators scales)
       (path-low-swing indicators scales)
       ;; trade signals
       (g-trade-signal indicators scales)
       ;; closed trades
       (g-closed-trades closed-trades scales)]
      ;; additonal custom inserts
      (map #(% scales) gfns))
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

(defn chart-price-indicators [ikey tfrm n dto]
  (if (not (fxb/session-connected?))
    (h/html [:div "Session not connected!"])
    (let [strategy ana/strategy-adx-01
          ind-keys ana/indicator-keys
          dt (case tfrm
               "m1" 900e3
               "m5" (* 3600e3)
               ("m30" "H1") (* 24 3600e3)
               "H4" (* 24 3600e3)
               "D1" (* 10 24 3600e3)
               (throw (Exception. "Invalid timeframe.
Must be one of m1,m5,m30,H1,H4,D1")))
          ps (fxb/get-hist-prices ikey tfrm dto (+ n ana/lead-ti-count))
          tis (->> ps
                   (ind/indicators ind-keys)
                   (ana/analyze strategy))
          ps (map :quote tis)
          [ti te] [(.getTime (:t (first ps))) (.getTime (:t (last ps)))]
          cts (->> (fxb/get-trade-status ikey)
                   :closed-trade
                   (filter #(< ti (.getTime (:close-time %)) te)))
          cview (price-indicators ps dt tis cts)]
      (h/html
       [:body {:style "background:#333;color:#aa9;font-family:Tahoma"}
        [:div
         [:h2 "Chart - Price Indicators"]
         (date-picker dto)
         [:div {:style "padding:10px"}
          [:div {:style "position:relative"}
           cview
           (cross-hair)]]]]))))

(defn active-chart-price-indicators [ikey]
  (if (not (fxb/session-connected?))
    (h/html [:div "Session not connected!"])
    (let [dt (* 24 3600e3)
          ps (fxb/get-hist-prices ikey wkr/ana-time-frame nil wkr/ana-max-count)
          tis (ana/get-indicators ikey)
          {:keys [closed-trade trade offer order]} (fxb/get-trade-status ikey)
          cts (->> closed-trade
                   (filter #(< (.getTime (:t (first ps)))
                               (.getTime (:close-time %)))))
          cview (price-indicators ps dt tis cts
                                  (partial g-active-trade
                                           (first trade) offer order))]
      (h/html
       [:body {:style "background:#333;color:#aa9;font-family:Tahoma"}
        [:div
         [:h2 "Active Chart - Price Indicators"]
         [:div {:style "padding:10px"}
          [:div {:style "position:relative"}
           cview
           (cross-hair)]]]]))))

(defn backtest-strategy [ikey tfrm n dto]
  (if (not (fxb/session-connected?))
    (h/html [:div "Session not connected!"])
    (let [strategy ana/strategy-adx-01
          ind-keys ana/indicator-keys
          dt (case tfrm
               "m5" (* 3600e3)
               ("m30" "H1") (* 24 3600e3)
               "H4" (* 24 3600e3)
               "D1" (* 3 24 3600e3)
               (throw (Exception. "Invalid timeframe. Must be one of m5,H1")))
          [tis {cts :closed-trades, bal :balance, bmax :max-bal, bmin :min-bal}]
          (bt/test-strategy strategy ind-keys ikey tfrm dto n)
          ps (map :quote tis)
          bt-view (price-indicators ps dt tis cts)]
      (h/html
       [:body {:style "background:#333;color:#aa9;font-family:Tahoma"}
        [:div
         [:h2 (format "Chart - back-test - balance: %10.2f (%10.2f : %10.2f)"
                      bal bmax bmin)]
         (date-picker dto)
         [:div {:style "padding:10px"}
          [:div {:style "position:relative"}
           bt-view
           (cross-hair)]]]]))))
