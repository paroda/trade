(ns bindi.view
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]
            [hiccup.core :as h]
            [bindi.util :as u]
            [bindi.fx-broker :as fxb]))

(defn- create-scale [xi xe yi ye margin-%]
  (let [d (- xe xi)
        xe (+ xe (* d margin-% 0.01))
        xi (- xi (* d margin-% 0.01))
        m (/ (- ye yi) (- xe xi))]
    (with-meta (fn [x]
                 (+ yi (* m (- x xi))))
      {:range [yi ye]
       :domain [xi xe]})))

(defn- svg-candle [x-scale y-scale p]
  (let [x (x-scale (.getTime (:t p)))
        h (y-scale (:h p))
        l (y-scale (:l p))
        o (y-scale (:o p))
        c (y-scale (:c p))]
    [:g
     [:line {:x1 x, :y1 l, :x2 x, :y2 h
             :style (if-not (> c o)
                      "stroke:#00f;stroke-width:1"
                      "stroke:#f00;stroke-width:1")}]]))

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

(defn svg-closed-trades [closed-trades prices]
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
