(ns bindi.util
  (:require [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import java.text.SimpleDateFormat))

(defn round [v n]
  (let [m (Math/pow 10.0 ^:Double n)]
    (/ (Math/round (* m v)) m)))

(defn remove-items
  "Remove specified items by matching the specified key (by default :id)"
  ([items items-to-remove] (remove-items items items-to-remove :id))
  ([items items-to-remove by-key]
   (assert (keyword by-key) "*by-key* must be a keyword!")
   (let [matching? (comp (set (map by-key items-to-remove)) by-key)]
     (->> items
          (remove matching?)
          vec))))

(defn load-price-history-csv
  "Read the csv file containing time, open/high/low/close prices.
=options=
start-at = start reading from line number (0 is first line)
date-format = by default \"yyyyMMdd HHmmss\" 
separator = by default comma(,)
"
  [path & options]
  (let [{:keys [start-at date-format separator]
         :or {start-at 0, date-format "yyyyMMdd HHmmss", separator \,}} options
        fmt (SimpleDateFormat. date-format)]
    (with-open [r (io/reader path)]
      (->> (csv/read-csv r :separator separator)
           (drop start-at)
           (mapv (fn [[t o h l c]]
                   {:t (.parse fmt t)
                    :h (Double/parseDouble h)
                    :l (Double/parseDouble l)
                    :o (Double/parseDouble o)
                    :c (Double/parseDouble c)}))))))