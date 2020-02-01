(ns misc
  (:require [clojure.edn :as edn]))

;; *ds* : map of symbol to price-series
(defn merge-price-series [ds]
  (->> (reduce (fn [m [symbol series]]
                 (reduce (fn [m {:keys [t] :as d}]
                           (update m t assoc symbol d))
                         m series))
               (sorted-map) ds)
       (map second)
       (vec)))

(comment
  (=
   (merge-price-series {:a [{:t 1, :p 1} {:t 10, :p 10}]
                        :b [{:t 2, :p 2} {:t 9, :p 9}]
                        :c [{:t 1, :p 1} {:t 9, :p 9}]})
   [{:a {:t 1, :p 1}, :c {:t 1, :p 1}}
    {:b {:t 2, :p 2}}
    {:b {:t 9, :p 9}, :c {:t 9, :p 9}}
    {:a {:t 10, :p 10}}])


  (let [y 2019
        f (fn [s]
            (let [ps (-> "workspace/data/%s-%d.edn"
                         (format (name s) y)
                         slurp
                         edn/read-string)]
              {s ps}))
        ds (merge (f :eur-usd)
                  (f :eur-gbp)
                  (f :gbp-usd))]
    (spit (format "workspace/data/%d.edn" y)
          (merge-price-series ds)))

  )
