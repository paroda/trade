(ns user
  (:require [clojure.edn :as edn]
            [bindi.config :as cfg]
            [bindi.trade :as tr]
            [bindi.broker :as br]
            [bindi.scout :as sc]
            [bindi.evaluate :as ev]
            [bindi.util :as u]))

(defn my-scout [current-wealth prices]
  (let [id :b
        {:keys [o c]} (get prices :eur-usd)
        d (:data current-wealth)
        {:keys [h bn sn] :or {h [], bn 0, sn 0}} (get @d id)
        h (conj h (- c o))
        n (count h)
        h (if (> n 10) (vec (rest h)) h)
        s (reduce + h)
        mode (if (> s 0.003) :buy (if (< s -0.003) :sell))
        chance (if mode (/ (Math/abs s) (reduce + (map #(Math/abs %) h))))
        bn (if (= :buy mode) (inc bn) bn)
        sn (if (= :sell mode) (inc sn) sn)]
    (swap! d update id assoc :h h :bn bn :sn sn)
    (if mode
      [{:chance chance
        :mode mode, :quantity 10
        :stop-loss-price ((case mode :buy - :sell +) c 0.00500)
        :target-price ((case mode :buy + :sell -) c 0.05000)
        :symbol :eur-usd, :scout-id id}])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment

  (def d2017 (->> "workspace/data/eur-usd-2017.edn" slurp edn/read-string))
  (def d2018 (->> "workspace/data/eur-usd-2018.edn" slurp edn/read-string))
  (def d2019 (->> "workspace/data/eur-usd-2019.edn" slurp edn/read-string))

  (let [scouts [sc/a1]
        weights {:eur-usd {:a1 1, :a2 1}}
        wealth (-> (tr/new-wealth)
                   (assoc :scout (sc/make-scout :a scouts weights)
                          :evaluate (ev/make-evaluate ev/evaluators)
                          :broker (br/get-simulated-broker br/symbols-data-simulated)))
        wealth
        (->> d2019
             (map #(hash-map :eur-usd %))
             (reduce tr/trade wealth))]
    [(count (:ventures wealth))
     (select-keys wealth [:profit :balance :max-balance :min-balance])
     (count (:terminated wealth))])

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; file contains multiple symbols
  (def d7 (->> "workspace/data/2017.edn" slurp edn/read-string))
  (def d8 (->> "workspace/data/2018.edn" slurp edn/read-string))
  (def d9 (->> "workspace/data/2019.edn" slurp edn/read-string))

  (require 'example)
  (let [price-series d9
        scouts [example/a1]
        weights {:eur-usd {:a1 1, :a2 1}
                 :eur-gbp {:a1 1, :a2 1}
                 :gbp-usd {:a1 1, :a2 1}}
        wealth (-> (tr/new-wealth)
                   (assoc :scout (sc/make-scout :a scouts weights)
                          :evaluate (ev/make-evaluate ev/evaluators)
                          :broker (br/get-simulated-broker br/symbols-data-simulated)))
        wealth (reduce tr/trade wealth price-series)]
    [(count (:ventures wealth))
     (select-keys wealth [:profit :balance :max-balance :min-balance])
     (count (:terminated wealth))])

  )
