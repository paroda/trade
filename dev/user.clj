(ns user
  (:require [clojure.edn :as edn]
            [bindi.trade :as tr]
            [bindi.broker :as br]
            [bindi.scout :as sc]
            [bindi.evaluate :as ev]))

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
  (def d2017 (->> "data/2017-eurusd.edn" slurp edn/read-string))
  (def d2018 (->> "data/2018-eurusd.edn" slurp edn/read-string))
  (def d2019 (->> "data/2019-eurusd.edn" slurp edn/read-string))

  (let [scouts [sc/a1]
        weights {:eur-usd {:b 1, :a1 1, :a2 1}}
        wealth (-> (tr/new-wealth)
                   (assoc :scout (sc/make-scout :a scouts weights)
                          :evaluate (ev/make-evaluate ev/evaluators)
                          :broker (br/get-simulated-broker br/symbols-data-simulated)))
        b 0.00010
        wealth
        (->> d2019
             (map #(hash-map :eur-usd %))
             (reduce tr/trade wealth))]
    [(count (:ventures wealth))
     (select-keys (:b @(:data wealth)) [:bn :sn])
     (select-keys wealth [:profit :balance :max-balance :min-balance])])
  
  )
