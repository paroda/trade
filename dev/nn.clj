(ns nn
  (:require [clojure.core.async :as a]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [uncomplicate.commons.core :as ucc]
            [uncomplicate.neanderthal.core :as unc]
            [uncomplicate.neanderthal.native :as unn]
            [bindi.config :as cfg]
            [bindi.log :as log-cfg]
            [bindi.nn :as nn]))

(def cardata (with-open [r (io/reader "workspace/car.data")]
               (->> (csv/read-csv r :separator \,)
                    (mapv (fn [[b m d p l s v]]
                            [[(case b "vhigh" 1 "high" 2 "med" 3 "low" 4)
                              (case m "vhigh" 1 "high" 2 "med" 3 "low" 4)
                              (case d "2" 2 "3" 3 "4" 4 "5more" 5)
                              (case p "2" 2 "4" 4 "more" 5)
                              (case l "small" 1 "med" 2 "big" 3)
                              (case s "low" 1 "med" 2 "high" 3)]
                             (case v
                               "unacc" [1 0 0 0]
                               "acc" [0 1 0 0]
                               "good" [0 0 1 0]
                               "vgood" [0 0 0 1])])))))

(let [[train test] (reduce (fn [[train test] d]
                             (if (< 0.25 (rand))
                               [(conj train d) test]
                               [train (conj test d)]))
                           [()()] cardata)]
  (def train-data train)
  (def test-data test))

(defn imax [a]
  (->> a
       (map-indexed list)
       (reduce (fn [[i m] [j v]]
                 (if (> v m) [j v] [i m])))
       first))

(comment

  (let [[xs ys] (apply map list train-data)
        ns (count ys)
        [xt yt] (apply map list test-data)
        nt (count yt)
        in-dim (count (first xs))
        out-dim (count (first ys))
        opts [0.05 0.1 0.9]]
    (ucc/with-release [xsm (unc/ge unn/native-float in-dim ns xs)
                       ysm (unc/ge unn/native-float out-dim ns ys)
                       xtm (unc/ge unn/native-float in-dim nt xt)
                       ytm (unc/ge unn/native-float out-dim nt yt)
                       inference (nn/inference-network
                                  unn/native-float in-dim
                                  [(nn/fully-connected 24 nn/tanh)
                                   (nn/fully-connected 144 nn/tanh)
                                   (nn/fully-connected 48 nn/tanh)
                                   (nn/fully-connected 4 nn/sigmoid)])
                       training (nn/training-network inference xsm)]
      (nn/init! inference)
      [[in-dim out-dim ns nt]
       ["ysf" (frequencies (map imax ys))]
       ["ytf" (frequencies (map imax yt))]
       {:cost (vec (nn/sgd training ysm nn/quadratic-cost!
                           [[3000 opts]
                            [3000 opts]
                            [3000 opts]]))
        :trained (->> (seq (unc/transfer (inference xsm)))
                      (map list ys)
                      (group-by first)
                      (mapv (comp first val)))
        :test (nn/quadratic-cost! (unc/axpy -1.0 ytm (inference xtm)))
        :fail (->> (map imax (unc/transfer (inference xtm)))
                   (map = (map imax yt))
                   (remove true?)
                   count)}]))

  )
