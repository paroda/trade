(ns nn-data
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
            [bindi.nn :as nn]
            [bindi.fx-broker :as fxb]
            [bindi.analysis :as ana]
            [bindi.indicator :as ind])
  (:import java.util.Date))

(def ikey :eur-usd)
(def pip 1e-4)

(defn imax [a]
  (->> a
       (map-indexed list)
       (reduce (fn [[i m] [j v]]
                 (if (> v m) [j v] [i m])))
       first))

;; returns trade mode [buy sell none]
(defn check-trade [quotes limit stop n]
  (if (< n (count quotes))
    (let [{:keys [o c]} (first quotes)
          p (* 0.5 (+ o c))
          blp (+ p (* limit pip))
          bsp (- p (* stop pip))
          slp (- p (* limit pip))
          ssp (+ p (* stop pip))
          qs (map-indexed (fn [i {:keys [h l]}]
                            [i h l])
                          (take n (rest quotes)))
          [bi b?] (some (fn [[i h l]]
                          (if (< l bsp)
                            [i false]
                            (if (> h blp)
                              [i true])))
                        qs)
          [si s?] (some (fn [[i h l]]
                          (if (> h ssp)
                            [i false]
                            (if (< l slp)
                              [i true])))
                        qs)]
      (cond
        (and (not s?) b?) [1 0 0]
        (and (not b?) s?) [0 1 0]
        (and b? s?) (if (< bi si) [1 0 0] [0 1 0])
        :else [0 0 1]))))

(def inds (let [pf (/ 0.1 pip)]
            [[:rsi 0.01] [:atr pf]
             [:adx 0.01] [:pos-di 0.01] [:neg-di 0.01]
             [:cci-20 0.01] [:cci-200 0.01]
             [:ema-12 1] [:ema-26 1] [:macd pf] [:macd-signal pf]]))

(defn compile-train-data [quotes]
  (let [[iks ifs] (apply map list inds)
        evaluator (ind/make-evaluator iks)
        f (apply juxt iks)]
    (loop [[q & qs] quotes
           tis ()
           state nil
           ds ()]
      (let [n 2
            [ti state] (evaluator state q)
            tis (if (every? number? (f ti))
                  (take n (conj tis ti)))
            {:keys [atr]} ti
            limit (if atr (/ atr pip))
            limit (if (and limit (< 10 limit 20)) (* 1.5 limit))
            stop (if limit (* 1 limit))
            y (if limit (check-trade qs limit stop 21))
            x (if (and y (= n (count tis)))
                (mapcat #(map * (f %) ifs) tis))
            ds (if (and y x)
                 (conj ds [x y])
                 ds)]
        (if (seq qs)
          (recur qs tis state ds)
          ds)))))

(comment

  (def quotes (fxb/get-hist-prices ikey "m5" nil 5000))
  (def train-quotes (take 9000 quotes))
  (def test-quotes (drop 9000 quotes))

  (def train-xy (->> (compile-train-data train-quotes)
                     (apply map list)))

  (map count train-xy) ;; (3962 3962)
  (map (comp count first) train-xy) ;; (22 3)
  (frequencies (map imax (second train-xy))) ;; {0 1978, 1 1755, 2 229}

  (def test-xy (->> (compile-train-data test-quotes)
                    (apply map list)))

  (map count test-xy) ;; (712 712)
  (map (comp count first) train-xy) ;; (22 3)
  (frequencies (map imax (second test-xy))) ;; {1 326, 0 345, 2 41}

  (def xs (first train-xy))
  (def ys (second train-xy))
  (def n (count ys))
  (def xt (first test-xy))
  (def yt (second test-xy))
  (def nt (count yt))
  (def in-dim (count (first xs)))
  (def out-dim (count (first ys)))

  (def opts [0.05 0.1 0.9])

  (def xsm (unc/ge unn/native-float in-dim n xs))
  (def ysm (unc/ge unn/native-float out-dim n ys))
  (def xtm (unc/ge unn/native-float in-dim nt xt))
  (def ytm (unc/ge unn/native-float out-dim nt yt))
  (def inference (nn/inference-network
                  unn/native-float in-dim
                  [(nn/fully-connected 22 nn/tanh)
                   (nn/fully-connected 44 nn/tanh)
                   (nn/fully-connected 22 nn/tanh)
                   (nn/fully-connected 11 nn/tanh)
                   (nn/fully-connected 3 nn/sigmoid)]))
  (def training (nn/training-network inference xsm))

  (do
    (ucc/release xsm)
    (ucc/release ysm)
    (ucc/release xtm)
    (ucc/release ytm)
    (ucc/release inference)
    (ucc/release training))

  (nn/init! inference)

  [[in-dim out-dim n nt]
   ["ysf" (frequencies (map imax ys))]
   ["ytf" (frequencies (map imax yt))]
   {:cost (vec (nn/sgd training ysm nn/quadratic-cost!
                       [[100 opts]
                        [100 opts]
                        [100 opts]]))
    :trained (->> (seq (unc/transfer (inference xsm)))
                  (map list ys)
                  (group-by first)
                  (mapv (comp first val)))
    :test (nn/quadratic-cost! (unc/axpy -1.0 ytm (inference xtm)))
    :fail (let [n (->> (map imax (unc/transfer (inference xtm)))
                       (map = (map imax yt))
                       (partition (int (/ nt 5)))
                       (map #(->> %
                                  (remove true?)
                                  count)))]
            [(apply + n) n])}
   (Date.)]

  (def opts [0.05 0.1 0.9])
  (def opts [0.5 0.1 0.9])
  (def opts [0.3 0.1 0.9])
  (def opts [0.3 0.7 0.1])
  (def opts [0.3 0.1 0.1])
  (def opts [0.05 0.1 0.1])

  )
