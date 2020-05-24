(ns bindi.config
  (:require [clojure.edn :as edn]))

(defonce config (atom {}))

(defn merge-deep [m1 m2]
  (reduce-kv
   (fn [m k v]
     (assoc m k
            (let [v1 (get m k)]
              (if (and (map? v1) (map? v))
                (merge-deep v1 v)
                v))))
   m1 m2))

(defn init []
  (let [c (->> "config.edn" slurp edn/read-string)
        env (System/getenv "TRADE_ENV")
        wpath (str (:workspace c) "/" env)
        c2 (->> (str wpath "/config.edn") slurp edn/read-string)
        c (-> c
              (assoc :workspace wpath)
              (update-in [:log :rotor-log-file] #(str wpath "/" (or % "rotor.log")))
              (merge-deep c2))
        insts (->> (:instruments c)
                   (reduce-kv (fn [insts ikey iconfig]
                                (assoc insts ikey
                                       (update iconfig :nn-file
                                               #(str wpath "/" (or % (format "nn/%s.edn"
                                                                             (name ikey)))))))
                              {}))
        c (assoc c :instruments insts)]
    (reset! config c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (init)

  )
