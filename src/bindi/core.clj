(ns bindi.core
  (:require [clojure.core.async :as a]
            [taoensso.timbre :as log]
            [bindi.config :as cfg]
            [bindi.log :as log-cfg]
            [bindi.worker :as wkr]
            [bindi.server :as svr]
            [bindi.backtest :as bt]))

(defn- gc []
  (a/go-loop []
    (System/gc)
    (a/<! (a/timeout 5000))
    (recur)))

(defn init []
  (gc)
  (cfg/init)
  (log-cfg/init)
  (wkr/init)
  (bt/init)
  (svr/init)
  (log/info "booted my fx trading tool!"))

(defn exit []
  (wkr/exit)
  (System/exit 0))

(comment

  (exit)

  )
