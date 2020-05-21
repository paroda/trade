(ns bindi.core
  (:require [taoensso.timbre :as log]
            [bindi.config :as cfg]
            [bindi.log :as log-cfg]
            [bindi.worker :as wkr]
            [bindi.server :as svr]))

(defn init []
  (cfg/init)
  (log-cfg/init)
  (wkr/init)
  (svr/init)
  (log/info "booted my fx trading tool!"))

(defn exit []
  (wkr/exit)
  (System/exit 0))

(comment

  (exit)

  )
