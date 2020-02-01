(ns bindi.config
  (:require [clojure.edn :as edn]))

(defonce config (atom {}))

(defn init []
  (let [c (->> "config.edn" slurp edn/read-string)]
    (reset! config c)))