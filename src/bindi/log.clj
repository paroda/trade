(ns bindi.log
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.3rd-party.rotor :refer [rotor-appender]]
            [bindi.config :refer [config]]))

(defn init []
  (let [{{:keys [level rotor-log-file]
          :or {level :debug}} :log
         wpath :workspace} @config
        rotor-log-file (or rotor-log-file (str wpath "/rotor.log"))]
    (timbre/set-config!
     {:level level
      :timestamp-opts timbre/default-timestamp-opts
      :output-fn timbre/default-output-fn
      :appenders
      {:println {:enabled? true
                 :async? false
                 :min-level nil
                 :rate-limit nil
                 :output-fn :inherit
                 :fn (fn [data]
                       (let [{:keys [output_]} data
                             formatted-output-str (force output_)]
                         (println formatted-output-str)))}
       :rotor (if rotor-log-file
                (assoc (rotor-appender {:path rotor-log-file
                                        :max-size (* 1024 1024)
                                        :backlog 10})
                       :output-fn (partial timbre/default-output-fn
                                           {:stacktrace-fonts {}})))}})))
