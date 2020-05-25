(ns bindi.server
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.middleware.defaults :as ring-defaults :refer [wrap-defaults]]
            [org.httpkit.server :as hk]
            [compojure.core :as cc]
            [compojure.route :as cr]
            [ring.util.response :refer [redirect response
                                        file-response resource-response]]
            [bindi.util :as util]
            [bindi.config :as cfg]
            [bindi.view :as view])
  (:import java.util.Date
           java.text.SimpleDateFormat))

(defonce ^:private state (atom {}))

(defn reflect-request [req]
  {:status 200
   :body (-> req
             (update :async-channel str)
             (update :body #(if % (slurp %))))})

(defn file-html-response [path]
  (if-let [res (file-response path)]
    (assoc-in res [:headers "Content-Type"] "text/html")))

(defn resource-html-response [path]
  (if-let [res (resource-response path)]
    (assoc-in res [:headers "Content-Type"] "text/html")))

(defn info [_]
  {:status 200
   :body {:config (assoc-in @cfg/config [:fxcm :password] nil)}})

(defn chart-price-indicators [req]
  (let [{:keys [ikey tfrm n]} (:params req)
        ikey (keyword ikey)
        n (Integer. n)
        {:strs [dto]} (:query-params req)
        dto (if-not (str/blank? dto)
              (.parse (SimpleDateFormat. "yyyy-MM-dd") dto))]
    (view/chart-price-indicators ikey tfrm n dto)))

(defn backtest-strategy [req]
  (let [{:keys [ikey tfrm n]} (:params req)
        ikey (keyword ikey)
        n (Integer. n)
        {:strs [dto]} (:query-params req)
        dto (if-not (str/blank? dto)
              (.parse (SimpleDateFormat. "yyyy-MM-dd") dto))]
    (view/backtest-strategy ikey tfrm n dto)))

(cc/defroutes app-routes
  (cc/GET "/" [] "My FX trading tool!")
  (cc/GET "/info" [] info)
  (cc/context "/reflect" []
              (cc/ANY "/*" [] reflect-request))

  (cc/GET "/active-pi/:ikey" [ikey] (view/active-chart-price-indicators
                                     (keyword ikey)))
  (cc/GET "/chart-pi/:ikey/:tfrm/:n" [] chart-price-indicators)
  (cc/GET "/backtest/:ikey/:tfrm/:n" [] backtest-strategy)

  (cc/GET "/closed-trades/:ikey" [ikey] (view/closed-trades (keyword ikey)))
  (cc/GET "/chart-1/:ikey" [ikey] (view/chart-1 (keyword ikey))))


;; site-defaults for reference
#_
{:params    {:urlencoded true
             :multipart  true
             :nested     true
             :keywordize true}
 :cookies   true
 :session   {:flash true
             :cookie-attrs {:http-only true, :same-site :strict}}
 :security  {:anti-forgery   true
             :xss-protection {:enable? true, :mode :block}
             :frame-options  :sameorigin
             :content-type-options :nosniff}
 :static    {:resources "public"}
 :responses {:not-modified-responses true
             :absolute-redirects     true
             :content-types          true
             :default-charset        "utf-8"}}

(defn make-handler []
  (-> app-routes
      (wrap-restful-format :formats [:json-kw :edn]
                           :response-options {:json-kw {:pretty true}})
      (wrap-defaults (assoc ring-defaults/site-defaults
                            :security false, :static false))))

(defn stop []
  (when-let [stop-server (:stop-server @state)]
    (stop-server :timeout 1000)
    (swap! state dissoc :stop-server)))

(defn run-server []
  (stop)
  (let [port (try
               (Integer. (System/getenv "HTTP_PORT"))
               (catch Exception _ 9080))
        handler (make-handler)
        options {:port port
                 :error-logger #(log/error %2 "HttpKit:" %1)
                 :warn-logger #(log/warn "HttpKit:" %1 ":"
                                         (.getName (type %2))
                                         (if %2 (.getMessage %2)))}
        stop-server (hk/run-server handler options)]
    (swap! state assoc :stop-server stop-server)
    (log/info "started HTTP server on port:" port)
    true))

(defn init []
  (run-server))
