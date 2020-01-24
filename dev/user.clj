(ns user)

(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(import java.util.Date)
(import java.text.SimpleDateFormat)

(def fmt (SimpleDateFormat. "yyyyMMdd HHmmss"))

(defn load-csv [path] 
  (with-open [r (io/reader path)]
    (->> (csv/read-csv r :separator \;)
          ;; (take 1)
         (mapv (fn[[t o h l c]]
                  {:t (.parse fmt t)
                   :h (Double/parseDouble h)
                   :l (Double/parseDouble l)
                   :o (Double/parseDouble o)
                   :c (Double/parseDouble c)})))))

(def comm 0.00010)

;; quote
;; :t = time
;; :o,:h,:l,:c = open,high,low,close price

;; transaction
;; :m = buy/sell (:b or :s)
;; :q = quantity
;; :sl,:tp = stop loss, target price
;; :st,:sp,:sa = start time,price,amount
;; :ct,:cp,:ca = close time,price,amount
;; :g = gain

(defn open [{:keys [m q sl tp] :as tr} {:keys [t o]}]
  (if (case m
        :b (and (< sl o) (> tp o))
		:s (and (> sl o) (< tp o)))
   (let [a (* q (case m :b (+ o comm) :s (- o comm)))]
     (assoc tr :st t, :sp o, :sa a))))

(defn try-close [{:keys [m tp sl q sa] :as tr}
                 {:keys [t o h l]}]
  (if-let [p (case m
              ;; buy mode, need to sell out
              :b (if (>= h tp) (max o tp) (if (<= l sl) (min o sl)))
			  ;; sell mode, need to buy back
		      :s (if (<= l tp) (min o tp) (if (>= h sl) (max o sl))))]
    ;; close
	(let [a (* q (case m :b (- p comm) :s (+ p comm)))
          g (case m :b (- a sa) :s (- sa a))]	
	  (assoc tr :ct t, :cp p, :g g, :ca a))
	;; no close
	tr))

;; state
;; bal = balance
;; otrs,ctrs = open and closed transactions

(defn new-state []
  {:n 0, :lts 0, :bal 0, :gain 0, :otrs [], :ctrs []})

(def hrs 24) ;; new order after hours
(def sl-margin 0.00300) ;; stop loss margin
(def tp-margin 0.01000) ;; target price margin

(defn new-tr [{:keys [lts n] :as state}
              {:keys [t o]}]
  (if (< (* hrs 3600e3) (- (.getTime t) lts))
    ;; a day has passed, ready to start new transaction
    [(assoc state :lts (.getTime t), :n (inc n))
	 ;; {:i n, :m :b, :q 10, :sl (- o sl-margin), :tp (+ o tp-margin)}
	 {:i n, :m :s, :q 10, :sl (+ o sl-margin), :tp (- o tp-margin)}
	 ]
	;; too soon, do not start new transaction
	[state nil]))

(defn proc-open [state quote]
  (let [[state tr] (new-tr state quote)]
    (if-let [{:keys [m sa] :as tr} (and tr (open tr quote))]
	  (-> state
		(update :otrs conj tr)
		(update :bal (case m :b - :s +) sa))
	  state)))

(defn proc-close [state quote]
  (let [[otrs ctrs bal gain]
        (->> (:otrs state)
             (map #(try-close % quote))
	         (reduce (fn [[os cs bal gain] {:keys [m ca g] :as tr}]
	                   (if ca
                         [os (conj cs tr) ((case m :b + :s -) bal ca) (+ gain g)]
                         [(conj os tr) cs bal gain]))
                     [[] [] (:bal state 0) (:gain state 0)]))]
    (assoc state :otrs otrs
                 :ctrs (into (:ctrs state) ctrs)
                 :bal bal, :gain gain)))

(defn track-bal [state]
  (let [{:keys [bal max-bal min-bal] :or {max-bal 0, min-bal 0}} state]
    (assoc state :max-bal (max bal max-bal), :min-bal (min bal min-bal))))

(defn proc [state quote]
  (-> state
      (proc-close quote)
	  (track-bal)
	  (proc-open quote)
	  (track-bal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
  (def d2017 (->> "2017-eurusd.edn" slurp clojure.edn/read-string))
  (def d2018 (->> "2018-eurusd.edn" slurp clojure.edn/read-string))
  (def d2019 (->> "2019-eurusd.edn" slurp clojure.edn/read-string))

  (def hrs 24) ;; new order after hours
  (def sl-margin 0.00500) ;; stop loss margin
  (def tp-margin 0.05000) ;; target price margin
  (do (def s (reduce proc (new-state) d2017)) (dissoc s :otrs :ctrs))
)