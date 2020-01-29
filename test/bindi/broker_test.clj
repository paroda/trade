(ns bindi.broker-test
  (:require [clojure.test :refer [deftest is testing]]
            [bindi.broker :as br]))

(deftest test-order-simulated
  (testing "* test forex order"
    (let [p1 {:status :prospect
              :symbol :eur-usd, :mode :buy, :quantity 100
              :open-price 1.23456}
          p2 {:status :prospect
              :symbol :eur-usd, :mode :sell, :quantity 10
              :open-price 1.12345}
          p3 {:status :prospect
              :symbol :eur-usd, :mode :buy, :quantity 100}
          p4 {:symbol :eur-usd, :mode :sell, :quantity 100
              :open-price 1.12345}]
      (is (= [(assoc p1 :status :venture, :cost 320.0)
              (assoc p2 :status :venture, :cost 32.0)]
           (br/order-simulated
            {:eur-usd {:type :fx, :mmr 3.2}}
            [p1 p2 p3 p4]))))))

(deftest test-terminate-simulated
  (testing "* test forex terminate"
    (let [v1 {:status :venture
              :symbol :eur-usd, :mode :buy, :quantity 100
              :cost 320.0, :open-price 1.00010
              :close-price 1.00030}
          v2 {:status :venture
              :symbol :eur-usd, :mode :sell, :quantity 10
              :cost 32.0, :open-price 1.00010
              :close-price 1.00050}
          v3 {:status :venture
              :symbol :eur-usd, :mode :buy, :quantity 10, :cost 32.0}
          v4 {:symbol :eur-usd, :mode :buy, :quantity 100
              :cost 320.0, :open-price 1.00010
              :close-price 110}]
      (is (= [(assoc v1 :status :terminated, :credit 330.0, :profit 10.0)
              (assoc v2 :status :terminated, :credit 27.0, :profit -5.0)]
             (br/terminate-simulated
              {:eur-usd {:type :fx, :mmr 3.2, :spread 1
                         :pip-value 0.0001, :pip-cost 0.1}}
              [v1 v2 v3 v4]))))))
