(ns bindi.broker-test
  (:require [clojure.test :refer [deftest is testing]]
            [bindi.broker :as br]))

(deftest test-order-simulated
  (testing "* test order"
    (let [p1 {:status :prospect, :mode :buy, :quantity 10
              :order-price {:open-price 100}}
          p2 {:status :prospect, :mode :sell, :quantity 10
              :order-price {:open-price 110}}
          p3 {:status :prospect, :mode :buy, :quantity 10}
          p4 {:mode :sell, :quantity 10
              :order-price {:open-price 110}}]
      (is (= [(assoc p1 :status :venture, :cost 1000)
              (assoc p2 :status :venture, :cost -1100)]
           (br/order-simulated [p1 p2 p3 p4]))))))

(deftest test-terminate-simulated
  (testing "* test terminate"
    (let [v1 {:status :venture, :mode :buy, :quantity 10, :cost 900
              :terminate-price {:close-price 100}}
          v2 {:status :venture, :mode :sell, :quantity 10, :cost -1200
              :terminate-price {:close-price 110}}
          v3 {:status :venture, :mode :buy, :quantity 10, :cost 900}
          v4 {:mode :buy, :quantity 10, :cost -1200
              :terminate-price {:close-price 110}}]
      (is (= [(assoc v1 :status :terminated, :credit 1000, :profit 100)
              (assoc v2 :status :terminated, :credit -1100, :profit 100)]
             (br/terminate-simulated [v1 v2 v3 v4]))))))
