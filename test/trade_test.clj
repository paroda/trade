(ns trade-test.clj
  (:require [clojure.test :refer [deftest is testing]]
            [trade :as trade]))

(deftest unused-functions
  (testing "* note-closed-ventures: in-future"
    (is (= 0 (trade/note-closed-ventures 0)))))

(deftest test-try-deal
  (let [pt1 {:mode :buy, :quantity 10, :stop-loss-price 11, :target-price 21}
        pt2 {:mode :sell, :quantity 10, :stop-loss-price 21, :target-price 11}
        p1 {:o 15, :b 1}
        p2 {:o 9, :b 1}
        p3 {:o 25, :b 1}]
    (testing "* buy orders"
      (is (= {:target-price 21
              :got-deal? true
              :mode :buy
              :cost 160
              :quantity 10
              :order-price {:o 15, :b 1}
              :stop-loss-price 11}
             (trade/try-deal (assoc pt1 :price p1))))
      (is (= {:mode :buy
              :quantity 10
              :stop-loss-price 11
              :target-price 21
              :price {:o 9, :b 1}}
             (trade/try-deal (assoc pt1 :price p2))))
      (is (= {:mode :buy
              :quantity 10
              :stop-loss-price 11
              :target-price 21
              :price {:o 25, :b 1}}
             (trade/try-deal (assoc pt1 :price p3)))))
    (testing "* sell order"
      (is (= {:mode :sell
              :quantity 10
              :stop-loss-price 21
              :target-price 11
              :got-deal? true
              :order-price {:o 15, :b 1}
              :cost -140}
             (trade/try-deal (assoc pt2 :price p1))))
      (is (= {:mode :sell
              :quantity 10
              :stop-loss-price 21
              :target-price 11
              :price {:o 9, :b 1}}
             (trade/try-deal (assoc pt2 :price p2))))
      (is (= {:mode :sell
              :quantity 10
              :stop-loss-price 21
              :target-price 11
              :price {:o 25, :b 1}}
             (trade/try-deal (assoc pt2 :price p3)))))))