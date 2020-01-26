(ns bindi.evaluate-test
  (:require [clojure.test :refer [deftest is testing]]
            [bindi.evaluate :as ev]))

(deftest test-won-lost-simulator
  (let [vb {:symbol :a ,:mode :buy, :target-price 20, :stop-loss-price 10}
        vs {:symbol :a, :mode :sell, :target-price 10, :stop-loss-price 20}
        p1 {:a {:h 30, :l 15, :o 15, :b 1}}
        p2 {:a {:h 30, :l 15, :o 25, :b 1}}
        p3 {:a {:h 15, :l 15, :o 15, :b 1}}
        p4 {:a {:h 15, :l 1, :o 5, :b 1}}
        p5 {:a {:h 15, :l 1, :o 15, :b 1}}
        f #(assoc %1 :terminate-price
                  (assoc (get %2 (:symbol %1)) :close-price %3))]
    (testing "* won or lost buy ventures"
      (is (= (f vb p1 19)
             (ev/won-lost-simulator vb p1)))
      (is (= (f vb p2 24)
             (ev/won-lost-simulator vb p2)))
      (is (= nil
             (ev/won-lost-simulator vb p3)))
      (is (= (f vb p4 4)
             (ev/won-lost-simulator vb p4)))
      (is (= (f vb p5 9)
             (ev/won-lost-simulator vb p5))))
    (testing "* won or lost sell ventures"
      (is (= (f vs p1 21)
             (ev/won-lost-simulator vs p1)))
      (is (= (f vs p2 26)
             (ev/won-lost-simulator vs p2)))
      (is (= nil
             (ev/won-lost-simulator vs p3)))
      (is (= (f vs p4 6)
             (ev/won-lost-simulator vs p4)))
      (is (= (f vs p5 11)
             (ev/won-lost-simulator vs p5))))))

(deftest test-evaluate
  (testing "* evaluate"
    (let [w {:ventures [{:a 1} {:b 1} {:c 1}]}]
      (is (= [{:a 1} {:b 1}]
             ((ev/make-evaluate [(fn [v _] (if (:b v) v))
                                 (fn [v _] (if (:a v) v))])
              w nil))))))
