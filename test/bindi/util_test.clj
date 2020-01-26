(ns bindi.util-test
  (:require [clojure.test :refer [deftest is testing]]
            [bindi.util :as u]))

(deftest test-remove-items
  (testing "* test remove-items"
    (is (= [{:id 2}]
           (u/remove-items [{:id 1} {:id 2}] [{:id 1}])))
    (is (= [{:id 1} {:id 2}]
           (u/remove-items [{:id 1} {:id 2}] [])))
    (is (= [{:a 3}]
           (u/remove-items [{:a 1} {:a 2} {:a 3}] [{:a 1} {:a 2}] :a)))))
