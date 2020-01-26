(ns bindi.trade-test
  (:require [clojure.test :refer [deftest is testing]]
            [bindi.trade :as tr]))

(deftest test-venture
  (testing "* test venture"
    (letfn [(scout [_ _] [{:a 1}, {:a 2}])
            (order [ps] (map #(assoc % :cost 100) ps))]
      (is (= {:scout scout, :broker {:order order}
              :ventures [{:a 1, :cost 100} {:a 2, :cost 100}]
              :balance -200}
             (tr/venture {:scout scout
                          :broker {:order order}
                          :ventures []
                          :balance 0}
                         nil))))))

(deftest test-register-broker-terminations
  (testing "* test register-broker-terminations"
    (letfn [(check [vs] (filter :credit vs))]
      (is (= {:ventures [{:id 1}]
              :broker {:check check}
              :balance 50}
             (tr/register-broker-terminations
              {:ventures [{:id 1} {:id 2, :credit 20} {:id 3, :credit 30}]
               :broker {:check check}
               :balance 0}))))))

(deftest test-evaluate-and-terminate
  (testing "* test evaluate-and-terminate"
    (letfn [(evaluate [_ _] [{:id 2} {:id 3}])
            (terminate [vs] (map #(assoc % :credit 100, :profit 10) vs))]
      (is (= {:evaluate evaluate, :broker {:terminate terminate}
              :ventures [{:id 1}]
              :balance 200, :profit 20}
             (tr/evaluate-and-terminate
              {:evaluate evaluate, :broker {:terminate terminate}
               :ventures [{:id 1} {:id 2} {:id 3}]
               :balance 0, :profit 0}
              nil))))))
