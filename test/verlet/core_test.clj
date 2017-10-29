(ns verlet.core-test
  (:require [clojure.test :refer :all]
            [verlet.core :refer :all]))

(deftest should-update-points
  (testing "should update points"
    (let [input-points    (atom
                           [{:x 10 :y 10 :oldx 5  :oldy 5}
                            {:x 50 :y 30 :oldx 30 :oldy 15}])
          expected-points (atom
                           [{:x 15 :y 15 :oldx 10 :oldy 10}
                            {:x 70 :y 45 :oldx 50 :oldy 30}])
          output-points   (update-points input-points)]
      (is (= @expected-points output-points)))))
