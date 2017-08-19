(ns pink.sfz-test
  (:require [clojure.test :refer :all]
            [pink.sfz :refer :all]
            [clojure.java.io :as io]
            ))

(deftest sfz-lookup-test 
  (let [sfz-data (load-sfz (io/resource "pink/1st-violin-SEC-sustain.sfz"))]
    (testing "lookup"
      (is (= nil (sfz-lookup sfz-data 0 60 127)))
      )))
