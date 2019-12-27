(ns ido.core-test
  (:require [clojure.test :refer :all]
            [ido.ccnx :as ccnx]))


(deftest interestParsing-test
  (testing "message generator/parser"
    (let [testbuf (byte-array 1024)
          content "testing message generator/parser"
          msg (ccnx/objectMsg "/foo/bar/42" content)]
          (do
            (ccnx/object2buf msg testbuf 0 1024)
            (let [pmsg (ccnx/parseMsg testbuf)
                  resultVal (strObject pmsg)]
              (is (= content resultVal))
              (is (= (:version pmsg) 1))
              (is (= (:type pmsg) :object)))))))



;;;;;;;;;;

(def ccnxmsg1
  (byte-array (map byte [1 1 2 8
                         0 0 0 24
                         0 42 0 4
                         1 1 1 1
                         0 13 0 4
                         2 2 2 2])))

(def ccnxmsg2
  (byte-array (map byte [1 1 2 8
                         0 0 0 8
                         0 42 0 4
                         1 1 1 1
                         0 13 0 4
                         2 2 2 2])))



