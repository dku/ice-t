;; Copyright 2016 Dirk Kutscher

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns ido.core-test
  (:require [clojure.test :refer :all])
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:require [ido.tlv :as tlv])
  (:require [ido.ccnx :as ccnx])
  (:require [ido.forwarder :as fwd])
  (:require [ido.cl.udp :as udp])
  (:require [ido.cl.echo :as echo])
  (:require [ido.cl.directory :as dir])
  (:require [ido.cl.http :as http])
  (:require [clojure.tools.logging :as log])
  )



;; (deftest nameToTlv-test
;;   (testing "nameToTlv"
;;     (let [testbuf (byte-array 1000)
;;           ccnName "/foo/bar/42"]
;;       (do
;;         (nameToTlv ccnName testbuf 0 1000)
;;         (let [tlv (getTlvAt testbuf 0)]
;;           (is (= ccnName (parseNameTlv tlv))))))))



;; (deftest a-test
;;   (testing "FIXME, I fail."
;;     (is (= 0 1))))


(def tlvtest1
  (byte-array (map byte [0 1 2 3 10])))

(def tlvtest2
  (byte-array (map int [0 1 0 255 10])))


(def tlvtest4
  (byte-array (map int [0 0 0 1])))


(def tlvtest5
  (byte-array (map int [1 0 0 0 0])))

(def tlvtest6
  (byte-array (map int [0 0 1 0 0])))


(def tlvtest7
  (byte-array (map int [1 1 1 0 0])))


(def ccnxNames
  ["/foo/bar/42"
   "/App:42=0x123456789/App:43=0x1024/App:44=0x1/test"])

(deftest nameParsing-test
  (testing "name parser"
    (doseq [n ccnxNames]
      (is (= n (ccnx/nameToStr (ccnx/strToName n)))))))


(deftest nameTlvParsing-test
  (testing "name TLV parser"
    (doseq [n ccnxNames]
      (is (= n (ccnx/nameToStr (ccnx/parseNameTlv (ccnx/nameTlv n))))))))


(deftest interestParsing-test
  (testing "message generator/parser"
    (let [testbuf (byte-array 1024)
          content "testing message generator/parser"
          msg (ccnx/objectMsg "/foo/bar/42" content)]
          (do
            (ccnx/object2buf msg testbuf 0 1024)
            (let [pmsg (ccnx/parseMsg testbuf)
                  resultVal (ccnx/strObject pmsg)]
              (is (= content resultVal))
              (is (= (:version pmsg) 1))
              (is (= (:type pmsg) :object)))))))



(defn fetch [objectName]
  (let [fetchChan (chan)
        itfContext {:itfName :internal}
        msg (ccnx/interestMsg objectName {:itfContext itfContext})]
    (do
      (fwd/addOutputChannel fetchChan :internal)
      (fwd/processInterest msg :internal)
      (let [[msg channel] (alts!! [fetchChan (timeout 1000)])]
        (do
          (fwd/rmOutputChannel :internal)
          msg)))))


(deftest echo-test
  (testing "echo CL"
    (let [echo1 (echo/instance {:prefix "/echo1"})]
      (do
        (echo/run echo1)   
        (fwd/ccnxServer)
        (let [msg (fetch "/echo1/testing")
              result (if msg (ccnx/strObject msg) nil)]
          (log/debug "received: '" result "'")
          (is (not (= nil msg)))
          (is (.startsWith  result "echo for interest for /echo1/testing")))))))



(deftest buffer-parsing-speed
  (testing "parsing byte buffer to internal representation 10000 times"
    (let [testbuf (byte-array 1024)
          content (apply str (take 10 (repeat "0123456789"))) ; 100 bytes
          msg (ccnx/objectMsg "/test/parsing-speed/1000times" content)
          _ (ccnx/object2buf msg testbuf 0 1024)]
      (time (dotimes [n 1000000]
        (ccnx/parseMsg testbuf))))))


(deftest echo-test-many
  (testing "sending 100000 test INTERESTS to echo server"
    (let [echo1 (echo/instance {:prefix "/echo1"})]
      (do
        (echo/run echo1)   
        (fwd/ccnxServer)
        (time  (dotimes [n 100000]
        (let [msg (fetch (str "/echo1/testing/" n))
              result (if msg (ccnx/strObject msg) nil)]
          (log/debug "received: '" result "'")
          (is (not (= nil msg)))
          (is (.startsWith  result "echo for interest for /echo1/testing")))))))
    ))
    
