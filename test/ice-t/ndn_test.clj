(ns ido.ndn-test
  (:require [clojure.test :refer :all]
            [ido.ndn :as ndn]))


(deftest tlv-encoding
  (testing "TLV number encoding"
    (let [buf (byte-array 64)
          numbers [64 128 256 512 1024 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152]]
      (doseq [n numbers]
        (let [written (ndn/asTLVNum n buf)
              num (ndn/TLVNum buf)]
          (is (= n num)))))))


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



