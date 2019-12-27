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


(ns ido.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:require [ido.tlv :as tlv])
  (:require [ido.ccnx :as ccnx])
  (:require [ido.ndn :as ndn])
  (:require [ido.forwarder :as fwd])
  (:require [ido.cl.udp :as udp])
  (:require [ido.cl.echo :as echo])
  (:require [ido.cl.directory :as dir])
  (:require [ido.cl.http :as http])
  (:require [clojure.tools.logging :as log])
  )



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
            

(defn -main []
  (do
    (println "starting...")
    (let [
          echo1 (echo/instance {:prefix "echo1"})
          udp1 (udp/instance {:prefix "/udp1"})
          dir1 (dir/instance {:prefix "/dirk", :directory "/home/kutscher/prj/icn/ido"})
          http1 (http/instance "/http")]
      (do
        (udp/run echo1)
        (udp/run udp1)
        (udp/run dir1)
        (udp/run http1)
        (fwd/ccnxServer)
        (Thread/sleep 60000)                ; run for 1 minute for now
        (println "done")))))
