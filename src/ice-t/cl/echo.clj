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

(ns ido.cl.echo
  (:import (java.net InetAddress DatagramPacket DatagramSocket))
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer]])
  (:require [ido.tlv :as tlv])
  (:require [ido.ccnx :as ccnx])
  (:require [ido.forwarder :as fwd])
  (:require [ido.cl.cl :as cl])
  (:require [clojure.tools.logging :as log])
  )


(def clName :echo)

(def instanceCount (atom 0))


(defn echoCL-reply [cl interestMsg]
  (let [itfContext {:itfContext interestMsg}
        name (:name interestMsg)
        object (str "echo for interest for "(:name interestMsg) " from " itfContext)
        response (ccnx/objectMsg name object {:itfContext itfContext})]
    (do
      (log/debug "echoCL-reply " response)
      (>!! fwd/incomingMsgChannel [response (:name cl)]))))



(defn run [cl]
  (let [myChan (:channel cl)]
    
    (do
      (fwd/regItfChan myChan (:prefix cl))
      (reset! (:running cl) true)
      (log/debug "echoCL - FIB:" @fwd/fib)
;1
      (thread                           ; processing outgoing queue
        (do
          (log/debug "echoCL: receiver thread")
          (fwd/addOutputChannel myChan (:name cl))
          (while @(:running cl)
            (do
              (log/debug "echoCL: reading from " myChan)
              (let [interestMsg (<!! myChan)]
                (echoCL-reply cl interestMsg)))))))))
        

(defrecord Cl [name channel prefix running])


(defn instance [& [paras]]
  (let [name (cl/newInstanceName clName instanceCount)
        prefx (cl/prefix paras name)
        this (->Cl name (cl/channel paras) prefx
                   (atom false))]
    this
    ))
  

(defn stop [cl]
  (cl/stop cl))

