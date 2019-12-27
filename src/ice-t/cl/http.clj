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


(ns ido.cl.http
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


(def clName :dir)

(def instanceCount (atom 0))


(defn resolvable? [dnsName]
  (try
    (. InetAddress getByName dnsName)
    (catch Exception e nil)))

(defn toDnsName [hostNameParts]
  (apply str (interpose "." hostNameParts)))


(defn toFileName [localNameParts]
  (apply str (interpose "/" localNameParts)))


(defn toUriPair [[hostPart localPart]]
  [(toDnsName hostPart) (toFileName localPart)])

(defn addNameComponent [name component]
  (str name "." component))

(defn potentialHostNames [parts]
  (loop [r () p (reverse parts)]
  (if (empty? p)
    r
    (recur (conj r p) (rest p)))))

(defn potentialLocalParts [parts]
  (loop [r () p (rest parts)]           ;we can already omit the top level component
  (if (empty? p)
    (conj  r '(""))
    (recur (conj r p) (rest p)))))

(defn potentialUris [parts]
  (ccnx/pairs (reverse (potentialHostNames parts)) (potentialLocalParts parts)))
   
(defn toUri
  "transforms a ccnx name (/org/ietf/about) into a URI (http://ietf.org/about).
  build list of all potential HTTP URIs from name.
  return URI or nil."
  [name]
  (let [parts (ccnx/segments name)
        options (map toUriPair (potentialUris parts))
        [host local] (first (drop-while #(not (resolvable? (% 0))) options))]
    (if host
      (str "http://" host "/" local)
      nil)))

(defn httpCL-sendResource [cl msg object]
  (let [name (:name msg)
        itfContext {:itfContext msg}
        response (ccnx/objectMsg name object {:itfContext itfContext})]
    (do
      (log/debug "httpCL-sendFile " response)
      (>!! fwd/incomingMsgChannel [response (:name cl)]))))


(defn fetch [name]
  "fetches named HTTP resource, specified as CCNX name (like /org/ietf/about)"
  (do
    (log/debug "httpCL-fetch " name)
    (slurp (toUri name))))


(defn stripPrefix [string prefix]
  (clojure.string/replace-first string prefix ""))

(defn httpCL-fetchAndReply [cl msg]
  (let [name (stripPrefix(:name msg) (:prefix cl))
        object (fetch name)]
    (if object
      (httpCL-sendResource cl msg object)
      (log/error "invalid URI for " name)
      )))


(defn run [cl]
  (let [myChan (:channel cl)]
    
    (do
      (fwd/regItfChan myChan (:prefix cl))
      (reset! (:running cl) true)
      (log/debug "httpCL - FIB:" @fwd/fib)
;1
      (thread                           ; processing outgoing queue
        (do
          (log/debug "httpCL: receiver thread")
          (fwd/addOutputChannel myChan :http1)
          (while @(:running cl)
            (do
              (log/debug "httpCL: reading from " myChan)
              (let [msg (<!! myChan)]
                (do
                  (httpCL-fetchAndReply cl msg))))))))))
        



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
