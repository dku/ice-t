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

(ns ido.forwarder
  (:import (java.net InetAddress DatagramPacket DatagramSocket))
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer]])
  (:require [ido.tlv :as tlv])
  (:require [ido.ccnx :as ccnx])
  (:require [clojure.tools.logging :as log])
  )


(def forwarder-running (atom false))

(defn stop-forwarder []
  (reset! forwarder-running false))

(def incomingMsgChannel (chan (sliding-buffer 10240)))         ;queue of 10 msgs to facilitate testing

;(def objectChannel (chan))
;(def otherChannel (chan))

;;(def defaultForwardingChannel (chan 2))

                                        ;(def outputChannels [{:default defaultForwardingChannel}])
                                        

;; ;vector of maps of {:interface chan}
;; (def outputChannels [])                 ; vector of maps of {:interface chan}


(def outputChannel (ref {})) ; map of {:interface chan}


(defn addOutputChannel [ch itfName]
  (dosync (alter outputChannel assoc itfName ch)))

(defn rmOutputChannel [itfName]
  (dosync (alter outputChannel dissoc itfName)))


(defn now []
  (System/currentTimeMillis))

(defn newTtl [& args]
  (+ (now) 1000))

;; (def pit (ref nil))

;; (dosync
;;  (ref-set pit {"/info/dirk-kutscher/example"
;;                {:incomingIfs #{:udp0}   ; maybe this should not a set, but rather a map {:itf [itfContext]}
;;                 :itfContext {:udp0 {:addr "127.0.0.1", :port 6666}}
;;                 :ttlms (newTtl)}}))


;; (defn rmPitEntry [name]
;;   (dosync (alter pit dissoc name)))


(def pit (atom {"/info/dirk-kutscher/example"
               {:incomingIfs #{:udp0}   ; maybe this should not a set, but rather a map {:itf [itfContext]}
                :itfContext {:udp0 {:addr "127.0.0.1", :port 6666}}
                :ttlms (newTtl)}}))

(defn addPitEntry [name entry]
  (swap! pit assoc name entry))


(defn rmPitEntry [name]
  (swap! pit dissoc name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def fib (ref nil))

;; (dosync (ref-set fib (sorted-map)))

;; (defn regItfChan [ch prefix]
;;   (dosync (alter fib assoc prefix ch)))

;; (defn unregItfChan [prefix]
;;   (dosync (alter fib dissoc prefix)))


(def fib (atom (sorted-map)))

(defn regItfChan [ch prefix]
  (swap! fib assoc prefix ch))

(defn unregItfChan [prefix]
  (swap! fib dissoc prefix))


(defn prefix-match
  "return subset of fib that matches name based on name prefix keys.
  sorted by matched prefix length"
  [sorted-map name]
  (let [matches (filter #(.startsWith name (key %)) (rsubseq sorted-map <= name))]
    (reverse (sort-by #(count (% 0)) matches))))


(defn forwardInterest
  "try to find suitable interface based on LPM && forward msg to corresponding channel.
  Return new PIT entry or nil if no prefix matched"
  [msg itf]
  
  (do
    (log/debug "forwarding interest:" msg)
    (let [interfaces (prefix-match @fib (msg :name))
          nextItf (first interfaces)]   ;pick one interface (most specific prefix) for now
      (if nextItf
        (do
          (log/debug "forwarding to " nextItf)
          (>!! (val nextItf)  msg)
          {:itfContext {itf (get msg :itfContext)}, :incomingIfs #{itf}, :ttlms (newTtl)}
          )
        (do
          (log/debug "no prefix match -- ignoring interest...") ; should not include this in the PIT then
          nil))
      )
    ))

(defn processInterest [msg itf]
  (let [name (msg :name)]
    (do
      (log/debug ["processInterest:" msg itf])
      (dosync
       (let [entry (get @pit name)
             newEntry (if entry          ; already exists...
                        (-> entry
                            (update-in [:incomingIfs] conj itf)
                            (update-in [:itfContext] assoc itf (get msg :itfContext))
                            (update-in [:ttlms] newTtl))
                        ;; else
                        (forwardInterest msg itf))] ; should forward interest *after* putting it into PIT...
         (if newEntry
           (addPitEntry (:name msg) newEntry))
;           (alter pit assoc (:name msg) newEntry))
         (log/debug "processInterest, PIT: " @pit))))))


(defn processObject [msg itf]           ;TODO: check this...
  (let [name (msg :name)
        interest (get @pit name)]
    (do
      (log/debug ["processObject:" msg itf interest])
      (if interest
        (doseq [i (:incomingIfs interest)]
          (let [object (:object msg)
                objectSize (:objectSize msg)
                itfContext (get (:itfContext interest) i)
                paras {:itfContext itfContext, :object object, :objectSize objectSize}
                ;; note: we should rather copy all entries and then update itfContext
                response (merge (ccnx/objectMsg name) paras)
                channel (@outputChannel i)]
            (do
              (log/debug "sent " response)
              (log/debug "processObject: removing PIT entry for " name)
              (rmPitEntry name)
              (log/debug "processObject: sending to channel " channel interest)
              (>!! channel response))))
        ;; else
        (log/debug "No matching interest for " name)))))




(def processFun
  {:interest processInterest
   :object processObject
;   :interestReturn processInterestReturn
   nil (fn [msg itf] nil)
   })


(defn ccnxServer []
  (do
;    (dosync (ref-set pit {}))           ;clear PIT
    (reset! pit {})
    (reset! forwarder-running true)
  (thread
    (while @forwarder-running
                                        ;      (let [inputChannels [incomingMsgChannel objectChannel otherChannel (timeout 1000)]
      (let [inputChannels [incomingMsgChannel (timeout 1000)]
            ;;        allChannels (into inputChannels outputChannels) ; why listen on outputChannel?
            allChannels inputChannels
            [[msg itf] channel] (alts!! allChannels)]
        (do
          (log/debug ["ccnxServer:" msg itf])
          (if msg
            ((processFun (msg :type)) msg itf)
            (log/debug "ccnxServer expired")))))
    (log/debug "ccnxServer stopped"))))





