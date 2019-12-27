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

(ns ido.cl.udp
  (:import (java.net InetAddress DatagramPacket DatagramSocket))
  (:require [clojure.string :refer [replace-first]])
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer dropping-buffer]])
  (:require [ido.tlv :as tlv])
  (:require [ido.ccnx :as ccnx])
  (:require [ido.forwarder :as fwd])
  (:require [clojure.tools.logging :as log])
  )



;; instance: create new instance, parameters: port, destination ip,port
;; stop: deregister and close channel

;; instance state:
;; - UDP socket
;; - channel
;; - receive buffer

(def clName :udp)

(def instanceCount (atom 0))

(defn newInstanceName []
  (symbol (str clName (swap! instanceCount inc))))

(defn localhost [] (. InetAddress getLocalHost))

(def defaultPort 8888)
(def defaultDest {:addr (localhost) :port 8888})
  


(defn message [cl text]
  (new DatagramPacket (. text getBytes) (. text length) (localhost) (:port cl)))

(defn send-udp [cl datagram]
  (.send @(:socket cl) datagram))

(defn send-message [cl text]
  (send-udp cl (message cl text)))

(defn send-buf [cl buf length addr port]
  (do
    (log/debug "send-buf: " cl buf length addr port)
    (send-udp cl (new DatagramPacket buf length addr port))))

(defn create-udp-server [port]
  (DatagramSocket. port))

(defn stop-udp-server [cl]
  (.close @(:socket cl)))


(defn empty-message [n] 
  (new DatagramPacket (byte-array n) n))

(defn parseBuf [cl buf addr port]
  (let [itfContext {:addr addr, :port port, :itfName (:name cl)}
        msg (assoc (ccnx/parseMsg buf) :itfContext itfContext)]
    (do
      (log/debug "parseBuf: " msg)
      msg)))

(defn rcv2buf [cl]
  (let [orig-packet (:rcvBuf cl)]
    (do
      (.receive @(:socket cl) orig-packet)
      (log/debug "rcv2buf: received data")
      [(.getData orig-packet) (.getAddress orig-packet) (.getPort orig-packet)])))


(defn udpCL-send [cl msg buf]
  (do
    (log/debug "udpCL-send received " msg " on output channel.")
    (let [
          itfContext (msg :itfContext)
          length (ccnx/toBuf msg buf 0 10240) ;; TODO check size
          res (case (:type msg)
                :object (send-buf cl buf length (:addr itfContext ) (:port itfContext))
                :interest (send-buf cl buf length
                                    (:addr (:destination cl))
                                    (:port (:destination cl))))
          ]
      (log/debug "send-buf result: " res))))


(defn run [cl]
  (let [myBuf (byte-array 10240)
        myChan (:channel cl)]      ;; when is this going to be released?
    
    (do
      (fwd/regItfChan myChan (:prefix cl))  
;1
      (thread                           ; processing outgoing queue
        (do
          (log/debug "udpCL: receiver thread")
          (fwd/addOutputChannel myChan (:name cl))
          (while @(:running cl)
            (do
              (log/debug "udpCL: reading from " myChan)
              (let [msg (<!! myChan)]
                (udpCL-send cl msg myBuf))))
          (log/debug "udpCL: terminating receiver thread")))
;2        
    (thread                             ;processing UDP interface
      (while @(:running cl)
        (let [[buf addr port] (rcv2buf cl)
              msg (parseBuf cl buf addr port)]
          (if msg
            (do
              (log/debug "udpCL: forwarding to incomingMsgChannel")
              (>!! fwd/incomingMsgChannel [msg (:name cl)]))
            (log/error "udpCL parsing error, msg: " msg)
            )))))))
        



(defn fetch-udp [cl objectName]
  (let [fetchChan (chan)
        itfContext {:itfName :internal2}
        msg (ccnx/interestMsg objectName {:itfContext itfContext})
        myBuf (byte-array 10240)]
    (do
      (fwd/addOutputChannel fetchChan :internal2)
                                        ;      (fwd/processInterest msg :internal2)
      (udpCL-send cl msg myBuf)
      
      (let [[msg channel] (alts!! [fetchChan (timeout 1000)])]
        (do
          (fwd/rmOutputChannel :internal2)
          msg)))))


(defrecord Cl [name channel prefix socket
               port destination rcvBuf running])


(defn clChannel [paras]                 ; this should be a generic CL function
  (let [bufsize (:bufsize paras 1)
        qdisc (:qdisc paras :droppOld)
        buffer (case qdisc
                 :droppOld (sliding-buffer bufsize)
                 :dropNew (dropping-buffer bufsize)
                 )]
    (chan buffer)))
  

(defn endpoint [paras]
  {:port (:port (:destination paras) defaultPort)
   :addr (. InetAddress getByName (:addr (:destination paras) nil))}
  )

(defn instance [& [paras]]
  (let [name (newInstanceName)
        prefix (:prefix paras (replace-first name ":" "/"))
        port (:port paras defaultPort)
        socket (atom (create-udp-server port))
        destination (endpoint paras)
        this (->Cl name (clChannel paras) prefix socket
                   port destination (empty-message 10240)
                   (atom true))] ;check size
    this
    ))
  

(defn stop [cl]
  (do
    (fwd/unregItfChan (:prefix cl))
    (reset! (:running cl) false)
    (stop-udp-server cl)
    (close! (:channel cl))
    cl))
