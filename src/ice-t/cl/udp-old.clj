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
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer]])
  (:require [ido.tlv :as tlv])
  (:require [ido.ccnx :as ccnx])
  (:require [ido.forwarder :as fwd])
  (:require [clojure.tools.logging :as log])
  )


(defprotocol Hexl
  (hexl-hex [val])
  (hexl-char [char]))

(extend-type Number
  Hexl
  (hexl-hex [i]
	    (let [rtnval (Integer/toHexString (if (< i 0) (+ 256 i) i)) ]
	      (if (< (count rtnval) 2) (str "0" rtnval) rtnval)))
  (hexl-char [b]
	     (let [v (if (< b 0) (+ 256 b) b)
		   c  (char v)]
	       (if  (and (< v 128 )(Character/isLetter c)) (.toString c) "."))))



(extend-type Character
  Hexl
  (hexl-hex [char]
	    (hexl-hex (int (.charValue char))))
  (hexl-char [char]
	     (hexl-char (int (.charValue char)))))


(defn hexlify 
  "Perform similar to hexlify in emacs.  Accept a seq of bytes and
convert it into a seq of vectors.  The first element of the vector is a
seq of 16 strings for the HEX value of each byte.  The second element
of the vector is a seq of the printable representation of the byte and the
third elevment of thee vector is a seq of the integer value for each
byte.  Works for chars as well."
  ([bytes] (hexlify bytes 16))
  ([bytes size]
     (let [parts (partition-all size bytes)]
       (for [part parts]
	 [ (map hexl-hex part) (map hexl-char part) (map int part)]))))

(defn hexlify-chars 
    "Convert the bytes into a string of printable chars 
     with . being used for unprintable chars"
  [bytes]
  (let [chars (mapcat second (hexlify bytes))]
    (apply str chars)))



(defn localhost [] (. InetAddress getLocalHost))
(def udp-server (ref nil))

(def port 10032)
(def defaultDest {:addr (localhost) :port 8888})
  


(defn message [text]
  (new DatagramPacket (. text getBytes) (. text length) (localhost) port ))

(defn send-udp [datagram]
  (.send @udp-server datagram))

(defn send-message [text]
  (send-udp (message text)))

(defn send-buf [buf length addr port]
  (do
    (log/debug "send-buf: " buf length addr port)
    (send-udp (new DatagramPacket buf length addr port))))

(defn create-udp-server []
  (DatagramSocket. port))

(defn start-udp-server []
  (dosync (ref-set udp-server (create-udp-server))))

(defn stop-udp-server []
  (.close @udp-server))


(defn empty-message [n] 
  (new DatagramPacket (byte-array n) n))

(def orig-packet (empty-message 1024))

(defn parseBuf [buf addr port]
  (let [itfContext {:addr addr, :port port, :itfName :udp1}
        msg (assoc (ccnx/parseMsg buf) :itfContext itfContext)]
    (do
      (log/debug "parseBuf: " msg)
      msg)))


(defn rcv2buf []
  (do
    (.receive @udp-server orig-packet)
    (log/debug "rcv2buf: received data")
    [(.getData orig-packet) (.getAddress orig-packet) (.getPort orig-packet)]))




(defn udpCL-send [msg buf]
  (do
    (log/debug "udpCL-send received " msg " on output channel.")
    (let [
          itfContext (msg :itfContext)
          length (ccnx/toBuf msg buf 0 10240)
          res (case (:type msg)
                :object (send-buf buf length (:addr itfContext ) (:port itfContext))
                :interest (send-buf buf length (:addr defaultDest) (:port defaultDest)))
          ]
      (log/debug "send-buf result: " res))))


(defn udpCL []
  (let [myChan (chan (sliding-buffer 1024)) ;this should be a sliding window or dropping channel...
        myBuf (byte-array 10240)]      ;; when is this going to be released?
    
    (do
      (start-udp-server)
      (fwd/regItfChan myChan "/ccn-lite")   ; for testing
;1
      (thread                           ; processing outgoing queue
        (do
          (log/debug "udpCL: receiver thread")
          (fwd/addOutputChannel myChan :udp1)
          (while true
            (do
              (log/debug "udpCL: reading from " myChan)
              (let [msg (<!! myChan)]
                (udpCL-send msg myBuf))))))
;2        
    (thread                             ;processing UDP interface
      (while true
        (let [[buf addr port] (rcv2buf)
              msg (parseBuf buf addr port)]
          (if msg
            (do
              (log/debug "udpCL: forwarding to incomingMsgChannel")
              (>!! fwd/incomingMsgChannel [msg :udp1]))
            (log/error "udpCL parsing error, msg: " msg)
            )))))))
        



(defn fetch-udp [objectName]
  (let [fetchChan (chan)
        itfContext {:itfName :internal2}
        msg (ccnx/interestMsg objectName {:itfContext itfContext})
        myBuf (byte-array 10240)]
    (do
      (fwd/addOutputChannel fetchChan :internal2)
                                        ;      (fwd/processInterest msg :internal2)
      (udpCL-send msg myBuf)
      
      (let [[msg channel] (alts!! [fetchChan (timeout 1000)])]
        (do
          (fwd/rmOutputChannel :internal2)
          msg)))))
