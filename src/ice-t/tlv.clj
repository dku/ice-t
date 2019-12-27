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

(ns ido.tlv
 ; (:use bytebuffer.buff)
;  (:import (java.nio ByteBuffer))
  (:require [clojure.tools.logging :as log])
  )


;; ideas:
;;buffer -> TLVInput 
;;TLVInput as a parsed overlay over the buffer, no copying
;;values can be changed, but structure cannot

;;TLVInput -> Msg
;;Msg is the parsed Clojure structure
;;can point to data object in original buffer


;;Msg -> TLVOutput
;; TLVOutput can be hierarchical TLV structure
;; data objects in value fields
;; value can be sequence --> list of sub TLVs
;; needs to be serialized into buffer
;; idea to build nested structure first, then count the TLVs length values from bottom level to top level


(def len-len 2)
(def type-len 2)
(def header-len (+ type-len len-len))


(defn getu [buf pos]
  (bit-and 0xFF (long (get buf pos))))

(defmulti bytes2word (fn [_ _ len] len)) ;; return number (word)


(defmethod bytes2word 1 [buf start _]
  (getu buf start))


(defn getByteShifted [buf start len]
  (bit-shift-left (getu buf start) (* 8 len)))

(defmethod bytes2word 2 [buf start _]
  (long (+
         (getByteShifted buf start 1)
         (getu buf (+ start 1)))))



(defn bytes2wordGen [buf start len]
  (reduce + (map #(getByteShifted buf (+ start %) (- len % 1)) (range len))))


(defmethod bytes2word :default [buf start len]
  (bytes2wordGen buf start len))



(defn tType
  "read TLV type from byte array"
  ([array] (tType array 0))
  ([array start] (bytes2word array start type-len)))


(defn length
  "read TLV length from byte array"
  ([array] (length array 0))
  ([array start] (bytes2word array (+ start type-len) len-len)))


(defn tVal [array start]
  "get TLV value position in byte array"
  ([array] (tVal array 0))
  ([array start] (.position array (+ start header-len))))


;; duh -- java bytes are signed, so 127 max...
;; see https://groups.google.com/forum/#!topic/clojure/nScP6pOrgjo
;; kudos to Andy Fingerhut
(defn ubyte [val]
   (if (>= val 128)
     (byte (- val 256))
     (byte val)))

(defn lsb [val]
  (ubyte (bit-and 0xFF val)))


(defn getByteAt [val idx]
  (bit-shift-right val (* 8 idx)))


(defn valToBuf
  "write specified value with specified length to buffer.
  returns number of bytes written"
  [buf start value len]
  (do
    (log/debug "valToBuf writing to " buf start value len)
    (doseq [idx (range len)]       ; force evaluation of lazy sequence
      (let [data (lsb (getByteAt value (- len idx 1)))]
      (do 
        (log/debug " at " (+ start idx) data)
        (aset-byte buf (+ start idx) data ))))
    len))
  



(defrecord TLV [type length value])

(defrecord Value [buf start])
; value in TLV representing a buffer and a starting point within that buffer


(defn toBuf
  "write TLV to buf starting from start.
  Return number of bytes written"
  [thisTLV buf start & len]
  (do
    (valToBuf buf start (thisTLV :type) type-len)
    (valToBuf buf (+ start type-len) (thisTLV :length) len-len)
    (+ header-len
       (valToBuf buf (+ start header-len) (thisTLV :value) (thisTLV :length)))))


(defn getAt
  "create and return TLV from bytes at specified location in buf.
  The TLV value is a vector [buf pos]"
  [buf start & optType]
  (let [len (length buf start)
        type (tType buf start)
        [targetType] optType]
    (if (or (not targetType) (= targetType type))
      (->TLV type len [buf (+ start header-len)])
      (do
        (log/error "getAt: got type " type ", expected " targetType)
        nil))))

(defn size [tlv]
  (+ header-len (:length tlv)))


(defn getFromValue
  "create and return TLV from thisTlv's value.
    The TLV value is a vector [buf pos]"
  [thisTlv & optType]
  (let [[buf start] (:value thisTlv)
        [targetType] optType]
    (getAt buf start targetType)))
  

(defn moreTlvs? [bufsize bytesRead]
  (> (- bufsize bytesRead) 0))


(defn tryGetTlv [buf pos endpos]
  (if (moreTlvs? endpos pos)
      (getAt buf pos)
      nil))


(defn nextTlv? [[_ _ _ pTlv]]
  (some? pTlv))

(defn nextTlv [[buf start endPos pTlv]]
  (let [newPos (+ start (size pTlv))]
    [buf newPos endPos (tryGetTlv buf newPos endPos)]))


(defn getTlvList
  "read a sequence of TLVs in this buffer until maxLen is reached.
  returns a lazy sequence of TLVs"
  [buf start maxLen]
  (let [endPos (+ start maxLen)]
    (map (fn [[_ _ _ tlv]] tlv) (take-while nextTlv? (iterate nextTlv [buf start endPos (tryGetTlv buf start endPos)])))))



(defn writeHeader
  "write type and len to TLV buffer
  type is the integer representation of the type that should be in the buffer"
  ([type len buf start bytesLeft] ;; should check whether bytesLeft>=tlv-header-len
   (let [typeRes (valToBuf buf start type type-len)
         valRes (valToBuf buf (+ type-len start)  len len-len)]
     (+ typeRes valRes)))) ; return number of bytes written

