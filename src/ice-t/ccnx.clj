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

(ns ido.ccnx
  (:require [clojure.set :as set])
  (:require [ido.tlv :as tlv])
  (:require [clojure.tools.logging :as log])
  )

   ;;                     1                   2                   3
   ;; 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   ;; +---------------+---------------+---------------+---------------+
   ;; |    Version    |  PacketType   |         PacketLength          |
   ;; +---------------+---------------+---------------+---------------+
   ;; |           PacketType specific fields          | HeaderLength  |
   ;; +---------------+---------------+---------------+---------------+
   ;; / Optional Hop-by-hop header TLVs                               /
   ;; +---------------+---------------+---------------+---------------+
   ;; / PacketPayload TLVs                                            /
   ;; +---------------+---------------+---------------+---------------+



   ;;PacketPayload TLVs:
   ;; 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   ;; +---------------+---------------+---------------+---------------+
   ;; | CCNx Message TLV                                              /
   ;; +---------------+---------------+---------------+---------------+
   ;; / Optional CCNx ValidationAlgorithm TLV                         /
   ;; +---------------+---------------+---------------+---------------+
   ;; / Optional CCNx ValidationPayload TLV (ValidationAlg required)  /
   ;; +---------------+---------------+---------------+---------------+


   
   ;; CCNx Message TLV
   ;;                      1                   2                   3
   ;;  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   ;; +---------------+---------------+---------------+---------------+
   ;; |         MessageType           |         MessageLength         |
   ;; +---------------+---------------+---------------+---------------+
   ;; | Name TLV       (Type = T_NAME)                                |
   ;; +---------------+---------------+---------------+---------------+
   ;; / Optional Message TLVs   (Various Types)                       /
   ;; +---------------+---------------+---------------+---------------+
   ;; / Optional Payload TLV  (Type = T_PAYLOAD)                      /
   ;; +---------------+---------------+---------------+---------------+



   ;; Name TLV
   ;;                      1                   2                   3
   ;;  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   ;; +---------------+---------------+---------------+---------------+
   ;; |            T_NAME             |            Length             |
   ;; +---------------+---------------+---------------+---------------+
   ;; / Name segment TLVs                                             /
   ;; +---------------+---------------+---------------+---------------+

;; constants

(def msgType
  {0 :interest
   1 :object
   2 :interestReturn
   })

(def msgTypeCode (set/map-invert msgType))


(def tlvTypes
  {
   :T_INTEREST 0x0001
   :T_OBJECT 0x0002
   :T_VALIDATION_ALG 0x0003
   :T_VALIDATION_PAYLOAD 0x0004
   :T_NAME 0x0000
   :T_NAME_SEGMENT 0x0001
   :T_IPID 0x0002
   :T_PAYLOAD 0x0001
   :T_PAYLDTYPE 0x0005
   :T_EXPIRE 0x0006
   })


(def defaults
  {:hoplimit 64
   :version 1
   :maxObjectSize 4096})


(def returnCodeName
  [:noError :noRoute, :hopLimitExceeded, :noResources,
    :pathError, :prohibited, :congested, :mtuTooLarge,
   :unsupportedContentObjectHashRestriction,
   :malformedInterest])

(def returnCodeVal
  (reduce conj {} 
          (map-indexed (fn [idx itm] {itm idx}) returnCodeName)))


(def hashType
  {:T_SHA-256 0x0001
   :T_SHA-512 0x0002
   })


(def hashTypeCode (set/map-invert hashType))

;; do this with macros later

(defn version [buf]
  (tlv/bytes2word buf 0 1))

(defn packetType [buf]
  (tlv/bytes2word buf 1 1))

(defn packetLength [buf]
  (tlv/bytes2word buf 2 2))

(defn packetTypeSpecific [buf]
  (tlv/bytes2word buf 4 3))

(defn headerHopLimit [buf]
  (tlv/bytes2word buf 4 1))

(defn headerFlags [buf]
  (tlv/bytes2word buf 6 1))

(defn headerLength [buf]
  (tlv/bytes2word buf 7 1))

(defn interestReturnCode [buf]
  (tlv/bytes2word buf 5 1))


;;;;;;;;;;

;; TODO: see what can be generalized and factored out to tlv.clj

(defn getHeaderTlv [buf bytesRead]
  (let [headerLen (headerLength buf)]
    (tlv/tryGetTlv buf bytesRead headerLen)))


(defn nextHeaderTlv [[buf bytesRead pTlv]]
  (let [newBytesRead (+ bytesRead (tlv/size pTlv))]
    [buf newBytesRead (getHeaderTlv buf newBytesRead)]))
     

(defn nextHeader? [[_ _ pTlv]]
  (some? pTlv))

(defn headerTlvs [buf bytesRead]
  (map (fn [[_ _ tlv]] tlv) (take-while nextHeader? (iterate nextHeaderTlv [buf bytesRead (getHeaderTlv buf bytesRead)]))))


(defn byte-arrayToString [ba]
  (apply str (map char ba)))

(defn bufToString [buf start len]
  (byte-arrayToString (for [idx (range len)]
                         (tlv/getu buf (+ start idx)))))

(defn hexify
  ([buf start len]
  (let [val (tlv/bytes2word buf start len)]
    (format "%x" val)))

  ([nval]
  (hexify nval 0 (count nval))))


(defn formatType [ntype]
  (format "%04x" (get tlvTypes ntype)))

(defn hexFormat [ntype nval]
  (str "0x" (formatType ntype) "=0x" (hexify nval)))

(defn hexFormatApp [ntype nval]
  (str ntype "=0x" (hexify nval))) ; just generate the string type


;; (defrecord NameSeg [ntype nval]
;;   Object ; implements the Object protocol
;;   (toString [_] ntype nval
;;     (condp = ntype
;;       (get tlvTypes :T_NAME_SEGMENT) (byte-arrayToString nval)
;;       (get tlvTypes :T_IPIP) (hexFormat ntype nval)
;;       (str "A:" (hexFormat ntype nval))))) ; default

(defrecord NameSeg [ntype nval])


(defn nameSegToString
  "converts NameSeg to string"
  [nseg]
  (let [ntype (:ntype nseg)
        nval (:nval nseg)]
  (condp = ntype
      :T_NAME_SEGMENT (byte-arrayToString nval)
      :T_IPIP (hexFormat ntype nval)
      (str "App:" (hexFormatApp ntype nval))))) ; default
  

(defn nameSegToTlv [nameSeg]
  (let [typ (:ntype nameSeg)
        theType
        (case typ
          (:T_NAME_SEGMENT :T_IPID) (get tlvTypes typ)
          typ)]
  (tlv/->TLV theType (count (:nval nameSeg)) (tlv/->Value (:nval nameSeg) 0))))


(defn subByteArray 
  "create and return new byte-array from subset of buf"
  [buf start len]
  (java.util.Arrays/copyOfRange buf start (+ start len)))

(defn parseNameSeg
  "convert name segment tlv to NameSeg"
  [tlv]
  (let [len (:length tlv)
        typ (:type tlv)
        {buf :buf start :start} (:value tlv)]
    (condp = typ
      (:T_NAME_SEGMENT tlvTypes) ;; TODO: mapping function from int types to symbolic and vice versa
      (->NameSeg :T_NAME_SEGMENT (subByteArray buf start len))
    ;;
      (:T_IPID tlvTypes)
      (->NameSeg :T_IPID (subByteArray buf start len))
      ;; default
;      (if (and (< 0x1000 typ) (<= typ 0x1fff))
      (->NameSeg typ (subByteArray buf start len)) ; APP:x -- just copy directly
;        nil
      )
    ))


;; (defn parseNameSeg
;;   "convert name segment tlv to string"
;;   [tlv]
;;   (let [[buf start] (:value tlv)]
;;     (case (:type tlv)
;;       (:T_NAME_SEGMENT tlvTypes)
;;                                         ;      (str "Name=" (bufToString buf start (:length tlv)))
;;       (str "Name=" (subByteArray buf start (:length tlv)))
;;     ;;
;;     (:T_IPID tlvTypes)
;;     (str "T_IPID=" (bufToHexString buf start (:length tlv)))
;;     ;; default
;;     (str "%x" (format "%04x" (:type tlv)) "=%x" (hexify buf start (:length tlv)))
;;     )))



;; (defn nameSegToString
;;   "convert name segment tlv to string"
;;   [tlv]
;;   (if (= (:type tlv) (:T_NAME_SEGMENT tlvTypes))
;;     (let [[buf start] (:value tlv)]
;;       (bufToString buf start (:length tlv)))
;;                                         ; else
;;     (do
;;       (log/error "nameSegToString: incorrect type in " tlv)
;;       nil)))


(defn isType? [tlv type]
  (= (:type tlv) (get tlvTypes type)))

(defn TNAME? [tlv]
  (isType? tlv :T_NAME))

(defn parseNameTlv
  "return sequence of NameSegs"
  [tlv]
  (if (TNAME? tlv)
    (let [
          segList (:value tlv)]
;          {buf :buf start :start} segList]
      (map #(parseNameSeg %) segList))
                                        ; else
  (do
    (log/error "parseNameTlv: incorrect type in " tlv)
    nil)))


;; (defn parseNameTlv [tlv]
;;   (if (= (:type tlv) (:T_NAME tlvTypes))
;;     (let [[buf start] (:value tlv)
;;           maxLen (:length tlv)
;;           segList (getTlvList buf start maxLen)]
;;       (apply str (map #(str "/" (nameSegToString %)) segList)))
;;                                         ; else
;;   (do
;;     (log/error "parseNameTlv: incorrect type in " tlv)
;;     nil)))



(defn getMsg
  "return message TLV (part after header).
  The TLV value is a vector [buf pos]"
  [buf]
  (tlv/getAt buf (headerLength buf)))


(defn getMsgNameTlv
  "create and return name TLV from msg in this buf
    The TLV value is a vector [buf pos]"
  [buf]
  (let [msg (getMsg buf)]
    (tlv/getFromValue msg (:T_NAME tlvTypes))))


(defn getMsgName [buf]
  (parseNameTlv getMsgNameTlv buf))
        



(defn segments [name]
  (re-seq #"[^/]+" name))


(defn strToBuf
  "copies specified nameSeg to buf"
  [nameSeg buf start]
  (let [bytes (.getBytes nameSeg)]
    (count (for [i (range (count nameSeg))]
      (aset-byte buf (+ start i) (get bytes i)))))) ; TODO: optimize



(defn copyNameSeg
  "copies specified nameSeg to buf"
  [nameSeg buf start bytesLeft]
  (let [segLen (count nameSeg)
        newBytesLeft (- bytesLeft segLen)]
    (if (>= newBytesLeft 0)
      (do
        (log/debug "copyNameSeg " nameSeg)
        (java.lang.System/arraycopy (:buf nameSeg) 0 buf start segLen))
;        (strToBuf nameSeg buf start))
      (do
        (log/error "copyNameSeg: buffer overflow: " nameSeg bytesLeft)
      nil))))


(defn writeTlvHeader
  "write type and len to TLV buffer
  looks up type value in tlvTypes"
  ([type len buf start bytesLeft] ;; should check whether bytesLeft>=tlv-header-len
   (log/debug "writeTlvHeader" type)
   (if-let [typeVal (get tlvTypes type type)] ;defaults to type
     (tlv/writeHeader typeVal len buf start bytesLeft)
     (log/error "unknown type " type)))
  ([tlv buf start bytesLeft]
   (writeTlvHeader (:type tlv) (:length tlv) buf start bytesLeft)))



;; ;; this is not used anymore
;; (defn nameSegToTlv [nameSeg buf start bytesLeft]
;;   (let [segLen (.length nameSeg)
;;         headerLen (writeTlvHeader :T_NAME_SEGMENT segLen buf start bytesLeft)
;;         newBytesLeft (- bytesLeft headerLen)
;;         valLen (copyNameSeg nameSeg buf (+ start headerLen) newBytesLeft)]
;;     (+ headerLen valLen))) ; return total number of bytes written
    
  

;; ;; this is not used anymore
;; (defn nameToTlv [name buf start maxLen]
;;   (let [nameSegs (segments name)
;;         totalSize (+ (reduce + (map count nameSegs)) (* tlv/header-len (count nameSegs)))
;;         bytesWritten (writeTlvHeader :T_NAME totalSize buf start maxLen)
;;         bytesLeft (- maxLen bytesWritten)
;;         valueStart (+ start bytesWritten)
;;         ]

;;     (loop [segs nameSegs
;;            myStart valueStart
;;            myBytesLeft bytesLeft]
;;       (let [nextSeg (first segs)]
;;         (if nextSeg
;;           (let [written (nameSegToTlv nextSeg buf myStart myBytesLeft)]
;;             (if written
;;               (recur (rest segs) (+ myStart written) (- myBytesLeft written)))))))))



;; msg: header (8) + 4 + nameTLV + optional payload TLV + optional expiry time TLV


(defn payloadTlv
  "create PayloadType TLV from msg, without copying data"
  [msg]
  (log/debug "payloadTlv")
  (tlv/->TLV :T_PAYLDTYPE (:objectSize msg) (:object msg)))


;;

(defn unhexify "Convert hex string to byte sequence" [s]
      (letfn [(unhexify-2 [c1 c2]
                 (unchecked-byte
                  (+ (bit-shift-left (Character/digit c1 16) 4)
                     (Character/digit c2 16))))]
     (map #(apply unhexify-2 %) (partition 2 s))))

(defn padHex [s]
  (if (even? (.length s))
    s
    (str "0" s)))

(defn nameSegVal
  "if hex string then convert to binary, otherwise return byte array"
  [segVal]
  (if (.startsWith segVal "0x")
    (byte-array (unhexify (padHex (subs segVal 2))))
    (.getBytes segVal)))
    


(defn nameSegType 
  "tries to parse segType string as a number, stripping of 'A:' if present"
  [segType]
  (read-string                          ; tries to evaluate string as number
   (if (.startsWith segType "App:")
     (subs segType 4)                   ; remove "App:" from string
     segType)))


(defn nameSegFromString
  "parses name segement string and returns NameSeg"
  [seg]
  (let [elements (re-seq #"[^=]+" seg)]
    (case (count elements)
      1 (->NameSeg :T_NAME_SEGMENT (.getBytes seg)) ; regular name segment
      2 (->NameSeg (nameSegType (first elements)) (nameSegVal (nth elements 1))) ; parse type as string for now -- not quit correct
      (log/error "nameSegFromString: parse error -- " seg))))


(defn strToName
  "return sequence of NameSeg created from string"
  [s]
  (map nameSegFromString (segments s)))

(defn nameToStr
  "convert seq of NameSeg to str"
  [nameSegs]
  ;;  (str "/" (clojure.string/join "/" (map str nameSegs))))
  (str "/" (clojure.string/join "/" (map nameSegToString nameSegs))))

;; (defn nameSegTlv
;;   "create Name segment TLV from str"
;;   [nameseg]
;;   (log/debug "nameSgTlv: " nameseg)
;;   (tlv/->TLV :T_NAME_SEGMENT (.length nameseg) nameseg))

;; (defn nameTlv
;;   "create Name TLV from str"
;;   [name]
;;   (log/debug "nameTlv: " name)
;;   (let [segTlvs (map nameSegTlv (segments name))
;;         totalLen (reduce + (map tlv/size segTlvs))]
;;     (tlv/->TLV :T_NAME totalLen segTlvs)))


(defn nameTlv
  "create Name TLV from str"
  [name]
  (log/debug "nameTlv: " name)
  (let [nameSegs (strToName name)
        segTlvs (map nameSegToTlv nameSegs)
        totalLen (reduce + (map tlv/size segTlvs))]
    (tlv/->TLV (:T_NAME tlvTypes) totalLen segTlvs))) ;this is seq -- not a TLV Value




(defn objectMsgTlv
  "create Content Object Message TLV from msg"
  [msg]
  (log/debug "objectMsgTlv")
  (let [name (nameTlv (msg :name))
        payload (payloadTlv msg)]
    (tlv/->TLV :T_OBJECT (+ (tlv/size name) (tlv/size payload)) [name payload])))


(defn interestMsgTlv
  "create Interest Message TLV from msg"
  [msg]
  (log/debug "interestMsgTlv")
  (let [name (nameTlv (msg :name))]
    (tlv/->TLV :T_INTEREST (tlv/size name) [name])))


(defn nameSegToBuf
  "write name segment TLV to specified buf"
  [tlv buf start maxLen]
  (do
    (log/debug "nameSegToBuf " tlv)
    (writeTlvHeader tlv buf start maxLen)
    (copyNameSeg (:value tlv) buf (+ start tlv/header-len) maxLen)))


(defn startPositions
  "interate through seq of TLV in (tlv :value), generate a seq of start positions, offset by start"
  [start tlv]
  (map #(+ start %) (reductions + (cons tlv/header-len (drop-last (map tlv/size (:value tlv)))))))

(defn pairs
  "takes a collection of [1 2] [3 4] ...
  returns a lazy sequence like ([1 3 ...] [2 4 ...])"
  [& args]
  (partition (count args) (apply interleave args)))


(defn nameToBuf
  "write name TLV to specified buf"
  [tlv buf start maxLen]
  (do
    (log/debug "nameToBuf")
    (writeTlvHeader tlv buf start maxLen)
    (doseq [[nameSeg segStart] (pairs (:value tlv) (startPositions start tlv))] ; force eval
      (do
        (log/debug nameSeg segStart)
        (nameSegToBuf nameSeg buf segStart maxLen)))))

(defn payloadToBuf
  "copy the bytes of object from (tlv :value) to buf"
  [tlv buf start maxLen]
  (let [[src srcPos] (:value tlv)
        destPos (+ tlv/header-len start)]
    (do
      (log/debug "payloadToBuf" tlv buf start maxLen)
      (writeTlvHeader tlv buf start maxLen)
      (log/debug "payloadToBuf: arraycopy " src srcPos buf destPos (:length tlv))
      (System/arraycopy src srcPos buf destPos (:length tlv))
      (tlv/size tlv))))

(defn objectMessageToBuf
  "write Content Object Message TLV to specified buf"
  [tlval buf start maxLen]
  (do
    (log/debug "objectMessageToBuf: " tlval buf start maxLen)
    (let [[name payload] (:value tlval)]
      (do (writeTlvHeader tlval buf start maxLen)
          (nameToBuf name buf (+ start tlv/header-len) (- maxLen tlv/header-len))
          (payloadToBuf payload buf (+ start (tlv/size name) tlv/header-len) (- maxLen (tlv/size name) tlv/header-len)
                               )))
    (log/debug "done")
    (tlv/size tlval)))

(defn interestMessageToBuf
  "write Interest Message TLV to specified buf"
  [tlval buf start maxLen]
  (do
    (log/debug "interestMessageToBuf: " tlval buf start maxLen)
    (let [[name] (:value tlval)]
      (do (writeTlvHeader tlval buf start maxLen)
          (nameToBuf name buf (+ start tlv/header-len) (- maxLen tlv/header-len))
          ))
    (log/debug "done")
    (tlv/size tlval)))



(defn object2buf
  "create content object message.
  return number of bytes written"
  [msg buf start maxLen]                ;should check buffer size before writing...
    (do
      (log/debug "object2buf: converting " msg)
      (let [objectMsg (objectMsgTlv msg)
            bytecounts [(tlv/valToBuf buf start 1 1)              ; Version
                        (tlv/valToBuf buf (+ start 1) (msgTypeCode :object) 1) ; PacketType
                        (tlv/valToBuf buf (+ start 2) (+ 8 (tlv/size objectMsg)) 2) ; PacketLength
                        (tlv/valToBuf buf (+ start 7) 8 1); HeaderLenghth later
                        (objectMessageToBuf objectMsg buf (+ start 8) (- maxLen 8))]]
        (reduce + bytecounts))))



(defn headerToBuf [buf start fields]
  (doseq [[offset value size] fields]
    (tlv/valToBuf buf (+ start offset) value size)))


(defn interest2buf
  "create content object message.
  return number of bytes written"
  [msg buf start maxLen]                ;should check buffer size before writing...
    (do
      (log/debug "interest2buf: converting " msg)
      (let [myTlv (interestMsgTlv msg)]
        (do
          (headerToBuf buf start
                       [[0 (:version defaults) 1]
                        [1 (:interest msgTypeCode) 1]
                        [2 (+ 8 (tlv/size myTlv)) 2]
                        [4 (:hoplimit msg) 1]
                        [7 8 1]])     ; HeaderLength
          (+ 8(interestMessageToBuf myTlv buf (+ start 8) (- maxLen 8)))
          ))))



(defn toBuf
  "create ccnx message.
  return number of bytes written"
  [msg buf start maxLen]                ;should check buffer size before writing...
  (case (:type msg)
    :object (object2buf msg buf start maxLen)
    :interest (interest2buf msg buf start maxLen)))
;; make this nicer later...



;;;;;;;;;;

(def sampleInterestMsg {
                        :version 1
                        :type :interest
                        :len 40
                        :headerLen 8
                        :headerTLVs ()
                        :hopLimit 64
                        :optTLVs ()
                        :name "/foo/bar"})


(def sampleObjectMsg {
                      :version 1
                      :type :object
                      :len 40
                      :headerLen 8
                      :headerTLVs ()
                      :name "/foo/bar"
                      :object (byte-array (map byte "foo object"))
                      :objectSize 10
                      })

  


(defn parseInterest
  "parse an CCNx Interest message"
  [buf msg]
  (do
    (log/debug "parsing interest")
    (let [hopLimit (headerHopLimit buf)
          theMsg (getMsg buf)
          [msgBuf start] (:value theMsg)
          nameTLV (getMsgNameTlv buf)
          nameTLVlen (+ tlv/header-len (:length nameTLV))
          optTLVs (tlv/getTlvList msgBuf (+ start nameTLVlen)
                              (- (:length theMsg) nameTLVlen))
          ]
      (assoc msg
             :hopLimit hopLimit
             :name (parseNameTlv nameTLV)
             :optTLVs optTLVs
             ))))


        

(defn parseObject
  "parse an CCNx Object message (to be defined)"
  [buf msg]
  (do
    (log/debug "parseObject")
    (let [theMsg (getMsg buf)
          [msgBuf start] (:value theMsg)
          nameTLV (getMsgNameTlv buf)
          payloadTLV (tlv/getAt msgBuf (+ start (tlv/size nameTLV)))
          ]
      (assoc msg
             :name (parseNameTlv nameTLV)
             :object (:value payloadTLV)
             :objectSize (:length payloadTLV)))))


(defn strObject [msg]
  (if msg  
    (let [
          {object :object len :objectSize} msg
          [buf start] object]
      (bufToString buf start len))
    nil))
  


(defn parseInterestReturn
  "parse an CCNx InterestReturn message (to be defined)"
  [buf msg]
  (log/debug "parseInterestReturn")
  (let [theMsg (parseInterest buf msg)]
    (assoc theMsg {:returnCode (returnCodeName (interestReturnCode buf))})
    ))



(def parseFun
  {:interest parseInterest
   :object parseObject
   :interestReturn parseInterestReturn
   nil (fn [buf msg] nil)
   })


(defn parseMsg
  "parse common format parts and call message type specific functions.
  return msg"
  [buf]
  (let [msg {:version (version buf)
             :type (msgType (packetType buf))
             :len (packetLength buf)
             :headerLen (headerLength buf)
             :headerTLVs (headerTlvs buf 8)}]
    (do
      (log/debug "parsing " msg)
      ((parseFun (msg :type)) buf msg))))


;;;;;;;;;;


(defn strToObj [string]
 ; (byte-array (map byte string)))
  (.getBytes string))

(defn objectMsg
  "create content object message"
  ([name]                               ;from name
   {
    :version 1
    :type :object
    :name name
    })
  ([name string]                        ;from name and string value
   ;; FIXME: need to deal with larger objects...
   (let [objectSize (min (:maxObjectSize defaults)(.length string))]
     (merge (objectMsg name) {:object [(strToObj string) 0] :objectSize objectSize})))
  
  ([name string paras]                        ;from name and string value and additional paras hashmap
   (merge (objectMsg name string) paras)))

  

(defn interestMsg
  "create interest message"
  ([name]                               ;from name
   {
    :version 1
    :type :interest
    :name name
    :hoplimit (:hoplimit defaults)
    })
  ([name paras]                        ;from name and additional paras hashmap
   (merge (interestMsg name) paras)))


