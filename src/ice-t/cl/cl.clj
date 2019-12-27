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

(ns ido.cl.cl
  (:require [clojure.string :refer [replace-first]])
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer dropping-buffer]])
  (:require [clojure.tools.logging :as log])
  (:require [ido.forwarder :as fwd])
  )



(defn newInstanceName [clName instanceCount]
  (symbol (str clName (swap! instanceCount inc))))



(defn prefix
  "returns either the specified prefix or a prefix based on the interface name"
  [paras name]
  (:prefix paras (replace-first name ":" "/")))

(defn channel [paras]          
  (let [bufsize (:bufsize paras 1)
        qdisc (:qdisc paras :droppOld)
        buffer (case qdisc
                 :droppOld (sliding-buffer bufsize)
                 :dropNew (dropping-buffer bufsize)
                 )]
    (chan buffer)))


(defn stop [cl]
  (do
    (fwd/unregItfChan (:prefix cl))
    (reset! (:running cl) false)
    (close! (:channel cl))
    cl))


