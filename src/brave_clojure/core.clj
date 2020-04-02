(ns brave-clojure.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

;; Chapter 11: Core-Async
(def echo-chan (chan))
(go (println (<! echo-chan)))
(>!! echo-chan "ketchup")

;; Multimethods, Records, Protocols
;; multimethods dispatch to functions based on essentially a case statement
;; But you can add stuff outside the initial define block
(defmulti redux-like (fn [_ payload] (:type payload)))
(defmethod redux-like :delete
  [state {:keys [id]}]
  (dissoc state id))
(defmethod redux-like :put
  [state {:keys [id value]}]
  (assoc state id value))
;; :default is the default default value
(defmethod redux-like :default
  [state _] state)
;; Extend multimethods to integrate with other functions and idioms

;; (extend-type type Protocol f1 f2 .. fn)
