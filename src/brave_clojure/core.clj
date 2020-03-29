(ns brave-clojure.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

;; Chapter 11: Core-Async
(def echo-chan (chan))
(go (println (<! echo-chan)))
(>!! echo-chan "ketchup")
