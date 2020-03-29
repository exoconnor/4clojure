(ns user
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))
(tufte/add-basic-println-handler! {})


;; Cider runs (clojure.core/apply clojure.core/require clojure.main/repl-requires)
;; Maintaining manually so for experimental purposes, and because this ns thing ain't
;; a do block
(defn rjack
  "Load namespace and refer in repl env"
  [n]
  (eval `(ns ~n
           (:refer-clojure)
           (:require [clojure.repl :refer ~'(source apropos dir pst doc find-doc)])
           (:require [clojure.java.javadoc :refer ~'(javadoc)])
           (:require [clojure.pprint :refer ~'(pp pprint)]))))

