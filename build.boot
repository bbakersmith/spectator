(set-env!
 :source-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.8.0"]
                 [org.openpnp/opencv "2.4.11-2"]
                 [seesaw "1.4.5"]])


(deftask build []
  (comp (aot :namespace #{'spectator.core})
        (pom :project 'bbakersmith/spectator
             :version "1.1.0")
        (jar :main 'spectator.core)
        (install)))
