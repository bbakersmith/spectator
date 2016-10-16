(set-env!
 :source-paths #{"src" "test"}
 :dependencies '[[org.clojure/clojure "1.8.0"]
                 [adzerk/boot-test "1.1.1" :scope "test"]
                 [mount "0.1.10"]
                 [org.openpnp/opencv "2.4.11-2"]
                 [seesaw "1.4.5"]])


(require '[adzerk.boot-test :as boot-test])
