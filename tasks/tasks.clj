(ns tasks
  (:require
    [clojure.java.shell :refer [sh]]
    [clojure.string :as string]))


(defn jirazzz
  [& args]
  (-> args
      vec
      (conj :env {"JIRAZZZ_CONFIG_FILE" "example-config.edn"})
      (->> (apply sh "bb" "./jirazzz"))))


(defn indent
  [n s]
  (string/replace s #"(?m:^)" (string/join "" (repeat n " "))))


(defn placeholder
  [s marker replacement]
  (-> (string/replace
        s
        (re-pattern
          (str "(?s:(<!-- \\{ " marker " -->).+(<!-- " marker " \\} -->))"))
        (str "$1\n\n" (string/re-quote-replacement replacement) "\n\n$2"))
      (string/split-lines)
      (->> (map #(string/replace % #"^\s+$" ""))
           (string/join "\n"))
      (str "\n")))


(defn usage
  []
  (-> (jirazzz "--help")
      :err))


(def readme "README.md")


(defn docs
  []
  (-> readme
      slurp
      (placeholder "jirazzz help" (indent 4 (usage)))
      (placeholder "jirazzz example-config" (str "```clojure\n" (slurp "example-config.edn") "```"))
      (->> (spit readme))))
