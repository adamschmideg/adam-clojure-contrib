(ns acc.zipfile
  (:use clojure.test)
  (:require 
    [clojure.contrib.duck-streams :as io])
  (:import
    [java.io FileOutputStream IOException StringWriter]
    [java.util.zip ZipEntry ZipFile ZipOutputStream]))

(defn list-entries 
  "Return a set of entry names"
  [file]
  (map #(.getName %)
       (enumeration-seq (.entries (new ZipFile file)))))

(defn list-files
  "Return only files, ignore directories"
  [file]
  (filter #(not= \/ (last %)) (list-entries file)))

(defn read-zip 
  "Return a map of names to content"
  ([file] (read-zip file (list-files file)))
  ([file entries]
    (let [zipfile (new ZipFile file)]
      (reduce 
        (fn [acc name]
            (with-open [in (.getInputStream zipfile (.getEntry zipfile name))
                        out (new StringWriter)]
              (io/copy in out)
              (assoc acc name (str out))))
        {}
        entries))))

(defn write-zip 
  "Write a map of name to content"
  [file entries]
  (if-not (empty? entries)
    (with-open [out (new ZipOutputStream (new FileOutputStream file))]
      (doseq [it entries]
        (.putNextEntry out (new ZipEntry (key it)))
        (io/copy (val it) out)
        (.closeEntry out)))))

(deftest all
  (let [temp-file "testing.zip"]
    (write-zip temp-file {"readme.txt" "READ ME"
                      "directory/hello.txt" (.getBytes "Hello, world")})
    (is (= ["readme.txt" "directory/hello.txt"] 
           (list-files temp-file)))
    (is (= {"readme.txt" "READ ME" "directory/hello.txt" "Hello, world"}
           (read-zip temp-file)))))

