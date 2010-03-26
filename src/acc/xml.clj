(ns acc.xml
  (:require 
    [clojure.xml :as xml])
  (:import
    [java.io ByteArrayInputStream]
    [javax.xml.parsers DocumentBuilderFactory]))


(defn parse-text
  [text]
  (xml/parse 
    (new ByteArrayInputStream 
      (.getBytes text))))

(def sample-xml "<?xml version='1.0' encoding='UTF-8'?>
  <painting>
    <img src='madonna.jpg' alt='Foligno Madonna, by Raphael'/>
    <caption>This is Raphael's 'Foligno' Madonna, painted in<date>1511</date>-<date>1512</date>.</caption>
  </painting>")

(def sample (parse-text sample-xml))

(defn parse-to-dom
  [fname]
  (.. DocumentBuilderFactory newInstance newDocumentBuilder (parse fname)))

(defn list-nodes
  [nodes]
  (when nodes
    (for [i (range (.getLength nodes))] (.item nodes i))))

