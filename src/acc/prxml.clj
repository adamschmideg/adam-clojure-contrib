(ns acc.prxml
  (:use 
    [clojure test]
    [clojure.contrib prxml]
    [clojure.contrib.zip-filter.xml]
    [acc xml])
  (:require 
    [clojure.zip :as zip]
    [clojure.xml :as xml]
    [clojure.walk :as walk])
  (:import
    [org.w3c.dom Node]))

(with-test
  (defn from-struct
    ([struct & default-attrs]
      (if (map? struct)
        (let [use-default-attrs (= 1 (count default-attrs))
              tag (:tag struct)
              attrs (:attrs struct)
              defaults (first default-attrs)
              result (if use-default-attrs
                        [tag (or attrs defaults)]
                        (if attrs
                          [tag attrs]
                          [tag]))
              content (map
                         (fn [x]
                            (if use-default-attrs
                               (from-struct x defaults)
                               (from-struct x))) 
                         (:content struct))]
          (if (seq content)
            (into result content)
            result))
        struct)))
  (is (= [:a]
        (from-struct (parse-text "<a/>"))))
  (is (= [:a nil]
        (from-struct (parse-text "<a/>") nil)))
  (is (= [:a {}]
        (from-struct (parse-text "<a/>") {})))
  (is (= [:a {:x "1"}]
        (from-struct (parse-text "<a x='1'/>"))))
  (is (= [:a {:x "1"}]
        (from-struct (parse-text "<a x='1'/>") nil)))
  (is (= [:a [:b]]
        (from-struct (parse-text "<a><b/></a>"))))
  (is (= [:a {} [:b {}]]
        (from-struct (parse-text "<a><b/></a>") {})))
  (is (= [:painting nil
           [:img {:src "madonna.jpg" :alt "Foligno Madonna, by Raphael"}]
           [:caption nil "This is Raphael's 'Foligno' Madonna, painted in"
             [:date nil "1511"]
             "-"
             [:date nil "1512"]
             "."]]
         (from-struct sample nil))))

(defn from-dom
  [node]
  (let [attrs (apply merge
                (map 
                  (fn [anode] 
                    {(keyword (.getNodeName anode)), (.getNodeValue anode)})
                  (list-nodes (.getAttributes node))))]
  (if (= Node/TEXT_NODE (.getNodeType node))
    (.getNodeValue node)
    (into 
      [(keyword (.getNodeName node)) attrs] 
      (map from-dom 
        (list-nodes (.getChildNodes node)))))))
