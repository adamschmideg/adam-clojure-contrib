
(ns acc.http.server
  (:use clojure.test)
  (:require [clojure.contrib.str-utils2 :as s])
  (:import 
    [java.net InetSocketAddress URLDecoder]
    [com.sun.net.httpserver HttpServer HttpHandler]))

(defmacro get-all
  ([x form] `{(keyword '~form) (. ~x ~form)})
  ([x form & more] `(assoc (get-all ~x ~@more) (keyword '~form) (. ~x ~form))))

; Based on http://www.java2s.com/Code/Java/Network-Protocol/MinimalHTTPServerbyusingcomsunnethttpserverHttpServer.htm

(defn url-decode
  [url]
  (URLDecoder/decode url))

(with-test
  (defn match-url [url-mappings url]
    (let [matched (first
                    (filter #(re-matches % url) 
                      (keys url-mappings)))]
      (if matched
        [(url-mappings matched)
         (let [groups (re-matches matched url)]
           (if (vector? groups)
             (rest groups)
             []))]
        [nil []])))
  (let [mappings {#"/item/(.*)" "Item",
                  #"/list/?" "List",
                  #"/" "Root"}]
    (is (= ["Item" ["42"]] 
           (match-url mappings "/item/42")))
    (is (= ["List" []] 
           (match-url mappings "/list")))
    (is (= ["Root" []] 
           (match-url mappings "/")))
    (is (= [nil []] 
           (match-url mappings "/foo")))))

(defn debug [& rest] 
  (println (str rest))
  (. (.. System out) flush)
  rest)

(defn write-response [xchg code response]
    (. xchg sendResponseHeaders code (. response length))
    (doto (. xchg getResponseBody)
      (.write (. response getBytes))
      (.close)))
  
(with-test 
  (defn handle-request
    [url-mapping request]
    (let [[matching args] (match-url url-mapping (:path request))
          fun (cond
                (fn? matching) matching
                (map? matching) ((keyword (s/lower-case (:method request))) matching))]
      (if fun
        (try
          (apply fun request args)
          (catch IllegalArgumentException e (str "Args passed:" args "to" fun)))
        (str "No match: " (:path request)))))
  (let [mapping {#"/foo/(.*)" (fn [request id] (str "Foo " id))
                 #"/bar/(.*)" {:get 
                                (fn [request id] (str "Get bar " id))
                              :post 
                                (fn [request id] (str "Post bar " id))}}]
    (is (= "Foo 42" 
           (handle-request mapping {:path "/foo/42" :method "GET"})))
    (is (= "Get bar 42" 
           (handle-request mapping {:path "/bar/42" :method "GET"})))))

(defn start-server 
  [url-mappings port]
  (doto (. HttpServer create (new InetSocketAddress port) 0)
    (.createContext "/"
      (proxy [HttpHandler] []
        (handle [xchg]
          (try
            (let [request {:port (.. xchg getLocalAddress getPort)
                           :host (.. xchg getLocalAddress getHostName)
                           :method (. xchg getRequestMethod)
                           :headers (. xchg getRequestHeaders)
                           :query (.. xchg getRequestURI getRawQuery)
                           :path (.. xchg getRequestURI getPath)}
                 dummy (debug "request:" request)
                 response (handle-request url-mappings request)]
               (write-response xchg 200 response))
            (catch Exception e
               (.printStackTrace e)
               (write-response xchg 401 (str e)))))))
    (.start))
  (print "Started on " port))

(defn main []
  (start-server 
    {"/item/(.*)" 
      {:get
        (fn [request arg1] 
          (str "<html><body><h1>Hello, world</h1> getting" request  "and arg:" arg1 "</body></html>"))
       :post
        (fn [request arg1]
          (str "<html><body><h1>Hello, world</h1> posting" request  "and arg:" arg1 "</body></html>"))}
     "/"
     (fn [request] (str "<html><body><h1>The root</h1></body></html>"))}
    8080))
