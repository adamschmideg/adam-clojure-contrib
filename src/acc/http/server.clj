
(ns acc.http.server
  (:use clojure.test)
  (:require [clojure.contrib.str-utils2 :as s])
  (:import 
    [java.net InetSocketAddress]
    [com.sun.net.httpserver HttpServer HttpHandler]))

(defmacro get-all
  ([x form] `{(keyword '~form) (. ~x ~form)})
  ([x form & more] `(assoc (get-all ~x ~@more) (keyword '~form) (. ~x ~form))))

; Based on http://www.java2s.com/Code/Java/Network-Protocol/MinimalHTTPServerbyusingcomsunnethttpserverHttpServer.htm

(with-test
  (defn match-url [url-mappings url]
    (let [
	  matched 
	  (first 
	   (filter 
	    #(re-seq (re-pattern %) url) 
	    (keys url-mappings)))]
      [(url-mappings matched) (rest (re-matches (re-pattern matched) url))]))
  (let [mappings 
	{"/item/(.*)", "Item"
         "/", "Root"}]
    (is (= ["Item" ["42"]] (match-url mappings "/item/42")))
    (is (= ["Root" []] (match-url mappings "/")))
    (is (= [nil []] (match-url mappings "/foo")))))

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
	  (apply fun (cons request args))
	  (catch IllegalArgumentException e (str "Args passed:" args)))
	(str "No match: " (:path request)))))
  (let [mapping {"/foo/(.*)" (fn [request id] (str "Foo " id))
		 "/bar/(.*)" {:get (fn [request id] (str "Get bar " id))
			      :post (fn [request id] (str "Post bar " id))}}]
    (is (= "Foo 42" (handle-request mapping {:path "/foo/42" :method "GET"})))
    (is (= "Get bar 42" (handle-request mapping {:path "/bar/42" :method "GET"})))))

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
			   :query (.. xchg getRequestURI getQuery)
			   :path (.. xchg getRequestURI getPath)}
		  response (handle-request url-mappings request)]
	     (write-response xchg 200 response))
	   (catch Exception e
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


(run-tests)
