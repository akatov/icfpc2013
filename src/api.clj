(ns api
  [:require [clj-http.client :as http]])

(def AUTH "0423lToAuE8L3zsvLO5aAbCdoyKhxfabfY2VY88XvpsH1H")

(def base "http://icfpc2013.cloudapp.net/")

(defn request
  ([path] (request path {}))
  ([path data]
     (:body (http/post (str base path)
                       {:query-params {:auth AUTH}
                        :form-params data
                        :content-type :json
                        :as :json}))))

(defn myproblems []
  (let [res (request "myproblems")]
    res))                               ; TODO: convert result to our representation

(defn eval-program [prog args]
  (let [res (request "eval"
                     {:program prog
                      :arguments args})
        outputs (:outputs res)]
    outputs))                           ; TODO: convert outputs to longs

(defn eval-id [id args]
  (let [res (request  "eval"
                      {:id id
                       :arguments args})
        outputs (:outputs res)]
    outputs))                           ; TODO: convert outputs to longs

(defn guess [id prog]
  (let [res (request "guess"
                     {:id id
                      :program prog})]
    res))                               ; TODO: figure out how to use the response

(defn train []
  (let [res (request  "train")]
    res))                               ; TODO: convert output to our representation

(defn status []
  (request "status"))
