(ns api
  [:require [clj-http.client :as http]])

(def AUTH "0423lToAuE8L3zsvLO5aAbCdoyKhxfabfY2VY88XvpsH1H")

(defn myproblems []
  (:body (http/get "http://icfpc2013.cloudapp.net/myproblems"
                   {:query-params {:auth AUTH}
                    :as :json})))

(defn train []
  (:body (http/get "http://icfpc2013.cloudapp.net/train"
                   {:query-params {:auth AUTH}
                    :as :json})))
