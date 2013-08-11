(ns api
  [:require
   [clj-http.client :as http]
   [functions :as f]])

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

(defn eval-program
  "example: (eval-program '(lambda (x) (plus x 1)) [1,2,3]) ;;=> (2 3 4)"
  [prog args]
  (let [prog-str (f/to-string prog)
        args-hex (map f/to-hex args)
        res (request "eval"
                     {:program prog-str
                      :arguments args-hex})
        outputs (:outputs res)]
    (map f/to-num outputs)))

(defn eval-id
  "example: (eval-id \"L4ZaBkerPGDH38Xr4S29kXzS\" [1,2,3]) ;;=> (256 512 768)"
  [id args]
  (let [args-hex (map f/to-hex args)
        res (request "eval"
                     {:id id
                      :arguments args})
        outputs (:outputs res)]
    (map f/to-num outputs)))

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
