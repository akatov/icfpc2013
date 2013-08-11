(ns api
  [:require
   [clj-http.client :as http]
   [functions :as f]])

(def AUTH "0423lToAuE8L3zsvLO5aAbCdoyKhxfabfY2VY88XvpsH1H")

(def base "http://icfpc2013.cloudapp.net/")

(defn request
  "performs an API request to the specified path, optionally posting the data"
  ([path] (request path {}))
  ([path data]
     (:body (http/post (str base path)
                       {:query-params {:auth AUTH}
                        :form-params data
                        :content-type :json
                        :as :json}))))

(defn json-to-prog
  "converts the array of operators into a proper set of operators, the program string (for train) to a program."
  [json]
  (let [id (:id json)
        size (:size json)
        ops0 (set (map f/read-string (:operators json)))
        bonus (contains? ops0 'bonus)
        tfold (contains? ops0 'tfold)
        ops ops0
        prog (f/read-string (or (:challenge json) "nil")) ; myproblems has no challenge field
        solved (:solved json)
        time-left (:timeLeft json)]
    {:id id
     :size size
     :operators ops
     :program prog
     :tfold tfold
     :bonus bonus
     :solved solved
     :time-left time-left}))

(defn myproblems []
  "returns a seq of problem instances returned by `json-to-prog`"
  (->> (request "myproblems") (map json-to-prog)))

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

(defn guess
  "examples:
  (guess \"L4ZaBkerPGDH38Xr4S29kXzS\" '(lambda (x) x)) ;;=>
    {:win false, :input 36028797018963968, :output 9223372036854775808, ... }
  (let [t (train)] (guess (:id t) (:program t))) ;;=>
    {:win true, :lightning nil, ... }"
  [id prog]
  (let [program (f/to-string prog)
        res (request "guess"
                     {:id id
                      :program program})
        status (:status res)
        vals (:values res)
        input (if vals (f/to-num (vals 0)) nil)
        output (if vals (f/to-num (vals 1)) nil)]
    (conj res
          {:win (= status "win")
           :status status
           :input input
           :output output})))

(defn train
  "example: (train) ;;=> {:id \"...\", :size 23, :operators #{and or}, :prog (lambda (x) ...)}
   size must be in [3, 30]"
  ([] (-> (request "train" {}) json-to-prog))
  ([size]
    (-> (request "train" {:size size}) json-to-prog)))

(defn status []
  (request "status"))

(defn get-easy-unsolved-problem []
  (->> (myproblems)
       ;; remove these as necessary
      (filter #(not (contains? (:operators %) 'fold)))
      (filter #(not (contains? (:operators %) 'tfold)))
      (filter #(not (contains? (:operators %) 'bonus)))
      (filter #(not (:solved %)))
      (sort-by :size)
      (sort-by #(count (:operators %)))
      first
      ))
