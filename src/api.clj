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
     (println "sending request" path data)
     (let [result (http/post (str base path)
                             {:query-params {:auth AUTH}
                              :form-params data
                              :content-type :json
                              :as :json})]
       (println "--------request result:" result)
       (:body result))))

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
  (let [args-hex (map f/to-hex (vec args))
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
  (prn "prog is" prog)
  (let [program (f/to-string prog)
        _ (println "program is" program)
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
  ([] (-> (request "train" {:operators []}) json-to-prog))
  ([size]
    (-> (request "train" {:size size
                          :operators []}) json-to-prog)))

(defn status []
  (request "status"))

(def my-problems (atom nil))

(defn get-my-problems! []
  (or @my-problems (reset! my-problems (myproblems))))

(def available
  #{"1AtLRurozdJSUz4HsLPpBIOI"
"267te6Z56V0CLjtdJCtBEz9c"
"4dQDBvK3qJhZyLqbyAH155vn"
"4oKKwyMQK55CjCaBs88q3EtS"
"6XWbO9osaGcsTJHbBX3wJCQ7"
"9bjNF86WSpcauiGnc7yTcDdI"
"9kMNeXiSI9KAhBlwBKO0JNCp"
"9xcU5QlPgDuXG18mbLKeAugA"
"AXs7KAMH0Am0RCOjtb4u1zhQ"
"BdkJs52fqOGqb4OAtLHMIoOB"
"BomFrAdcAQ8AZJ08c2hm1Acg"
"BotFA7L6BOG6u4CCAg7UUzGA"
"DcR93hOmKFuyppI9WpwbqpTK"
"EBB4LJF6xgrImKO1NHucrlsx"
"ELxuZCKzX2gtriuOfwVgZZuG"
"EoWfXIIWMWBAmCOIlboq6AUT"
"GEhtRdisWFrCDCe53Hk0a8KQ"
"NOD0wdTBJJybT0RouFJvsKZa"
"PQAAtIznPTCBghC6gIrucCth"
"R81w79GRhtAj6s6031FddXCm"
"SX8I2yybvmdPyIoxg5nVTYOz"
"TEM4PpfivoxY8o2pXaHjUPO2"
"UBsVjkXvPsB9TLBic72hvmz4"
"V2MIHYiRA4vgdF6cxELIq0tR"
"Vdbj9IoInY809rsTiwBJELft"
"WFhsDqgEGAoAwrKQbYZRmjBi"
"YyH0yblFVY1FC2AkLASMYbzn"
"ZxAGyXqJDn2TRCPWeA6rWOHD"
"a0dTVRU2B18n6loN1ByOj65Z"
"aw2vgyFo9eOPZcHhH6Qc2hzI"
"bKAmX0QgM84xRSvyYoxPYUI2"
"cEv38BsBGrwMPOfA8bI4DBxI"
"cltnQxZ0IK3OsPu9raYVOadc"
"eVRQBYwS78KBs8YnVYyE5Xcm"
"ebGxrsOHZw7SYdVWPIeXw1a1"
"huMKX2GljkEUH3yOOEUAElmG"
"iSt8GtdWwAMItwzTdCOmnvRB"
"jPRNY4Cg1jBzTeVQdkiGCQ2v"
"m9AZDuQboj1xaEs93x2mW0jV"
"pMr9BylAqkwMEnDFLdGZEI83"
"sdjCAdb0Mva3ABHjC2mgkMpw"
"tNN8bXzF5PBBH6aAA3Vc7BUw"
"tnQPpwKbeuU8kUhrIuwxz31g"
"uS2yqgm408GN3fEOKPayxUVc"
"ukDBSvYlEQFcKq2c706MG9Uz"
"vXSdm72MFFEMwBRABcQZV0ON"
"wIkFVABj2SS1YQIuPSm2yBBG"
"wSpIETsMnPBsAXADHAwN0Vik"
"x59jP7IXte60gPA58QO1ckg9"
"xUDBo9pOZa815NN4AZsjJLk7"
"yBBx0JxGtxQjA5F9J9A9HzSO"
"yFAmTbbqIJ09bvvaKfr5A2EE"
"yqO71r20BQCB5ISc0U4ctoot"
"ywd1bT2GukCDu1K8LePAe6s7"})

(defn get-easy-unsolved-problem []
  (->> (get-my-problems!)
       ;; remove these as necessary
      (filter #(not (contains? (:operators %) 'fold)))
      (filter #(not (contains? (:operators %) 'tfold)))
      (filter #(not (contains? (:operators %) 'bonus)))
      (filter #(not (contains? (:operators %) 'plus)))
      (filter (fn [problem] (available (:id problem))))
      (filter #(not (:solved %)))
      (filter #(not (= 0 (:time-left %))))
      (sort-by :size)
      (sort-by #(count (:operators %)))

      first
      ))
