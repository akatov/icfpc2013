(ns main
  (:require [api]
            [enum]
            [functions :refer (to-num)]
            [clojure.string :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rand-big-int []
  (to-num (apply str (repeatedly 16 #(rand-nth "0123456789abcdef")))))

(defn generate-inputs []
  (conj (repeatedly 254 rand-big-int)
        (to-num 0)
        (to-num 1)))

(defn current-msecs []
  (System/currentTimeMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Run loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce db (atom nil))

(defn get-next-problem []
  (assoc (api/get-easy-unsolved-problem)
    :time-started (current-msecs)))

(defn time-period-expired? [{:keys [time-started] :as problem}]
  (> (- (current-msecs) (* 5 60 1000)) time-started))

(defn progs [{:keys [size operators] :as problem} inputs outputs]
  (println "Calculating problem using"
           {:size size
            :ops operators
            :num-inputs (count inputs)
            :num-outputs (count outputs)})
  (enum/progs size operators inputs outputs))

(defn guess-request [{:keys [id] :as problem} prog]
  (api/guess id prog))

(defn guess [problem inputs outputs]
  (if (time-period-expired? problem)
    (do (println "time period expired. Moving to next problem")
        :ran-out-of-time)
    (let [result (guess-request problem (first (progs problem inputs outputs)))]
      (println "Response:" (:status result))
      (if (:win result)
        :success
        (recur problem
               (conj inputs (:input result))
               (conj outputs (:output result)))))))

(defn try-problem [{:keys [id] :as problem}]
  (println "******************** PROBLEM" id "********************")
  (let [inputs (generate-inputs)]
    (guess-next problem
                inputs
                (api/eval-id id inputs))))

(defn log-result [problem result]
  (swap! db
         update-in [:results]
         conj [(:id problem)
               {:result result
                :time-taken (- (current-msecs) (:time-started result))}]))

(defn run-loop []
  (while true
    (let [problem (get-next-problem)]
      (log-result problem
                  (try-problem problem)))))