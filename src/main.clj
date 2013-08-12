(ns main
  (:require [api]
            [enum]
            [functions :refer (to-num)]
            [clojure.string :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rand-big-int []
  (to-num (apply str (repeatedly 8 #(rand-nth "0123456789abcdef")))))

(defn generate-inputs []
  (conj (repeatedly 254 rand-big-int)
        (to-num 0)
        (to-num 1)))

(defn current-msecs []
  (System/currentTimeMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Run loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce db (atom nil))

(defn get-next-problem []
  (assoc (api/train 3)
    :time-started (current-msecs)))

(defn time-period-expired? [{:keys [time-started] :as problem}]
  (> (- (current-msecs) (* 5 60 1000)) time-started))

(defn progs [{:keys [size operators] :as problem} inputs outputs]
  (println "Calculating problem using"
           {:size size
            :ops operators
            :num-inputs (count inputs)
            :num-outputs (count outputs)
            :first-few-inputs (take 5 inputs)
            :last-few-outputs (take 5 outputs)})
  (enum/progs size operators inputs outputs))

(defn guess-request [{:keys [id] :as problem} prog]
  (api/guess id prog))

(defn guess [problem inputs outputs]
  (println "outputs" outputs)
  (if (time-period-expired? problem)
    (do (println "time period expired. Moving to next problem")
        :ran-out-of-time)
    (if-let [progs-result (first (progs problem inputs outputs))]
      (let [result (guess-request problem progs-result)]
        (println "Response:" (:status result))
        (println "guess result:" result)
        (if (:win result)
          :success
          (recur problem
                 (conj inputs (:input result))
                 (conj outputs (:output result)))))
      :fail)))

(defn try-problem [{:keys [id] :as problem}]
  (println "******************** PROBLEM" id "********************")
  (let [inputs (generate-inputs)]
    (println (first inputs))
    (guess problem
           inputs
           (api/eval-id id inputs))))

(defn log-result [problem result]
  (swap! db
         update-in [:results]
         conj [(:id problem)
               {:result result
                :time-taken (- (current-msecs) (:time-started problem))}]))

(defn run-loop []
  (while true
    (let [problem (get-next-problem)]
      (log-result problem
                  (try-problem problem)))))
