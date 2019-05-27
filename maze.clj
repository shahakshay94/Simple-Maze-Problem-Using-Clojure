(ns maze)

;; reference link for the macro
;; https://stackoverflow.com/a/10056715
(defmacro for-loop [[sym init check change :as params] & steps]
  `(loop [~sym ~init value# nil]
     (if ~check
       (let [new-value# (do ~@steps)]
         (recur ~change new-value#))
       value#)))

(def maze_vector)
(def maze_array)
(def maze_row 0)
(def maze_col 0)
(def start_row 0)
(def start_col 0)
(def end_row 0)
(def end_col 0)
(def solution_exist)


(defn readfile [filename]
    (def mapdata (slurp filename))
    (vec (for [line (clojure.string/split-lines mapdata)]
         (clojure.string/split (apply str line) #"")))
)

(defn calculate_maze_row [maze_vector]
  (for-loop [i 0 (< i (count maze_vector)) (inc i)]
            (def maze_row (+ i 1) ))
)




(defn calculate_maze_col [maze_vector]
  (def firstrow (nth maze_vector 0))
  (for-loop [i 0 (< i (count firstrow)) (inc i)]
            (def maze_col (+ i 1) ))
)

(defn initialise-maze-array-with-values [maze_vector]
  (for-loop [row 0 (< row (count maze_vector)) (inc row)]
    (def somerow (nth maze_vector row))
    (for-loop [col 0 (< col (count somerow)) (inc col)]
      ;; be careful
      ;; it is col X row
      ;; so array parameters column value will come first
      ;; and the next value will be of the row i.e the line number which we want
      (aset maze_array col  row (char (first (.getBytes (nth somerow col)))))
              ;;store the destination coordinates if we find @ in the maze
         (if (= "@" (nth somerow col))
           (do (def end_row row) (def end_col col)))
    )
  )
)


(defn print-maze-array [maze_array]
  (for-loop [row 0 (< row (count maze_vector)) (inc row)]
    (def somerow (nth maze_vector row))
    (for-loop [col 0 (< col (count somerow)) (inc col)]
      ;; be careful
      ;; it is col X row
      ;; so array parameters column value will come first
      ;; and the next value will be of the row i.e the line number which we want
      (print (aget maze_array col  row ))
    )
            (println " ")
  )
)

(defn is-safe [ maze_array row col]
  (if (and (>= col 0) (< col maze_col) (>= row 0) (< row maze_row) (= (char ( first (.getBytes "-"))) (aget maze_array col row)))
    ( do (def solution_exist true))
    ( do (def solution_exist false)))
  )

(defn solve-maze-using-backtracking [maze_array row col]
  (if (and (= row end_row) (= col end_col))
    (do (def solution_exist true)))

  (is-safe maze_array row col)
  (if (= true solution_exist)
    ;; multiple do's will come
    ;; inside these do's nested if will come
    ;; over here



    (do

      (aset maze_array col  row (char (first (.getBytes "+"))));; marking the step as a solution in the path

      (solve-maze-using-backtracking maze_array (- row 1) col)
      (if (= true solution_exist) (do (def solution_exist true)));;moving up


      (solve-maze-using-backtracking maze_array (+ row 1) col)
      (if (= true solution_exist) (do (def solution_exist true))) ;;moving down


      (solve-maze-using-backtracking maze_array row (+ col 1))
      (if (= true solution_exist) (do (def solution_exist true)))  ;;moving right


      (solve-maze-using-backtracking maze_array row (- col 1))
      (if (= true solution_exist) (do (def solution_exist true)))  ;;moving left



      ;; if all the four direction are not possible then backtrack
      ;; and mark the path as not a part of the solution
      (aset maze_array col  row (char (first (.getBytes "!"))))
      (def solution_exist false)

    )
  )
)

(defn solve_maze [maze_array]
  (solve-maze-using-backtracking maze_array 0 0)
  (if (= false solution_exist )
    (do ( println "Uh oh, I could not find the treasure :-("))
    (do ( println "Woo hoo, I found the treasure :-)")))
  (print-maze-array maze_array)
  )


(defn start []
  (def maze_vector (readfile "/Users/akshay.shaz/Desktop/Study/Concordia/COMP6411/RISK_GAME/untitled1/src/map.txt"))
  (calculate_maze_row maze_vector)
  (calculate_maze_col maze_vector)
  (def maze_array (make-array Character/TYPE maze_col maze_row))

  (initialise-maze-array-with-values maze_vector)

  (println "This is my challenge:")
  (print-maze-array maze_array)



  (solve_maze maze_array)

  )


(start)

