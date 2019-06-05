(ns sudoku.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]
            clojure.set))

(defn print-grid
  [grid]
  (println (subvec grid 0 9))
  (println (subvec grid 9 18))
  (println (subvec grid 18 27))
  (println (subvec grid 27 36))
  (println (subvec grid 36 45))
  (println (subvec grid 45 54))
  (println (subvec grid 54 63))
  (println (subvec grid 63 72))
  (println (subvec grid 72 81)))

(defn- unique?
  [x]
  (= (count x) (count (distinct x))))


(defn- get-pos
  [row col]
  (+ (* row 9) col))

(defn- get-num
  [grid row col]
  (get grid (get-pos row col)))


(defn- next-col 
  [col]
  (if (< col 8)
    (inc col)
    0))

(defn- next-row
  [row col]
  (if (< col 8)
    row
    (inc row)))

(defn- last-square?
  [row col]
  (and (= row 9)))


(defn- get-col
  [grid row col]
  (let [point (get-pos 0 col)]
    (filter #(not (zero? %))(concat (subvec grid point (+ 1 point))
                                    (subvec grid (+ point 9) (+ 10 point))
                                    (subvec grid (+ point 18) (+ 19 point))
                                    (subvec grid (+ point 27) (+ 28 point))
                                    (subvec grid (+ point 36) (+ 37 point))
                                    (subvec grid (+ point 45) (+ 46 point))
                                    (subvec grid (+ point 54) (+ 55 point))
                                    (subvec grid (+ point 63) (+ 64 point))
                                    (subvec grid (+ point 72) (+ 73 point))))))

(defn get-cell-start
  [row col]
  (get-pos (* 3 (math/floor (/ row 3))) (* 3 (math/floor (/ col 3))))
  )

(defn get-cell
  [grid row col]
  (let [point (get-cell-start row col)]
    (filter #(not (zero? %)) (concat (subvec grid point (+ 3 point))
                                     (subvec grid (+ point 9) (+ 12 point))
                                     (subvec grid (+ point 18) (+ 21 point))))))

(defn get-row
  [grid row col]
  (filter #(not (zero? %)) (subvec grid (get-pos row 0) (get-pos row 9))))

(defn valid?
  [grid row col val]
  (and 
    (unique? (conj (get-row grid row col) val))
    (unique? (conj (get-cell grid row col) val))
    (unique? (conj (get-col grid row col) val))))

(defn replace-cell
  [grid row col val]
  (let [insert (get-pos row col)]
    (assoc grid insert val)))

(defn update-grid
  [grid row col]
  
  (if (last-square? row col)
    (print-grid grid)
    
    (do
      (if (not (zero? (get-num grid row col)))
        (update-grid grid (next-row row col) (next-col col))
        
        (loop [next-num 1]
          (when (< next-num 10)
            (if (valid? grid row col next-num)
              (do
                (update-grid (replace-cell grid row col next-num) (next-row row col) (next-col col))
                (recur (inc next-num)))
              (do
                (recur (inc next-num)))))))
      
      ))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  
  (let [grid  
        [2 9 0 0 0 1 0 0 0
         4 0 0 0 2 0 0 0 0
         5 0 0 0 0 9 2 3 0
         0 0 0 9 0 0 0 6 3
         0 0 1 2 0 4 9 0 0
         9 5 0 0 0 8 0 0 0
         0 3 9 1 0 0 0 0 8
         0 0 0 0 3 0 0 0 2
         0 0 0 8 0 0 0 4 6]]
    ;(println (conj (get-row grid 0 0) 4))
    ;(println (valid? grid 0 0 1))
    (update-grid grid 0 0)
    
    )
  
  
  )


