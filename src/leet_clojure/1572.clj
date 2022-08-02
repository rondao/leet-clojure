(ns leet-clojure.1572
  (:require [clojure.test :refer [with-test is]]))

(with-test
  (defn leet-1572
    "Given a square matrix mat, return the sum of the matrix diagonals.
     Only include the sum of all the elements on the primary diagonal and all the elements on the secondary diagonal that are not part of the primary diagonal.

     Example 1:
     Input: mat = [[1,2,3],
                   [4,5,6],
                   [7,8,9]]
     Output: 25

     Explanation: Diagonals sum: 1 + 5 + 9 + 3 + 7 = 25
     Notice that element mat[1][1] = 5 is counted only once.

     Example 2:
     Input: mat = [[1,1,1,1],
                   [1,1,1,1],
                   [1,1,1,1],
                   [1,1,1,1]]
     Output: 8

     Example 3:
     Input: mat = [[5]]
     Output: 5

     Constraints:
     n == mat.length == mat[i].length
     1 <= n <= 100
     1 <= mat[i][j] <= 100"
    [mat]
    (let [mat-size (count mat)
          primary-diagonal   (map #(%1 %2) mat (range))
          secondary-diagonal (map #(%1 %2) mat (range (- mat-size 1) -1 -1))
          output (apply + (concat primary-diagonal secondary-diagonal))]
      (if (even? mat-size)
        output
        (let [middle-index (int (/ mat-size 2))
              middle-element ((mat middle-index) middle-index)]
          (- output middle-element)))))
  (is (= 25 (leet-1572 [[1 2 3]
                        [4 5 6]
                        [7 8 9]])))
  (is (= 8 (leet-1572 [[1 1 1 1]
                       [1 1 1 1]
                       [1 1 1 1]
                       [1 1 1 1]])))
  (is (= 5 (leet-1572 [[5]]))))