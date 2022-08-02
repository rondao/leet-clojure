(ns leet-clojure.279 
  (:require [clojure.test :refer [with-test is]]))

(with-test
  (defn whole-number?
    [float]
    (= (- (int float) float) 0.0))
  (is (= true  (whole-number? 1.0)))
  (is (= false (whole-number? 1.1))))

(with-test
  (defn least-square-sum
    "Find the least number of perfect square numbers that sum up to 'n'.
     Param 'squares' is a list with all the perfect square numbers up to 'n'.
     Param 'results' is a list with all the result for 'least-square-sum' up to 'n'."
    [n squares results]
    (inc (reduce
          #(min (results %1) (results %2))
          (map #(- n %1) squares))))
  (is (= 2  (least-square-sum 2 [1] [0 1])))
  (is (= 3  (least-square-sum 3 [1] [0 1 2])))
  (is (= 2  (least-square-sum 5 [1 4] [0 1 2 3 1])))
  (is (= 2  (least-square-sum 8 [1 4] [0 1 2 3 1 2 3 4]))))

(with-test
  (defn leet-279
    "Given an integer n, return the least number of perfect square numbers that sum to n.
   A perfect square is an integer that is the square of an integer; in other words, it is the product of some integer with itself. For example, 1, 4, 9, and 16 are perfect squares while 3 and 11 are not.

   Example 1:
     Input: n = 12
     Output: 3
   Explanation: 12 = 4 + 4 + 4.

   Example 2:
     Input: n = 13
     Output: 2
   Explanation: 13 = 4 + 9.

   Constraints:
    1 <= n <= 104
   "
    ;; We need to find this solution for every number from 1 to 'number - 1',
    ;; and with this information we can find the solution for 'number'.
    ;;
    ;; Params:
    ;; - 'current' is used to iterate from 1 to 'number'.
    ;; - 'squares' is a list with all the perfect squares up to 'current'.
    ;; - 'results' is a list with all the solutions up to 'current'.
    [number]
    (loop [current 1
           squares (list)
           results [0]]
      (if (> current number)
        (last results)
        (if (whole-number? (Math/sqrt current))
          (recur (inc current)
                 (conj squares current)
                 (conj results 1))
          (recur (inc current)
                 squares
                 (conj results
                       (least-square-sum current squares results)))))))
  (is (= 3 (leet-279 12)))
  (is (= 2 (leet-279 13))))