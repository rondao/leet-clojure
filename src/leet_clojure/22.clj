(ns leet-clojure.22 
  (:require [clojure.test :refer [with-test is]]))

(defn add-paren-around
  [string]
  (str "(" string ")"))

(defn add-paren-right
  [string]
  (str string "()"))

(defn add-paren-left
  [string]
  (str "()" string))

(defn add-parenthesis
  [set string]
  (conj set
        (add-paren-around string)
        (add-paren-right string)
        (add-paren-left string)))

(with-test
  (defn leet-22
    "Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.

   Example 1:
     Input: n = 3
     Output: ['((()))','(()())','(())()','()(())','()()()']

   Example 2:
     Input: n = 1
     Output: ['()']

   Constraints:
    1 <= n <= 8
   "
    [number]
    (loop [current 1
           result #{"()"}]
      (if (= current number)
        result
        (recur (inc current) (reduce add-parenthesis #{} result)))))
  (is (= #{"((()))" "(()())" "(())()" "()(())" "()()()"} (leet-22 3)))
  (is (= #{"()"} (leet-22 1))))