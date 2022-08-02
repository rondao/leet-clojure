(ns leet-clojure.102 
  (:require [clojure.test :refer [with-test is]]))

(with-test
  (defn get-each-tree-elements
    [list-of-trees]
    (map first list-of-trees))
  (is (= [nil] (get-each-tree-elements [[]])))
  (is (= [1] (get-each-tree-elements [[1]])))
  (is (= [1 1] (get-each-tree-elements [[1][1]])))
  (is (= [1 1] (get-each-tree-elements [[1 [2 2]] [1 [2 [3]]]]))))

(with-test
  (defn get-each-tree-childrens
    [list-of-trees]
    (apply concat (map rest list-of-trees)))
  (is (= [] (get-each-tree-childrens [[]])))
  (is (= [] (get-each-tree-childrens [[1] [1]])))
  (is (= [[2 2] [2 [3]]] (get-each-tree-childrens [[1 [2 2]] [1 [2 [3]]]]))))

(with-test
  (defn leet-102
    "Given the root of a binary tree, return the level order traversal of its nodes' values. (i.e., from left to right, level by level).

   Example 1:
    Input: root = [3,9,20,nil,nil,15,7]
    Output: [[3],[9,20],[15,7]]

   Example 2:
    Input: root = [1]
    Output: [[1]]

   Example 3:
    Input: root = []
    Output: []

   Constraints:
    The number of nodes in the tree is in the range [0, 2000].
    -1000 <= Node.val <= 1000
   "
    [tree]
    ;; Param 'level-trees' contains all the trees within a level.
    ;; We iterate from the first level to the last level expaning to the output.
    (loop [level-trees (list tree)
           output []]
      (recur (conj output (get-each-tree-elements level-trees))
             (get-each-tree-childrens level-trees)))
    (is (= [[3] [9,20] [15,7]] (leet-102 '(3 (9) (20 (15) (7))))))
    (is (= [[1]] (leet-102 '(1))))
    (is (= [] (leet-102 '())))))

(with-test
  (defn get-elems-from-level
    "Return a list with all the elements at 'level' within 'tree'."
    [tree level]
    (let [num-elems (Math/pow 2 level)
          level-start (dec num-elems)]
      (if (>= level-start (count tree))
        nil
        (filterv some? (subvec tree level-start (+ level-start num-elems))))))
  (is (= [3] (get-elems-from-level [3 9 20 nil nil 15 7] 0)))
  (is (= [9 20] (get-elems-from-level [3 9 20 nil nil 15 7] 1)))
  (is (= [15 7] (get-elems-from-level [3 9 20 nil nil 15 7] 2))))

(with-test
  (defn tree-as-array-solution
    "Iterate through all levels of the tree joining the elements."
    [tree]
    (loop [level 0 output []]
      (let [level-elems (get-elems-from-level tree level)]
        (if (nil? level-elems)
          output
          (recur (inc level) (conj output level-elems))))))
  (is (= [[3] [9,20] [15,7]] (leet-102 [3 9 20 nil nil 15 7])))
  (is (= [[1]] (leet-102 [1])))
  (is (= [] (leet-102 []))))