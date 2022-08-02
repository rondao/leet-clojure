(ns leet-clojure.124 
  (:require [clojure.test :refer [with-test is]]))

(with-test
  (defn node-path-sums
    "Return ':one-child-sum' which is this node value added with the highest
     child ':one-child-sum' value. This can be used to make a bigger path.
     Also return ':output', which is the biggest path we found so far.
     This value cannot be used to make a bigger path, as it already contain
     a starting and ending point."
    [tree]
    (let [node-value (first tree)
          children (rest tree)]
      (if (empty? children)
        {:one-child-sum node-value :output node-value}
        (let [children-path-sums (map node-path-sums children)
              children-one-child-sum (map :one-child-sum children-path-sums)
              one-child-sum (apply max children-one-child-sum)
              both-child-sum (apply + children-one-child-sum)
              best-child-output (apply max (map :output children-path-sums))]
          {:one-child-sum (+ node-value one-child-sum)
           :output (max (+ node-value both-child-sum) best-child-output)}))))
  (is (= {:one-child-sum 1 :output 1} (node-path-sums '(1))))
  (is (= {:one-child-sum 4 :output 6} (node-path-sums '(1 (2) (3)))))
  (is (= {:one-child-sum 2 :output 10} (node-path-sums '(-5 (1 (4)) (2 (3) (5)))))))

(with-test
  (defn leet-124
    "A path in a binary tree is a sequence of nodes where each pair of adjacent nodes in the sequence has an edge connecting them. A node can only appear in the sequence at most once. Note that the path does not need to pass through the root.
   The path sum of a path is the sum of the node's values in the path.
   Given the root of a binary tree, return the maximum path sum of any non-empty path.

   Example 1:
     Input: root = [1,2,3]
     Output: 6
   Explanation: The optimal path is 2 -> 1 -> 3 with a path sum of 2 + 1 + 3 = 6.
   
   Example 2:
     Input: root = [-10,9,20,nil,nil,15,7]
     Output: 42
   Explanation: The optimal path is 15 -> 20 -> 7 with a path sum of 15 + 20 + 7 = 42.
   
   Constraints:
    The number of nodes in the tree is in the range [1, 3 * 104].
    -1000 <= Node.val <= 1000
   "
    [tree]
    (:output (node-path-sums tree))
    (is (= 6 (leet-124 '(1 (2) (3)))))
    (is (= 42 (leet-124 '(-10 (9) (20 (15) (7))))))))

(defn node-path-sum-array
  "Return this 'node' value added with the highest child value
   and added with both children.
   The first one can be used to make a bigger path sum, while the
   second already contains a starting and ending point."
  [tree node]
  (let [left-child? (get tree (+ (* 2 node) 1))
        right-child? (get tree (+ (* 2 node) 2))
        left-child (or left-child? 0)
        right-child (or right-child? 0)
        expandable-path-sum (+ (get tree node) (max left-child right-child))
        best-path-sum (+ (get tree node) left-child right-child)]
      (list expandable-path-sum best-path-sum)))

(with-test
  (defn solution-tree-as-array
    [tree]
    (loop [i (dec (count tree))
           tree tree
           maximum-path-sum 0]
      (cond
        (< i 0) maximum-path-sum
        (nil? (get tree i)) (recur (dec i) tree maximum-path-sum)
        :else (let [[expandable-path-sum node-best-path-sum] (node-path-sum-array tree i)]
                (recur (dec i)
                       (assoc tree i expandable-path-sum)
                       (max maximum-path-sum (or node-best-path-sum 0)))))))
  (is (= 6 (leet-124 [1 2 3])))
  (is (= 42 (leet-124 [-10 9 20 nil nil 15 7]))))