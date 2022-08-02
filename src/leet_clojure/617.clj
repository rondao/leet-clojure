(ns leet-clojure.617 
  (:require [clojure.test :refer [with-test is]]))

;; Source: https://stackoverflow.com/questions/4460986/clojure-map-longest
(defn map-longest
  ([func c1]
    (map func c1))
  ([func c1 & colls]
    (lazy-seq
      (when (not-every? empty? (conj colls c1))
        (let [firsts (map first (conj colls c1))]
          (cons
            (apply func firsts)
            (apply map-longest
              (conj (map rest colls) (rest c1) func))))))))

(with-test
  (defn leet-617
    "You are given two binary trees root1 and root2.
   Imagine that when you put one of them to cover the other, some nodes of the two trees are overlapped while the others are not. You need to merge the two trees into a new binary tree. The merge rule is that if two nodes overlap, then sum node values up as the new value of the merged node. Otherwise, the NOT null node will be used as the node of the new tree.
   Return the merged tree.

   Nope: The merging process must start from the root nodes of both trees.


   Example 1:
     Input: root1 = [1,3,2,5], root2 = [2,1,3,null,4,null,7]
     Output: [3,4,5,5,4,null,7]

   Example 2:
     Input: root1 = [1], root2 = [1,2]
     Output: [2,2]

   Constraints:
    The number of nodes in both trees is in the range [0, 2000].
    -104 <= Node.val <= 104
   "
    [root1 root2]
    (if (not-every? seq (list root1 root2))
      (some seq (list root1 root2))
      (cons (+ (first root1) (first root2))
            (map-longest leet-617 (rest root1) (rest root2)))))
  (is (= '(3 (4 (5) (4)) (5 nil (7))) (leet-617 '(1 (3 (5)) (2))
                                              '(2 (1 nil (4)) (3 nil (7))))))
  (is (= '(2 (2)) (leet-617 '(1)
                          '(1 (2))))))

(with-test
  (defn tree-as-array-solution
    [root1 root2]
    (let [merge (map #(+ (or %1 0) (or %2 0)) root1 root2)
          len1 (count root1)
          len2 (count root2)]
      (cond
        (> len1 len2) (concat merge (subvec root1 len2))
        (> len2 len1) (concat merge (subvec root2 len1))
        :else merge)))
  (is (= [3 4 5 5 4 nil 7] (leet-617 [1 3 2 5] [2 1 3 nil 4 nil 7])))
  (is (= [2 2] (leet-617 [1] [1 2]))))