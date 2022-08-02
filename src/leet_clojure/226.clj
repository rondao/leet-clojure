(ns leet-clojure.226 
  (:require [clojure.test :refer [is with-test]]))

(with-test
  (defn leet-226
    "Given the root of a binary tree, invert the tree, and return its root.

   Example 1:
     Input: root = [4,2,7,1,3,6,9]
     Output: [4,7,2,9,6,3,1]

   Example 2:
     Input: root = [2,1,3]
     Output: [2,3,1]

   Example 3:
     Input: root = []
     Output: []

   Constraints:
    The number of nodes in the tree is in the range [0, 100].
    -100 <= Node.val <= 100
   "
    ;; Let's reverse the childrens of this node, and recursively
    ;; repeat this for all it's childrens.
    [input]
    (if (empty? input)
      input
      (cons (first input)
            (map leet-226 (reverse (rest input))))))
  (is (= '(4 (7 (9) (6)) (2 (3) (1))) (leet-226 '(4 (2 (1) (3)) (7 (6) (9))))))
  (is (= '(2 (3) (1)) (leet-226 '(2 (1) (3)))))
  (is (= '() (leet-226 '()))))

(with-test
  (defn solution-tree-as-array
    [input]
    (loop [power 0
           input input
           result ()]
      (if (empty? input)
        result
        (let [split-index (Math/pow 2 power)]
          (recur (inc power)
                 (subvec input split-index)
                 (concat result (reverse (subvec input 0 split-index))))))))
  (is (= [4 7 2 9 6 3 1] (solution-tree-as-array [4 2 7 1 3 6 9])))
  (is (= [2 3 1] (solution-tree-as-array [2 1 3])))
  (is (= [] (solution-tree-as-array []))))