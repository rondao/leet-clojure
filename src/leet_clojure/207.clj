(ns leet-clojure.207 
  (:require [clojure.test :refer [is with-test]]))

(with-test
  (defn update-prereq
    "Add 'prereq' to the list of 'map[course]'."
    [map [course prereq]]
    (update map course #(conj (or % []) prereq)))
  (is (= {1 [0]} (update-prereq {} [1 0])))
  (is (= {1 [0 2]} (update-prereq {1 [0]} [1 2])))
  (is (= {1 [0 2] 4 [2]} (update-prereq {1 [0 2]} [4 2]))))

(with-test
  (defn prerequisites-to-map
    "Param 'prerequisites' is a list of tuples, where it's first value is the
     course and it's second value is the course prerequirement.
     Return a map where the 'keys' are the courses and the values is a list
     with all the prerequirements."
    [prerequisites]
    (reduce update-prereq {} prerequisites))
  (is (= {1 [0]} (prerequisites-to-map [[1 0]])))
  (is (= {1 [0 2]} (prerequisites-to-map [[1 0] [1 2]])))
  (is (= {1 [0 2] 4 [2]} (prerequisites-to-map [[1 0] [1 2] [4 2]]))))

(with-test
  (defn has-prerequirement-cycle?
    "Returns 'true' if 'course' have a prerequirement with itself.
     We get the list of prerequirements for 'course', and expand every prerequired
     course of this list with their own prerequirements.
     Repeat it until exhaustion, or until a cycle is found."
    [course prereq-map]
    (loop [prereq-list (prereq-map course)]
      (if (empty? prereq-list)
        false
        (let [prereq-course (peek prereq-list)]
          (if (= course prereq-course)
            true
            (recur (apply conj (pop prereq-list) (prereq-map prereq-course))))))))
  (is (= false (has-prerequirement-cycle? 1 {1 []})))
  (is (= false (has-prerequirement-cycle? 1 {1 [0 2] 0 [3]})))
  (is (= true (has-prerequirement-cycle? 1 {1 [0 1]})))
  (is (= true (has-prerequirement-cycle? 1 {1 [0] 0 [1]}))))

(with-test
  (defn leet-207
    "There are a total of numCourses courses you have to take, labeled from 0 to numCourses - 1. You are given an array prerequisites where prerequisites[i] = [ai, bi] indicates that you must take course bi first if you want to take course ai.
   For example, the pair [0, 1], indicates that to take course 0 you have to first take course 1.
   Return true if you can finish all courses. Otherwise, return false.

   Example 1:
    Input: numCourses = 2, prerequisites = [[1,0]]
    Output: true
   Explanation: There are a total of 2 courses to take. 
   To take course 1 you should have finished course 0. So it is possible.

   Example 2:
    Input: numCourses = 2, prerequisites = [[1,0],[0,1]]
    Output: false
   Explanation: There are a total of 2 courses to take.
   To take course 1 you should have finished course 0, and to take course 0 you should also have finished course 1. So it is impossible.

   Constraints:
    1 <= numCourses <= 105
    0 <= prerequisites.length <= 5000
    prerequisites[i].length == 2
    0 <= ai, bi < numCourses
    All the pairs prerequisites[i] are unique.
   "
    [_num-courses prerequisites]
    (loop [prereq-map (prerequisites-to-map prerequisites)
           courses (keys prereq-map)]
      (if (empty? courses)
        true
        (if (has-prerequirement-cycle? (first courses) prereq-map)
          false
          (recur (assoc prereq-map courses [])
                 (rest courses))))))
  (is (= true (leet-207 2 [[1 0]])))
  (is (= false (leet-207 2 [[1 0][0 1]]))))
