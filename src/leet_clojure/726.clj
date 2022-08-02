(ns leet-clojure.726 
  (:require [clojure.test :refer [is with-test]]
            [clojure.string :as str]))

(with-test
  (defn get-number
    [input]
    (re-matches #"(\d*)(.*)" input))
  (is (= ["2bA" "2" "bA"] (get-number "2bA")))
  (is (= ["23bA4" "23" "bA4"] (get-number "23bA4"))))

(with-test
  (defn get-rev-element
    [input]
    (re-matches #"([a-z]?[A-Z])(.*)" input))
  (is (= ["A2C3" "A" "2C3"] (get-rev-element "A2C3")))
  (is (= ["bA2C3" "bA" "2C3"] (get-rev-element "bA2C3"))))

(with-test
  (defn leet-726
    "Given a string formula representing a chemical formula, return the count of each atom.
   The atomic element always starts with an uppercase character, then zero or more lowercase letters, representing the name.
   One or more digits representing that element's count may follow if the count is greater than 1. If the count is 1, no digits will follow.

   For example, 'H2O' and 'H2O2' are possible, but 'H1O2' is impossible.
   Two formulas are concatenated together to produce another formula.

   For example, 'H2O2He3Mg4' is also a formula.
   A formula placed in parentheses, and a count (optionally added) is also a formula.

   For example, '(H2O2)' and '(H2O2)3' are formulas.
   Return the count of all elements as a string in the following form: the first name (in sorted order), followed by its count (if that count is more than 1), followed by the second name (in sorted order), followed by its count (if that count is more than 1), and so on.
   
   The test cases are generated so that all the values in the output fit in a 32-bit integer.

   Example 1:
     Input: formula = 'H2O'
     Output: 'H2O'
   Explanation: The count of elements are {'H': 2, 'O': 1}.

   Example 2:
     Input: formula = 'Mg(OH)2'
     Output: 'H2MgO2'
   Explanation: The count of elements are {'H': 2, 'Mg': 1, 'O': 2}.

   Example 3:
     Input: formula = 'K4(ON(SO3)2)2'
     Output: 'K4N2O14S4'
   Explanation: The count of elements are {'K': 4, 'N': 2, 'O': 14, 'S': 4}.

   Constraints:
    1 <= formula.length <= 1000
    formula consists of English letters, digits, '(', and ')'.
    formula is always valid.
   "
    ;; 'rev-formula' is simply the 'input' reversed.
    ;; 'stack-numbers' is a stack with every number available at a moment.
    ;; 'multiplier' is any number applied to a parenthesis. They can accumulate for
    ;; parenthesis within parenthesis.
    ;; 'prev-number' the last number found so far.
    ;; 'result' maps the elements found to how many times they occur.
    [input]
    (loop [rev-formula (str/reverse input)
           stack-numbers '()
           multiplier 1
           prev-number 1
           result {}]
      (let [char (first rev-formula)]
        (cond
          ;; Iterated all the formula. Let's format the result and return it.
          (nil? char)
          (let [result (sort result)
                elems (keys result)
                counts (vals result)]
            (apply str (map str elems counts)))
          ;; Element? Let's apply current 'multiplier' and increase how many times it occurs.
          (Character/isAlphabetic (int char))
          (let [[_ str-elem rest-formula] (get-rev-element rev-formula)
                elem (str/reverse str-elem)]
            (recur rest-formula
                   stack-numbers multiplier 1
                   (update result elem #(+ (* prev-number multiplier) (or %1 0)))))
          ;; Number? We just want to remember it.
          (Character/isDigit (int char))
          (let [[_ str-number rest-formula] (get-number rev-formula)
                number (Integer/parseInt (str/reverse str-number))]
            (recur rest-formula
                   stack-numbers multiplier number result))
          ;; Opening (reversed) paranthesis? Increase our 'multiplier' with 'prev-number'.
          (= \) char)
          (recur (subs rev-formula 1)
                 (conj stack-numbers prev-number)
                 (* multiplier prev-number)
                 1 result)
          ;; Closing (reversed) paranthesis? Decrease our 'multiplier' with last number used.
          (= \( char)
          (recur (subs rev-formula 1)
                 (rest stack-numbers)
                 (/ multiplier (first stack-numbers))
                 1 result)))))
  (is (= "H2O1" (leet-726 "H2O")))
  (is (= "H2Mg1O2" (leet-726 "Mg(OH)2")))
  (is (= "K4N2O14S4" (leet-726 "K4(ON(SO3)2)2"))))
