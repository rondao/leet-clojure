(ns leet-clojure.647
  (:require [clojure.test :refer [is with-test]]))

(with-test
  (defn count-palindrome-pyramid
    "Count how many palindromes can be found expanding 'start' and 'end' inside 'text'.
   If 'start' and 'end' are equal, we then check 'start - 1' and 'end + 1' and so on.
   Param:
   - 'text': String to search palindromes.
   - 'start': Index starting the palindrome.
   - 'end': Index ending the palindrome.
   - 'total': Total number of palindromes found."
    [text start end total]
    (if (or (neg? start) (>= end (count text)))
      total
      (if-not (= (get text start) (get text end))
        total
        (recur text (dec start) (inc end) (inc total)))))
  (is (= 1 (count-palindrome-pyramid "aaaaa" 0 0 0)))
  (is (= 2 (count-palindrome-pyramid "aaaaa" 1 1 0)))
  (is (= 2 (count-palindrome-pyramid "aaaaa" 1 2 0)))
  (is (= 3 (count-palindrome-pyramid "aaaaa" 2 2 0))))

(with-test
  (defn count-odd-size-palindromes
    "Count all the odd sized palindromes within 'text'.
   Start checking every individual character and try to expand each one."
  ;; For example, if 'text' is "aaaaa".
  ;;  a    a    a    a    a
  ;;      aaa  aaa  aaa
  ;;          aaaaa
    [text]
    (apply + (map #(count-palindrome-pyramid text % % 0)
                  (range (count text)))))
  (is (= 1 (count-odd-size-palindromes "a")))
  (is (= 2 (count-odd-size-palindromes "aa")))
  (is (= 4 (count-odd-size-palindromes "aaa")))
  (is (= 6 (count-odd-size-palindromes "aaaa")))
  (is (= 9 (count-odd-size-palindromes "aaaaa"))))

(with-test
  (defn count-even-size-palindromes
    "Count all the even sized palindromes within 'text'.
   Start checking every pair of characters and try to expand each one."
  ;; For example, if 'text' is "aaaaa".
  ;;  aa   aa   aa   aa
  ;;      aaaa aaaa
    [text]
    (apply + (map #(count-palindrome-pyramid text % (+ % 1) 0)
                  (range (count text)))))
  (is (= 0 (count-even-size-palindromes "a")))
  (is (= 1 (count-even-size-palindromes "aa")))
  (is (= 2 (count-even-size-palindromes "aaa")))
  (is (= 4 (count-even-size-palindromes "aaaa")))
  (is (= 6 (count-even-size-palindromes "aaaaa"))))

(with-test
  (defn leet-647
    "Given a string s, return the number of palindromic substrings in it.
   A string is a palindrome when it reads the same backward as forward.
   A substring is a contiguous sequence of characters within the string.

   Example 1:
     Input: s = 'abc'
     Output: 3
   Explanation: Three palindromic strings: 'a', 'b', 'c'.

   Example 2:
     Input: s = 'aaa'
     Output: 6
   Explanation: Six palindromic strings: 'a', 'a', 'a', 'aa', 'aa', 'aaa'.

   Constraints:
    1 <= s.length <= 1000
    s consists of lowercase English letters.
   "
    [text]
    (+ (count-odd-size-palindromes text)
       (count-even-size-palindromes text)))
  (is (= 3 (leet-647 "abc")))
  (is (= 6 (leet-647 "aaa"))))

(comment
  (defn palindrome?
    ([text]
     (cond
       (empty? text) true
       (= (count text) 1) true
       :else (if (= (first text) (last text))
               (recur (subs text 1 (- (count text) 1)))
               false))))

  (defn palindrome-by-group
    [text group total]
    (if (< (count text) group)
      total
      (recur (subs text 1)
             group
             (if (palindrome? (subs text 0 group))
               (inc total)
               total))))

  (defn palindrome-all-groups
    [text group total]
    (if (> group (count text))
      total
      (recur text
             (inc group)
             (palindrome-by-group text group total))))

  (defn brute-force-solution
    [text]
    (palindrome-all-groups text 1 0)))