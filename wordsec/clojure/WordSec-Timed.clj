;*********************************************************************
;  CODE FILENAME:  WordSecTimed.clj
;
;  DESCRIPTION:    A program to analyze words in a text file. The text
;                  files contain paragraphs of sentences of English
;                  words and possibly numbers. The program:
;                  * Reads, counts and organizes the words into lists
;                    based on starting letters.
;                  * Words containing any digits are ignored.
;                  * Determines the number of unique words in the text
;                    file that begin with each alphabetic letter.
;                  * Determines which letter or letters begin(s) the
;                    most unique words.
;                  * Outputs resulst to standard out.
;
;  DESIGNER:       Jerry D'Antonio
;*********************************************************************

(use 'clojure.set)
(use '[clojure.java.io :only [reader]])
(require '[clojure.string :as string])

;*********************************************************************
; Function Declarations

(defn print-usage 
  "Display progam usage information to standard out."
  [] (println (str
      "Usage: clojure DLLProg.clj FILENAME\n"
      "Analyzes words in a text file containing paragraphs of sentences of English words and possibly numbers.\n"
      "\tFILENAME\t Relative or absolute path to the input file.")))

(defn letter-list
  "A list of characters ranging from A to Z. Used for iterating over the alphabet."
  [] (map char (range (int \A) (int \Z))))

(defn letter-header
  "Print \"'a'/'A'\" using the first letter of the word."
  [word] (str "'" (string/lower-case (first word)) "'/'" (first word) "'"))

(defn split-line-into-words
  "Separate a line of text into words."
  [line] (drop-while empty? (string/split line #"\s+")))

(defn read-words-from-file
  "Read all words from the input text file."
  [filename]
    (try
      (with-open [rdr (reader filename)]
        (doall (flatten (map split-line-into-words (line-seq rdr)))))
      (catch java.io.FileNotFoundException _ nil)))

(defn validate-and-filter-words
  "Drop all words with numbers, strip punctuation from the ends, and make upper-case."
  [words] (map #(string/replace %1 #"[^A-Z]+$" "")
            (map #(string/replace %1 #"^[^A-Z]+" "")
              (filter #(re-find #"^[^\d]+$" %1)
                (map string/upper-case words)))))

(defn words-by-first-letter
  "Separate a list of words into a lists of lists where sublists all share the same first letter."
  [words] (list* (filter not-empty
    (for [letter (letter-list)]
      (for [word words :when (.startsWith word (str letter))] word)))))

(defn print-word-list
  "Print a list of words with a brief intro."
  [word-list] (println (str
    (count word-list)
    " words beginning with "
    (letter-header (first word-list))
    ":\n\t"
    (apply str (interpose ", " word-list))
    )))

;*********************************************************************
; Main Routine

; get the input file name from the first command-line arg
(def filename (first *command-line-args*))

; if no file name given, print usage and exit
(if (nil? filename) ((print-usage) (System/exit 0)))

; process the input file and time it
(def timer-1 (with-out-str (time
  (do (def all-words (read-words-from-file filename))
    (do (def total-words (count all-words))
      (do (def unique-words (set (validate-and-filter-words all-words)))
        (do (def total-unique-words (count unique-words))
          (do (def unique-words-by-letter (words-by-first-letter unique-words))))))))))

; print aggregate data
(println (str
  "Results for file " filename ":  " total-words " total words processed."))
(println)

; print per-letter data and word lists
(def timer-2 (with-out-str (time (def highest-word-count (apply max (map count unique-words-by-letter))))))
(doall (for [word-list unique-words-by-letter] (print-word-list word-list)))

; print uniqueness data and word lists
(println (str "\nThere were " total-unique-words " unique words in the file."))
(println (str "The highest word count was " highest-word-count "."))
(println (str "\nLetter(s) that began words " highest-word-count " times were"))
(doall (for [word-list unique-words-by-letter]
  (if (= highest-word-count (count word-list))
    (println (str "\t" (letter-header (first word-list)))))))

; print timer info
(print (str "\nRead words from file: " (string/replace timer-1 #"\"" "")))
(print (str "Tally highest word count: " (string/replace timer-2 #"\"" "")))
