;*********************************************************************
;  CODE FILENAME:  WordSec-Incorrect.clj
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

;(in-ns 'dllprog)

;(clojure.core/use 'clojure.core)
(use 'clojure.set)
(use '[clojure.java.io :only [reader]])
(require '[clojure.string :as string])

;*********************************************************************
; Function Declarations

(defn starts-with?
  "Determine if #word starts with #letter"
  [word letter] (= letter (first word)))

(defn print-usage 
  "Display progam usage information to standard out."
  [] (println (str
      "Usage: clojure DLLProg.clj FILENAME\n"
      "Analyzes words in a text file containing paragraphs of sentences of English words and possibly numbers.\n"
      "\tFILENAME\t Relative or absolute path to the input file.")))

(defn filter-words-from-line
  "Separate a line of text into words, drop all words with numbers, and strip punctuation from the beginning and end of each word."
  [line] (map #(string/replace %1 #"[^A-Z]+$" "")
           (map #(string/replace %1 #"^[^A-Z]+" "")
             (drop-while #(re-find #"\d" %1)
               (map string/upper-case 
                 (drop-while empty? 
                   (string/split line #",?\s+")))))))

(defn read-words-from-file
  "Read all words from the input text file, validate each word, then store each unique, valid word in a WorkList."
  [filename]
    (try
      (with-open [rdr (reader filename)]
        (doall (flatten (map filter-words-from-line (line-seq rdr)))))
      (catch java.io.FileNotFoundException _ nil)))

(defn print-word-list
  [words]
  (println
    (str
      (count words)
      " beginning with '"
      (string/lower-case (first (first words)))
      "'/'"
      (first (first words))
      "'\n\t"
      (apply str (interpose ", " words))
      )))

;*********************************************************************
; Main Routine

; get the input file name from the first command-line arg
(def filename (first *command-line-args*))

; if no file name given, print usage and exit
(if (nil? filename) ((print-usage) (System/exit 0)))

; NOTE: This program does not correctly count 'total words' in file
; because it filters the list as it's being read from the file. Counting
; all words in the file uing pure functions only will require the entire
; file to be read into memory so that all words can be counted before
; performing filtering and validation. Counting words as they are being
; read would improve performance, but would be impure.
(def timer-1 (with-out-str (time
  (do (def all-words (read-words-from-file filename))
    (do (def total-words (count all-words))
      (do (def all-words (set all-words))))))))

(def unique-words (count all-words))

(print timer-1)
(println (str
  "Results for file " filename ":  " total-words " total words processed."))
(println)

(def letters (map char (range (int \A) (int \Z))))

(def all-words (list*
  (for [letter letters]
    (for [word all-words :when (starts-with? word letter)] word))))

(doall
  (for [word-list all-words :when (not (empty? word-list))] (print-word-list word-list)))

