;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname final-tania&anaya-hw8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction

;; In this assignment, you will continue working on Spelling Bee (Homework 3).
;; You will rely on several concepts and techniques that you've learned since
;; Homework 3, to build a version of Spelling Bee that is significantly closer
;; to the real game. In particular, you will:
;;
;; 1. Move to a seven-letter Spelling Bee, instead of a five-letter game,
;;
;; 2. Implement scoring,
;;
;; 3. Support the backspace / delete key, so that players can correct their
;;    word, and
;;
;; 4. Check that the entered word is in a dictionary.

;; NOTE #1: Follow the "no halloween colors" and 2+ check-expects rule for
;; all function designs.
;;
;; NOTE #2: In the original Spelling Bee, we restricted you from using certain
;; functions. For this assignment, the only restricted functions are those
;; in the class style guide.
;;
;; NOTE #3: Despite having fewer restrictions, we still expect good program
;; design. For example, lists are the appropiate type of data to represent
;; scored words and available letters, and this assignment asks you to update
;; your program to use lists. It is possible to immediately convert a 
;; [List-of 1String] into a String. But, that is not the approach we want you
;; to take. We want you to use list abstractions when possible.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 0: Meet With Your Partner

;; You must do this assignment with your assigned partner. However, since you
;; did the previous stage alone, that means you have multiple implementations to
;; Spelling Bee to use as a starting point. Which one will you use? Making that
;; decision is part of the assignment.
;;
;; Note: If neither you nor your partner did well on HW3, we strongly encourage
;; you to use our HW3 sample solution as a starting point.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 1: Introducing Lists
;;
;; When we worked on HW3, we did not know about lists, which are fundamental to
;; good program design. Instead, we played tricks with strings, such as using
;; newlines to separate found words. Now that we are familiar with lists, we
;; are going to modify Spelling Bee to use them in two places:
;;
;; a. Revise your data definition for Letters to either:
;;
;;    - Represent the available letters as an [NE-List-of 1String], or
;;
;;    - Represent the non-center letters as a [List-of 1String]
;;
;; b. Revise your World data definition to represent the list of words found
;;    as a [List-of String]. Thus you should no longer use "\n" in your
;;    examples of World.
;;

;; [TODO] Revise World and Letters as described above.

;; A Letters is an [NE-List-of 1String]
;; Interpretation: Represents all the available letters where the first letter is the required letter

(define LETTERS-1-REVISED (list "d" "a" "y" "t" "e"))
(define LETTERS-2-REVISED (list "c" "l" "u" "e" "b"))

;; letters-temp : Letters -> ?
(define (letters-temp letters)
  (cond
    [(empty? (rest letters)) (... (first letters) ...)]
    [(cons? (rest letters))
     (... (first letters)
          ... (letters-temp (rest letters)))]))

(define-struct world [available-letters typed-word words-entered])
;; A World is a (make-world Letters String List-of String)
;; available-letters: is the available letters
;; typed-word: is he partial words that the user has currently entered
;; words-entered: is a track of all the words a user has entered
;; Interpretation: tracking of what words are in use
;; and are available for making a full seven letter word

(define WORLD-1 (make-world LETTERS-1-REVISED "dat" (list "Current Words Entered:")))
(define WORLD-1-A (make-world LETTERS-1-REVISED "day" (list "date" "Current Words Entered:")))

(define WORLD-2 (make-world LETTERS-2-REVISED "clu" (list "Current Words entered:")))
(define WORLD-2-A (make-world LETTERS-2-REVISED "club" (list "clue" "Current Words Entered:")))

;; world-temp: World -> ?
(define (world-temp w)
  (... (letters-temp (world-available-letters w)) ...
       (world-typed-word w) ...
       (list-temp (world-words-entered w)) ...))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(
;; Step 2: More Letters
;;
;; The update that you made in Step 1 should allow your program to support any
;; number of available letters (though you need at least one letter at the
;; center). However, your old examples use exactly five letters.
;;
;; 1. Construct new examples and check-expects that have varying numbers of
;;    available letters.
;;
;;  2. Modify letters->image so that it either:
;;
;;     - Assumes that there are exactly seven letters (i.e., as in real
;;       Spelling Bee), or
;;
;;     - Supports any number of available letters (this is not required)

;; [TODO] New examples and check-expects with varying numbers of available
;; letters.

(define LETTERS-A (list "C" "A" "E" "G" "K" "M" "O"))
(define LETTERS-B (list "D" "A" "C" "I" "N" "O" "Y"))
(define LETTERS-C (list "W" "A" "D" "E" "T" "X" "Z"))
(define LETTERS-D (list "C" "Q" "U" "E" "N" "K"))

(define WORLD-A (make-world LETTERS-A "" '()))
(define WORLD-A-1 (make-world LETTERS-A "gake" '()))
(define WORLD-A-2 (make-world LETTERS-A "cake" '()))

(define WORLD-B (make-world LETTERS-B "" '()))


;; [TODO] Update letters->image to display seven letters (or optionally,
;; any number of letters)

(define BG-WIDTH 400)
(define BG-HEIGHT 400)
(define BG-SCENE (rectangle BG-WIDTH BG-HEIGHT "solid" "black"))
(define LETTER-SIZE 30)
(define MAIN-LETTER-COLOR "black")
(define LETTER-COLOR "white")
(define CIRCLE (circle 50 "solid" "purple"))
(define MAIN-CIRCLE (circle 50 "solid" "yellow"))

;; signature: letters->image : Letters -> Image
;; purpose statement: draws the given letters

;; check-expects:
(check-expect (letters->image LETTERS-A)
              (overlay (overlay (text (first LETTERS-A) LETTER-SIZE MAIN-LETTER-COLOR) MAIN-CIRCLE)
                       (draw-letters-remaining (rest LETTERS-A))))

(check-expect (letters->image LETTERS-B)
              (overlay (overlay (text (first LETTERS-B) LETTER-SIZE MAIN-LETTER-COLOR) MAIN-CIRCLE)
                       (draw-letters-remaining (rest LETTERS-B))))

(check-expect (letters->image LETTERS-C)
              (overlay (overlay (text (first LETTERS-C) LETTER-SIZE MAIN-LETTER-COLOR) MAIN-CIRCLE)
                       (draw-letters-remaining (rest LETTERS-C))))

;; function: 
(define (letters->image letters)
  (overlay (overlay (text (first letters) LETTER-SIZE MAIN-LETTER-COLOR) MAIN-CIRCLE)
           (draw-letters-remaining (rest letters))))

;; signature: draw-letters-remaining : [List-of 1String] -> Image
;; purpose statement: draws the given remaining letters around the main required letter

;; check-expects: 
(check-expect (draw-letters-remaining (rest LETTERS-A))
              (overlay/offset
               (overlay (text "A" LETTER-SIZE LETTER-COLOR) CIRCLE) -90 52
               (overlay/offset
                (overlay (text "E" LETTER-SIZE LETTER-COLOR) CIRCLE) -90 -52
                (overlay/offset
                 (overlay (text "G" LETTER-SIZE LETTER-COLOR) CIRCLE) 0 -105
                 (overlay/offset
                  (overlay (text "K" LETTER-SIZE LETTER-COLOR) CIRCLE) 90 -52
                  (overlay/offset
                   (overlay (text "M" LETTER-SIZE LETTER-COLOR) CIRCLE) 90 52
                   (overlay/offset
                    (overlay (text "O" LETTER-SIZE LETTER-COLOR) CIRCLE) 0 105
                    BG-SCENE)))))))


(check-expect (draw-letters-remaining (rest LETTERS-B))
              (overlay/offset
               (overlay (text "A" LETTER-SIZE LETTER-COLOR) CIRCLE) -90 52
               (overlay/offset
                (overlay (text "C" LETTER-SIZE LETTER-COLOR) CIRCLE) -90 -52
                (overlay/offset
                 (overlay (text "I" LETTER-SIZE LETTER-COLOR) CIRCLE) 0 -105
                 (overlay/offset
                  (overlay (text "N" LETTER-SIZE LETTER-COLOR) CIRCLE) 90 -52
                  (overlay/offset
                   (overlay (text "O" LETTER-SIZE LETTER-COLOR) CIRCLE) 90 52
                   (overlay/offset
                    (overlay (text "Y" LETTER-SIZE LETTER-COLOR) CIRCLE) 0 105
                    BG-SCENE)))))))

(check-expect (draw-letters-remaining (rest LETTERS-C))
              (overlay/offset
               (overlay (text "A" LETTER-SIZE LETTER-COLOR) CIRCLE) -90 52
               (overlay/offset
                (overlay (text "D" LETTER-SIZE LETTER-COLOR) CIRCLE) -90 -52
                (overlay/offset
                 (overlay (text "E" LETTER-SIZE LETTER-COLOR) CIRCLE) 0 -105
                 (overlay/offset
                  (overlay (text "T" LETTER-SIZE LETTER-COLOR) CIRCLE) 90 -52
                  (overlay/offset
                   (overlay (text "X" LETTER-SIZE LETTER-COLOR) CIRCLE) 90 52
                   (overlay/offset
                    (overlay (text "Z" LETTER-SIZE LETTER-COLOR) CIRCLE) 0 105
                    BG-SCENE)))))))
;; function:
(define (draw-letters-remaining letters)
  (local [;; x-extract : Number -> Number
          ;; get the x-coor of the circle given the letter's position in the list
          (define (x-extract x)
            (cond
              [(or (= x 4) (= x 5)) -90]
              [(= x 3) 0]
              [(or (= x 2) (= x 1)) 90]
              [(= x 0) 0]))
          ;; y-extract : Number -> Number
          ;; get the y-coor of the circle given the letter's position in the list
          (define (y-extract x)
            (cond
              [(or (= x 1) (= x 5)) 52]
              [(= x 3) -105]
              [(or (= x 2) (= x 4)) -52]
              [(= x 0) 105]))]
    (cond
      [(empty? letters) BG-SCENE]
      [(cons? letters)
       (overlay/offset (overlay (text (first letters) LETTER-SIZE LETTER-COLOR) CIRCLE)
                       (x-extract (length (rest letters)))
                       (y-extract (length (rest letters)))
                       (draw-letters-remaining (rest letters)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 3: The Backspace/Delete Key
;;
;; Update your program so that when the player presses Backspace (Windows) or
;; Delete (Mac), the game clears the last letter that they entered. The special
;; string "\b" stands for delete/backspace.
;;
;; Note: Ensure your program is "well-behaved" when the player presses
;; delete/backspace and there are no characters to delete.

;; [TODO] Revise your program to support for backspace/delete.

(define LITTLE-DICTIONARY (list "icon" "acorn" "rain" "corn"))
(define DICTIONARY (read-words "words.txt"))

;; Main function
;; signature: key-pressed : World KeyEvent -> World
;; purpose statement: to update the world given the key press

;; check-expects:

(check-expect (key-pressed WORLD-A "a")
              (make-world (list "C" "A" "E" "G" "K" "M" "O") "a" '()))
(check-expect (key-pressed WORLD-A "z")
              (make-world (list "C" "A" "E" "G" "K" "M" "O") "" '()))

(check-expect (key-pressed WORLD-A-1 "\r") WORLD-A-1)
(check-expect (key-pressed WORLD-A "\b") WORLD-A)

(check-expect (key-pressed WORLD-A-2 "\r")
              (make-world (list "C" "A" "E" "G" "K" "M" "O") "" (list "cake")))

(check-expect (key-pressed WORLD-A-2 "\b")
              (make-world (list "C" "A" "E" "G" "K" "M" "O") "cak" '()))

(check-expect (key-pressed WORLD-B "d")
              (make-world (list "D" "A" "C" "I" "N" "O" "Y") "d" '()))

(check-expect (key-pressed WORLD-B "w")
              (make-world (list "D" "A" "C" "I" "N" "O" "Y") "" '()))

;; main function: 

(define (key-pressed w k)
  (local [; str-in-list? : String [List-of String] -> Boolean
          ; Purpose: is this string in the list?
          (define (str-in-list? str los)
            (ormap (λ (s) (string-ci=? s str)) los))
          
          ; contains-center? : Number String String -> Boolean
          ; Purpose: Does the string contain the given _char_
          (define (contains-center? n str char)
            (cond
              [(zero? n) #false]
              [(positive? n)
               (or (string-ci=? (substring str (sub1 n) n) char)
                   (contains-center? (sub1 n) str char))]))]
    
    (cond
      [(and (string=? k "\r")
            (contains-center? (string-length (world-typed-word w))
                              (world-typed-word w)
                              (first (world-available-letters w)))
            (>= (string-length (world-typed-word w)) 4)
            (not (str-in-list? (world-typed-word w)
                               (world-words-entered w)))
            (str-in-list? (world-typed-word w) DICTIONARY))
       (make-world (world-available-letters w)
                   ""
                   (cons (world-typed-word w) (world-words-entered w)))]
      [(string=? k "\b")
       (if (string=? (world-typed-word w) "")
           w
           (make-world (world-available-letters w)
                       (substring (world-typed-word w)
                                  0
                                  (sub1 (string-length (world-typed-word w))))
                       (world-words-entered w)))]
      [(str-in-list? k (world-available-letters w))
       (make-world (world-available-letters w)
                   (string-append (world-typed-word w) k)
                   (world-words-entered w))]
      [else w])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 4: More Checks: Duplicate Words, 4+ Letter-Words, Dictionary Word
;;
;; Revise your program to ensure that the word entered by the player is:
;; 1. An available letter (already done in HW3),
;; 2. Contains the center letter (already done in HW3),
;; 3. A dictionary word,
;; 4. At least four letters long, and
;; 5. Is not a duplicate of an already entered word.
;;
;; We've given you a file called words.txt, which you can use as a dictionary.
;; It is not a comprehensive dictionary, but has roughly 50,000 words. Every
;; line of the file is a word, and they are arranged in alphabetical order
;; (technically, lexicographic order), if W1 appears before W2 in the file,
;; then (string<? W1 W2) is true.
;;
;; Suggestion #1: you can use the read-lines function in the 2htdp/batch-io
;; library to read the dictionary to a constant.
;;
;; Suggestion #2: It is very difficult to work with a list of 50,000 words.
;; So, to get started we recommend defining a small dictionary of words, and
;; then replacing it the the code that reads words from the file. E.g.,

;; [TODO] Revise your program to implement the five checks above.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 5: Scoring a Game
;;
;; Finally, revise your program to display the current score. The score for
;; each word is:
;;
;; 1. One point for a four-letter word,
;;
;; 2. An additional point for every additional letter beyond the first four, and
;;
;; 3. An additional seven bonus points for using all seven letters.

;; [TODO] Revise your program to support scoring.


; signature: world->image : World -> Image
; purpose statement: to draw the game

;; check-expects:
(check-expect (world->image WORLD-A)
              (overlay/offset
               (text/font "score: 0" 30 "white" "Gill Sans" 'swiss 'normal 'normal #f)
               0 250
               (overlay/offset
                (text/font "words entered:\n" 30 "white" "Gill Sans" 'swiss 'normal 'normal #f)
                -200 0
                (overlay/offset (text/font "word: " 30 "white" "Gill Sans" 'swiss 'normal 'normal #f)
                                100 150
                                (overlay/offset
                                 (letters->image (world-available-letters WORLD-A))
                                 100 -100
                                 (rectangle 700 700 "solid" "black"))))))
(check-expect (world->image WORLD-B)
              (overlay/offset
               (text/font "score: 0" 30 "white" "Gill Sans" 'swiss 'normal 'normal #f)
               0 250
               (overlay/offset
                (text/font "words entered:\n" 30 "white" "Gill Sans" 'swiss 'normal 'normal #f)
                -200 0
                (overlay/offset (text/font "word: " 30 "white" "Gill Sans" 'swiss 'normal 'normal #f)
                                100 150
                                (overlay/offset
                                 (letters->image (world-available-letters WORLD-B))
                                 100 -100
                                 (rectangle 700 700 "solid" "black"))))))
(check-expect (world->image (make-world (list "C" "A" "E" "G" "K" "M" "O") "" '("cake")))
              (overlay/offset
               (text/font "score: 1" 30 "white" "Gill Sans" 'swiss 'normal 'normal #f)
               0 250
               (overlay/offset
                (text/font "words entered:\ncake\n\n" 30 "white" "Gill Sans"
                           'swiss 'normal 'normal #f)
                -200 0
                (overlay/offset (text/font "word: " 30 "white" "Gill Sans"
                                           'swiss 'normal 'normal #f)
                                100 150
                                (overlay/offset
                                 (letters->image (world-available-letters WORLD-A))
                                 100 -100
                                 (rectangle 700 700 "solid" "black"))))))
;; function:
(define (world->image w)
  (local [; words->string : [List-of String] -> String
          ; Purpose: to join each element of the list with a "\n"
          (define (words->string los)
            (foldr (λ (s so-far) (string-append s "\n" so-far)) "\n" los))

          ; draw-typed-word : String -> Image
          ; Purpose: to draw the typed word
          (define (draw-typed-word prefix word)
            (text/font (string-append prefix word) 30 "white" "Gill Sans" 'swiss 'normal 'normal #f))]
    (overlay/offset
     (draw-typed-word "score: " (number->string (get-score (world-words-entered w)
                                                           (world-available-letters w))))
     0 250
     (overlay/offset
      (draw-typed-word "words entered:\n" (words->string (world-words-entered w)))
      -200 0
      (overlay/offset
       (draw-typed-word "word: " (world-typed-word w))
       100 150
       (overlay/offset
        (letters->image (world-available-letters w))
        100 -100
        (rectangle 700 700 "solid" "black")))))))


; helper function
; signature: get-score : [List-of String] -> Number
; purpose statement: to get the scores of the words entered

;; check-expects:

(check-expect (get-score (list "cage") LETTERS-A) 1)
(check-expect (get-score (list "gamecock") LETTERS-A) 12)

(check-expect (get-score (list "acid") LETTERS-B) 1)
(check-expect (get-score (list "acid" "cyanoid") LETTERS-B) 12)

(define (get-score los letters)
  (local [; get-score/word : String Number -> Number
          ; Purpose: to get and add the score of the given word
          (define (get-score/word str score)
            (+ (if (all-7-used? str)
                   (+ (- (string-length str) 3) 7)
                   (- (string-length str) 3))
               score))

          ; all-7-used? : String -> Boolean
          ; Purpose: have all 7 letters been used?
          (define (all-7-used? str)
            (andmap (λ (s) (contains-letter? (string-length str) str s)) letters))

          ; contains-letter? : Number String String -> Boolean
          ; Purpose: Does the string contain the given _char_
          (define (contains-letter? n str char)
            (cond
              [(zero? n) #false]
              [(positive? n)
               (or (string-ci=? (substring str (sub1 n) n) char)
                   (contains-letter? (sub1 n) str char))]))]
    
    (foldr get-score/word 0 los)))

; big-bang function

(define (spelling-bee w)
  (big-bang w
    [to-draw world->image]
    [on-key key-pressed]))

(spelling-bee WORLD-A)
