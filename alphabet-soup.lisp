(defpackage alphabet-soup
  (:use :cl)
  (:export can-form-message-p))
(in-package :alphabet-soup)

(defun can-form-message-p (message soup)
  "Represents trying to form a message using letters from an alphabet soup.
   Checks whether soup contains the right letters and enough letters of
    each required kind to form the message. Examples:
    (can-form-message-p \"hi\" \"sqfisqfh\") will return T
    (can-form-message-p \"hello\" \"helojqsd\") will return NIL

   Assumes that both message and soup only contain lowercase letters (a-z).
   Also assumes that these letters are encoded so that for each letter 
    (- (char-code letter) (char-code #\a)) will return its position in the
    alphabet (0-25), which will be true when ASCII encoding is used, which
    is the case for standard strings on almost every platform, but there may
    be Common Lisp implementations that use another encoding. In such cases,
    the function alphabet-position would have to be adapted.

   The algorithm is as follows:
    It makes a vector of 26 0's, each representing the number of occurrences
    it already encountered for the letter at that location in the alphabet, so
    the first number is the number of a's it already encountered, the second is
    the number of b's, etc.
    It will then for every letter in the message do the following:
     Check whether the number in the vector is higher than 0.
     If it is, the number is decremented and the
     algorithm will move on to the next letter.
     If it isn't, the algorithm will search for the letter in the
     soup, starting from the position right after the end of the last search
     (so if the last search found the letter at position 5 it will start at
     position 6 this time). For all the letters it encounters that are not the
     letter it searches, it will increment the appropriate number in the vector.
     This way, it never has to traverse the same position in the soup twice, the
     information is stored in the vector. If the letter the algorithm was looking
     for is found in the soup, the algorithm will move on to the next letter in 
     the message. If it isn't and the end of the soup is reached, NIL is returned.
    If the end of the message is reached, T is returned.
   
   The complexity is O(s) with s the length of soup. This is because each
    character in the soup is only traversed at most once and if the end of the
    soup is reached, a value will be returned regardless of the position in the
    message. In each iteration of each loop, at least one character in the soup
    is traversed.
   In many cases, not even the whole soup will be traversed because the
    algorithm will also stop as soon as all the letters of the message have been
    found."
  (let ((take-from-soup-p (make-take-from-soup-p-closure soup)))
    (every take-from-soup-p message)))

(defun make-take-from-soup-p-closure (soup)
  "Helper function of can-form-message-p. Returns a closure that takes a letter
    from the soup, analogous to physically taking a letter out of an alphabet
    soup, and returns whether the letter was found.
   See the documentation of can-form-message-p for the algorithm. The closure
    represents what is executed for every letter in the message."
  (let* ((counts (make-array 26 :initial-element 0 :element-type 'integer))
	 (find-while-counting-p (make-find-while-counting-p-closure counts soup)))
    (lambda (char)
      (let ((char-count (elt counts (alphabet-position char))))
	(if (> char-count 0)
	    (progn (decf (elt counts (alphabet-position char)))
		   t)
	    (funcall find-while-counting-p char))))))

(defun make-find-while-counting-p-closure (counts soup)
  "Helper function of make-take-from-soup-p-closure. Returns a closure that
    searches the soup for a given letter while incrementing the appropriate
    number in counts for every letter it encounters in the soup that is not
    the letter that has to be found.
   See the documentation of can-form-message-p for the algorithm."
  (let ((index 0))
    (lambda (char)
      (loop while (and
		   (< index (length soup))
		   (char/= (char soup index) char))
	 do (incf (elt counts (alphabet-position (char soup index))))
	   (incf index))
      (if (< index (length soup))
	  (progn (incf index)
		 t)
	  nil))))

(defun alphabet-position (letter)
  "Gets the position in the alphabet (0-25) for a lowercase letter (a-z)"
  (- (char-code letter) (char-code #\a)))
