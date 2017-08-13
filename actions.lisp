(in-package :tp)
;;
;; Handling extensions based on the :EXTENSIONS key that contains
;; a :KEY value PLIST mapping extension types to action.  Action
;; is one of the following:
;; :copy    -- copy file verbatim
;; :process -- expand in place
;; :ignore  -- do nothing
;;
(defparameter *extensions* (make-hash-table :test 'equal))

;;----------------------------------------------------------------------------
;;
(defun extensions-initialize ()
  (clrhash *extensions*)
  (when-let ((extensions (gethash :EXTENSIONS *PARAMS*)))
    (loop for (key value) on extensions by #'cddr
       do (setf  (gethash key *extensions*) value))))

(defparameter *files* (make-hash-table :test 'equal))

(defun files-initialize ()
  (clrhash *files*)
  (when-let ((files (gethash :FILES *PARAMS*)))
    (loop for (key value) on files by #'cddr
       do (setf  (gethash key *files*) value))))


;;----------------------------------------------------------------------------
;;
;; Figure out action for a file by its extension
;;
(defun filename-action (enoughpath)
  (or
   ;; Is the file specified explicitly?
   (gethash enoughpath *files*)
   ;; Is the file extension specified?
   (gethash (pathname-type enoughpath) *extensions*)
   ;; default action
   (getf *params* :DEFAULT-ACTION) ))



