(in-package #:tp)

(defparameter *params* (make-hash-table))
;;==============================================================================
;;
;;

;;
;; Substitute all keys in the form of --KEY-- with a value obtained
;; with KEY from alist params.  Continue until there is nothing left,
;; returning the final string, or error.
;;
(defun process-string (string  &optional (regex (gethash :REGEX-NORMAL *params*)))
  ;;(format t "~%1)~S" string)
  (multiple-value-bind (result processed)
      (cl-ppcre:regex-replace-all
       regex  string
       (lambda (string start end ms me rs re)
	 (declare (ignore start end ms me))
	 (let* ((maybe-key (string-upcase
			    (subseq string (aref rs 0) (aref re 0))) )
		(key (find-symbol maybe-key "KEYWORD")))
	   (multiple-value-bind (val exists) (gethash key *params*)
	     (unless exists
	       (error "process-string: key ~S is not provided" maybe-key))
	     val
	     ))))
    ;;(format t "~%2) ~A ~S" result processed)
    (if processed
	;; multiple expansion is always doen with normal regex!
	(process-string result (gethash :REGEX-NORMAL *params*))
	result)))



(defun process-file (srcpath destpath)
  (with-open-file (in srcpath)
    ;; read entire file into a string
    (let ((string (make-string (file-length in))))
      (read-sequence string in)
      (setf string (process-string string))
      (with-open-file (out destpath :direction :output :if-exists :supersede)
	(write-sequence string out)))))


(defun all-files-below (directory root)
  "collect all filenames at or below root, relative to root, as strings"
  ;; this primitive recursively builds a tree of all files
  ;; below directory
  (labels
      ((filetree (directory)
	 (append
	  (loop for subdirectory in (uiop:subdirectories directory)
	     collect (filetree subdirectory))
	  (loop for file in (uiop:directory-files directory)
	     collect file))))
    ;; now, remove the prefix leaving just the path below root...
    (mapcar (lambda (path)
	      (namestring (uiop:enough-pathname path root)))
	    (alexandria:flatten (filetree directory)))))


(defun process-files ()
  (let ((filename-regex (gethash :REGEX-FILENAME *params*))
	(src-path (gethash :TEMPLATE-PATH *params*))
	(dest-path (gethash :OUTPUT-PATH *params*)))

    (loop for subpath in (all-files-below src-path src-path)
       for new-subpath = (process-string subpath filename-regex )
       do
;;	 (print enoughpath)
	 (case (filename-action subpath);; todo: subdirectories
	   (:copy
	    (copy-file
	     (merge-pathnames subpath src-path)
	     (uiop::ensure-directories-exist
	      (merge-pathnames new-subpath dest-path ))))
	   (:process
	    (process-file
	     (merge-pathnames subpath src-path)
	     (uiop::ensure-directories-exist
	      (merge-pathnames new-subpath dest-path))))
	   (t ())))
    ;; TODO: pushnew seems to not recognize duplicates
    (if (gethash :TP-REGISTER-WITH-ASDF *params*)
	(pushnew (truename dest-path) asdf:*central-registry*))
    dest-path))

;;=========================================================================
;; 
;;
(defun make-project (&rest params &key NAME &allow-other-keys)
  "Create a project named (name must be a string) based on a template and other keys."
  (unless (and name (stringp name))
    (error ":NAME parameter is required to be a string"))
  (initial-keys params)
  (extensions-initialize)
  (files-initialize)
  (process-files)
  )



