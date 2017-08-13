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
  (multiple-value-bind (result processed)
      (cl-ppcre:regex-replace-all
       regex  string
       (lambda (string start end ms me rs re)
	 (declare (ignore start end ms me))
	 (let* ((maybe-key (string-upcase
			    (subseq string (aref rs 0) (aref re 0))) )
		(key (find-symbol maybe-key "KEYWORD")))
	   (or (gethash key *params*)
	       (error "process-string: key ~S is not provided" maybe-key)))))
    (if processed
	(process-string result regex)
	result)))



(defun process-file (srcpath destpath)
  (with-open-file (in srcpath)
    ;; read entire file into a string
    (let ((string (make-string (file-length in))))
      (read-sequence string in)
      (setf string (process-string string))
      (with-open-file (out destpath :direction :output :if-exists :supersede)
	(write-sequence string out)))))


(defun process-files ()
  (let ((filename-regex (gethash :REGEX-FILENAME *params*))
	(src-path (gethash :TEMPLATE-PATH *params*))
	(dest-path (gethash :OUTPUT-PATH *params*)))
    (loop for fullpath in (uiop:directory-files src-path)
       for enoughpath = (process-string
			 (namestring (uiop:enough-pathname fullpath src-path))
			 filename-regex )
       do
	 (case (filename-action enoughpath);; todo: subdirectories
	   (:copy (copy-file fullpath (merge-pathnames enoughpath dest-path )))
	   (:process (process-file
		      fullpath (merge-pathnames enoughpath dest-path)))
	   (t ())))))

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

  (process-files))
