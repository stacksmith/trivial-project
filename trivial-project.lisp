(in-package #:tp)

;;==============================================================================
;;
;;
(defun local-template ()
  (let ((local (uiop:ensure-directory-pathname
		(merge-pathnames "trivial-project-template" (user-homedir-pathname)))))
    (if (probe-file local)
	local
	(progn
	  (warn
	   "Using a lame default template.  For full effect, create a custom template directory called \"~A\"~%You can start with a copy of the default template directory:~%(ql:quickload :copy-directory)~%(copy-directory:copy ~S ~S)~%"
	   local
	   (uiop:ensure-directory-pathname
	    (asdf:system-relative-pathname 'trivial-project "template"))
	   local)
	  nil))))

(defparameter *default-template-path* nil)

;;
;; Substitute all keys in the form of --KEY-- with a value obtained
;; with KEY from alist params.  Continue until there is nothing left,
;; returning the final string, or error.
;;
(defun process-string (string params &optional (regex "--(.*?)--"))
  (multiple-value-bind (result processed)
      (cl-ppcre:regex-replace-all
       regex  string
       (lambda (string start end ms me rs re)
	 (declare (ignore start end ms me))
	 (let* ((maybe-key (string-upcase
			    (subseq string (aref rs 0) (aref re 0))) )
		(key (find-symbol maybe-key "KEYWORD")))
	   (or (and key
		    (getf params key))
	       (error "process-string: key ~S is not provided" maybe-key)))))
    (if processed
	(process-string result params regex)
	result)))

(defun process-file (srcpath destpath params)
  (with-open-file (in srcpath)
    ;; read entire file into a string
    (let ((string (make-string (file-length in))))
      (read-sequence string in)
      (setf string (process-string string params))
      (with-open-file (out destpath :direction :output :if-exists :supersede)
	(write-sequence string out)))))


(defun process-files (src-path dest-path params)
  (loop for fullpath in (uiop:directory-files src-path)
     for enoughpath = (process-string
		       (namestring (uiop:enough-pathname fullpath src-path))
		       params "TP_(.*?)_TP")
     for ext = (string-downcase (pathname-type fullpath))
     for action = (cond
		    ((char= #\. (char ext 0)) :ignore)
		    ((char= #\~ (char ext (1- (length ext)))) :ignore)
		    ((string= ext "lisp") :process)
		    ((string= ext "asd") :process)
		    ((string= ext "md") :process)
		    ((string= ext "txt") :process)
		    (t :copy))
     do
       
  ;;       (format t "~A.~A: ~A~%" (pathname-name file) ext action )
     ;;     (print enoughpath) (print fullpath)
;;       (format t "...~A~%" enoughpath)
       (case action
	 (:copy (copy-file fullpath (merge-pathnames enoughpath dest-path )))
	 (:process (process-file
		    fullpath (merge-pathnames enoughpath dest-path)
		    params))
	 (t ()))))

;;=========================================================================
(defun initial-keys (params)
  (let ((name (getf params :NAME)))
    (setf (getf params :SYSTEM) name
	  (getf params :PACKAGE) name))
  params
  )

;;=========================================================================

(defun make-project (&rest params &key NAME &allow-other-keys)
  "Create a project named (name must be a string) based on a template and other keys."
  (unless (and name (stringp name))
    (error ":NAME parameter is required to be a string"))
  (setf params (initial-keys params))
  
  (let* ((template-path
	  (uiop:ensure-directory-pathname
	   (or (getf params :TEMPLATE-PATH)
	       (local-template)
	       (asdf:system-relative-pathname 'trivial-project "template"))))
	 (project-path
	  (ensure-directories-exist
	   (uiop:ensure-directory-pathname
	    (merge-pathnames
	     name
	     (or (getf params :OUTPUT-PATH)
		 (first quicklisp:*local-project-directories*)))))))
    (when-let
	((local-params
	  (with-open-file (config (merge-pathnames ".local.txt" template-path))
	    (read config))))
      (setf params (append params local-params)))

    (setf params  (configure-keys params))

    (process-files template-path project-path params))  

)
