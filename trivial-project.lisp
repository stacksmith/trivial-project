(in-package #:tp)

(defparameter *params* nil)
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
	   "~%=====================================================================~%Using a lame default template.  For full effect, create a custom 
template directory called \"~A\"~%~%You can start with a copy of the default template directory:~%~%(ql:quickload :copy-directory)~%(copy-directory:copy ~S ~S)~%=====================================================================~%"
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
(defun process-string (string  &optional (regex (getf *params* :REGEX-NORMAL)))
  (multiple-value-bind (result processed)
      (cl-ppcre:regex-replace-all
       regex  string
       (lambda (string start end ms me rs re)
	 (declare (ignore start end ms me))
	 (let* ((maybe-key (string-upcase
			    (subseq string (aref rs 0) (aref re 0))) )
		(key (find-symbol maybe-key "KEYWORD")))
	   (or (getf *params* key)
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


(defun process-files (src-path dest-path )
  (let ((filename-regex (getf *params* :REGEX-FILENAME)))
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
(defun initial-keys (params)
  (let* ( (name (getf params :NAME)))
    (setf (getf params :SYSTEM) name
	  (getf params :PACKAGE) name
	  (getf params :DEFAULT-ACTION) :COPY
	  (getf params :REGEX-FILENAME) "TP_(.*?)_TP"
	  (getf params :REGEX-NORMAL) "--(.*?)--"
	  (getf params :DESCRIPTION) "set me!"
	  (getf params :AUTHOR) "set me!"
	  (getf params :LICENSE) "set me!"
	  (getf params :DEPENDS-ON) "" )
    )
  params)

;;==============================================================================
;; 
;;
(defun make-project (&rest params &key NAME &allow-other-keys)
  "Create a project named (name must be a string) based on a template and other keys."
  (unless (and name (stringp name))
    (error ":NAME parameter is required to be a string"))
  (setf *params* (initial-keys params))
  
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
	  (with-open-file (config (merge-pathnames ".local.tp" template-path))
	    (read config))))
      (setf *params* (append *params* local-params)))
    ;;
    (setf params  (initial-keys params))
    ;; initialize *extensions* from :EXTENSIONS
    (extensions-initialize params)
    ;; initialize default file action list from :FILES
    (process-files template-path project-path )))
