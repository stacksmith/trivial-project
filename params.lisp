(in-package #:tp)

(defparameter *params* nil)

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

;;==============================================================================
(defun initial-keys (params)
  (setf *params* (make-hash-table :test 'equal))
  ;; set :TEMPLATE-PATH and :OUTPUT-PATH
  (let* ((name (getf params :NAME)) ;required
	 (template-path
	  (uiop:ensure-directory-pathname
	   (or (getf params :TEMPLATE-PATH)
	       (local-template)
	       (asdf:system-relative-pathname 'trivial-project "template"))))
	 (output-path
	  (ensure-directories-exist
	   (uiop:ensure-directory-pathname
	    (merge-pathnames
	     name
	     (or (getf params :OUTPUT-PATH)
		 (first quicklisp:*local-project-directories*)))))))
    (setf (gethash :TEMPLATE-PATH *params*) template-path
	  (gethash :OUTPUT-PATH   *params*) output-path
	  (gethash :SYSTEM *params*) name
	  (gethash :PACKAGE *params*) name)
    ;;--------------------------------------------------------------
    ;; attempt to read user's local files.
    (when-let
	((local
	  (with-open-file (config (merge-pathnames ".local.tp" template-path))
	    (read config))))
      (loop for (key value) on local by #'cddr
	 do (setf (gethash key *params*) value)))
    ;;--------------------------------------------------------------
    ;; Populate with invocation parameters, overriding defaults...
    (loop for (key value) on params by #'cddr
       do (setf (gethash key *params*) value))))


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
  (when-let ((files (gethash :MANIFEST *PARAMS*)))
    (loop for (key value) on files by #'cddr
       do (setf  (gethash key *files*) value))))


;;----------------------------------------------------------------------------
;;
;; Figure out action for a file by its extension
;;
(defun filename-action (enoughpath)
  (or
   (and (char= #\. (char enoughpath 0))
	:ignore)
   ;; Is the file specified explicitly?
   (gethash enoughpath *files*)
   ;; Is the file extension specified?
   (gethash (pathname-type enoughpath) *extensions*)
   ;; default action
   (gethash :DEFAULT-ACTION *params*)  ))





