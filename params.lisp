(in-package #:tp)


(defparameter *params* nil)
;;------------------------------------------------------------------------------
;; Output path must be either explictly set with :OUTPUT-PATH keyword,
;; or set from quicklisp
(defparameter *output-path* nil)
#+:quicklisp
(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf *output-path* (first quicklisp:*local-project-directories*)))
;;------------------------------------------------------------------------------
;; get the project template; if original, warn and advise user
(defun local-template ()
  (let ((local (uiop:ensure-directory-pathname
		(merge-pathnames "trivial-project-template"
				 (user-homedir-pathname)))))
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
(defun set-params-from-list (list)
  "set (overwriting) *params* from plist-like list"
  (loop for (key value) on list by #'cddr
     do (setf (gethash key *params*) value)))
;;==============================================================================
(defun set-params-from-file (path &key (in ""))
  (set-params-from-list
   (with-open-file (config (merge-pathnames path in))
     (read config))))
;;==============================================================================
;;
;; initial-keys
;;
;; Set up initial keys starting with TEMPLATE-PATH and OUTPUT-PATH, which may
;; involve some guesswork...  Then, populate with data from local initialization
;; file.  Finally, use invocation keyword parameters to maybe override defaults.
;;
(defun initial-keys (params)
  (setf *params* (make-hash-table :test 'equal))
  ;; set :TEMPLATE-PATH and :OUTPUT-PATH
  (let* ((name (getf params :NAME)) ;required
	 ;; We need template path to get local configuration...
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
		 *output-path*))))))
    (setf (gethash :TEMPLATE-PATH *params*) (uiop:truename* template-path)
	  (gethash :OUTPUT-PATH   *params*) output-path) 

    ;;--------------------------------------------------------------
    ;; attempt to read user's template parameters
    (set-params-from-file
     (or (getf params :TP-LOCAL-CONFIG-FILENAME)
	 ".local.tp") :in  template-path)
    ;;--------------------------------------------------------------
    ;; attempt to read user's global parameters from user's homedir;
    ;; these override the local parameters.  Mostly, to set the
    ;; AUTHOR field, and any 'global' fields, while allowing the
    ;; local configuration to provide defaults initially...
    (set-params-from-file
     ".trivial-project-global.tp" :in  (user-homedir-pathname))
    
    ;;--------------------------------------------------------------
    ;; Populate with invocation parameters, overriding defaults...
    (set-params-from-list params)
))


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

;; For MANIFEST declared file database:
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





