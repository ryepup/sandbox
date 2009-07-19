;;;; holly.lisp

(require 'cl-irc)
(require 'iterate)
(require 'cl-ppcre)
(require 'drakma)

(defpackage #:holly
  (:use #:cl #:iterate))

(in-package #:holly)

(defvar *connection* nil)

(defun main ()
  (let ((*connection* (irc:connect :nickname "home7"
				   :server "irc.freenode.com")))
    ;;    (irc:add-hook *connection*  )
    (irc:join *connection* "#ryepup")
    (irc:add-hook *connection*
		  'irc:irc-privmsg-message
		  #'message)
    (irc:read-message-loop *connection*)))

(defvar *handlers* nil)

(defun message (message)
  (iter (for handler in *handlers*)
	(until (ignore-errors (handle handler message)))))

(defgeneric handle (handler message)
  (:method (handler message)
    (destructuring-bind (channel text)
	(irc:arguments message)
      (irc:privmsg *connection* channel
		   (format nil
			   "/me shrugs: I don't know what to do with '~a'"
			   text)))))

(defun reply-to (message formatstring &rest args)
  (destructuring-bind (channel text) (irc:arguments message)
    (declare (ignore text))
    (irc:privmsg *connection* channel
		 (format nil "~a: ~?" (irc:source message)
			 formatstring args
			 ))
    
    )
  )

(defclass irc-handler ()
  ()
  (:documentation "default do-nothing handler"))

(push (make-instance 'irc-handler) *handlers*)

(defclass regex-handler (irc-handler)
  ((regex :accessor regex
	  :initform nil
	  :initarg :regex))
  (:documentation "matches a regex"))

(defmethod handle :around ((handler regex-handler) message)
  (destructuring-bind (channel text) (irc:arguments message)
    (declare (ignore channel))
    (when (ppcre:scan (regex handler) text)
      (call-next-method))))

(defclass help (regex-handler)
  ()
  (:default-initargs
      :regex "help")
  (:documentation "help - Lists all the actions we can take."))

(defmethod handle ((handler help) message) 
  (iter (for h in *handlers*)
	(reply-to message		  
		  (documentation (type-of h) 'type)))
  T)

(push (make-instance 'help) *handlers*)

(defclass downloader (regex-handler)
  ()
  (:documentation "dl [uri] - downloads the given thing to the local store")
  (:default-initargs
      :regex "dl\\s.+"))

(defmethod handle ((handler downloader) message)
  (destructuring-bind (channel text) (irc:arguments message)
    (declare (ignore channel))
    (cl-ppcre:register-groups-bind
     (uri) ("dl\\s(.+)" text)
     (reply-to message "downloading ~a" uri)
     
     (let ((filename (format nil
			     "/mnt/guardo/irc/~a"
			     (file-namestring uri))))
       (with-open-file (dst filename
			    :direction :output
			    :element-type 'unsigned-byte
			    :if-does-not-exist :create
			    :if-exists :error )
	 (write-sequence (drakma:http-request uri)
			 dst))
       (reply-to message "~a downloaded to ~a" uri filename)))))

(push (make-instance 'downloader) *handlers*)