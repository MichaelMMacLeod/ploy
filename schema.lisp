#!/bin/sbcl --script

;;; Color utils

(defun parse-hex (str)
  (parse-integer str :radix 16))

;;; Schema utils

(defvar *schema*)

(defun initialize-schema ()
  (setf *schema* (make-hash-table :test 'equal)))

(defun add-color (name hex)
  (setf (gethash name *schema*) hex))

(defun remove-color (name)
  (remhash name *schema*))

(defun get-color (name)
  (gethash name *schema*))

(defun format-as-human-readable ()
  (with-output-to-string (s)
    (maphash
      #'(lambda (k v) (format s "~a #~6,'0x~%" k v))
      *schema*)
    (format nil "~a" s)))

(defun format-as-xresources ()
  (with-output-to-string (s)
    (maphash 
      #'(lambda (k v) (format s "*.~a: #~6,'0x~%" k v))
      *schema*)
    (format nil "~a" s)))

;;; Color scheme IO

(defun load-schema (name)
  (with-open-file (in name)
    (with-standard-io-syntax
      (setf *schema* (read in)))))

(defun write-schema (file)
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *schema* out))))

;;; File IO

(defun load-file (file)
  (with-open-file (in file)
    (let ((data (make-string (file-length in))))
      (read-sequence data in)
      data)))

;;; Command line interface

(defun add-color-to (params)
  (let ((schema (car params))
        (name (car (cdr params)))
        (value (parse-hex (car (cddr params)))))
    (load-schema schema)
    (add-color name value)
    (write-schema schema)))

(defun remove-color-from (params)
  (let ((schema (car params))
        (name (car (cdr params))))
    (load-schema schema)
    (remove-color name)
    (write-schema schema)))

(defun list-colors-in (params)
  (let ((schema (car params)))
    (load-schema schema)
    (format t "~a" (format-as-human-readable))))

(defun create-schema (params)
  (let ((schema (car params)))
    (initialize-schema)
    (write-schema schema)))

;;; Entry point

(defun main ()
  (let* ((args (cdr *posix-argv*))
         (cmd (car args))
         (params (cdr args)))
    (cond ((string= cmd "add-color-to")
           (add-color-to params))
          ((string= cmd "remove-color-from")
           (remove-color-from params))
          ((string= cmd "list-colors-in")
           (list-colors-in params))
          ((string= cmd "create-schema")
           (create-schema params)))))

(main)
