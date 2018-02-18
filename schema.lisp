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

(defun load-schema (schema)
  (let ((file
          (format nil "~a/.config/schema/~a"
                  (posix-getenv "HOME")
                  schema)))
    (with-open-file (in file)
      (with-standard-io-syntax
        (setf *schema* (read in))))))

(defun write-schema (schema)
  (let ((file
          (format nil "~a/.config/schema/~a"
                  (posix-getenv "HOME")
                  schema)))
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (print *schema* out)))))

;;; File IO

(defun load-file (file)
  (with-open-file (in file)
    (let ((data (make-string (file-length in))))
      (read-sequence data in)
      data)))

(defun ensure-config-exists ()
  (let ((path (format nil "~a/.config/schema/" (posix-getenv "HOME"))))
    (ensure-directories-exist path)))


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

(defun apply-schema (params)
  (load-schema (car params))
  (let* ((home (posix-getenv "HOME"))
         (xresources-part (load-file
                      (format nil "~a/.config/schema/part.xresources"
                              home)))
         (xresources-path
           (format nil "~a/.config/schema/.xresources" home)))
    (with-open-file (out xresources-path
                         :direction :output
                         :if-exists :supersede)
      (format out "~a~%~a"
              xresources-part
              (format-as-xresources)))
    (run-program "/bin/xrdb" (list xresources-path))))

(defun help ()
  (format t 
"Usage: schema [OPTION]

Create and modify color schemes

OPTION
    --help                          Display this message
    --create SCHEMA                 Create a new schema
    --add-color SCHEMA COLOR HEX    Add a color to a schema
    --remove-color SCHEMA COLOR     Remove a color from a schema
    --list-colors SCHEMA            List the colors in a schema
    --apply SCHEMA                  Apply a schema

Ex:
    $ schema --create my-schema
    $ schema --add-color my-schema foreground ffdba8
    $ schema --add-color my-schema background 000000
    $ schema --list-colors my-schema
    foreground #FFDBA8
    background #000000
    $ schema --remove-color my-schema background
    $ schema --list-colors my-schema
    foreground #FFDBA8
    $ schema --apply my-schema
"))

;;; Entry point

(defun main ()
  (ensure-config-exists)
  (let* ((args (cdr *posix-argv*))
         (cmd (car args))
         (params (cdr args)))
    (cond ((string= cmd "--add-color")
           (add-color-to params))
          ((string= cmd "--help")
           (help))
          ((string= cmd "--apply")
           (apply-schema params))
          ((string= cmd "--remove-color")
           (remove-color-from params))
          ((string= cmd "--list-colors")
           (list-colors-in params))
          ((string= cmd "--create")
           (create-schema params)))))

(main)
