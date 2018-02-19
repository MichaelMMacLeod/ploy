(defun ensure-config-exists ()
  "Creates config directories if they don't already exist"
  (ensure-directories-exist "~/.config/ploy/")
  (ensure-directories-exist "~/.config/ploy/schemes/"))

(defvar *scheme*)

(defun write-scheme (scheme)
  (let ((file (format nil "~~/.config/ploy/schemes/~a" scheme)))
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (print *scheme* out)))))

(defun -a (scheme) 
  "Adds a new scheme"
  (setf *scheme* (make-hash-table :test 'equal))
  (write-scheme scheme))

(defun -l () 
  "Lists all schemes"
  (let ((path (format nil "~a/.config/ploy/schemes/"
                      (posix-getenv "HOME"))))
    (run-program "/bin/ls" (list path) ; sorry
                 :output *standard-output*)))

(defun -r (scheme)
  "Removes a scheme"
  (let ((file (format nil "~~/.config/ploy/schemes/~a" scheme)))
    (delete-file file)))

(defun load-scheme (scheme)
  "Loads a hashmap from ~/.config/ploy/schemes/scheme into *scheme*"
  (let ((file (format nil "~~/.config/ploy/schemes/~a" scheme)))
    (with-open-file (in file)
      (with-standard-io-syntax
        (setf *scheme* (read in))))))

(defun load-file (file)
  "Returns the contents of a file as a string"
  (with-open-file (in file)
    (let ((data (make-string (file-length in))))
      (read-sequence data in)
      data)))

(defun format-as-xresources ()
  "Returns *scheme* formatted as xresources' *.colorname: #hex pairs"
  (with-output-to-string (s)
    (maphash 
      #'(lambda (k v) (format s "*.~a: #~6,'0x~%" k v))
      *scheme*)
    (format nil "~a" s)))

(defun -s (scheme)
  "Selects a scheme"
  (load-scheme scheme)
  (let ((selected-path 
          (format nil "~a/.config/ploy/selected-scheme.Xresources"
                  (posix-getenv "HOME")))
        (xresources (load-file "~/.Xresources")))
    (with-open-file (out selected-path
                         :direction :output
                         :if-exists :supersede)
      (format out "~a~%~a" 
              xresources 
              (format-as-xresources)))
    (run-program "/bin/xrdb" (list selected-path))))

(defun SCHEME-c (scheme color)
  "Displays a color from a scheme"
  (load-scheme scheme)
  (format t "~a: #~6,'0x~%" color (gethash color *scheme*)))

(defun SCHEME-ca (scheme color hex)
  "Adds a color to a scheme"
  (load-scheme scheme)
  (let ((value (parse-integer hex :radix 16)))
    (setf (gethash color *scheme*) value))
  (write-scheme scheme))

(defun SCHEME-cr (scheme color)
  "Removes a color from a scheme"
  (load-scheme scheme)
  (remhash color *scheme*)
  (write-scheme scheme))

(defun SCHEME-cl (scheme)
  "Lists colors in a scheme"
  (load-scheme scheme)
  (maphash
    #'(lambda (k v) (format t "~a #~6,'0x~%" k v))
    *scheme*))

(defun -h ()
  "Displays command help"
  (format t
"Usage: ploy scheme-option [arg ..]
       ploy SCHEME color-option [arg ..]

Create and modify color schemes.

scheme-option [-a|-r|-l|-s]:
  -a SCHEME      Adds a new scheme named SCHEME.
  -r SCHEME      Removes the scheme SCHEME.
  -l             Lists all schemes.
  -s SCHEME      Selects the scheme SCHEME.

color-option [-c|-ca|-cr|-cl]:
  -c COLOR       Displays information about the color COLOR in the 
                 scheme SCHEME.
  -ca COLOR HEX  Adds a color with the name COLOR and the hexidecimal 
                 value HEX to the scheme SCHEME.
  -cr COLOR      Removes the color COLOR from the scheme SCHEME.
  -cl            Lists the colors in scheme SCHEME.
"))

(defun run-command-line-args ()
  "Looks at command line args and chooses which functions to then call"
  (let* ((args (cdr *posix-argv*))
         (arg1 (car args))
         (arg2 (car (cdr args)))
         (arg3 (car (cddr args)))
         (arg4 (car (cdddr args))))
    (cond ((string= arg1 "-a") (-a arg2))
          ((string= arg1 "-l") (-l))
          ((string= arg1 "-r") (-r arg2))
          ((string= arg1 "-s") (-s arg2))
          ((string= arg2 "-c") (SCHEME-c arg1 arg3))
          ((string= arg2 "-ca") (SCHEME-ca arg1 arg3 arg4))
          ((string= arg2 "-cr") (SCHEME-cr arg1 arg3))
          ((string= arg2 "-cl") (SCHEME-cl arg1))
          (t (-h)))))

(defun main ()
  (ensure-config-exists)
  (run-command-line-args))
