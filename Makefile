make-executable:
	echo '#!/bin/sbcl --script' | cat - ploy.lisp > ploy
	echo '(main)' >> ploy
	chmod +x ploy
