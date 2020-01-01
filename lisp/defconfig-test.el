(defconfig (emacs)
  (print "for emacs"))

(defconfig (package)
  (print "for package"))

(defconfig (emacs package)
  (print "for both"))

(defconfig (ace-jump-mode)
  (print "bad"))

(require 'ace-jump-mode)

(defconfig|read-config '(ace-jump-mode)
  '((print "bad")))

(defconfig|read-config '(emacs) '((print "foo")))

(defconfig|read-config '(package) '((print "bar")))

(defconfig|read-config '(emacs package) '((print "baz")))
