;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20250105.2230"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "6afd86bbc740f3008e91881f990bf346b31d3f0b"
  :revdesc "6afd86bbc740"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))
