;; -*- coding: utf-8; -*-
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.

((structure
  ("binder.el"
   (status . "alpha")
   (notes . "Seriously though, am I even gonna use this?"))
  ("README.md"
   (status . "todo"))
  ("incident.txt")
  ("foo.txt"
   (status . "firstdraft")
   (include . t)
   (display . "beginning"))
  ("bar.txt"
   (status . "revised")
   (include . t)
   (display . "middle"))
  ("baz.txt"
   (notes . "You can have flaws, but wow em in the end and you've got a hit.")
   (status . "firstdraft")
   (include . t)
   (display . "end")))
 (default-mode . text-mode)
 (default-extension . "txt"))
