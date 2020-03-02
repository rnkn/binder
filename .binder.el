;; -*- coding: utf-8; -*-
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.

((structure
  ("binder.el"
   (status . "alpha")
   (notes . "Seriously though, am I even gonna use this?"))
  ("README.md"
   (include)
   (status . "todo"))
  ("incident.txt"
   (include)
   (notes . "Story-writing books are particularly concerned with an \"inciting incident\" so make sure yours has one!"))
  ("foo.txt"
   (status . "firstdraft")
   (include . t)
   (display . "beginning"))
  ("bar.txt"
   (status . "revised")
   (include . t)
   (display . "middle"))
  ("baz.txt"
   (notes . "You can have flaws, but wow 'em in the end and you've got a hit.")
   (include)
   (display . "end"))
  ("akai-ryu.jpg"
   (include)
   (status . "image")
   (notes . "Dionaea muscipula \"Akai Ryu\", Japanese for \"Red Dragon\"\n~ Wikipedia")))
 (default-mode . text-mode)
 (default-extension . "txt"))
