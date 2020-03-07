;; -*- coding: utf-8; -*-
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.

((structure
  ("binder.el"
   (status . "alpha"))
  ("README.md"
   (include)
   (status . "todo"))
  ("start-here.txt"
   (notes . "This file is going by a pseudonym")
   (status . "start-here")
   (include)
   (display . "foo"))
  ("sidebar.txt"
   (include . t))
  ("navigating.txt"
   (include . t))
  ("stapling.txt"
   (include . t))
  ("stapling-2.txt"
   (display . "editing stapled stuff")
   (include))
  ("notes.txt"
   (notes . "Some notes about notes.\n\nNullam eu ante vel est convallis dignissim. Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio. Nunc porta vulputate tellus. Nunc rutrum turpis sed pede. Sed bibendum. Aliquam posuere. Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio."))
  ("more-notes.txt"
   (notes . "Even more notes.\n\nEmbiggen this window with C-c C-l."))
  ("akai-ryu.jpg"
   (include)
   (status . "image")
   (notes . "The binder is not just for writing, but references too! Here's an image*\n\nDionaea muscipula \"Akai Ryu\", Japanese for \"Red Dragon\"\n~ Wikipedia\n\n* your Emacs may not support images :("))
  ("dog.txt"
   (notes . "ASCII art byt jgs"))
  ("end.txt"
   (display . "The End")))
 (default-mode . text-mode)
 (default-extension . "txt"))
