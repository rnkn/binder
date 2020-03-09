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
   (display . "tutorial/foo"))
  ("tutorial/sidebar.txt"
   (include . t))
  ("tutorial/navigating.txt"
   (include . t))
  ("tutorial/stapling.txt"
   (include . t))
  ("tutorial/stapling-2.txt"
   (display . "tutorial/editing stapled stuff")
   (include))
  ("tutorial/notes.txt"
   (notes . "Some notes about notes.\n\nNullam eu ante vel est convallis dignissim. Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio. Nunc porta vulputate tellus. Nunc rutrum turpis sed pede. Sed bibendum. Aliquam posuere. Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio."))
  ("tutorial/more-notes.txt"
   (notes . "Even more notes.\n\nEmbiggen this window with C-c C-l."))
  ("tutorial/akai-ryu.jpg"
   (include)
   (status . "image")
   (notes . "The binder is not just for writing, but references too! Here's an image*\n\nDionaea muscipula \"Akai Ryu\", Japanese for \"Red Dragon\"\n~ Wikipedia\n\n* your Emacs may not support images :("))
  ("tutorial/dog.txt"
   (notes . "ASCII art by jgs"))
  ("tutorial/end.txt"
   (display . "tutorial/The End")))
 (default-mode . text-mode)
 (default-extension . "txt"))
