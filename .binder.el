;; -*- coding: utf-8; -*-
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.

((structure
  ("binder.el"
   (tags "code"))
  ("README.md")
  ("TODO.md")
  ("tutorial/start-here.txt"
   (notes . "This file is going by a pseudonym")
   (tags "start-here" "tutorial")
   (display . "tutorial/foo"))
  ("tutorial/sidebar.txt"
   (include . t)
   (tags "tutorial" "foo"))
  ("tutorial/navigating.txt"
   (include . t)
   (tags "tutorial" "foo" "bar"))
  ("tutorial/stapling.txt"
   (include . t)
   (tags "tutorial" "bar"))
  ("tutorial/stapling-2.txt"
   (display . "tutorial/editing stapled stuff")
   (tags "tutorial"))
  ("tutorial/notes.txt"
   (notes . "Some notes about notes.\n\nNullam eu ante vel est convallis dignissim. Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio. Nunc porta vulputate tellus. Nunc rutrum turpis sed pede. Sed bibendum. Aliquam posuere. Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio.")
   (tags "tutorial"))
  ("tutorial/more-notes.txt"
   (notes . "Even more notes.\n\nEmbiggen this window with C-c C-l.")
   (tags "tutorial"))
  ("tutorial/akai-ryu.jpg"
   (display . "tutorial/akai-ryu.jpg")
   (tags "tutorial" "image")
   (notes . "The binder is not just for writing, but references too! Here's an image*\n\nDionaea muscipula \"Akai Ryu\", Japanese for \"Red Dragon\"\n~ Wikipedia\n\n* your Emacs may not support images :("))
  ("tutorial/dog.txt"
   (notes . "ASCII art by jgs")
   (tags "tutorial"))
  ("tutorial/end.txt"
   (display . "tutorial/The End")
   (tags "tutorial")))
 (default-extension . "txt")
 (default-concat-mode . text-mode))
