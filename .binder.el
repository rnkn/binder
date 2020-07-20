;; -*- coding: utf-8; -*-
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.

((structure
  ("binder.el"
   (tags "code"))
  ("README.md")
  ("tutorial/start-here.txt"
   (notes . "This file is going by a pseudonym")
   (tags "start-here" "tutorial"))
  ("tutorial/navigating.txt"
   (include . t)
   (tags "tutorial" "foo" "bar"))
  ("tutorial/sidebar.txt"
   (include . t)
   (tags "tutorial"))
  ("tutorial/marks.txt"
   (include . t)
   (tags "tutorial"))
  ("tutorial/notes.txt"
   (notes . "Some notes about notes.\n\nNullam eu ante vel est convallis dignissim. Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio. Nunc porta vulputate tellus. Nunc rutrum turpis sed pede. Sed bibendum. Aliquam posuere. Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio.")
   (tags "tutorial"))
  ("tutorial/tags.txt"
   (tags "tutorial" "foo"))
  ("tutorial/concat.txt"
   (include . t)
   (tags "tutorial" "bar"))
  ("tutorial/concat-2.txt"
   (display . "tutorial/concat-including")
   (tags "tutorial"))
  ("tutorial/concat-3.txt"
   (tags "tutorial")
   (display . "tutorial/concat-editing"))
  ("tutorial/akai-ryu.jpg"
   (display . "tutorial/akai-ryu.jpg")
   (tags "tutorial" "image")
   (notes . "The binder is not just for writing, but references too! Here's an image*\n\nDionaea muscipula \"Akai Ryu\", Japanese for \"Red Dragon\"\n~ Wikipedia\n\n* your Emacs may not support images :("))
  ("tutorial/dog.txt"
   (notes . "ASCII art by jgs")
   (tags "tutorial"))
  ("tutorial/end.txt"
   (tags "tutorial"))))
