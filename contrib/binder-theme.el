;; To install this theme, copy or symlink it to your `user-emacs-directory'.

(deftheme binder
  "Binder theme, inspired by Scrivener.")

(custom-theme-set-faces
 'binder
 '(binder-sidebar
   ((default (:height 0.9))
    (((background light)) (:background "#f1f0f6"))
    (((background dark)) (:background "#33323f"))))
 '(binder-notes
   ((default (:height 0.9))
    (((background light)) (:background "#fefbe8"))
    (((background dark)) (:background "#302c2c")))))

(provide-theme 'binder)
