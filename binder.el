;;; binder.el --- major mode for structuring multi-file projects  -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Paul W. Rankin

;; Author: Paul W. Rankin <hello@paulwrankin.com>
;; Keywords: files, outlines

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 


;;; Code:

(defgroup binder ()
  "Work with a structured project of files."
  :group 'files)


;;; Options

(defcustom binder-mode-lighter " B/"
  "Mode-line indicator for `binder-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp)

(defcustom binder-default-file ".binder.el"
  "Default file in which to store Binder project data."
  :type 'string
  :safe 'stringp)

(defcustom binder-file-separator "\n\n"
  "String to insert between files when joining."
  :type 'string)

(defcustom binder-pop-up-windows nil
  "Non-nil means displaying a new buffer should make a new window."
  :type 'boolean)


;;; Faces

(defface binder-sidebar-mark
  '((t (:inherit (warning))))
  "Default face marked items.")

(defface binder-sidebar-highlight
  '((t (:inherit (secondary-selection))))
  "Default face for highlighted items.")

(defface binder-sidebar-missing
  '((t (:inherit (trailing-whitespace))))
  "Default face for missing items.")

(defface binder-sidebar-remove
  '((t (:inherit (error))))
  "Default face for items marked for removal.")


;;; Core Variabels

(defvar binder-alist-cache
  nil
  "Global binder alist data.")


;;; Core Functions

(defun binder-find-binder-file (&optional dir)
  (let ((binder-file
         (expand-file-name
          binder-default-file
          (locate-dominating-file (or dir default-directory)
                                  binder-default-file))))
    (if (file-readable-p binder-file) binder-file)))

(defun binder-read (&optional dir)
  (or dir (setq dir default-directory))
  (let ((binder-file (or (binder-find-binder-file dir)
                         (user-error "No binder file found"))))
    (unless (and (string= dir (get 'binder-alist-cache 'dir))
                 (time-less-p (file-attribute-modification-time
                               (file-attributes binder-file))
                              (get 'binder-alist-cache 'modtime)))
      (with-temp-buffer
        (insert-file-contents binder-file)
        (setq binder-alist-cache (read (current-buffer)))))
    (put 'binder-alist-cache 'dir dir)
    (put 'binder-alist-cache 'modtime (current-time)))
  binder-alist-cache)

(defun binder-write (&optional dir)
  (let ((binder-file
         (or (binder-find-binder-file dir)
             (if (y-or-n-p (format "Create `%s' in %s? "
                                   binder-default-file default-directory))
                 (expand-file-name binder-default-file)))))
    (with-temp-buffer
      (insert "\
;; -*- coding: utf-8; -*-
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.\n\n"
              (pp (binder-read)))
      (write-file binder-file))))

(defun binder-get-structure ()
  (alist-get 'structure (binder-read)))

(defun binder-get-item (id)
  (assoc-string id (binder-get-structure)))

(defun binder-get-item-prop (id prop)
  (alist-get prop (cdr (binder-get-item id))))


;;; Global Minor Mode

(defun binder-next-file (&optional n)
  "Goto Nth next file in binder.
Or goto Nth previous file if N is negative."
  (interactive "p")
  ;; FIXME: error on dired buffers
  (let* ((this-file (file-name-nondirectory (buffer-file-name)))
         (structure (binder-get-structure))
         item index next-index next-file)
    (setq item (binder-get-item this-file))
    (if (not item)
        (user-error "Item `%s' not in a binder" this-file)
      (setq index (seq-position structure item 'eq)
            next-index (+ index n))
      (if (and (<= 0 next-index)
               (< next-index (length structure)))
          (and
           (setq next-file (expand-file-name (car (nth next-index structure))
                                             default-directory))
           (find-file-existing next-file))
        (message "End of binder"))
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map "]" #'binder-next-file)
         (define-key map "[" #'binder-previous-file) map)))))

(defun binder-previous-file (&optional n)
  "Goto Nth previous file in binder.
Or goto Nth next file if N is negative."
  (interactive "p")
  (binder-next-file (- n)))

(defun binder-toggle-sidebar ()
  (interactive)
  (let ((display-buffer-mark-dedicated t)
        (dir default-directory)
        (buffer (binder-sidebar-create)))
    (if (get-buffer-window buffer (selected-frame))
        (delete-windows-on buffer (selected-frame))
      (display-buffer-in-side-window buffer binder-sidebar-display-alist)
      (with-current-buffer buffer
        (setq default-directory dir))
      (if binder-sidebar-select-window
          (select-window (get-buffer-window buffer (selected-frame)))))))

(defvar binder-mode-map (make-sparse-keymap))

(define-key binder-mode-map (kbd "C-c ]") #'binder-next-file)
(define-key binder-mode-map (kbd "C-c [") #'binder-previous-file)
(define-key binder-mode-map (kbd "C-c ;") #'binder-toggle-sidebar)

;;;###autoload
(define-minor-mode binder-mode
  "Globally interact with `binder'."
  :init-value nil
  :lighter binder-mode-lighter
  :global t)


;;; Sidebar Major Mode

(defcustom binder-sidebar-display-alist
  '((side . left)
    (window-width . 35)
    (slot . -1))
  "Alist used to display binder sidebar buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist)

(defcustom binder-sidebar-dir-char
  ?+
  "Character to display on direcotry."
  :type 'character)

(defcustom binder-sidebar-include-char
  ?x
  "Character to display on items including in joining."
  :type 'character)

(defcustom binder-sidebar-notes-char
  ?*
  "Character to display on items with notes."
  :type 'character)

(defcustom binder-sidebar-missing-char
  ??
  "Character to display on items with missing files."
  :type 'character)

(defcustom binder-sidebar-select-window
  t
  "If non-nil, switch to binder sidebar upon displaying it."
  :type 'boolean)

(defun binder-sidebar-create ()
  (with-current-buffer (get-buffer-create "*Binder Sidebar*")
    (binder-sidebar-list)
    (binder-sidebar-mode)
    (current-buffer)))

(defun binder-sidebar-list ()
  (with-silent-modifications
    (let ((x (point)))
      (erase-buffer)
      (dolist (item (binder-get-structure))
        (let ((id (car item))
              (filename (alist-get 'filename item))
              (notes (alist-get 'notes item))
              (tags (alist-get 'tags item))
              (level 1))
          (insert ?\s
                  (cond ((and filename (not (file-exists-p filename)))
                         binder-sidebar-missing-char)
                        ((and notes (not (string-empty-p notes)))
                         binder-sidebar-notes-char)
                        (t ?\s))
                  ?\s
                  (if (file-directory-p filename)
                      binder-sidebar-dir-char ?\s)
                  (make-string level ?\s)
                  id)
          (put-text-property (line-beginning-position) (line-end-position)
                             'binder-id id)
          (put-text-property (line-beginning-position) (line-end-position)
                             'front-sticky '(binder-id))
          (insert ?\n)))
      (goto-char x))))

(defun binder-sidebar-refresh ()
  (interactive)
  (binder-sidebar-list))

(defun binder-sidebar-get-id ()
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'binder-id)))

(defun binder-sidebar-find-file ()
  (interactive)
  (let ((pop-up-windows binder-pop-up-windows))
    (find-file-existing
     (alist-get 'filename (binder-get-item
                           (or (binder-sidebar-get-id)
                               (user-error "No item at point")))))))

(defun binder-sidebar-save ()
  (interactive)
  (binder-write))

(defun binder-sidebar-mark ()
  (interactive)
  (beginning-of-line)
  ;; (bookmark-bmenu-ensure-position)
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert-and-inherit ">")
    (put-text-property (line-beginning-position)
                       (line-end-position)
                       'face 'binder-sidebar-mark)
    (forward-line 1)))

(defun binder-sidebar-unmark ()
  (interactive)
  (beginning-of-line)
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert-and-inherit " ")
    (put-text-property (line-beginning-position)
                       (line-end-position)
                       'face nil)
    (forward-line 1)))

(defun binder-get-index (id)
  (seq-position (binder-get-structure) (binder-get-item id) 'eq))

(defun binder-sidebar-get-index ()
  (binder-get-index (binder-sidebar-get-id)))

(defun binder-delete-item (id)
  (setcdr (assq 'structure (binder-read))
          (delq (binder-get-item id) (binder-get-structure))))

(defun binder-sidebar-delete-item ()
  (binder-delete-item (binder-sidebar-get-id)))

(defun binder-insert-item (item index)
  (let ((structure (binder-get-structure)))
    (setcdr (assq 'structure (binder-read))
            (nconc (seq-take structure index)
                   (cons item (seq-drop structure index))))))

(defun binder-sidebar-goto-item (id)
  (goto-char (point-min))
  (let (found)
    (while (and (< (point) (point-max))
                (not found))
      (if (string= (binder-sidebar-get-id) id)
          (setq found t)
        (forward-line 1)))))

(defun binder-sidebar-shift-down (&optional n)
  (interactive "p")
  (let ((p (if (<= n 0) -1 1))
        (id (binder-sidebar-get-id))
        item index)
    (setq item (binder-get-item id)
          index (binder-get-index id))
    (binder-delete-item id)
    (binder-insert-item item (+ index p))
    (binder-sidebar-list)
    (binder-sidebar-goto-item id)))

(defun binder-sidebar-shift-up (&optional n)
  (interactive "p")
  (binder-sidebar-shift-down (- n)))

(defun binder-sidebar-multiview ()
  (interactive)
  (let ((buffer (get-buffer-create "*Binder Multiview*"))
        (mode (alist-get 'default-mode (binder-read)))
        filename-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^>" nil t)
        (setq filename-list
              (cons (alist-get 'filename
                               (binder-get-item (binder-sidebar-get-id)))
                    filename-list))))
    (setq filename-list (reverse filename-list))
    (with-current-buffer buffer
      (with-silent-modifications
        (erase-buffer)
        (dolist (file filename-list)
          (insert-file-contents file)
          (goto-char (point-max))
          (insert binder-file-separator)))
      (if mode (funcall mode))
      (view-mode t))
    (pop-to-buffer buffer)))

;;;###autoload
(define-derived-mode binder-sidebar-mode
  special-mode "Binder Sidebar"
  "Major mode for working with `binder' projects."
  (cursor-sensor-mode t))

(define-key binder-sidebar-mode-map (kbd "g") #'binder-sidebar-refresh)
(define-key binder-sidebar-mode-map (kbd "n") #'next-line)
(define-key binder-sidebar-mode-map (kbd "p") #'previous-line)
(define-key binder-sidebar-mode-map (kbd "RET") #'binder-sidebar-find-file)
(define-key binder-sidebar-mode-map (kbd "s") #'binder-sidebar-save)
(define-key binder-sidebar-mode-map (kbd "m") #'binder-sidebar-mark)
(define-key binder-sidebar-mode-map (kbd "u") #'binder-sidebar-unmark)
(define-key binder-sidebar-mode-map (kbd "v") #'binder-sidebar-multiview)
(define-key binder-sidebar-mode-map (kbd "i") #'binder-sidebar-toggle-notes)
(define-key binder-sidebar-mode-map (kbd "a") #'binder-sidebar-open-notes)
(define-key binder-sidebar-mode-map (kbd "M-n") #'binder-sidebar-shift-down)
(define-key binder-sidebar-mode-map (kbd "M-p") #'binder-sidebar-shift-up)
;; (define-key binder-sidebar-mode-map (kbd "a") #'binder-sidebar-add)
;; (define-key binder-sidebar-mode-map (kbd "U") #'binder-sidebar-unmark-all)
;; (define-key binder-sidebar-mode-map (kbd "d") #'binder-sidebar-remove)
;; (define-key binder-sidebar-mode-map (kbd "r") #'binder-sidebar-rename)


;;; Notes Major Mode

(defcustom binder-notes-display-alist
  '((side . left)
    (slot . 1))
  "Alist used to display binder notes buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist)

(defvar-local binder-notes-id nil)

(defun binder-sidebar-get-notes (dir id)
  (binder-notes-mode)
  (if dir (setq default-directory dir))
  (when id
    (unless (string= binder-notes-id id)
      (setq binder-notes-id id)
      (let ((notes (alist-get 'notes (binder-get-item binder-notes-id))))
        (with-silent-modifications
          (erase-buffer)
          (if notes (insert notes)))))))

(defun binder-sidebar-toggle-notes ()
  (interactive)
  (let ((display-buffer-mark-dedicated t)
        (dir default-directory)
        (id (binder-sidebar-get-id))
        (buffer (get-buffer-create "*Binder Notes*")))
    (with-current-buffer buffer
      (binder-sidebar-get-notes dir id))
    (if (get-buffer-window buffer (selected-frame))
        (delete-windows-on buffer (selected-frame))
      (display-buffer-in-side-window buffer binder-notes-display-alist))))

(defun binder-sidebar-open-notes ()
  (interactive)
  (let ((display-buffer-mark-dedicated t)
        (dir default-directory)
        (id (binder-sidebar-get-id))
        (buffer (get-buffer-create "*Binder Notes*")))
    (with-current-buffer buffer
      (binder-sidebar-get-notes dir id)
    (display-buffer-in-side-window buffer binder-notes-display-alist)
    (select-window (get-buffer-window buffer (selected-frame))))))

(defun binder-notes-commit ()
  (interactive)
  (unless (derived-mode-p 'binder-notes-mode)
    (user-error "Not in binder-notes-mode"))
  (if (not (buffer-modified-p))
      (message "(No changes need to be added to binder)")
    (let ((notes-prop (assq 'notes (binder-get-item binder-notes-id)))
          (notes (string-trim
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (if notes-prop
          (setcdr notes-prop notes)
        (push (cons 'notes notes) (cdr (binder-get-item binder-notes-id)))))
    (set-buffer-modified-p nil)
    (message "Added notes for `%s' to binder" binder-notes-id)))

(defun binder-notes-expand-window ()
  (interactive)
  (unless (derived-mode-p 'binder-notes-mode)
    (user-error "Not in binder-notes-mode"))
  (if (window-parameter (selected-window) 'window-side)
      (progn
        (quit-window)
        (pop-to-buffer (get-buffer-create "*Binder Notes*") t))
    (quit-window)
    (binder-sidebar-open-notes)))

;;;###autoload
(define-derived-mode binder-notes-mode
  text-mode "Binder Notes Mode"
  "Major mode for editing `binder' notes.")

(define-key binder-notes-mode-map (kbd "C-c C-c") #'binder-notes-commit)
(define-key binder-notes-mode-map (kbd "C-c C-l") #'binder-notes-expand-window)
(define-key binder-notes-mode-map (kbd "C-c C-q") #'quit-window)

(defcustom binder-notes-mode-hook
  '(turn-on-visual-line-mode)
  "Hook run after entering Binder Notes Mode mode."
  :type 'hook
  :options '(turn-on-visual-line-mode))

(provide 'binder)
;;; binder.el ends here
