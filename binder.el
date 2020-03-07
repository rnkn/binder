;;; binder.el --- Global minor mode to facilitate multi-file writing projects  -*- lexical-binding: t; -*-

;; Copyright (c) 2020  Paul William Rankin

;; Author: William Rankin <code@william.bydasein.com>
;; Keywords: files, outlines, wp, text
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.3"))
;; URL: https://github.com/rnkn/binder

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

;; # Binder #

;; **Warning: this is alpha-level software. Although it does not touch files on
;; disk, you should consider all binder data (project structure, notes, statuses)
;; as susceptible to loss. Key bindings, variable and function names, and overall
;; program design are subject to change.**

;; Binder is global minor mode (and associated major modes) to facilitate working
;; on a writing project in multiple files. It is heavily inspired by the binder
;; feature in the [macOS writing app Scrivener][scriv].

;; The rationale behind working this way is to split a large writing project into
;; much smaller pieces.

;; [scriv]: https://www.literatureandlatte.com/scrivener/features?os=macOS

;; ## Features ##

;; Primarily, Binder provides a global minor mode binder-mode. This allows
;; working with files in the current binder data, which is saved in a .binder.el
;; file in the current directory. (You can change the name of this file is
;; binder-default-file option.)

;; At this top-level, the main interaction with your binder will be in navigating
;; back and forth between binder files:

;; - binder-next (bound to C-c ]) visits the next file in the binder, and
;; - binder-previous (bound to C-c [) visits the previous.

;; Calling these commands activates a transient map so that each command can be
;; repeated by repeating only the last key.

;; You probably want some idea of the structure of your binder project...

;; - binder-reveal-in-sidebar (bound to C-c ;) will find the current file in
;;   the binder sidebar (see below) or call binder-init-binder-file if there is
;;   none.
;; - binder-toggle-sidebar (bound to C-c ') toggles the visibility of the
;;   binder sidebar.

;; And when you're writing and want to quickly add something new, you can with...

;; - binder-add-file (bound to C-c :) prompts for a file-name and adds this
;;   possibly non-existent file to the binder after the current file's index. If no
;;   file-name extension is provided, use value of the binder's default-extension
;;   property (set with binder-default-file-extention option).

;; ### Binder Sidebar Mode ###

;; A major mode for displaying the binder sidebar. This is where your main
;; interaction with the binder happens.

;; Binder items are displayed in a linear ordered list. Calling
;; binder-sidebar-find-file (bound to RET) or
;; binder-sidebar-find-file-other-window (bound to o) will visit the
;; corresponding file.

;; Each item in the binder sidebar displays the following information:

;; 1. binder-sidebar-include-char (default x) denotes that this item has a
;;    non-nil value for its include property and therefore is included in
;;    binder-stable-mode (see below).
;; 2. binder-sidebar-notes-char (default *) denotes that this item has a string
;;    value for its notes property, which can be edited in binder-notes-mode
;;    (see below), or
;; 3. binder-sidebar-missing-char (defautl ?) denote that the item's
;;    corresponding file cannot be found, but can be relocated by calling
;;    binder-sidebar-relocate (bound to R).
;; 4. The item name, either the car of the item element or its display
;;    property, which can be set by calling binder-sidebar-rename (bound to r).
;; 5. The item status property value, prefixed with binder-sidebar-status-char
;;    (default #). The display of this value can be set with the
;;    binder-sidebar-status-column option.

;; Add an item with binder-sidebar-add-file (bound to a) or add all files in
;; directory with binder-sidebar-add-all-files (bound to A). Add a new file
;; with binder-sidebar-new-file (bound to M-RET). Remove items with
;; binder-sidebar-remove (bound to d) -- this *does not delete the files*, only
;; removes them from the binder.

;; Items can be reordered with binder-sidebar-shift-up (bound to M-p & M-up)
;; and binder-sidebar-shift-down (bound to M-n & M-down).

;; Each item's include state is toggled with binder-sidebar-toggle-include (bound
;; to x).

;; Each item can be given a status with binder-sidebar-set-status (bound to #).
;; Filter item by including or excluding a status with binder-sidebar-filter-in
;; (bound to /) and binder-sidebar-filter-out (bound to \). To clear a
;; status, just set an empty string. (n.b. Each command filter in/out only a single
;; status, therefore it won't make sense to use them in conjunction.)

;; The notes buffer (see below) can be accessed with either
;; binder-sidebar-open-notes (bound to z) or binder-sidebar-toggle-notes
;; (bound to i).

;; Hide item file extensions by setting the binder-sidebar-hide-file-extensions
;; option. This can be toggled with binder-sidebar-toggle-file-extensions (bound
;; to E).

;; You can customize how the sidebar window is displayed by setting
;; binder-sidebar-display-alist option.

;; (There is a "mark" functionality, but this is yet to be implemented beyond just
;; temporarily making items look marked.)

;; ### Binder Notes Mode ###

;; A major mode for editing binder notes.

;; Notes are only saved to the binder when calling binder-notes-commit (bound to
;; C-c C-c). Calling quit-window (bound to C-c C-q) or binder-toggle-sidebar
;; does not save notes.

;; By default, the notes window will update to the corresponding item notes
;; whenever the cursor moves in the binder sidebar. This may be disconcerting, so
;; you can change it by setting the binder-notes-keep-in-sync option.

;; If the notes side window feels too small, you can pop the buffer out to a
;; regular sized window with binder-notes-expand-window (bound to C-c C-l).

;; You can customize how the notes window is displayed by setting
;; binder-notes-display-alist option.

;; ### Binder Staple Mode ###

;; A minor mode for "stapling" binder files together.

;; When calling binder-sidebar-staple (bound to v), items marked as included in
;; the binder will be concatenated in a new buffer, separated by
;; binder-staple-separator string.

;; In this buffer, calling binder-staple-find-original-file (bound to M-RET)
;; will visit the original file corresponding to the text at point.

;; ## Why not just use Org Mode? ##

;; [Org Mode] is nice, but it's also a very *heavy* tool that almost insists that
;; everything be done within Org Mode. This isn't useful if you want to write in a
;; different format, e.g. [Markdown] or [Fountain].

;; Also, I prefer to keep my writing in a collection of separate text files. It
;; feels nicer to work on something small and self-contained than to organize a
;; large file with headings and use indirect buffers with narrowing.

;; [org mode]: https://orgmode.org
;; [markdown]: http://jblevins.org/projects/markdown-mode/
;; [fountain]: https://github.com/rnkn/fountain-mode

;; ## Requirements ##

;; - Emacs 25.3

;; ## Bugs and Feature Requests ##

;; Report bugs and feature requests at: <https://github.com/rnkn/binder/issues>

;; ## Start Here ##

;; This file is part of a sample binder project. Enabled binder-mode and type C-c ;
;; to reveal this file in the binder siderbar.


;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))
(require 'seq)

(defgroup binder ()
  "Work with a structured project of files."
  :group 'files)


;;; Options

(defcustom binder-mode-lighter
  " B/"
  "Mode-line indicator for `binder-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp
  :group 'binder)

(defcustom binder-default-file
  ".binder.el"
  "Default file in which to store Binder project data."
  :type 'string
  :safe 'stringp
  :group 'binder)

(defcustom binder-default-file-extention
  "txt"
  "Default extension for new binder files."
  :type 'string
  :safe 'stringp
  :group 'binder)

(defcustom binder-save-threshold
  20
  "Integer of changes before binder file is automatically saved."
  :type 'integer
  :safe 'integerp
  :group 'binder)


;;; Faces

(defgroup binder-sidebar-faces ()
  "Default faces for `binder-sidebar-mode'."
  :group 'binder-sidebar)

(defface binder-sidebar-marked
  '((t (:inherit (warning))))
  "Default face marked items."
  :group 'binder-sidebar-faces)

(defface binder-sidebar-highlight
  '((t (:extend t :inherit (secondary-selection))))
  "Default face for highlighted items."
  :group 'binder-sidebar-faces)

(defface binder-sidebar-missing
  '((t (:inherit (trailing-whitespace))))
  "Default face for missing items."
  :group 'binder-sidebar-faces)

(defface binder-sidebar-status
  '((t (:inherit (font-lock-variable-name-face))))
  "Default face for status labels."
  :group 'binder-sidebar-faces)


;;; Core Variables

(defvar binder-file-header
  "\
;; -*- coding: utf-8; -*-
;; This is a Binder project file. It is meant to be human-readable, but you
;; probably shouldn't edit it.\n\n"
  "Header information for binder-file.")

(defvar binder--cache nil)
(defvar binder--directory nil)          ; FIXME: does this actually work?
(defvar binder--modification-time nil)
(defvar binder--modification-count 0)


;;; Core Functions

(defun binder-root ()
  "Find the root directory with a binder file."
  (let ((directory
         (locate-dominating-file default-directory binder-default-file)))
    (when (file-directory-p directory) (expand-file-name directory))))

(defun binder-init-binder-file ()
  "Initialize a binder file."
  (unless (binder-root)
    (when (y-or-n-p (format "Initialize empty %s in %s? " binder-default-file
                            (abbreviate-file-name default-directory)))
      (let ((binder-file
             (expand-file-name binder-default-file default-directory)))
      (with-temp-buffer
        (insert binder-file-header
                (pp-to-string
                 (list (cons 'structure nil)
                       (cons 'default-mode binder-default-stapled-mode)
                       (cons 'default-extension binder-default-file-extention))))
        (write-file binder-file))
      binder-file))))

(defun binder-find-binder-file ()
  "Find or initialize current binder file."
  (let ((binder-file (expand-file-name binder-default-file (binder-root))))
    (if (file-exists-p binder-file) binder-file
      (binder-init-binder-file))))

(defun binder-read ()
  "Read current binder data.
Reads from `binder--cache' if valid, or from binder file if not."
  (let ((binder-file (binder-find-binder-file)))
    (unless binder-file
      (user-error "No binder file found"))
    (unless (and binder--cache
                 (string= binder--directory
                          (expand-file-name default-directory))
                 (or (= binder--modification-count 0)
                     (time-less-p (nth 5 (file-attributes binder-file))
                                  binder--modification-time)))
      (with-temp-buffer
        (insert-file-contents binder-file)
        (goto-char (point-min))
        (setq binder--cache (read (current-buffer)))))
    (setq binder--directory (expand-file-name default-directory))
  binder--cache))

(defun binder-write ()
  "Write binder data to file."
  (interactive)
  (mapc (lambda (item)
          (setf item (rassq-delete-all "" item)))
        (binder-get-structure))
  (let ((binder-file (binder-find-binder-file)))
    (with-temp-buffer
      (insert binder-file-header (pp-to-string binder--cache))
      (write-file binder-file)))
  (setq binder--modification-count 0))

(defun binder-write-maybe ()
  "Write binder data if passed modified threshold."
  (if (< binder--modification-count binder-save-threshold)
      (cl-incf binder--modification-count)
    (binder-write)))

(defun binder-file-relative-to-root (filepath)
  "Return FILEPATH relative to binder root directory."
  (let ((root (binder-root)))
    (when root
      (string-trim (expand-file-name filepath)
                   (expand-file-name root)))))

(defun binder-get-structure ()
  "Return binder data structure component."
  (alist-get 'structure (binder-read)))

(defun binder-get-item (fileid)
  "Return binder item association list for FILEID."
  (assoc-string fileid (binder-get-structure)))

(defun binder-get-item-prop (fileid prop)
  "Return value of PROP for binder item for FILEID."
  (alist-get prop (cdr (binder-get-item fileid))))

(defun binder-set-item-prop (fileid prop value)
  "Set VALUE of PROP for binder item for FILEID."
  (let ((item (binder-get-item fileid)))
    (if (string-empty-p value)
        (setf item (assq-delete-all prop item))
      (let ((prop-elt (assq prop item)))
        (if prop-elt
            (setcdr prop-elt value)
          (push (cons prop value) (cdr item))))))
  (setq binder--modification-time (current-time)))

(defun binder-get-item-index (fileid)
  "Return index position for binder item for FILEID."
  (seq-position (binder-filter-structure) (binder-get-item fileid)))

(defun binder-insert-item (item index)
  "Insert binder ITEM at position INDEX."
  (unless (listp item) (setq item (list item)))
  (setcdr (assq 'structure (binder-read))
          (let ((structure (binder-get-structure)))
            (append (seq-take structure index)
                    (cons item (seq-drop structure index)))))
  (setq binder--modification-time (current-time))
  (binder-write-maybe))

(defun binder-delete-item (fileid)
  "Delete binder item for FILEID."
  (setcdr (assq 'structure (binder-read))
          (remove (binder-get-item fileid) (binder-get-structure)))
  (setq binder--modification-time (current-time))
  (binder-write-maybe))

(defun binder-get-prop-list (prop)
  "Return list of values for PROP."
  (delq nil
        (mapcar
         (lambda (item)
           (let ((value (alist-get prop item)))
             (when (and (stringp value) (< 0 (string-width value)))
               value)))
         (binder-get-structure))))

(defvar binder-status-filter-in nil)
(defvar binder-status-filter-out nil)

(defun binder-filter-structure ()
  (let ((structure (binder-get-structure)))
    (if binder-status-filter-in
        (setq structure
              (seq-filter
               (lambda (item)
                 (string= (alist-get 'status item) binder-status-filter-in))
               structure))
      (when binder-status-filter-out
        (setq structure
              (seq-remove
               (lambda (item)
                 (string= (alist-get 'status item) binder-status-filter-out))
               structure))))
    structure))


;;; Global Minor Mode

(defvar binder-mode-map (make-sparse-keymap))

(defun binder-get-buffer-fileid ()
  "Return buffer binder fileid."
  (binder-file-relative-to-root
   (or (buffer-file-name)
       (expand-file-name default-directory))))

(defun binder-next (&optional n)
  "Visit Nth next file in binder.
Or visit Nth previous file if N is negative."
  (interactive "p")
  ;; Find the current file/directory fileid, if one.
  (let ((this-fileid (binder-get-buffer-fileid))
        (structure (binder-filter-structure))
        index next-index)
    ;; If current file has an INDEX, get the NEXT-INDEX.
    (setq index (or (binder-get-item-index this-fileid) 0)
          next-index (+ index n))
    ;; If NEXT-INDEX is within the filtered structure length, find the
    ;; Nth next/previous file.
    (if (not (<= 0 next-index (1- (length structure))))
        (message "End of binder")
      (find-file-existing
       (expand-file-name (car (nth next-index structure)) (binder-root)))
      (setq binder--current-fileid (binder-get-buffer-fileid))
      (binder-sidebar-refresh)
      (setq binder--notes-fileid binder--current-fileid)
      (binder-notes-refresh))
    ;; Setup the overriding keymap.
    (unless overriding-terminal-local-map
      (let ((keys (substring (this-single-command-keys) 0 -1))
            (map (cdr binder-mode-map)))
        (mapc (lambda (k) (setq map (assq k map))) keys)
        (when (consp map) (set-transient-map (cdr map) t))))))

(defun binder-previous (&optional n)
  "Visit Nth previous file in binder.
Or visit Nth next file if N is negative."
  (interactive "p")
  (binder-next (- n)))

(defun binder-add-file (fileid &optional index)
  "Add a (possibly non-existent) FILEID to the binder at INDEX.
If the current file is in the binder, add at INDEX after that one."
  (interactive "FAdd file (extension optional): ")
  (setq fileid (binder-file-relative-to-root fileid))
  (unless (< 0 (string-width fileid))
    (user-error "No file name supplied"))
  ;; If FILEID is not a directory and no file extension was provided,
  ;; add the binder's default file extensions.
  (unless (or (directory-name-p fileid)
              (and (string-match "\\.[^.]+\\'" fileid)
                   (not (= 0 (match-beginning 0)))))
    (setq fileid
          (concat fileid "." (alist-get 'default-extension (binder-read)))))
  ;; If the file/directory does not exist, create it.
  (let ((filepath (expand-file-name fileid (binder-root))))
    (unless (file-exists-p filepath)
      (if (directory-name-p filepath)
          (make-directory filepath t)
        (with-current-buffer (find-file-noselect filepath)
          (write-file filepath))))
    ;; Insert FILEID into binder at INDEX, or after current file.
    (unless (binder-get-item fileid)
      (unless index
        (let ((this-file-index
               (binder-get-item-index (binder-get-buffer-fileid))))
          (setq index (if this-file-index
                          (1+ this-file-index)
                        (length (binder-get-structure))))))
      (binder-insert-item fileid index))
    (binder-write-maybe)
    ;; When binder sidebar is active, refresh it.
    (when (window-live-p (get-buffer-window binder-sidebar-buffer))
      (with-current-buffer binder-sidebar-buffer
        (binder-sidebar-refresh)
        (binder-sidebar-goto-item fileid)))
    ;; Finally, visit the file FILEPATH.
    (let ((pop-up-windows binder-sidebar-pop-up-windows))
      (find-file filepath))))

(define-key binder-mode-map (kbd "C-c ]") #'binder-next)
(define-key binder-mode-map (kbd "C-c [") #'binder-previous)
(define-key binder-mode-map (kbd "C-c ;") #'binder-reveal-in-sidebar)
(define-key binder-mode-map (kbd "C-c '") #'binder-toggle-sidebar)
(define-key binder-mode-map (kbd "C-c :") #'binder-add-file)

;;;###autoload
(define-minor-mode binder-mode
  "Globally interact with `binder'."
  :init-value nil
  :lighter binder-mode-lighter
  :global t)


;;; Sidebar Major Mode

(defgroup binder-sidebar ()
  "Options for `binder-sidebar-mode'."
  :group 'binder)

(defcustom binder-sidebar-buffer "*Binder Sidebar*"
  "Default buffer name for binder sidebar."
  :type 'string
  :safe 'stringp
  :group 'binder-sidebar)

(defcustom binder-sidebar-display-alist
  '((side . left)
    (window-width . 40)
    (slot . -1))
  "Association list used to display binder sidebar buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist
  :safe (lambda (value)
          (and (listp value)
               (seq-every-p 'consp value)))
  :group 'binder-sidebar)

(defcustom binder-sidebar-hide-file-extensions
  nil
  "When non-nil, list items without file extension."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-sidebar-pop-up-windows
  nil
  "Non-nil means displaying a new buffer should make a new window."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-sidebar-status-column
  25
  "Integer for column to align status tags."
  :type 'integer
  :safe 'integerp
  :group 'binder-sidebar)

(defcustom binder-sidebar-include-char
  ?x
  "Character to display on items including in joining."
  :type 'character
  :safe 'characterp
  :group 'binder-sidebar)

(defcustom binder-sidebar-notes-char
  ?*
  "Character to display on items with notes."
  :type 'character
  :safe 'characterp
  :group 'binder-sidebar)

(defcustom binder-sidebar-status-char
  ?#
  "Character to prefix to item status."
  :type 'character
  :safe 'characterp
  :group 'binder-sidebar)

(defcustom binder-sidebar-missing-char
  ??
  "Character to display on items with missing files."
  :type 'character
  :safe 'characterp
  :group 'binder-sidebar)

(defcustom binder-sidebar-select-window
  nil
  "If non-nil, switch to binder sidebar upon displaying it."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-sidebar-persistent-window
  t
  "When non-nil, sidebar will persist when calling `delete-other-windows'.

This marks `no-delete-other-windows' window parameter as non-nil.

Use `binder-toggle-sidebar' or `quit-window' to close the sidebar."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defvar binder--sidebar-marked nil)
(defvar binder--sidebar-narrowing-status nil)

(defun binder-sidebar-refresh ()
  "Redraw binder sidebar, reading from cache."
  (interactive)
  (with-silent-modifications
    (let ((x (point)))
      (erase-buffer)
      (dolist (item (binder-filter-structure))
        (let ((fileid (car item))
              (include (alist-get 'include item))
              (display (alist-get 'display item))
              (notes (alist-get 'notes item))
              (status (alist-get 'status item))
              marked missing status-overwrite)
          ;; Set whether FILEID is MARKED and MISSING.
          (when (member fileid binder--sidebar-marked)
            (setq marked t))
          (when (not (file-exists-p fileid))
            (setq missing t))
          ;; Insert the item line elements.
          (insert (cond (marked ">")
                        (include binder-sidebar-include-char)
                        (t " "))
                  " "
                  (cond (missing binder-sidebar-missing-char)
                        ((and notes (not (string-empty-p notes)))
                         binder-sidebar-notes-char)
                        (t " "))
                  " "
                  ;; Use either DISPLAY, or if directory ensure a
                  ;; trailing slash, and finally if we're hiding file
                  ;; extensions, do that, otherwise just the FILEID is
                  ;; fine.
                  (or display
                      (if (file-directory-p fileid)
                          (replace-regexp-in-string "/*$" "/" fileid)
                        (if binder-sidebar-hide-file-extensions
                            (replace-regexp-in-string ".+\\(\\..+\\)" ""
                                                      fileid nil nil 1)
                          fileid))))
          ;; Add the face properties. Make them front-sticky since we
          ;; were previously editing the buffer text (but not anymore).
          (put-text-property (line-beginning-position) (line-end-position)
                             'binder-fileid fileid)
          (put-text-property (line-beginning-position) (line-end-position)
                             'front-sticky '(binder-fileid))
          (when missing
            (put-text-property (line-beginning-position) (line-end-position)
                               'face 'binder-sidebar-missing))
          (when marked
            (put-text-property (line-beginning-position) (line-end-position)
                               'face 'binder-sidebar-marked))
          ;; Add the item STATUS with a hashtag, because hashtags are
          ;; cool, right?
          (when (and status (< 0 (string-width status)))
            (move-to-column binder-sidebar-status-column)
            (unless (eolp) (setq status-overwrite t))
            (indent-to-column binder-sidebar-status-column)
            (let ((x (1- (point))))
              (delete-region x (line-end-position))
              (insert (if status-overwrite "~" " ")
                      binder-sidebar-status-char status)
              (put-text-property x (line-end-position)
                                 'face 'binder-sidebar-status)))
          (insert "\n")))
      (goto-char x))))

(defun binder-sidebar-create-buffer (directory)
  "Return binder sidebar buffer for DIRECTORY."
  (with-current-buffer (get-buffer-create binder-sidebar-buffer)
    (setq default-directory directory)
    (binder-sidebar-mode)
    (current-buffer)))

(defun binder-sidebar-create-window (&optional directory)
  "Return binder sidebar window for DIRECTORY.
Defaults to current directory."
  (let ((display-buffer-mark-dedicated t))
    (with-current-buffer (binder-sidebar-create-buffer
                          (or directory (expand-file-name default-directory)))
      (display-buffer-in-side-window
       (current-buffer)
       (append binder-sidebar-display-alist
               (when binder-sidebar-persistent-window
                 (list '(window-parameters (no-delete-other-windows . t)))))))))

(defun binder-sidebar-get-fileid ()
  "Return fileid for item at point."
  (save-excursion
    (if (eobp) (forward-line -1) (beginning-of-line))
    (get-text-property (point) 'binder-fileid)))

(defun binder-sidebar-goto-item (fileid)
  "Move point to binder item with FILEID."
  (goto-char (point-min))
  ;; It would be nice to use find-next-text-property but that isn't
  ;; available until Emacs 27.
  (let (found)
    (while (and (< (point) (point-max))
                (not found))
      (if (string= (binder-sidebar-get-fileid) fileid)
          (setq found t)
        (forward-line 1)))))

;; FIXME: currently not in use.
(defun binder-sidebar-highligh-current ()
  (when (binder-root)
    (let ((fileid (binder-file-relative-to-root (buffer-file-name)))
          (sidebar-buffer (get-buffer binder-sidebar-buffer)))
      (when (buffer-live-p sidebar-buffer)
        (with-current-buffer sidebar-buffer
          (binder-sidebar-goto-item fileid)
          (with-silent-modifications
            (put-text-property (line-beginning-position) (line-beginning-position 2)
                               'face 'binder-sidebar-highlight)))))))

(defun binder-sidebar-find-file (arg)
  "Visit binder item at point.
When ARG is non-nil, visit in new window."
  (interactive "P")
  (let ((pop-up-windows (or arg binder-sidebar-pop-up-windows)))
    (find-file (binder-sidebar-get-fileid))))

(defun binder-sidebar-find-file-other-window ()
  "Visit binder item in other window."
  (interactive)
  (binder-sidebar-find-file t))

(defalias 'binder-sidebar-save 'binder-write)

(defun binder-sidebar-get-index ()
  "Return binder index position at point."
  (if (eobp) (length (binder-get-structure))
    (binder-get-item-index (binder-sidebar-get-fileid))))

(defun binder-sidebar-mark ()
  "Mark the binder item at point."
  (interactive)
  (cl-pushnew (binder-sidebar-get-fileid) binder--sidebar-marked)
  (forward-line 1)
  (binder-sidebar-refresh))

(defun binder-sidebar-unmark ()
  "Unmark the binder item at point."
  (interactive)
  (setq binder--sidebar-marked
        (remove (binder-sidebar-get-fileid) binder--sidebar-marked))
  (forward-line 1)
  (binder-sidebar-refresh))

(defun binder-sidebar-unmark-all ()
  "Unmark all binder items."
  (interactive)
  (let ((fileid (binder-sidebar-get-fileid)))
    (goto-char (point-min))
    (while (not (eobp))
      (binder-sidebar-unmark))
    (binder-sidebar-goto-item fileid)))

(defun binder-sidebar-add-file (fileid)
  "Add (possibly non-existent) file to binder as FILEID."
  (interactive "FAdd file: ")
  (setq fileid (binder-file-relative-to-root fileid))
  (unless (binder-get-item fileid)
    (binder-insert-item fileid (1+ (binder-sidebar-get-index))))
  (setq binder--modification-time (current-time))
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-add-all-files ()
  "Add all files in current directory to binder."
  (interactive)
  (when (y-or-n-p (format "Add all files in %s"
                          (abbreviate-file-name default-directory)))
    (dolist (file (directory-files default-directory nil "^[^.]"))
      (binder-sidebar-add-file file))))

(defun binder-sidebar-new-file (fileid)
  "Add a new file to binder as FILEID and visit it."
  (interactive "FAdd file (extension optional): ")
  (unless (eq major-mode 'binder-sidebar-mode)
    (user-error "Not in %S" 'binder-sidebar-mode))
  (binder-add-file fileid (1+ (binder-sidebar-get-index))))

(defun binder-sidebar-remove (arg)
  "Remove binder item at point.
When ARG is non-nil, do not prompt for confirmation."
  (interactive "P")
  (let ((fileid (binder-sidebar-get-fileid))
        display)
    (setq display (or (binder-get-item-prop fileid 'display)
                      fileid))
    (when (or arg (y-or-n-p (format "Really remove item %S?" display)))
      (binder-delete-item fileid))
    (setq binder--modification-time (current-time))
    (binder-sidebar-refresh)
    (binder-write-maybe)))

(defun binder-sidebar-rename ()
  "Change display name of binder item at point."
  (interactive)
  (let ((fileid (binder-sidebar-get-fileid))
        name)
    (setq name
          (read-string "New name: "
                       (or (binder-get-item-prop fileid 'display) fileid)))
    (binder-set-item-prop fileid 'display name)
    (setq binder--modification-time (current-time))
    (binder-sidebar-refresh)
    (binder-write-maybe)))

(defun binder-sidebar-relocate (filepath)
  "Change file path of binder item at point to FILEPATH."
  (interactive "fNew file path: ")
  (setq filepath (binder-file-relative-to-root filepath))
  (setcar (binder-get-item (binder-sidebar-get-fileid)) filepath)
  (setq binder--modification-time (current-time))
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-toggle-include ()
  "Toggle whether binder item at point is included in stapled view."
  (interactive)
  (let ((fileid (binder-sidebar-get-fileid))
        include)
    (setq include (not (binder-get-item-prop fileid 'include)))
    (binder-set-item-prop (binder-sidebar-get-fileid) 'include include))
  (setq binder--modification-time (current-time))
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-set-status (status)
  "Set the status of binder item at point to STATUS."
  (interactive
   (list (completing-read-default
          "Status: " (binder-get-prop-list 'status)
          nil nil (binder-get-item-prop (binder-sidebar-get-fileid) 'status))))
  (binder-set-item-prop (binder-sidebar-get-fileid) 'status status)
  (setq binder--modification-time (current-time))
  (binder-sidebar-refresh)
  (binder-write-maybe))

(defun binder-sidebar-toggle-file-extensions ()
  "Toggle visibility of binder item file extensions."
  (interactive)
  (customize-set-variable 'binder-sidebar-hide-file-extensions
                          (not binder-sidebar-hide-file-extensions))
  (let ((fileid (binder-sidebar-get-fileid)))
    (binder-sidebar-refresh)
    (binder-sidebar-goto-item fileid))
  (message "%s file extensions"
           (capitalize
            (if binder-sidebar-hide-file-extensions
                "hiding" "showing"))))

(defun binder-sidebar-shift-down (&optional n)
  "Shift index position of binder item at point down in list."
  (interactive "p")
  (let ((p (if (<= n 0) -1 1))
        (fileid (binder-sidebar-get-fileid))
        item index)
    (setq item (binder-get-item fileid)
          index (binder-get-item-index fileid))
    (when (<= 0 (+ index p) (1- (length (binder-get-structure))))
      (binder-delete-item fileid)
      (binder-insert-item item (+ index p))
      (setq binder--modification-time (current-time))
      (binder-sidebar-refresh)
      (binder-sidebar-goto-item fileid))))

(defun binder-sidebar-shift-up (&optional n)
  "Shift index position of binder item at point up in list."
  (interactive "p")
  (binder-sidebar-shift-down (- n)))

(defun binder-sidebar-filter-in (status)
  "Filter items to only include items with STATUS."
  (interactive
   (list (completing-read-default
          "Filter in status: " (binder-get-prop-list 'status))))
  (setq binder-status-filter-in
        (if (string-empty-p status) nil status))
  (binder-sidebar-refresh))

(defun binder-sidebar-filter-out (status)
  "Filter items to exclude items with STATUS."
  (interactive
   (list (completing-read-default
          "Filter out status: " (binder-get-prop-list 'status))))
  (setq binder-status-filter-out
        (if (string-empty-p status) nil status))
  (binder-sidebar-refresh))

;;;###autoload
(defun binder-reveal-in-sidebar ()
  "Reveal current file in binder sidebar.

Unconditionally activates `binder-mode'."
  (interactive)
  (binder-mode t)
  (let ((filepath (or (buffer-file-name)
                      (expand-file-name default-directory)))
        (root (binder-root)))
    (select-window (binder-sidebar-create-window (binder-root)))
    (unless (string= filepath root)
      (setq fileid (binder-file-relative-to-root filepath))
      (if (binder-get-item fileid)
          (binder-sidebar-goto-item fileid)
        (when (y-or-n-p (format "Add %s to binder? " fileid))
          (binder-sidebar-add-file fileid)
          (binder-sidebar-refresh)
          (binder-sidebar-goto-item fileid))))))

;;;###autoload
(defun binder-toggle-sidebar ()
  "Toggle visibility of binder sidebar window."
  (interactive)
  (if (window-live-p (get-buffer-window binder-sidebar-buffer))
      (binder-sidebar-delete-windows)
    (binder-sidebar-create-window)
    (with-current-buffer binder-sidebar-buffer
      (binder-sidebar-refresh)
      (when binder-sidebar-select-window
        (select-window (get-buffer-window))))))

;;;###autoload
(define-derived-mode binder-sidebar-mode
  special-mode "Binder Sidebar"
  "Major mode for working with `binder' projects."
  (binder-sidebar-refresh)
  (add-hook 'post-command-hook 'binder-notes-refresh-maybe t t))

(define-key binder-sidebar-mode-map (kbd "g") #'binder-sidebar-refresh)
(define-key binder-sidebar-mode-map (kbd "n") #'next-line)
(define-key binder-sidebar-mode-map (kbd "p") #'previous-line)
(define-key binder-sidebar-mode-map (kbd "RET") #'binder-sidebar-find-file)
(define-key binder-sidebar-mode-map (kbd "o") #'binder-sidebar-find-file-other-window)
(define-key binder-sidebar-mode-map (kbd "s") #'binder-sidebar-save)
(define-key binder-sidebar-mode-map (kbd "m") #'binder-sidebar-mark)
(define-key binder-sidebar-mode-map (kbd "u") #'binder-sidebar-unmark)
(define-key binder-sidebar-mode-map (kbd "#") #'binder-sidebar-set-status)
(define-key binder-sidebar-mode-map (kbd "U") #'binder-sidebar-unmark-all)
(define-key binder-sidebar-mode-map (kbd "v") #'binder-sidebar-staple)
(define-key binder-sidebar-mode-map (kbd "i") #'binder-sidebar-toggle-notes)
(define-key binder-sidebar-mode-map (kbd "z") #'binder-sidebar-open-notes)
(define-key binder-sidebar-mode-map (kbd "M-n") #'binder-sidebar-shift-down)
(define-key binder-sidebar-mode-map (kbd "<M-down>") #'binder-sidebar-shift-down)
(define-key binder-sidebar-mode-map (kbd "M-p") #'binder-sidebar-shift-up)
(define-key binder-sidebar-mode-map (kbd "<M-up>") #'binder-sidebar-shift-up)
(define-key binder-sidebar-mode-map (kbd "a") #'binder-sidebar-add-file)
(define-key binder-sidebar-mode-map (kbd "A") #'binder-sidebar-add-all-files)
(define-key binder-sidebar-mode-map (kbd "d") #'binder-sidebar-remove)
(define-key binder-sidebar-mode-map (kbd "r") #'binder-sidebar-rename)
(define-key binder-sidebar-mode-map (kbd "R") #'binder-sidebar-relocate)
(define-key binder-sidebar-mode-map (kbd "E") #'binder-sidebar-toggle-file-extensions)
(define-key binder-sidebar-mode-map (kbd "x") #'binder-sidebar-toggle-include)
(define-key binder-sidebar-mode-map (kbd "/") #'binder-sidebar-filter-in)
(define-key binder-sidebar-mode-map (kbd "\\") #'binder-sidebar-filter-out)
(define-key binder-sidebar-mode-map (kbd "M-RET") #'binder-sidebar-new-file)


;;; Notes Major Mode

(defcustom binder-notes-buffer
  "*Binder Notes*"
  "Buffer name for Binder notes."
  :type 'string
  :safe 'stringp
  :group 'binder-sidebar)

(defcustom binder-notes-display-alist
  '((side . left)
    (window-width . 50)
    (slot . 1))
  "Association list used to display binder notes buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist
  :group 'binder-sidebar)

(defcustom binder-notes-persistent-window
  t
  "When non-nil, notes will persist when calling `delete-other-windows'.

This marks `no-delete-other-windows' window parameter as non-nil.

Use `binder-toggle-notes' or `quit-window' to close notes."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defcustom binder-notes-keep-in-sync
  t
  "If non-nil, moving point in binder sidebar updates notes."
  :type 'boolean
  :safe 'booleanp
  :group 'binder-sidebar)

(defvar binder--notes-fileid nil)
(defvar binder--notes-display nil)

(defun binder-notes-set-notes ()
  (binder-set-item-prop binder--notes-fileid 'notes
                        (string-trim (buffer-substring-no-properties
                                      (point-min) (point-max))))
  (set-buffer-modified-p nil))

(defun binder-notes-get-notes (directory fileid)
  (setq default-directory directory
        binder--notes-fileid fileid)
  (with-silent-modifications
    (erase-buffer)
    (insert (or (binder-get-item-prop binder--notes-fileid 'notes)
            "")))
  (setq binder--notes-display
        (binder-get-item-prop binder--notes-fileid 'display)))

(defun binder-sidebar-toggle-notes (&optional show select)
  (interactive)
  (let ((display-buffer-mark-dedicated t)
        (directory (expand-file-name default-directory))
        (fileid (binder-sidebar-get-fileid)))
    (with-current-buffer (get-buffer-create binder-notes-buffer)
      (if (get-buffer-window)
          (if show (binder-notes-get-notes directory fileid)
            (delete-windows-on))
        (binder-notes-mode)
        (binder-notes-get-notes directory fileid)
        (display-buffer-in-side-window
         (current-buffer)
         (append binder-notes-display-alist
                 (when binder-sidebar-persistent
                   (list '(window-parameters (no-delete-other-windows . t)))))))
      (when select (select-window (get-buffer-window))))))

(defun binder-sidebar-open-notes ()
  (interactive)
  (binder-sidebar-toggle-notes t t))

(defun binder-notes-commit ()
  (interactive)
  (unless (derived-mode-p 'binder-notes-mode)
    (user-error "Not in %S" 'binder-notes-mode))
  (if (not (buffer-modified-p))
      (message "(No changes need to be added to binder)")
    (binder-notes-set-notes)
    (binder-write)
    (when (window-live-p (get-buffer-window binder-sidebar-buffer))
      (with-current-buffer binder-sidebar-buffer
        (binder-sidebar-refresh)))
    (message "Saved notes for %s to binder"
             (or binder--notes-display binder--notes-fileid))))

(defun binder-notes-expand-window ()
  (interactive)
  (unless (derived-mode-p 'binder-notes-mode)
    (user-error "Not in %S" 'binder-notes-mode))
  (if (window-parameter (selected-window) 'window-side)
      (progn
        (quit-window)
        (pop-to-buffer (get-buffer-create binder-notes-buffer) t))
    (quit-window)
    (binder-sidebar-open-notes)))

(defun binder-notes-refresh-maybe ()
  (while-no-input
    (redisplay)
    (when (and binder-notes-keep-in-sync
               (window-live-p (get-buffer-window binder-notes-buffer))
      (let ((directory (expand-file-name default-directory))
            (fileid (binder-sidebar-get-fileid)))
        (with-current-buffer binder-notes-buffer
          (binder-notes-get-notes directory fileid)))))))

(defcustom binder-notes-mode-hook
  '(visual-line-mode)
  "Hook run after entering Binder Notes Mode."
  :type 'hook
  :options '(visual-line-mode))

;;;###autoload
(define-derived-mode binder-notes-mode
  text-mode "Binder Notes Mode"
  "Major mode for editing `binder' notes."
  ;; (setq default-directory
  ;;       (with-current-buffer binder-sidebar-buffer
  ;;         default-directory))
  (setq header-line-format
        '((:propertize (or binder--notes-display binder--notes-fileid)
                       face bold)
          "  C-c C-c to commit; C-c C-q to quit")))

(define-key binder-notes-mode-map (kbd "C-c C-c") #'binder-notes-commit)
(define-key binder-notes-mode-map (kbd "C-c C-l") #'binder-notes-expand-window)
(define-key binder-notes-mode-map (kbd "C-c C-q") #'quit-window)


;;; Staple Mode

(defgroup binder-staple ()
  "Options for `binder-staple-mode'.
This is for \"stapling\" together multiple binder files."
  :group 'binder)

(defcustom binder-default-stapled-mode
  'text-mode
  "Default major mode when stapling together files"
  :type 'function
  :safe 'functionp
  :group 'binder-staple)

(defcustom binder-staple-separator "\n"
  "String to insert between files when stapling together."
  :type 'string
  :group 'binder-staple)

(defcustom binder-staple-buffer
  "*Binder Staple View*"
  "Buffer name for viewing stapled files."
  :type 'string
  :safe 'stringp
  :group 'binder-staple)

(defun binder-sidebar-staple ()
  (interactive)
  (let ((root (binder-root))
        (item-list
         (seq-filter
          (lambda (item) (alist-get 'include item))
          (binder-filter-structure))))
    (with-current-buffer (get-buffer-create binder-staple-buffer)
      (with-silent-modifications
        (erase-buffer)
        (dolist (item item-list)
          (let ((x (point)))
            (insert-file-contents (car item))
            (goto-char (point-max))
            (insert binder-staple-separator)
            (put-text-property x (point) 'binder-original-file
                               (expand-file-name (car item) root)))))
      (funcall (alist-get 'default-mode (binder-read)))
      (binder-staple-mode t)
      (let ((pop-up-windows binder-sidebar-pop-up-windows))
        (pop-to-buffer (current-buffer) t)))))

(defun binder-staple-find-original-file ()
  (interactive)
  (unless binder-staple-mode
    (user-error "Not in %S" 'binder-staple-mode))
  (let ((original-file (or (get-text-property (point) 'binder-original-file)
                           (get-text-property (1- (point)) 'binder-original-file))))
        ;; (position
        ;;  (cond ((bobp) 1)
        ;;        ((eobp)
        ;;         (previous-single-property-change
        ;;          (point) 'binder-original-file))
        ;;        ((string= (get-text-property (point) 'binder-original-file)
        ;;                  (get-text-property (1- (point)) 'binder-original-file))
        ;;         (or (previous-single-property-change
        ;;              (point) 'binder-original-file)
        ;;             1))
        ;;        (t (point))))
    ;; (setq position (1+ (- (point) position)))
    (find-file-existing original-file)))

(defcustom binder-staple-mode-hook
  '(view-mode)
  "Hook run after entering Binder Staple Mode."
  :type 'hook
  :group 'binder-staple)

(defvar binder-staple-mode-map (make-sparse-keymap))

(define-key binder-staple-mode-map (kbd "M-RET") #'binder-staple-find-original-file)

(define-minor-mode binder-staple-mode
  "Minor mode for viewing files \"stapled\" together."
  :init-value nil)



(provide 'binder)
;;; binder.el ends here
