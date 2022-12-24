;;; binder-tutorial.el --- Tutorial for Binder projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Paul W. Rankin

;; Author: Paul W. Rankin <pwr@skeletons.cc>
;; Keywords: help, wp, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains code to generate a Binder project tutorial. It is
;; loaded upon calling `binder-tutorial'.


;;; Code:

(require 'binder)

(defconst binder-tutorial-items
  (list
   (cons ".binder.el"
         (concat binder-file-header "\
((\"projects.txt\"
  (tags \"start-here\"))
 (\"sidebar.txt\"
  (tags \"foo\" \"bar\"))
 (\"navigation.txt\")
 (\"marks.txt\")
 (\"notes.txt\"
  (notes . \"Some notes about notes.\n\nNullam eu ante vel est convallis dignissim. Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio. Nunc porta vulputate tellus. Nunc rutrum turpis sed pede. Sed bibendum. Aliquam posuere. Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio.\"))
 (\"tags.txt\"
  (tags \"foo\"))
 (\"errors.txt\"))"))
   (cons "projects.txt" "\
A project in Binder is a linear list of files with associated
information (i.e. their order, notes, and tags). This information
is stored in a .binder.el file in the top-level directory of the
project.

You can only work on one project at a time in one Emacs session.

When locating the current project file, Binder looks in the current
directory then each parent directory, with the first found taking
precedence. Consider the following file layout:

    ├── .binder.el      <-- project A
    ├── foo.txt
    ├── dir1
    │   └── bar.txt
    └── dir2
        ├── .binder.el  <-- project B
        └── baz.txt

Here project A could reference files foo.txt, bar.txt and baz.txt.
However when visiting file baz.txt, project B would take precedence.

Most management of a project will happen via the sidebar. Type C-c ] to
go to sidebar.txt.")
   (cons "sidebar.txt" "\
By default items in the sidebar will show:

    * name        #tag1 #tag2

These things mean:

    *       the item has project notes
    name    either the item file name, or a custom item display name
    #tag    arbitrary item tag/s

You can move about in the sidebar by typing n and p, < and > and visit a
file by simply typing RET. Jump to the current item with j. You can
quickly resize the sidebar with { and }.

You can rearrange items with M-n and M-p. You can toggle file extensions
with E.

Type C-c ' to toggle the sidebar on/off. When visiting a project file,
type C-c ; to reveal it in the sidebar.

Try changing this item's display name with r. You can revert this by
doing the same and entering nothing.

Binder will automatically write its current project file after a certain
number of changes, but you can manually save a project by typing s.

Type C-c ] to go to the next file.")
   (cons "navigation.txt" "\
You can navigate through project items forward and backward with C-c ]
and C-c [, which means Emacs will visit each file in the project list in
succession.

Try typing C-c [ to go back to a previous file. You don't need to prefix
C-c each time, so you can type e.g. C-c [ [ ] ] ] to repeatedly navigate
backward/forward.

The highlight in the sidebar lets you know where you are so you don't
get lost.

Type C-c ] to go to the next file.")
   (cons "marks.txt" "\
You can mark multiple project items in the sidebar by typing m. Unmark
an item with u or unmark all items with U.

Marking items allows you to perform actions on multiple items at once.

Type C-c ] to go to the next file.")
   (cons "notes.txt" "\
Each item can have notes, which are stored in the project file. In the
sidebar, type z to open this item's notes.

Add or change the notes and type C-c C-c (save notes and close window)
or C-x C-s (save notes without closing window). To quit the notes window
without saving the notes, type either C-c C-q or C-c C-k.

You can toggle the notes window without selecting it by typing i.

To really mix things up, open the notes window and, with it selected,
type C-c C-l. This pops the window out to be a regular window (instead
of a side-window). This command works as a toggle.

A file's notes can be edited without opening the sidebar with C-c \".

Type C-c ] to go to the next file.")
   (cons "tags.txt" "\
Each project item can have any number of arbitrary tags, which are
prefixed with # by default.

To add a tag to an item, type t and enter the tag. To remove a tag, type
T and enter the tag. These commands work either with the item at point,
or all marked items.

Some tags are already added to play with. Type / then \"foo\" to narrow
the sidebar to only items tagged \"foo\". Now type \\ then \"bar\" to exclude
sidebar items tagged \"bar\".

You should have a sidebar with only this item showing.

Filtering the sidebar using tags allows a lot of flexibility with which
project files you want to work with in the sidebar.

When the sidebar items are filtered, navigating backward/forward only
considers those items.

Type | to clear the sidebar filters, then C-c ] to go to the next file.")
   (cons "errors.txt" "\
Binder caches project information to avoid contious reading and writing
to disk. However this means errors may occur if a project is moved on
disk while project information is cached. If you plan on moving a
projects on disk it's best to deactivate binder-mode first.")))

;;;###autoload
(defun binder-tutorial (directory)
  "Generate the Binder tutorial project in empty DIRECTORY."
  (interactive "DGenerate tutorial project in directory: ")
  (let ((default-directory directory))
    (mapc (lambda (item)
            (let ((old-buffer (get-buffer (car item))))
              (when (buffer-live-p old-buffer) (kill-buffer old-buffer)))
            (with-temp-buffer
              (let ((require-final-newline t))
                (insert (cdr item))
                (write-file (car item)))))
          binder-tutorial-items)
    (binder-mode 1)
    (binder-sidebar-create-window)
    (select-window (get-buffer-window binder-sidebar-buffer))))



(provide 'binder-tutorial)
;;; binder-tutorial.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; indent-tabs-mode: nil
;; require-final-newline: t
;; sentence-end-double-space: nil
;; End:
