# Binder #

**Warning: this is alpha-level software. Although it does not touch files on
disk, you should consider all binder data (project structure, notes, statuses)
as susceptible to loss. Key bindings, variable and function names, and overall
program design are subject to change.**

![screenshot](https://user-images.githubusercontent.com/1256849/75653252-ec7b8300-5ca8-11ea-9d7d-af6a528abc2c.png)

Binder is global minor mode (and associated major modes) to facilitate working
on a writing project in multiple files. It is heavily inspired by the binder
feature in the [macOS writing app Scrivener][scriv].

The rationale behind working this way is to split a large writing project into
much smaller pieces.

[scriv]: https://www.literatureandlatte.com/scrivener/features?os=macOS

## Features ##

Primarily, Binder provides a global minor mode `binder-mode`. This allows
working with files in the current binder data, which is saved in a `.binder.el`
file in the current directory. (You can change the name of this file is
`binder-default-file` option.)

At this top-level, the main interaction with your binder will be in navigating
back and forth between binder files:

- `binder-next` (bound to `C-c ]`) visits the next file in the binder, and
- `binder-previous` (bound to `C-c [`) visits the previous.

Calling these commands activates a transient map so that each command can be
repeated by repeating only the last key.

You probably want some idea of the structure of your binder project...

- `binder-reveal-in-sidebar` (bound to `C-c ;`) will find the current file in
  the binder sidebar (see below) or call `binder-init-binder-file` if there is
  none.
- `binder-toggle-sidebar` (bound to `C-c '`) toggles the visibility of the
  binder sidebar.

And when you're writing and want to quickly add something new, you can with...

- `binder-add-file` (bound to `C-c :`) prompts for a file-name and adds this
  possibly non-existent file to the binder after the current file's index. If no
  file-name extension is provided, use value of the binder's `default-extension`
  property (set with `binder-default-file-extention` option).

### Binder Sidebar Mode ###

A major mode for displaying the binder sidebar. This is where your main
interaction with the binder happens.

Binder items are displayed in a linear ordered list. Calling
`binder-sidebar-find-file` (bound to `RET`) or
`binder-sidebar-find-file-other-window` (bound to `o`) will visit the
corresponding file.

Each item in the binder sidebar displays the following information:

1. `binder-sidebar-include-char` (default `x`) denotes that this item has a
   non-nil value for its `include` property and therefore is included in
   `binder-stable-mode` (see below).
2. `binder-sidebar-notes-char` (default `*`) denotes that this item has a string
   value for its `notes` property, which can be edited in `binder-notes-mode`
   (see below), or
3. `binder-sidebar-missing-char` (defautl `?`) denote that the item's
   corresponding file cannot be found, but can be relocated by calling
   `binder-sidebar-relocate` (bound to `R`).
4. The item name, either the `car` of the item element or its `display`
   property, which can be set by calling `binder-sidebar-rename` (bound to `r`).
5. The item `status` property value, prefixed with `binder-sidebar-status-char`
   (default `#`). The display of this value can be set with the
   `binder-sidebar-status-column` option.

Add an item with `binder-sidebar-add-file` (bound to `a`) or add all files in
directory with `binder-sidebar-add-all-files` (bound to `A`). Add a new file
with `binder-sidebar-new-file` (bound to `M-RET`). Remove items with
`binder-sidebar-remove` (bound to `d`) -- this *does not delete the files*, only
removes them from the binder.

Items can be reordered with `binder-sidebar-shift-up` (bound to `M-p` & `M-up`)
and `binder-sidebar-shift-down` (bound to `M-n` & `M-down`).

Each item's include state is toggled with `binder-sidebar-toggle-include` (bound
to `x`).

Each item can be given a status with `binder-sidebar-set-status` (bound to `#`).
Filter item by including or excluding a status with `binder-sidebar-filter-in`
(bound to `/`) and `binder-sidebar-filter-out` (bound to `\`). To clear a
status, just set an empty string. (n.b. Each command filter in/out only a single
status, therefore it won't make sense to use them in conjunction.)

The notes buffer (see below) can be accessed with either
`binder-sidebar-open-notes` (bound to `z`) or `binder-sidebar-toggle-notes`
(bound to `i`).

Hide item file extensions by setting the `binder-sidebar-hide-file-extensions`
option. This can be toggled with `binder-sidebar-toggle-file-extensions` (bound
to `E`).

You can customize how the sidebar window is displayed by setting
`binder-sidebar-display-alist` option.

(There is a "mark" functionality, but this is yet to be implemented beyond just
temporarily making items look marked.)

### Binder Notes Mode ###

A major mode for editing binder notes.

Notes are only saved to the binder when calling `binder-notes-commit` (bound to
`C-c C-c`). Calling `quit-window` (bound to C-c C-q) or `binder-toggle-sidebar`
does not save notes.

By default, the notes window will update to the corresponding item notes
whenever the cursor moves in the binder sidebar. This may be disconcerting, so
you can change it by setting the `binder-notes-keep-in-sync` option.

If the notes side window feels too small, you can pop the buffer out to a
regular sized window with `binder-notes-expand-window` (bound to `C-c C-l`).

You can customize how the notes window is displayed by setting
`binder-notes-display-alist` option.

### Binder Staple Mode ###

A minor mode for "stapling" binder files together.

When calling `binder-sidebar-staple` (bound to `v`), items marked as included in
the binder will be concatenated in a new buffer, separated by
`binder-staple-separator` string.

In this buffer, calling `binder-staple-find-original-file` (bound to `M-RET`)
will visit the original file corresponding to the text at point.

## Why not just use Org Mode? ##

[Org Mode][] is nice, but it's also a very *heavy* tool that almost insists that
everything be done within Org Mode. This isn't useful if you want to write in a
different format, e.g. [Markdown][] or [Fountain][].

Also, I prefer to keep my writing in a collection of separate text files. It
feels nicer to work on something small and self-contained than to organize a
large file with headings and use indirect buffers with narrowing.

[org mode]: https://orgmode.org
[markdown]: http://jblevins.org/projects/markdown-mode/
[fountain]: https://github.com/rnkn/fountain-mode

## Requirements ##

- Emacs 25.3

## Bugs and Feature Requests ##

Report bugs and feature requests at: <https://github.com/rnkn/binder/issues>

## Start Here ##

This file is part of a sample binder project. Enabled `binder-mode` and type `C-c ;`
to reveal this file in the binder siderbar.
