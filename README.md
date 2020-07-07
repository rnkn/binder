# Binder #

**Warning: this is alpha-level software. Although it does not touch
files on disk, you should consider all binder data (project structure,
notes, statuses) as susceptible to loss. Key bindings, variable and
function names, and overall program design are subject to change.**

![screenshot](https://user-images.githubusercontent.com/1256849/76825591-1ebee000-6866-11ea-93db-a533485cdd7e.png)

Binder is global minor mode (and associated major modes) to facilitate
working on a writing project in multiple files. It is heavily inspired
by the binder feature in the [macOS writing app Scrivener][scriv].

The rationale behind working this way is to split a large writing
project into much smaller pieces.

[scriv]: https://www.literatureandlatte.com/scrivener/

## Features ##

Primarily, Binder provides a global minor mode `binder-mode`. This
allows working with files in the current `binder-project-directory`.
Data concerning these files is saved in a `.binder.el` file in the
project directory. (You can change the name of this file with the
`binder-default-file` option.)

## Navigation ##

At the most basic level, you can navigate back and forth through the
files in a project:

- `binder-next` (`C-c ]`) visits the next file in the binder, and
- `binder-previous` (`C-c [`) visits the previous.

Calling these commands activates a transient map so that each command
can be repeated by repeating only the last key.

## Sidebar ##

You'll mostly interact with the project structure via the sidebar.

- `binder-toggle-sidebar` (`C-c '`) toggles the visibility of the binder
  sidebar.
- `binder-reveal-in-sidebar` (`C-c ;`) finds the current file in the
  sidebar.

Each project item is a file, referenced relative to the project
directory. Project items are displayed in a linear ordered list. Calling
`binder-sidebar-find-file` (bound to `RET`) or
`binder-sidebar-find-file-other-window` (bound to `o`) will visit the
corresponding file.

Each item in the sidebar displays the following information:

1. `binder-sidebar-include-char` (default `x`) denotes that this item is
   included when the project is "joined" (see below).
2. `binder-sidebar-notes-char` (default `*`) denotes that this item has
   some notes, which can be edited in `binder-notes-mode` (see below),
   or...
3. `binder-sidebar-missing-char` (default `?`) denote that the item's
   corresponding file cannot be found, but can be relocated by calling
   `binder-sidebar-relocate` (`R`).
4. The item name, which is either the file relative to the project
   directory or an arbitrary display name, which can be set by calling
   `binder-sidebar-rename` (`r`).
5. The item tags, each prefixed with `binder-sidebar-status-char`
   (default `#`). The tags column can be set with the
   `binder-sidebar-tags-column` option.

To add an item, call `binder-sidebar-add-file` (`a`) or add all files in
directory with `binder-sidebar-add-all-files` (`A`).

Add a new file with `binder-sidebar-new-file` (`M-RET`). This prompts
for a file-name and adds this (possibly non-existent) file to the
project after the current file's index. If no file-name extension is
provided, use value of the current project's `default-extension`
property (default set with `binder-default-file-extention` option).

Files can also be added to a project from outside the sidebar with
`binder-add-file` (`C-c :`).

Remove items with `binder-sidebar-remove` (`d`) -- this *does not delete
the files*, only removes them from the project, but it *does delete* the
corresponding notes and tags.

Items can be reordered with `binder-sidebar-shift-up` (`M-p` or `M-up`)
and `binder-sidebar-shift-down` (`M-n` or `M-down`).

Hide item file extensions by setting the
`binder-sidebar-hide-file-extensions` option. This can be toggled with
`binder-sidebar-toggle-file-extensions` (`E`).

The sidebar can be resized with `binder-sidebar-shrink-window` (`{`) and
`binder-sidebar-enlarge-window` (`}`). The window size is changed by the
number of columns specified in option
`binder-sidebar-resize-window-step`.

You can customize how the sidebar window is displayed by setting
`binder-sidebar-display-alist` option.

### Tags ###

A project is strictly a linear list. As your project grows, you may find
the number of items becomes unweidly. Tags can help organize a project.
An item can have any number of tags.

Add a tag to an item with `binder-sidebar-add-tag` (`t`). Remove a tag
from an item with `binder-sidebar-remove-tag` (`T`).

Items listed in the sidebar can be narrowed to only show items with a
certain tag with `binder-sidebar-narrow-by-tag` (`/`) and/or only show
items without a certain tag with `binder-sidebar-exclude-by-tag` (`\`).
Each of these commands can be called multiple times with additional
tags. Reset the sidebar with `binder-sidebar-refresh` (`g`).

### Marking ###

Multiple items can be marked to add tags or toggle include state.

Call `binder-sidebar-mark` (`m`) to mark an item. Call
`binder-sidebar-unmark` (`u`) to unmark an item or
`binder-sidebar-unmark-all` (`U`) for all sidebar items.

## Notes ##

Project items can have notes, which are stored within the project file.

To open the notes buffer from the sidebar, call either
`binder-sidebar-open-notes` (`z`) or `binder-sidebar-toggle-notes`
(`i`). To open a project file's notes when visiting that file, call
`binder-toggle-notes` (`C-c "`).

n.b. *Notes are not automatically saved*.

Calling `quit-window` (`C-c C-q`) or `binder-toggle-sidebar` does not
save notes. You need to call either `binder-notes-save` (`C-x C-s`) or
`binder-notes-save-and-quit-window` (`C-c C-c`).

You can embiggen the notes window, to pop it out from the sidebar and
edit like a regular buffer window, with `binder-notes-expand-window`
(`C-c C-l`).

If you want the notes buffer to stay in sync with the item under the
cursor in the sidebar, change the option `binder-notes-keep-in-sync`,
but again, notes are not automatically saved!

You can customize how the notes window is displayed by setting
`binder-notes-display-alist` option.

## Concatenate ##

A writing project written in discrete pieces probably has an end goal of
being put together. Each Binder project item has a property of being
"included" or not. In the sidebar, an item's include state is toggled
with `binder-sidebar-toggle-include` (`x`).

When calling `binder-sidebar-concat` (`c` or `v`), project items marked
as included will be concatenated in a new buffer (separated by
`binder-concat-separator` string.)

In this buffer, calling `binder-concat-find-original-file` (bound to
`M-RET`) will visit the original file corresponding to the text at
point.

## Why not just use Org Mode? ##

[Org Mode][] is nice, but it's also a very *heavy* tool that almost
insists that everything be done within Org Mode. This isn't useful if
you want to write in a different format, e.g. [Markdown][] or
[Fountain][].

Also, I prefer to keep my writing in a collection of separate text
files. It feels nicer to work on something small and self-contained than
to organize a large file with headings and use indirect buffers with
narrowing.

[org mode]: https://orgmode.org
[markdown]: http://jblevins.org/projects/markdown-mode/
[fountain]: https://github.com/rnkn/fountain-mode

## Requirements ##

- Emacs 25.3

## Bugs and Feature Requests ##

Report bugs and feature requests at: <https://github.com/rnkn/binder/issues>

## Start Here ##

This file is part of a Binder tutorial project. Enabled `binder-mode`
and type `C-c ;` to reveal this file in the binder siderbar.
