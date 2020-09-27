# Binder #

[![MELPA Stable](https://stable.melpa.org/packages/binder-badge.svg)](https://stable.melpa.org/#/binder)
[![MELPA](https://melpa.org/packages/binder-badge.svg)](https://melpa.org/#/binder)

![screenshot](https://user-images.githubusercontent.com/1256849/87218460-464a3300-c396-11ea-9ce7-30f7a5bc4377.png)

Binder is global minor mode (and associated major modes) to facilitate
working on a writing project in multiple files. It is heavily inspired
by the binder feature in the [macOS writing app Scrivener][1].

The rationale behind working this way is to split a large writing
project into much smaller pieces.

[1]: https://www.literatureandlatte.com/scrivener/

## Features ##

Primarily, Binder provides a global minor mode `binder-mode`. This
allows working with files in the current `binder-project-directory`.
Data concerning these files is saved in a `.binder.el` file in the
project directory. (You can change the name of this file with the
`binder-default-file` option.)

## Navigation ##

At the most basic level, you can navigate back and forth through the
files in a project:

- `binder-next (C-c ])` visits the next file in the binder, and
- `binder-previous (C-c [)` visits the previous.

Calling these commands activates a transient map so that each command
can be repeated without prefix key/s.

## Sidebar ##

You'll mostly interact with the project structure via the sidebar.

-  `binder-toggle-sidebar (C-c ')` toggles the visibility of the binder
   sidebar
-  `binder-reveal-in-sidebar (C-c ;)` finds the current file in the
   sidebar

Each project items is a file reference relative to the project
directory. Items are displayed as:

    x * name        #tag1 #tag2

These things mean:

|------|-------------------------------------------------|
| x    | item is included when concatenating the project |
| *    | item has notes                                  |
| ?    | the item's corresponding file cannot be found   |
| name | the file name (or custom display name)          |
| #tag | arbitrary item tags                             |
|------|-------------------------------------------------|

An item's display name can be changed with `binder-sidebar-rename (r)`.
If a file cannot be found, relocate with `binder-sidebar-relocate (R)`.

Calling `binder-sidebar-find-file (RET)` will visit the corresponding
file.

To add an existing file, call `binder-sidebar-add-file (a)` or add all
files in directory with `binder-sidebar-add-all-files (A)`.

Add a new file with `binder-sidebar-new-file (M-RET)`. This prompts for a
file-name and adds this (possibly non-existent) file to the project
after the current file's index. If no file-name extension is provided,
use `binder-default-file-extension`.

> Hint: you can use an alternate default file extension for different
> projects by setting a directory local variable.

Files can also be added to a project from outside the sidebar with
`binder-add-file (C-c :)`.

Remove items with `binder-sidebar-remove (d)` -- this *does not delete the
files*, only removes them from the project, but it *does delete* the
corresponding notes and tags.

Items can be reordered with `binder-sidebar-shift-up (M-p | M-up)` and
`binder-sidebar-shift-down (M-n | M-down)`.

Hide item file extensions by setting the `binder-sidebar-hide-file-extensions`
option. This can be toggled with `binder-sidebar-toggle-file-extensions (E)`.

The sidebar can be resized with `binder-sidebar-shrink-window ({)` and
`binder-sidebar-enlarge-window (})`. The window size is changed by the
number of columns specified in option `binder-sidebar-resize-window-step`.

You can customize how the sidebar window is displayed by setting
`binder-sidebar-display-alist` option.

### Marking ###

Multiple items can be marked to add tags, toggle include state or
delete.

Call `binder-sidebar-mark (m)` to mark an item and `binder-sidebar-unmark (u)`
to unmark an item or `binder-sidebar-unmark-all (U)` for all sidebar items.

### Tags ###

A project is strictly a linear list. As your project grows, you may find
the number of items becomes unweidly. Tags can help organize a project.
An item can have any number of tags.

Add a tag to an item with `binder-sidebar-add-tag (t)`. Remove a tag
from an item with `binder-sidebar-remove-tag (T)`. You can tag/untag
multiple items at once by using marks.

Items listed in the sidebar can be narrowed to only show items with a
certain tag with `binder-sidebar-narrow-by-tag (/)` and/or only show
items without a certain tag with `binder-sidebar-exclude-by-tag (\)`.
Each of these commands can be called multiple times with additional
tags. Reset the sidebar with `binder-sidebar-refresh (g)`.

## Notes ##

Project items can have notes, which are stored within the project file.

To open the notes buffer from the sidebar, call either
`binder-sidebar-open-notes (z)` to (open and) select the notes window,
or `binder-sidebar-toggle-notes (i)` to toggle the window. To open a
project file's notes when visiting that file, call `binder-toggle-notes
(C-c ")`.

> n.b. Notes are not automatically saved.

Calling `quit-window (C-c C-q | C-c C-k)` or `binder-toggle-sidebar`
does not save notes. You need to call either `binder-notes-save (C-x C-s)`
or `binder-notes-save-and-quit-window (C-c C-c)`.

You can embiggen the notes window, to pop it out from the sidebar and
edit like a regular buffer window, with `binder-notes-expand-window (C-c C-l)`.

If you want the notes buffer to stay in sync with the item under the
cursor in the sidebar, change the option `binder-notes-keep-in-sync`. This
can be disconcerting, and again, notes are not automatically saved!

You can customize how the notes window is displayed by setting
`binder-notes-display-alist` option.

## Concatenate ##

A writing project written in discrete pieces probably has an end goal of
being put together. Each Binder project item has a property of being
"included" or not. In the sidebar, an item's include state is toggled
with `binder-sidebar-toggle-include (x)`.

When calling `binder-sidebar-concat (c | v)`, project items marked as
included will be concatenated in a new buffer (separated by
`binder-concat-separator` string.) The default mode of this buffer is set
by `binder-default-concat-mode`.

> Hint: you can use an alternate default mode for different projects by
> setting a directory local variable.

In this buffer, calling `binder-concat-find-original (C-c RET)` will
visit the original file corresponding to the text at point.

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
[markdown]: https://jblevins.org/projects/markdown-mode/
[fountain]: https://github.com/rnkn/fountain-mode

## Requirements ##

- Emacs 24.4
- seq 2.20

## Bugs and Feature Requests ##

Report bugs and feature requests at:
<https://github.com/rnkn/binder/issues>

## Tutorial ##

Binder comes with a tutorial. Calling `M-x binder-tutorial` will prompt
for an empty directory in which to generate the tutorial files.
