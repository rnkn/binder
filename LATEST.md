Version 0.4.3

- Use file-relative-name for binder-file-relative-to-root -- this will
  set a fileid outside of the project directory as a relative path
  instead of absolute
- Add faces binder-sidebar and binder-notes with face remap to default
- Quit and kill binder side windows in all frames on quit
- Remove call to hack-local-variables from binder-sidebar-refresh, as
  this can cause a segfault
- Make safer use of set-transient-map
- Add hints for clearing sidebar tag filters (g key)
- Add binder-theme.el to contrib/
- Small imporvements to tutorial
- Track known issues with FIXME comments in source
- Stipulate acknowledgement of Scrivener in redistribution terms
- Update email, and upstream URL to https://git.skeletons.cc/binder
- Update repository for portability (add a screenshot, no more relying
  on GitHub stuff)
