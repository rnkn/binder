PROGRAM		= binder
LISP_FILE	= $(PROGRAM).el
DEPS		= seq package-lint
NEWS_FILE	= NEWS
VERS		= $(shell grep -oE -m1 'Version:[ 0-9.]+' $(LISP_FILE) | tr -d :)
TAG			= $(shell echo $(VERS) | sed -E 's/Version:? ([0-9.]+)/v\1/')
INIT		= '(progn \
  (require (quote package)) \
  (push (cons "melpa" "https://melpa.org/packages/") package-archives) \
  (package-initialize) \
  (mapc (lambda (pkg) \
          (unless (package-installed-p pkg) \
            (unless (assoc pkg package-archive-contents) \
              (package-refresh-contents)) \
            (package-install pkg))) \
        (quote ($(DEPS)))))'

all: check compile

check: $(LISP_FILE)
	emacs -Q --eval $(INIT) --batch -f package-lint-batch-and-exit $(LISP_FILE)

compile: $(LISP_FILE)
	emacs -Q --eval $(INIT) -L . --batch -f batch-byte-compile $(LISP_FILE)

tag-release: check compile
	sed -i~ '1 s/.*/* $(VERS)/' $(NEWS_FILE)
	git commit -m 'Add $(VERS) to $(NEWS_FILE)' $(NEWS_FILE)
	awk '/^* Version/ {v ++ 1} v == 1' $(NEWS_FILE) | sed 's/^* //' | git tag -sF - $(TAG)

clean:
	rm -f $(PROGRAM).elc
