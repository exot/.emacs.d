;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Custom-mode-abbrev-table '())

(define-abbrev-table 'Info-edit-mode-abbrev-table '())

(define-abbrev-table 'Man-mode-abbrev-table '())

(define-abbrev-table 'Rd-mode-abbrev-table
  '(
    ("`ag" "\\arguments" nil 0)
    ("`al" "\\alias" nil 0)
    ("`au" "\\author" nil 0)
    ("`bf" "\\bold" nil 0)
    ("`co" "\\code" nil 0)
    ("`de" "\\describe" nil 0)
    ("`dn" "\\description" nil 0)
    ("`dt" "\\details" nil 0)
    ("`em" "\\emph" nil 0)
    ("`en" "\\enumerate" nil 0)
    ("`ex" "\\examples" nil 0)
    ("`fi" "\\file" nil 0)
    ("`fo" "\\format" nil 0)
    ("`it" "\\item" nil 0)
    ("`iz" "\\itemize" nil 0)
    ("`kw" "\\keyword" nil 0)
    ("`li" "\\link" nil 0)
    ("`me" "\\method" nil 0)
    ("`na" "\\name" nil 0)
    ("`no" "\\note" nil 0)
    ("`re" "\\references" nil 0)
    ("`sa" "\\seealso" nil 0)
    ("`se" "\\section" nil 0)
    ("`so" "\\source" nil 0)
    ("`ss" "\\subsection" nil 0)
    ("`sy" "\\synopsis" nil 0)
    ("`ta" "\\tabular" nil 0)
    ("`ti" "\\title" nil 0)
    ("`us" "\\usage" nil 0)
    ("`va" "\\value" nil 0)
   ))

(define-abbrev-table 'TeX-error-overview-mode-abbrev-table '())

(define-abbrev-table 'TeX-output-mode-abbrev-table '())

(define-abbrev-table 'apropos-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table '())

(define-abbrev-table 'bbdb-mode-abbrev-table '())

(define-abbrev-table 'biblio-selection-mode-abbrev-table '())

(define-abbrev-table 'bibtex-mode-abbrev-table '())

(define-abbrev-table 'bookmark-bmenu-mode-abbrev-table '())

(define-abbrev-table 'bookmark-edit-annotation-mode-abbrev-table '())

(define-abbrev-table 'bsh-script-mode-abbrev-table '())

(define-abbrev-table 'c++-mode-abbrev-table '())

(define-abbrev-table 'c-mode-abbrev-table '())

(define-abbrev-table 'calc-trail-mode-abbrev-table '())

(define-abbrev-table 'calendar-mode-abbrev-table '())

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'cider-browse-ns-mode-abbrev-table '())

(define-abbrev-table 'cider-connections-buffer-mode-abbrev-table '())

(define-abbrev-table 'cider-docview-mode-abbrev-table '())

(define-abbrev-table 'cider-inspector-mode-abbrev-table '())

(define-abbrev-table 'cider-repl-history-mode-abbrev-table '())

(define-abbrev-table 'cider-repl-mode-abbrev-table '())

(define-abbrev-table 'cider-stacktrace-mode-abbrev-table '())

(define-abbrev-table 'cider-test-report-mode-abbrev-table '())

(define-abbrev-table 'cljr--change-signature-mode-abbrev-table '())

(define-abbrev-table 'clojure-mode-abbrev-table '())

(define-abbrev-table 'clojurec-mode-abbrev-table '())

(define-abbrev-table 'clojurescript-mode-abbrev-table '())

(define-abbrev-table 'clojurex-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'conf-colon-mode-abbrev-table '())

(define-abbrev-table 'conf-javaprop-mode-abbrev-table '())

(define-abbrev-table 'conf-ppd-mode-abbrev-table '())

(define-abbrev-table 'conf-space-mode-abbrev-table '())

(define-abbrev-table 'conf-unix-mode-abbrev-table '())

(define-abbrev-table 'conf-windows-mode-abbrev-table '())

(define-abbrev-table 'conf-xdefaults-mode-abbrev-table '())

(define-abbrev-table 'cperl-mode-abbrev-table
  '(
    ("=head1" "=head1" cperl-electric-pod 0)
    ("=head2" "=head2" cperl-electric-pod 0)
    ("=over" "=over" cperl-electric-pod 0)
    ("=pod" "=pod" cperl-electric-pod 0)
    ("continue" "continue" cperl-electric-else 0)
    ("do" "do" cperl-electric-keyword 0)
    ("else" "else" cperl-electric-else 4)
    ("elsif" "elsif" cperl-electric-keyword 1)
    ("for" "for" cperl-electric-keyword 2)
    ("foreach" "foreach" cperl-electric-keyword 0)
    ("foreachmy" "foreachmy" cperl-electric-keyword 0)
    ("formy" "formy" cperl-electric-keyword 0)
    ("head1" "head1" cperl-electric-pod 0)
    ("head2" "head2" cperl-electric-pod 0)
    ("if" "if" cperl-electric-keyword 4)
    ("over" "over" cperl-electric-pod 0)
    ("pod" "pod" cperl-electric-pod 0)
    ("unless" "unless" cperl-electric-keyword 0)
    ("until" "until" cperl-electric-keyword 0)
    ("usedump" "use Data::Dumper qw(Dumper);" pde-abbv-no-blank 0)
    ("useenc" "use Encode qw(encode decode from_to);" pde-abbv-no-blank 0)
    ("usegtk" "use Gtk2 '-init';
use Glib qw(TRUE FALSE);

my $window = Gtk2::Window->new('toplevel');
$window->signal_connect('delete_event' => sub { Gtk2->main_quit; });" pde-abbv-no-blank 0)
    ("useopt" "use Getopt::Long;
GetOptions();" pde-abbv-no-blank 0)
    ("while" "while" cperl-electric-keyword 4)
   ))

(define-abbrev-table 'css-mode-abbrev-table '())

(define-abbrev-table 'data-debug-mode-abbrev-table '())

(define-abbrev-table 'debugger-mode-abbrev-table '())

(define-abbrev-table 'diary-fancy-display-mode-abbrev-table '())

(define-abbrev-table 'diary-mode-abbrev-table '())

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'dig-mode-abbrev-table '())

(define-abbrev-table 'direx:direx-mode-abbrev-table '())

(define-abbrev-table 'display-time-world-mode-abbrev-table '())

(define-abbrev-table 'doctex-mode-abbrev-table '())

(define-abbrev-table 'dsssl-mode-abbrev-table '())

(define-abbrev-table 'edebug-eval-mode-abbrev-table '())

(define-abbrev-table 'edit-abbrevs-mode-abbrev-table '())

(define-abbrev-table 'edit-list-mode-abbrev-table '())

(define-abbrev-table 'elisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'elpy-refactor-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'emms-lyrics-mode-abbrev-table '())

(define-abbrev-table 'emms-show-all-mode-abbrev-table '())

(define-abbrev-table 'emms-tag-editor-mode-abbrev-table '())

(define-abbrev-table 'epa-info-mode-abbrev-table '())

(define-abbrev-table 'epa-key-list-mode-abbrev-table '())

(define-abbrev-table 'epa-key-mode-abbrev-table '())

(define-abbrev-table 'ert-results-mode-abbrev-table '())

(define-abbrev-table 'ert-simple-view-mode-abbrev-table '())

(define-abbrev-table 'eshell-mode-abbrev-table '())

(define-abbrev-table 'ess-julia-mode-abbrev-table '())

(define-abbrev-table 'finder-mode-abbrev-table '())

(define-abbrev-table 'flycheck-error-list-mode-abbrev-table '())

(define-abbrev-table 'ftp-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'gap-mode-abbrev-table '())

(define-abbrev-table 'gap-process-mode-abbrev-table '())

(define-abbrev-table 'gdb-script-mode-abbrev-table '())

(define-abbrev-table 'geiser-messages-mode-abbrev-table '())

(define-abbrev-table 'geiser-repl-mode-abbrev-table '())

(define-abbrev-table 'gfm-mode-abbrev-table '())

(define-abbrev-table 'git-commit-mode-abbrev-table '())

(define-abbrev-table 'git-rebase-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("brd" "Best regards,

  Daniel" nil 6)
    ("brdb" "Best regards,

  Daniel Borchmann" nil 0)
    ("btw" "by the way" nil 4)
    ("wrt" "with respect to" nil 1)
   ))

(define-abbrev-table 'gnus-article-edit-mode-abbrev-table '())

(define-abbrev-table 'gnus-article-mode-abbrev-table '())

(define-abbrev-table 'gnus-browse-mode-abbrev-table '())

(define-abbrev-table 'gnus-category-mode-abbrev-table '())

(define-abbrev-table 'gnus-custom-mode-abbrev-table '())

(define-abbrev-table 'gnus-edit-form-mode-abbrev-table '())

(define-abbrev-table 'gnus-group-mode-abbrev-table '())

(define-abbrev-table 'gnus-score-mode-abbrev-table '())

(define-abbrev-table 'gnus-sticky-article-mode-abbrev-table '())

(define-abbrev-table 'google-maps-static-mode-abbrev-table '())

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'gud-mode-abbrev-table '())

(define-abbrev-table 'haskell-cabal-mode-abbrev-table '())

(define-abbrev-table 'haskell-compilation-mode-abbrev-table '())

(define-abbrev-table 'haskell-debug-mode-abbrev-table '())

(define-abbrev-table 'haskell-error-mode-abbrev-table '())

(define-abbrev-table 'haskell-interactive-mode-abbrev-table '())

(define-abbrev-table 'haskell-mode-abbrev-table '())

(define-abbrev-table 'haskell-presentation-mode-abbrev-table '())

(define-abbrev-table 'helm-grep-mode-abbrev-table '())

(define-abbrev-table 'helm-major-mode-abbrev-table '())

(define-abbrev-table 'helm-moccur-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'html-mode-abbrev-table '())

(define-abbrev-table 'hy-mode-abbrev-table '())

(define-abbrev-table 'ibuffer-mode-abbrev-table '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'image-dired-display-image-mode-abbrev-table '())

(define-abbrev-table 'image-dired-thumbnail-mode-abbrev-table '())

(define-abbrev-table 'imenu-tree-mode-abbrev-table '())

(define-abbrev-table 'inf-perl-mode-abbrev-table '())

(define-abbrev-table 'inferior-emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'inferior-haskell-mode-abbrev-table '())

(define-abbrev-table 'inferior-julia-mode-abbrev-table '())

(define-abbrev-table 'inferior-lisp-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'inferior-scheme-mode-abbrev-table '())

(define-abbrev-table 'ivy-occur-grep-mode-abbrev-table '())

(define-abbrev-table 'ivy-occur-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table '())

(define-abbrev-table 'jde-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
   ))

(define-abbrev-table 'jde-run-mode-abbrev-table '())

(define-abbrev-table 'julia-mode-abbrev-table '())

(define-abbrev-table 'latex-mode-abbrev-table
  '(
    ("sqc" "sqcap" nil 2)
    ("sqse" "sqsubseteq" nil 1)
    ("sqsn" "sqsubsetneq" nil 0)
   ))

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'literate-haskell-mode-abbrev-table '())

(define-abbrev-table 'locate-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'magit-annex-list-mode-abbrev-table '())

(define-abbrev-table 'magit-annex-unused-mode-abbrev-table '())

(define-abbrev-table 'magit-branch-manager-mode-abbrev-table '())

(define-abbrev-table 'magit-cherry-mode-abbrev-table '())

(define-abbrev-table 'magit-commit-mode-abbrev-table '())

(define-abbrev-table 'magit-diff-mode-abbrev-table '())

(define-abbrev-table 'magit-log-mode-abbrev-table '())

(define-abbrev-table 'magit-log-select-mode-abbrev-table '())

(define-abbrev-table 'magit-merge-preview-mode-abbrev-table '())

(define-abbrev-table 'magit-mode-abbrev-table '())

(define-abbrev-table 'magit-popup-mode-abbrev-table '())

(define-abbrev-table 'magit-process-mode-abbrev-table '())

(define-abbrev-table 'magit-reflog-mode-abbrev-table '())

(define-abbrev-table 'magit-refs-mode-abbrev-table '())

(define-abbrev-table 'magit-repolist-mode-abbrev-table '())

(define-abbrev-table 'magit-revision-mode-abbrev-table '())

(define-abbrev-table 'magit-stash-mode-abbrev-table '())

(define-abbrev-table 'magit-stashes-mode-abbrev-table '())

(define-abbrev-table 'magit-status-mode-abbrev-table '())

(define-abbrev-table 'magit-submodule-list-mode-abbrev-table '())

(define-abbrev-table 'magit-wazzup-mode-abbrev-table '())

(define-abbrev-table 'mail-abbrevs
  '(
   ))

(define-abbrev-table 'markdown-mode-abbrev-table '())

(define-abbrev-table 'message-mode-abbrev-table
  '(
    ("bd" "Best,

  Daniel" nil 54)
    ("bdb" "Best,

  Daniel Borchmann" nil 0)
    ("bgd" "Beste Grüße,

  Daniel" nil 0)
    ("bgdb" "Beste Grüße,

  Daniel Borchmann" nil 9)
    ("lg" "Liebe Grüße,

  Daniel" nil 18)
    ("mbg" "Mit besten Grüßen,

  Daniel Borchmann" nil 0)
    ("vgd" "Viele Grüße,

  Daniel" nil 6)
    ("vgdb" "Viele Grüße,

  Daniel Borchmann" nil 2)
   ))

(define-abbrev-table 'messages-buffer-mode-abbrev-table '())

(define-abbrev-table 'net-utils-mode-abbrev-table '())

(define-abbrev-table 'network-connection-mode-abbrev-table '())

(define-abbrev-table 'notmuch-hello-mode-abbrev-table '())

(define-abbrev-table 'notmuch-message-mode-abbrev-table '())

(define-abbrev-table 'notmuch-search-mode-abbrev-table '())

(define-abbrev-table 'notmuch-show-mode-abbrev-table '())

(define-abbrev-table 'notmuch-tree-mode-abbrev-table '())

(define-abbrev-table 'nrepl-connections-buffer-mode-abbrev-table '())

(define-abbrev-table 'nrepl-messages-mode-abbrev-table '())

(define-abbrev-table 'nrepl-mode-abbrev-table '())

(define-abbrev-table 'nslookup-mode-abbrev-table '())

(define-abbrev-table 'nxml-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table '())

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'org-export-stack-mode-abbrev-table '())

(define-abbrev-table 'org-mode-abbrev-table '())

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'pdf-annot-list-mode-abbrev-table '())

(define-abbrev-table 'pdf-occur-buffer-mode-abbrev-table '())

(define-abbrev-table 'pdf-outline-buffer-mode-abbrev-table '())

(define-abbrev-table 'perl-mode-abbrev-table '())

(define-abbrev-table 'perldoc-mode-abbrev-table '())

(define-abbrev-table 'pike-mode-abbrev-table '())

(define-abbrev-table 'plain-tex-mode-abbrev-table '())

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'python-mode-abbrev-table '())

(define-abbrev-table 'python-mode-skeleton-abbrev-table
  '(
   ))

(define-abbrev-table 'reb-lisp-mode-abbrev-table '())

(define-abbrev-table 'reb-mode-abbrev-table '())

(define-abbrev-table 'reftex-select-bib-mode-abbrev-table '())

(define-abbrev-table 'reftex-select-label-mode-abbrev-table '())

(define-abbrev-table 'ruby-mode-abbrev-table '())

(define-abbrev-table 'scheme-mode-abbrev-table '())

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sepia-cpan-mode-abbrev-table '())

(define-abbrev-table 'sepia-mode-abbrev-table '())

(define-abbrev-table 'sepia-repl-mode-abbrev-table '())

(define-abbrev-table 'sepia-scratchpad-mode-abbrev-table '())

(define-abbrev-table 'sgml-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table '())

(define-abbrev-table 'sldb-mode-abbrev-table '())

(define-abbrev-table 'slime-compiler-notes-mode-abbrev-table '())

(define-abbrev-table 'slime-connection-list-mode-abbrev-table '())

(define-abbrev-table 'slime-fuzzy-completions-mode-abbrev-table '())

(define-abbrev-table 'slime-inspector-mode-abbrev-table '())

(define-abbrev-table 'slime-thread-control-mode-abbrev-table '())

(define-abbrev-table 'slime-trace-dialog--detail-mode-abbrev-table '())

(define-abbrev-table 'slime-trace-dialog-mode-abbrev-table '())

(define-abbrev-table 'slime-xref-mode-abbrev-table '())

(define-abbrev-table 'slitex-mode-abbrev-table '())

(define-abbrev-table 'smbclient-mode-abbrev-table '())

(define-abbrev-table 'smime-mode-abbrev-table '())

(define-abbrev-table 'snippet-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'speedbar-mode-abbrev-table '())

(define-abbrev-table 'synonyms-mode-abbrev-table '())

(define-abbrev-table 'tablist-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'tar-mode-abbrev-table '())

(define-abbrev-table 'telnet-mode-abbrev-table '())

(define-abbrev-table 'term-mode-abbrev-table '())

(define-abbrev-table 'tex-mode-abbrev-table '())

(define-abbrev-table 'tex-shell-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'tree-mode-abbrev-table '())

(define-abbrev-table 'url-cookie-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-edit-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-git-region-history-mode-abbrev-table '())

(define-abbrev-table 'xref--xref-buffer-mode-abbrev-table '())

