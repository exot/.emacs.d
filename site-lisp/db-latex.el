;;; db-latex --- TeX and LaTeX specific configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Setup

(use-package tex
  :ensure auctex)

(require 'latex)
(require 'tex-buf)
(require 'reftex)


;; Basic configuration

(TeX-engine-set 'default)

(setq-default TeX-auto-save t
              TeX-save-query nil
              TeX-parse-self t
              TeX-master t
              TeX-electric-sub-and-superscript t
              TeX-electric-math '("$" . "$")
              TeX-electric-escape nil
              LaTeX-electric-left-right-brace t
              LaTeX-fill-break-at-separators nil)

(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
(add-hook 'LaTeX-mode-hook #'turn-on-visual-line-mode)
(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'outline-minor-mode)
(add-hook 'LaTeX-mode-hook #'page-break-lines-mode)
(add-hook 'LaTeX-mode-hook #'turn-on-reftex)

(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

(setq TeX-fold-math-spec-list '(("≤" ("le"))
                                ("≥" ("ge"))
                                ("∉" ("notin"))))

(setq TeX-source-correlate-start-server nil)

(setq LaTeX-eqnarray-label "eqn:"
      LaTeX-equation-label "eqn:"
      LaTeX-figure-label "fig:"
      LaTeX-table-label "tab:"
      TeX-newline-function 'reindent-then-newline-and-indent
      LaTeX-section-hook '(LaTeX-section-heading
                           LaTeX-section-title
                           LaTeX-section-section
                           LaTeX-section-label))

(add-hook 'LaTeX-mode-hook '(lambda ()
                             (TeX-PDF-mode 1)
                             (TeX-source-correlate-mode 1)
                             (TeX-fold-mode 1)))


;; PDF Viewer

(add-to-list 'TeX-view-program-selection
             '(output-pdf "Evince"))

;; use pdf-tools when loaded
(eval-after-load 'pdf-tools
  `(progn
     (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
     (add-to-list 'TeX-after-compilation-finished-functions
                  #'TeX-revert-document-buffer)))



;; Printer

(add-to-list 'TeX-printer-list '("Remote" "lp-remote %o" "ssh lat lpstat -o"))
(setq TeX-printer-default "Remote")


;; Custom style

(TeX-add-style-hook
 "mydefs"
 (lambda ()
   (TeX-run-style-hooks "etex"
                        "etoolbox"
                        "ifthen"
                        "amsmath"
                        "amssymb"
                        "latexsym"
                        "mathabx"
                        "stmaryrd"
                        "verbatim"
                        "graphicx"
                        "enumerate"
                        "array"
                        "booktabs"
                        "ulem"
                        "nicefrac"
                        "listings"
                        "microtype"
                        "tabularx"
                        "tikz"
                        "csquotes"
                        "ntheorem"
                        "xspace")
   (LaTeX-add-environments
    '("Exercise" LaTeX-env-label)
    '("Theorem" LaTeX-env-label)
    '("Proposition" LaTeX-env-label)
    '("Lemma" LaTeX-env-label)
    '("Corollary" LaTeX-env-label)
    '("Remark" LaTeX-env-label)
    '("Example" LaTeX-env-label)
    '("Definition" LaTeX-env-label)
    '("Proof" LaTeX-env-label))

   ;; https://tex.stackexchange.com/questions/217799/auctex-11-88-bug-on-latex-env-label-cannot-automatically-insert-label
   (setf (cadr reftex-insert-label-flags)
         (concat (cadr reftex-insert-label-flags) "TLPDRCE"))

   (dolist (label-spec '(("Theorem"      ?T "thm:"  "~\\ref{%s}" t ("Theorem" "Thm.") nil)
                         ("Lemma"        ?L "lem:"  "~\\ref{%s}" t ("Lemma" "Lem.") nil)
                         ("Proposition"  ?P "prop:" "~\\ref{%s}" t ("Proposition" "Prop.") nil)
                         ("Satz"         ?T "thm:"  "~\\ref{%s}" t ("Satz") nil)
                         ("Definition"   ?D "def:"  "~\\ref{%s}" t ("Definition" "Def.") nil)
                         ("Remark"       ?R "rem:"  "~\\ref{%s}" t ("Remark" "Rem.") nil)
                         ("Corollary"    ?C "cor:"  "~\\ref{%s}" t ("Corollary" "Cor.") nil)
                         ("Example"      ?E "expl:" "~\\ref{%s}" t ("Example") nil)))
     (add-to-list 'reftex-label-alist label-spec)
     (add-to-list 'LaTeX-label-alist (cons (nth 0 label-spec)
                                           (nth 2 label-spec))))))


;; More style definitions

(TeX-add-style-hook
 "cleveref"
 (lambda ()
   (add-to-list 'reftex-ref-style-alist
                '("Cleveref" "cleveref"
                  (("\\cref" ?c) ("\\Cref" ?C)
                   ("\\cpageref" ?d) ("\\Cpageref" ?D))))
   (reftex-ref-style-activate "Cleveref")
   (TeX-add-symbols
    '("cref" TeX-arg-ref)
    '("Cref" TeX-arg-ref)
    '("cpageref" TeX-arg-ref)
    '("Cpageref" TeX-arg-ref))))


;; Language specification

(add-hook 'TeX-language-de-hook
          (lambda () (ispell-change-dictionary "de_DE")))

(add-hook 'TeX-language-en-hook
          (lambda () (ispell-change-dictionary "en_US")))

(add-hook 'TeX-mode-hook
          (lambda () (setq ispell-parser 'tex)))


;; LaTeXMk

(when (require 'auctex-latexmk nil 'no-error)
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


;; End

(provide 'db-latex)

;;; db-latex ends here
