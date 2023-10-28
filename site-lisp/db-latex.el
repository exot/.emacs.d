;;; db-latex --- Configuration for LaTeX and consorts -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package reftex
  :commands (turn-on-reftex)
  :autoload (reftex-ref-style-activate)
  :init (setq reftex-plug-into-AUCTeX t)
  :config (with-eval-after-load 'helm-mode
            (add-to-list 'helm-completing-read-handlers-alist
                         '(reftex-citation . nil))))

(use-package latex
  :commands (LaTeX-math-mode)
  :autoload (LaTeX-add-environments))

(use-package tex-fold)

(use-package tex
  :commands (TeX-engine-set
             TeX-PDF-mode
             TeX-source-correlate-mode)
  :functions (TeX-add-style-hook
              TeX-run-style-hooks
              TeX-add-symbols)
  :init (setq TeX-auto-save t
              TeX-save-query nil
              TeX-parse-self t
              TeX-master t
              TeX-electric-sub-and-superscript t
              TeX-electric-math '("$" . "$")
              TeX-electric-escape nil
              LaTeX-electric-left-right-brace t
              LaTeX-fill-break-at-separators nil
              TeX-fold-math-spec-list '(("≤" ("le"))
                                        ("≥" ("ge"))
                                        ("∉" ("notin")))

              TeX-source-correlate-start-server nil

              LaTeX-eqnarray-label "eqn:"
              LaTeX-equation-label "eqn:"
              LaTeX-figure-label "fig:"
              LaTeX-table-label "tab:"
              TeX-newline-function 'reindent-then-newline-and-indent
              LaTeX-section-hook '(LaTeX-section-heading
                                   LaTeX-section-title
                                   LaTeX-section-section
                                   LaTeX-section-label))
  :config (progn

            (TeX-engine-set 'default)

            (put 'TeX-narrow-to-group 'disabled nil)
            (put 'LaTeX-narrow-to-environment 'disabled nil)

            (add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
            (add-hook 'LaTeX-mode-hook #'turn-on-visual-line-mode)
            (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
            (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
            (add-hook 'LaTeX-mode-hook #'page-break-lines-mode)
            (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
            (add-hook 'latex-mode-hook #'turn-on-reftex)

            (add-hook 'LaTeX-mode-hook #'(lambda ()
                                           (TeX-PDF-mode 1)
                                           (TeX-source-correlate-mode 1)
                                           (TeX-fold-mode 1)))


            (add-to-list 'TeX-view-program-selection
                         '(output-pdf "Evince"))

            ;; style used for my personal definitions; not clear whether this
            ;; works as intended
            (TeX-add-style-hook
             "mydefs"
             #'(lambda ()
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

                 (dolist (label-spec
                           '(("Theorem" ?T "thm:" "~\\ref{%s}" t ("Theorem" "Thm.") nil)
                             ("Lemma" ?L "lem:" "~\\ref{%s}" t ("Lemma" "Lem.") nil)
                             ("Proposition" ?P "prop:" "~\\ref{%s}" t ("Proposition" "Prop.") nil)
                             ("Satz" ?T "thm:" "~\\ref{%s}" t ("Satz") nil)
                             ("Definition" ?D "def:" "~\\ref{%s}" t ("Definition" "Def.") nil)
                             ("Remark" ?R "rem:" "~\\ref{%s}" t ("Remark" "Rem.") nil)
                             ("Corollary" ?C "cor:" "~\\ref{%s}" t ("Corollary" "Cor.") nil)
                             ("Example" ?E "expl:" "~\\ref{%s}" t ("Example") nil)))
                   (add-to-list 'reftex-label-alist label-spec)
                   (add-to-list 'LaTeX-label-alist (cons (nth 0 label-spec)
                                                         (nth 2 label-spec))))))

            ;; Add completion for cleverref’s reference macros; not clear
            ;; whether this works as intended
            (TeX-add-style-hook
             "cleveref"
             #'(lambda ()
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

            ;; Language definitions
            (add-hook 'TeX-language-de-hook
                      #'(lambda () (ispell-change-dictionary "de_DE")))
            (add-hook 'TeX-language-en-hook
                      #'(lambda () (ispell-change-dictionary "en_US")))
            (add-hook 'TeX-mode-hook
                      #'(lambda () (setq ispell-parser 'tex)))))

(provide 'db-latex)

;;; db-latex.el ends here
