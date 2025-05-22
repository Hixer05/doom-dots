;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "Fira Code" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 15))
(setq warning-minimum-level :warning)
(setq warning-minimum-log-level :warning)
;; __ THEME __
(setq current-theme 'doom-gruvbox)
(setq doom-theme current-theme)
(custom-theme-set-faces! current-theme
'(org-level-4 :inherit outline-4 :height 1.1)
'(org-level-3 :inherit outline-3 :height 1.15)
'(org-level-2 :inherit outline-2 :height 1.2)
'(org-level-1 :inherit outline-1 :height 1.3)
'(org-document-title :height 2.0 :underline nil))
(set-frame-parameter nil 'alpha-background 100) ; For current frame

;; __ LINES __
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; __ ZEN __
(after! mixed-pitch
  (add-hook! 'org-mode-hook #'mixed-pitch-mode))
(setq +zen-text-scale 1)

(after! zen
  (setq writeroom-mode-line t))
;; (add-hook! 'writeroom-mode-hook #'doom/reset-font-size)

;; __ EVIL __
(after! evil
  (setq evil-want-minibuffer :true)
  (map! :leader
        "w n" #'evil-window-vnew
        "w N" #'evil-window-new))
;; __ ORG __
(setq org-directory "~/org/")
(setq org-default-notes-file "refile.org")
(setq +org-capture-notes-file "refile.org")
(setq +org-capture-todo-file "refile.org")
(setq +org-capture-journal-file "refile.org")
(setq org-archive-location "archive/%s_archive::")

(defun my-org-for-tagged-heading (FUNC TAG)
  "Execute FUNC for every heading with TAG in the current Org buffer."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let ((tags (org-get-tags)))
          (when (and tags (member TAG tags))
            (funcall FUNC)))))))

(defun my-org-autocollapse ()
  (my-org-for-tagged-heading #'org-fold-hide-subtree "fold"))

(after! org
  (setq visual-line-mode t
        org-preview-latex-default-process 'dvisvgm
        org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANX(c)"))
        org-hide-emphasis-markers t
        org-format-latex-options (plist-put org-format-latex-options ':scale 1.0)
        org-format-latex-options (plist-put org-format-latex-options ':html-scale 1.0)
        org-startup-with-latex-preview t
        org-deadline-warning-days 7)
        ;; org-agenda-custom-commands '(("e" agenda "esame")))
  (add-to-list 'org-latex-packages-alist '("" "ebproof" t))
  (add-to-list 'org-latex-packages-alist '("" "amssymb" t))
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  (setq org-capture-templates
        '(("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Refile")
           "* %?\n%U\n" :prepend t)
          ("t" "TODO entries" entry
           (file+headline +org-capture-notes-file "Refile")
           "* TODO [#C] %?\n%U\n")
          ("a" "Appuntamento" entry
           (file+headline +org-capture-todo-file "Appuntamenti")
           "* %?\n%T\n"))))



(add-hook! 'org-mode-hook :local :append #'my-org-autocollapse)

;; __ORG::MODERN__
(after! org-modern
  (setq org-modern-star 'replace))

;; __ ORG::ROAM __
(setq org-roam-directory (file-truename "~/org/roam"))
(after! org-roam
  (map! :map org-mode-map
        :leader
      "m f a" #'org-roam-alias-add
      "m f t" #'org-roam-tag-add
      "m t o" #'org-roam-buffer-toggle
      "m f i" #'org-roam-node-insert)
  (setq org-roam-mode-sections
        (list '(org-roam-backlinks-section :unique t)
              '(org-roam-reflinks-section)))
  (org-roam-db-autosync-mode)
  (add-hook! 'org-roam-buffer-postrender-functions
      (org--latex-preview-region (point-min) (point-max)))
  (setq org-roam-capture-templates '(("d" "default" plain "#+title: ${title}\nContext: %?\nNext:\n"
                                      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                         "")
                                      :unarrowed t))))

(map! :leader
      "n o" #'org-roam-node-find
      "n d" #'org-roam-dailies-capture-today)


;; __ ORG::ROAM::UI __
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; __ ORG::TRANSCLUSION __
(after! org-transclusion )

;; __ POPUP __
(after! popup
  (set-popup-rule! "\\*org-roam\\*"
    :modeline nil
    ;; :actions '(display-buffer-in-side-window)
    :side 'right
    :width 0.33
    :slot 0
    :parameters '((no-other-window .t) (no-delete-orther-windows t))))

;; __ LISP __
(map!
 :g "C-M-l" 'forward-sexp
 :g "C-M-h" 'backward-sexp)


;; __ FIX JAVA__
(setq lsp-java-jdt-download-url
   "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.46.0/jdt-language-server-1.46.0-202503271314.tar.gz")


;; __ DOC __
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;; (setq doom-font (font-spec :size 16))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Get a specific config
