;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Peter Goedeke"
      user-mail-address "peterbgoedeke@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:

(setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Fira Code" :size 13))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/org/")
(setq org-agenda-files '("~/Nextcloud/org/main.org" "~/Nextcloud/org/habits.org"))

;; --------------------------------
;; org settings
;; --------------------------------

(after! org
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq
   org-habit-following-days 1
   org-highest-priority ?A
   org-default-priority ?D
   org-lowest-priority ?D
   org-capture-templates '(("t" "Todo [general]" entry
                            (file+headline "~/Nextcloud/org/inbox.org" "General")
                            "** TODO %i%?")
                           ("d" "Todo [dotfiles]" entry
                            (file+headline "~/Nextcloud/org/inbox.org" "Dotfiles")
                            "** TODO %i%?"))
   org-refile-targets '((nil :maxlevel . 2)
                        ("~/Nextcloud/org/main.org" :maxlevel . 1)
                        ("~/Nextcloud/org/main.org" :tag . "project"))
   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)"  "|" "DONE(d)" "WONT(w)"))
   org-hide-emphasis-markers t
   org-image-actual-width (list 800)
   org-agenda-custom-commands
   `(("j" "Agenda Overview"
      ((agenda "")
       (todo "NEXT"
             ((org-agenda-overriding-header "Next tasks:")
              (org-agenda-sorting-strategy
               '((todo priority-down)))
              (org-agenda-prefix-format
               '((todo . " %?-10:(seq-elt (org-get-outline-path) 1) %-12:c")))))
       (tags "+uni+TODO=\"TODO\""
             ((org-agenda-overriding-header "Next uni deadlines:")
              (org-agenda-prefix-format
               '((tags  . "%?-4:(seq-elt (org-get-outline-path) 1) %?-10(let ((deadline (org-get-deadline-time (point)))) (if deadline (format-time-string \"%Y-%m-%d\" deadline) \"\")) ")))
              (org-agenda-max-entries 5))))))))

;; --------------------------------
;; org roam settings
;; --------------------------------

(setq org-roam-directory "~/Nextcloud/org/roam")

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; --------------------------------
;; custom functions
;; --------------------------------

(defun insert-type ()
  "Display the type signature and documentation of the symbol at
point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                    (lsp--make-request "textDocument/hover")
                    (lsp--send-request)
                    (lsp:hover-contents))))
    (if (and contents (not (equal contents "")))
        (progn
          (evil-previous-line)
          (newline)
          (insert (string-trim-right (lsp--render-on-hover-content contents t))))
      (lsp--info "No content at point."))))

(defun toggle-theme ()
  "Toggle between the doom-one theme and the doom-one-light theme"
  (interactive)
  (if (equal doom-theme 'doom-one)
      (load-theme 'doom-one-light)
    (load-theme 'doom-one)))

(defun swap-between-source-header ()
  "If the current file is a C++ source or header file then open the buffer for
its counterpart source or header"
  (interactive)
  (let* ((file (buffer-file-name))
         (extension (file-name-extension file))
         (bare-file (file-name-sans-extension file)))
    (if (or (string= extension "h") (string= extension "cpp"))
        (if (string= extension "h")
            (switch-to-buffer (find-file-noselect (concat bare-file ".cpp")))
          (switch-to-buffer (find-file-noselect (concat bare-file ".h"))))
      (message "Current buffer is not a C++ source or header file"))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; --------------------------------
;; editor settings
;; --------------------------------

;; scroll the screen before the cursor hits the edge
(setq scroll-margin 20)
;; keep the cursor in the same position after entering insert mode using "i"
(setq evil-move-cursor-back nil)

;; add jumps with ace-jump to history
(evil-add-command-properties #'ace-jump-mode :jump t)

;; show an indicator for max code width
(global-display-fill-column-indicator-mode)

;; persist authentication information
(setq auth-sources '("~/.authinfo"))

;; colour variable names differently
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; use autopair instead of smartparens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(use-package! autopair
  :config
  (autopair-global-mode))

;; --------------------------------
;; projectile settings
;; --------------------------------

;; enable projectile history for projects over TRAMP
(use-package projectile
  :ensure t
  :init
  (require 'tramp)
  (projectile-mode +1))

(setq projectile-project-search-path '("~/github"))

;; --------------------------------
;; bazel settings
;; --------------------------------

(defun open-bazel-file ()
  "Open the BUILD.bazel file in the directory or create it if it
does not exist"
  (interactive)
  (find-file "BUILD.bazel"))

(setq bazel-command-options '("--config=clang")
      bazel-buildifier-before-save t)

(add-hook 'bazel-mode-hook #'company-mode)
(add-to-list 'auto-mode-alist '("\\.BUILD\\'" . bazel-mode))

;; --------------------------------
;; vterm settings
;; --------------------------------

;; remove vterm from evil-escape-excluded-major-modes
(setq evil-escape-excluded-major-modes '(neotree-mode treemacs-mode))

;; --------------------------------
;; c++ settings
;; --------------------------------

(after! lsp
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :priority -1
                    :remote? t
                    :server-id 'clangd-remote
                    :library-folders-fn (lambda (_workspace) lsp-clients-clangd-library-directories)
                    :download-server-fn (lambda (_client callback error-callback _update?)
                                          (lsp-package-ensure 'clangd callback error-callback)))))

(defun peter-cc-style()
  (c-set-style "linux")
  (c-set-offset 'innamespace '0)
  (c-set-offset 'inextern-lang '0)
  (c-set-offset 'inline-open '0)
  (c-set-offset 'func-decl-cont '0)
  (c-set-offset 'substatement '0)
  (c-set-offset 'statement-cont '0)
  (c-set-offset 'label '*)
  (c-set-offset 'case-label '*)
  (c-set-offset 'access-label '/)
  (setq c-basic-offset 2
        tab-width 2
        indent-tabs-mode nil))

(add-hook 'c++-mode-hook 'peter-cc-style)

(set-file-template! "\\.\\(h\\|hpp\\)$" :trigger "__h" :mode 'c++-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq lsp-clients-clangd-args
      '("--header-insertion-decorators=0"
        "--background-index"
        "--header-insertion=iwyu"
        "--limit-references=100"
        "--limit-results=20"
        "--j=8"
        "--malloc-trim"
        "--pch-storage=memory"))

;; make evil-args work with template arguments
(after! evil-args
  (add-to-list 'evil-args-closers ">")
  (add-to-list 'evil-args-openers "<"))

;; --------------------------------
;; company / lsp settings
;; --------------------------------

;; disable completion of method signatures
(setq lsp-enable-snippet nil)

(add-to-list 'company-global-modes 'text-mode)
(remove-hook 'company-mode-hook #'text-mode)
(add-hook 'company-mode-hook #'company-prescient-mode)

;; (add-hook 'typescript-mode-hook
;;           (lambda () (setq-local company-idle-delay 0.1)))

;; (add-hook 'javascript-mode-hook
;;           (lambda () (setq-local company-idle-delay 0.1)))

(setq company-idle-delay 0.05
      company-require-match nil
      company-tooltip-limit 10
      ;; do not offer snippets as completions
      company--disabled-backends '(company-yasnippet)
      +lsp-company-backends '(company-capf))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

;; --------------------------------
;; writing settings
;; --------------------------------

;; enable text wrapping for "writing" modes
(add-hook! 'markdown-mode-hook 'visual-fill-column-mode)
(add-hook! 'LaTeX-mode-hook 'visual-fill-column-mode)
(add-hook! 'org-mode-hook 'visual-fill-column-mode)

;; --------------------------------
;; javascript / typescript settings
;; --------------------------------

;; (after! lsp-mode
;;   (setq lsp-javascript-format-enable nil
;;         lsp-typescript-format-enable nil))

;; --------------------------------
;; debugger settings
;; --------------------------------

;; (setq dap-auto-configure-mode t)
;; (require 'dap-cpptools)

;; --------------------------------
;; prisma settings
;; --------------------------------

(add-hook! 'prisma-mode-hook 'lsp)

;; --------------------------------
;; magit settings
;; --------------------------------

(setq magit-clone-default-directory "~/github/")

;; --------------------------------
;; treemacs settings
;; --------------------------------

(add-hook 'treemacs-mode-hook 'treemacs-add-and-display-current-project-exclusively)
(setq treemacs-follow-mode t)

;; --------------------------------
;; dwim settings
;; --------------------------------

(require 'dwim-shell-commands)

;; --------------------------------
;; abbrev / dabbrev settings
;; --------------------------------

(setq abbrev-file-name "~/.doom.d/abbrev_defs")

;; --------------------------------
;; keybindings
;; --------------------------------

(map! :leader
      (:prefix-map ("a" . "user")
       :desc "browse" "a" #'browse-url
       :desc "transpose windows" "w" #'toggle-window-split
       :desc "type" "t" #'insert-type)
      :desc "jump" "j" #'evil-avy-goto-word-1
      :desc "browse path" "fa" #'find-file-at-point
      :desc "other" "fo" #'swap-between-source-header
      :desc "bazel" "fb" #'open-bazel-file
      :desc "undo" "fu" #'recentf-open-most-recent-file
      :desc "Save project files" "pS" #'projectile-save-project-buffers
      :desc "Search subdirectory" "ps" #'projectile-search-subproject
      :desc "toggle theme" "tt" #'toggle-theme
      (:prefix-map ("d" . "directory")
       :desc "ranger" "d" #'ranger)
      "0" #'treemacs-select-window
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "5" #'winum-select-window-5
      "6" #'winum-select-window-6
      "7" #'winum-select-window-7
      "8" #'winum-select-window-8
      "9" #'winum-select-window-9
      ";" #'+workspace/switch-to)

(global-set-key (kbd "M-v") (lambda ()
                  (interactive)
                  (insert-char #x27)))
(map!
 :nv "0" #'evil-first-non-blank
 :nv "#" #'evil-ex-search-word-forward
 :nv "j" #'evil-next-visual-line
 :nv "k" #'evil-previous-visual-line
 :nv "*" #'evil-ex-search-word-backward
 :nv "ga" #'ace-jump-mode
 :i  "C-h" #'evil-delete-backward-char
 :i  "C-d" #'yas-insert-snippet
 :i  "C-s" #'completion-at-point
 :i  "C-f" #'dabbrev-expand
 :i  "C-SPC" (lambda () (interactive) (insert-char #x20))
 :v  "s" #'evil-surround-region
 :map dired-mode-map
 :nv "h" #'dired-up-directory
 :nv "l" #'dired-find-file
 :map ranger-normal-mode-map
 :g "c" #'dired-create-empty-file
 :map evil-inner-text-objects-map
 :g "b" #'evil-textobj-anyblock-inner-block
 :map evil-operator-state-map
 :g "b" #'evil-textobj-anyblock-inner-block)

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
