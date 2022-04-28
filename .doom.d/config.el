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
;;
(setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'medium)
     doom-variable-pitch-font (font-spec :family "Fira Code" :size 13))
;;
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
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files '("~/Dropbox/org/main.org"))

(after! org
  (setq org-capture-templates '(("t" "Todo [general]" entry
                                 (file+headline "~/Dropbox/org/inbox.org" "General")
                                 "** TODO %i%?")
                                ("o" "Todo [obligations]" entry
                                 (file+headline "~/Dropbox/org/main.org" "Obligations")
                                 "** TODO %i%?")
                                ("r" "Todo [reflection]" entry
                                 (file+headline "~/Dropbox/org/main.org" "Reflection")
                                 "** TODO %i%?")
                                ("e" "Todo [dev environment]" entry
                                 (file+headline "~/Dropbox/org/main.org" "Dotfiles")
                                 "** TODO %i%?")))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             ("~/Dropbox/org/main.org" :maxlevel . 1)
                             ("~/Dropbox/org/main.org" :tag . "project")
                             ("~/Dropbox/org/someday.org" :level . 1)
                             ("~/Dropbox/org/reminder.org" :maxlevel . 2)
                             ("~/Dropbox/org/elfeed.org" :maxlevel . 1)))
  )

(setq scroll-margin 20)
;; (setq scroll-conservatively 101)
(setq evil-move-cursor-back nil)
(setq projectile-project-search-path '("~/Documents/github"))

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

  (defun insert-type ()
    "Display the type signature and documentation of the thing at
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

(map! :leader
      (:prefix-map ("a" . "user")
       :desc "browse" "a" #'browse-url
       :desc "type" "t" #'insert-type))
(map! :leader
      "0" #'treemacs-select-window
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "5" #'winum-select-window-5
      "6" #'winum-select-window-6
      "7" #'winum-select-window-7
      "8" #'winum-select-window-8
      "9" #'winum-select-window-9)

(map!
 :nv "0" #'evil-first-non-blank
 :nv "#" #'evil-ex-search-word-forward
 :nv "j" #'evil-next-visual-line
 :nv "k" #'evil-previous-visual-line
 :nv "*" #'evil-ex-search-word-backward
 :v "s" #'evil-surround-region)

(setq display-line-numbers-type 'relative)

(setq auth-sources '("~/.authinfo"))

(global-display-fill-column-indicator-mode)

(setq doom-modeline-enable-word-count t)
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode latex-mode))

(setq doc-view-continuous t)

(add-hook! 'markdown-mode-hook 'visual-fill-column-mode)
(add-hook! 'LaTeX-mode-hook 'visual-fill-column-mode)
(add-hook! 'org-mode-hook 'visual-fill-column-mode)

(setq c-offsets-alist '((innamespace . 0)
                        (access-label . /)
                        (inclass . +)))

(after! cc-mode (setq c-basic-offset 2))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(use-package! autopair
  :config
  (autopair-global-mode))

(after! evil-surround
  (let ((pairs '((?j "[" . "]")
                 (?k "(" . ")")
                 (?l "{" . "}")
                 (?\; "<" . ">")
                 (?g "'" . "'")
                 (?h "\"" . "\""))))
    (prependq! evil-surround-pairs-alist pairs)
    (prependq! evil-embrace-evil-surround-keys (mapcar #'car pairs))))

(setq magit-clone-default-directory "~/Documents/github/")

;; (after! evil-mode
;;         (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;;         (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;;         (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;;         (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line))

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
