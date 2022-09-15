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
(setq org-directory "~/Nextcloud/org/")
(setq org-agenda-files '("~/Nextcloud/org/main.org"))

;; (cl-loop for file in '("/usr/bin/fish" "/usr/bin/zsh" "/usr/bin/bash" "/bin/bash")
;;          when (file-exists-p file)
;;          do (progn
;;               (setq shell-file-name file)
;;               (cl-return)))

;; (setenv "SHELL" shell-file-name)

;; (setq vterm-shell "/usr/bin/fish")

(use-package projectile
  :ensure t
  :init
  (require 'tramp)
  (projectile-mode +1))

(defun days-since-last-repeat ()
  "Returns the number of full days which have passed since the last repeat of
the org heading at ~(point)~. If there is no repeat an empty string is returned"
  (interactive)
  (let* ((raw-timestamp (org-entry-get (point) "LAST_REPEAT"))
         (now-days (string-to-number (format-seconds "%d" (current-time)))))
    (message (if raw-timestamp
                 (let*
                     ((timestamp (substring raw-timestamp 1 -1))
                      (seconds (seconds-to-time (org-time-string-to-seconds timestamp)))
                      (last-repeat-days (string-to-number (format-seconds "%d" seconds)))
                      (diff-days (- now-days last-repeat-days)))
                   (number-to-string diff-days))
               ""))))

(setq org-agenda-prefix-format
      '((agenda  . " %i %-12:c%?-12t% s ")
        (todo  . "%-4:(days-since-last-repeat)")
        (tags  . "%?-4:(seq-elt (org-get-outline-path) 1) %?-10(let ((deadline (org-get-deadline-time (point)))) (if deadline (format-time-string \"%Y-%m-%d\" deadline) \"\")) ")
        (search . " %i %-12:c")))

(setq org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
 (todo scheduled-up)
 (tags deadline-up)
 (search category-keep)))

(after! org
  (setq org-capture-templates '(("t" "Todo [general]" entry
                                 (file+headline "~/Nextcloud/org/inbox.org" "General")
                                 "** TODO %i%?")
                                ("d" "Todo [dotfiles]" entry
                                 (file+headline "~/Nextcloud/org/inbox.org" "Dotfiles")
                                 "** TODO %i%?")))
  (setq org-refile-targets '((nil :maxlevel . 2)
                             ("~/Nextcloud/org/main.org" :maxlevel . 1)
                             ("~/Nextcloud/org/main.org" :tag . "project")))
  (setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)"  "|" "DONE(d)")))
  (setq org-agenda-custom-commands
      `(("l" "Weekly Log"
         ((agenda "")
          (tags "+TODO=\"NEXT\""
                ((org-agenda-overriding-header "Next tasks:")
                 (org-agenda-prefix-format
                  '((tags . " %?-10:(seq-elt (org-get-outline-path) 1) %-12:c")))))
          (tags "+uni+TODO=\"TODO\""
                ((org-agenda-overriding-header "Next uni deadlines:")
                 (org-agenda-max-entries 5))))))))

(setq evil-escape-excluded-major-modes '(neotree-mode treemacs-mode))

(defun swap-between-source-header ()
  (interactive)
  (let* ((file (buffer-file-name))
        (extension (file-name-extension file))
        (bare-file (file-name-sans-extension file)))
    (if (or (string= extension "h") (string= extension "cpp"))
        (if (string= extension "h")
                (switch-to-buffer (find-file-noselect (concat bare-file ".cpp")))
                (switch-to-buffer (find-file-noselect (concat bare-file ".h"))))
      (message "Current buffer is not a C++ source or header file"))))

(map! :map dired-mode-map
      :nv "h" #'dired-up-directory
      :nv "l" #'dired-find-file)

(map! :map ranger-normal-mode-map
      :g "c" #'dired-create-empty-file)

(map! :map evil-inner-text-objects-map
      :g "b" #'evil-textobj-anyblock-inner-block)

(map! :map evil-operator-state-map
      :g "b" #'evil-textobj-anyblock-inner-block)

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

(defun open-bazel-file ()
  "Open the BUILD.bazel file in the directory or create it if it
does not exist"
  (interactive)
  (find-file "BUILD.bazel"))

(defun toggle-theme ()
  "Toggle between the doom-one theme and the doom-one-light theme"
  (interactive)
  (if (equal doom-theme 'doom-one)
      (load-theme 'doom-one-light)
    (load-theme 'doom-one)))


(defun directory-to-files (DIRECTORY)
  "Yeet"
  (mapcar (lambda (FILE) (concat  DIRECTORY "/" FILE))
          (projectile-dir-files
           (concat (projectile-project-root) DIRECTORY))))

(defun directories-to-files (DIRECTORIES)
  "List the files in DIRECTORIES and in their sub-directories"
  (mapcan #'directory-to-files DIRECTORIES))

(defun parse-subproject-config-file (project-root)
  "Owo"
  (let (groups (groupsfile (concat project-root "/.groups")))
    (when (projectile-file-exists-p groupsfile)
      (with-temp-buffer
        (insert-file-contents groupsfile)
        (while (not (eobp))
          (pcase (char-after)
            (?! (push (cons (buffer-substring (1+ (point)) (line-end-position)) nil) groups))
            (_ (push (buffer-substring (point) (line-end-position)) (car groups))))
          (forward-line)))
      (nreverse (mapcar (lambda (x) (nreverse x)) groups)))))

(defun projectile-search-subproject ()
  (interactive)
  (let* ((project-root (projectile-acquire-root))
         (groups (parse-subproject-config-file project-root))
         (files (cdr (assoc (completing-read "Choose: " groups) groups)))
         (file (projectile-completing-read "Woah" (directories-to-files files))))
    (find-file (expand-file-name file project-root))))


(map! :leader
      (:prefix-map ("a" . "user")
       :desc "browse" "a" #'browse-url
       :desc "type" "t" #'insert-type)
      :desc "jump" "j" #'evil-avy-goto-word-1
      :desc "other" "fo" #'swap-between-source-header
      :desc "bazel" "fb" #'open-bazel-file
      :desc "undo" "fu" #'recentf-open-most-recent-file
      :desc "Save project files" "pS" #'projectile-save-project-buffers
      :desc "Search subdirectory" "ps" #'projectile-search-subproject
      :desc "toggle theme" "tt" #'toggle-theme
      (:prefix-map ("d" . "directory")
       :desc "ranger" "d" #'ranger))

(setq ranger-show-hidden t)

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
      "9" #'winum-select-window-9
      ";" #'+workspace/switch-to)

(map!
 :nv "0" #'evil-first-non-blank
 :nv "#" #'evil-ex-search-word-forward
 :nv "j" #'evil-next-visual-line
 :nv "k" #'evil-previous-visual-line
 :nv "*" #'evil-ex-search-word-backward
 :nv "ga" #'ace-jump-mode
 :i  "C-h" #'evil-delete-backward-char
 :i  "C-t" #'yas-insert-snippet
 :v  "s" #'evil-surround-region)

(evil-add-command-properties #'ace-jump-mode :jump t)

(setq display-line-numbers-type 'relative)

(setq auth-sources '("~/.authinfo"))

(global-display-fill-column-indicator-mode)

(setq doom-modeline-enable-word-count t)
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode latex-mode))

(setq doc-view-continuous t)

(add-hook! 'markdown-mode-hook 'visual-fill-column-mode)
(add-hook! 'LaTeX-mode-hook 'visual-fill-column-mode)
(add-hook! 'org-mode-hook 'visual-fill-column-mode)

;; (add-hook! 'prog-mode-hook 'tree-sitter-hl-mode)
(add-hook! 'prisma-mode-hook 'lsp)

(add-hook 'after-init-hook 'global-color-identifiers-mode)

(setq c-offsets-alist '((innamespace . 0)
                        (access-label . /)
                        (inclass . +)))

(setq org-image-actual-width (list 800))

(after! cc-mode (setq c-basic-offset 2))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(use-package! autopair
  :config
  (autopair-global-mode))

(setq magit-clone-default-directory "~/Documents/github/")

(setq org-superstar-headline-bullets-list '(9673 9675))
(setq org-hide-emphasis-markers t)

(add-to-list 'company-global-modes 'text-mode)
(add-hook 'bazel-mode-hook #'company-mode)
(remove-hook 'company-mode-hook #'text-mode)
(add-hook 'company-mode-hook #'company-prescient-mode)

(setq company-idle-delay 0.01)
(setq company-require-match nil)
(setq company-tooltip-limit 4)
(setq lsp-signature-auto-activate nil)


(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


(after! tree-sitter
        (global-tree-sitter-mode)
  )

(setq display-line-numbers-type nil)

(set-file-template! "\\.\\(h\\|hpp\\)$" :trigger "__h" :mode 'c++-mode)

(after! evil-args
        (add-to-list 'evil-args-closers ">")
        (add-to-list 'evil-args-openers "<"))

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
