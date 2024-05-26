;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sylvain Bougerel"
      user-mail-address "sylvain.bougerel.devel@gmail.com")

;; Fonts
;;

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
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font-increment 1)
(if IS-MAC
    (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'light)
          doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 20)
          doom-variable-pitch-font (font-spec :family "Source Serif Pro" :size 16)
          doom-serif-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'bold))
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 22)
        doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 32)
        doom-variable-pitch-font (font-spec :family "Source Serif Pro" :size 26)
        ;; 'fixed-pitch-serif' face is generally for emphasis only
        doom-serif-font (font-spec :family "JetBrainsMono Nerd Font" :size 22 :weight 'bold)))

(after! org-faces
  (dolist
      (attr
       '((org-document-title 1.6 bold)
         (org-level-1 1.5 semi-bold)
         (org-level-2 1.4 semi-bold)
         (org-level-3 1.3 normal)
         (org-level-4 1.2 normal)
         (org-level-5 1.1 normal)
         (org-level-6 1.0 normal)
         (org-level-7 1.0 normal)
         (org-level-8 1.0 normal)))
    (set-face-attribute (nth 0 attr) nil :weight (nth 2 attr) :height (nth 1 attr)))
  (set-face-attribute 'org-table nil :weight 'normal :height 1.0))

(after! mixed-pitch
  (setq mixed-pitch-set-height t))

(after! (:and mixed-pitch org-faces)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-drawer))

;; Editing
;;

(after! doom-editor
  ;; Doom thinks this is expensive, but I can't leave with having to scroll
  ;; horizontally due to me displaying multiple vertical windows. Wrapping lines
  ;; lifts this inconvenience.
  (setq-default truncate-lines nil)
  ;; Given the above, I got used to wrapping at any point in the line.
  (setq-default word-wrap nil)
  ;; Given that I regularly dispay 3 to 4 vertical buffers, I find 60 is good
  ;; limit.
  (setq-default truncate-partial-width-windows 60))

(after! (:and elisp-mode doom-editor)
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq sentence-end-double-space t))))

;; UI, Look & Feel
;;

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Enable mixed-pitch-mode for Org-mode, Org-roam and Markdown
(use-package! mixed-pitch
  :hook ((org-mode . mixed-pitch-mode)
         (markdown-mode . mixed-pitch-mode)))

;; Navigation
;;
(put 'scroll-left 'disabled t)
(repeat-mode)

;; Org-mode and Org-roam configuration
;;

;; Setup to make Org, Org-roam and Org-roam-dailies work together nicely, with
;; Logseq. Switch most of the workflow to Org-roam, but keep the Org-agenda.
;;
;; - Logseq uses the same directory as Org-roam
;; - Org-agenda searches the Org-roam directory
;;
;; My Org-roam notes are version controlled, and the push-pull workflow is being
;; worked on. The Notes directory should contains all the "graphs" (with the
;; default one being "roam/").
(setq org-directory "~/Notes/"
      org-roam-directory (file-truename (file-name-concat org-directory "roam/"))
      org-attach-id-dir (expand-file-name "assets" org-roam-directory)
      org-roam-dailies-directory "journals/")

(after! org
  (setq org-log-done 'time
        org-log-into-drawer t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-ellipsis " ▼"
        ;; Override the default Doom keywords settings
        org-todo-keywords '((sequence "TODO(t)" "IDEA(i)" "DOING(n!)" "STRT(s!)" "HOLD(h!)" "|" "DONE(d!)" "KILL(k!)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(after! org-agenda
  ;; TODO the list below is extremely long and slow, need to build it from an org-roam-db-query instead
  (setq org-agenda-files
        (append (list org-directory org-roam-directory)
                (mapcar (lambda (dir)
                          (file-truename (file-name-concat org-roam-directory dir)))
                        '("pages/" "journals/")))))

(after! org-roam
  (setq
   org-roam-file-exclude-regexp "\\.git/.*\\|logseq/.*$"
   org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      ;; Accomodates for the fact that Logseq uses the "pages" directory
      :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t))
   org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n")))
   org-roam-mode-sections '(org-roam-backlinks-section
                            org-roam-reflinks-section
                            org-roam-unlinked-references-section))
  ;; Merge ~org-roam~ bindings with module ~:lang org~ keybindings in localleader map
  (map! :map org-mode-map
        :localleader
        "D" #'org-roam-demote-entire-buffer
        "i" #'org-roam-node-insert
        ;; Unbinds all settings from ~modules/lang/org/contrib/roam2.el~
        "m" #'org-toggle-item
        "r" #'org-roam-refile
        "R" #'org-roam-link-replace-all
        (:prefix ("o" . "node properties")
                 "." #'org-set-property
                 "a" #'org-roam-alias-add
                 "A" #'org-roam-alias-remove
                 "q" #'org-set-tags-command
                 "t" #'org-roam-tag-add
                 "T" #'org-roam-tag-remove
                 "r" #'org-roam-ref-add
                 "R" #'org-roam-ref-remove
                 )))

(after! (:and org-roam popup)
  ;; Display the popup buffer below the current window, by slpitting the window
  ;; NOTE Should this be automated for all ~@pages~ buffers?
  ;; NOTE Doom Emacs does not seem to allow creation of normal windows
  (set-popup-rules!
    `(("\\*org-roam\\*"
       :actions (display-buffer-below-selected)
       :size 0.33 :ttl nil :modeline nil :quit nil :slot 1 :select nil)
      ("^\\*org-roam: "
       :actions (display-buffer-below-selected)
       :size 0.33 :ttl nil :modeline nil :quit nil :slot 2 :select nil))))

(map! :leader
      ;; Overwrite default keybindings of Org from Doom Emacs to add Org-roam autoloads
      "n" nil ; Unbind settings from ~modules/config/default/+emacs-bindings.el~
      (:prefix ("n" . "notes")
       :desc "Search notes for symbol"        "." #'+default/search-notes-for-symbol-at-point
       :desc "Sync database"                  "-" #'org-roam-db-sync
       :desc "Find ref"                       "/" #'org-roam-ref-find

       :desc "Org agenda"                     "a" #'org-agenda
       (:when (modulep! :tools biblio)
         :desc "Bibliographic notes"          "b"
         (cond ((modulep! :completion vertico)   #'citar-open-notes)
               ((modulep! :completion ivy)       #'ivy-bibtex)
               ((modulep! :completion helm)      #'helm-bibtex)))

       :desc "Toggle last org-clock"          "c" #'+org/toggle-last-clock
       :desc "Cancel current org-clock"       "C" #'org-clock-cancel

       (:prefix ("d" . "by date")
        :desc "Goto previous note"            "b" #'org-roam-dailies-goto-previous-note
        :desc "Goto date"                     "d" #'org-roam-dailies-goto-date
        :desc "Capture date"                  "D" #'org-roam-dailies-capture-date
        :desc "Goto next note"                "f" #'org-roam-dailies-goto-next-note
        :desc "Find directory"                "F" #'org-roam-dailies-find-directory
        :desc "Goto tomorrow"                 "m" #'org-roam-dailies-goto-tomorrow
        :desc "Capture tomorrow"              "M" #'org-roam-dailies-capture-tomorrow
        :desc "Capture today"                 "n" #'org-roam-dailies-capture-today
        :desc "Goto today"                    "t" #'org-roam-dailies-goto-today
        :desc "Capture today"                 "T" #'org-roam-dailies-capture-today
        :desc "Goto yesterday"                "y" #'org-roam-dailies-goto-yesterday
        :desc "Capture yesterday"             "Y" #'org-roam-dailies-capture-yesterday)

       (:when (modulep! :ui deft)
         :desc "Open deft"                    "D" #'deft)
       (:when (modulep! :lang org +noter)
         :desc "Org noter"                    "e" #'org-noter)

       :desc "Find node"                      "f" #'org-roam-node-find
       :desc "Browse notes"                   "F" #'+default/browse-notes
       :desc "Show graph"                     "g" #'org-roam-graph
       :desc "Insert node"                    "i" #'org-roam-node-insert
       :desc "Goto today"                     "j" #'org-roam-dailies-goto-today
       :desc "Capture today"                  "J" #'org-roam-dailies-capture-today
       :desc "Capture new node"               "n" #'org-roam-capture
       :desc "Active org-clock"               "o" #'org-clock-goto
       :desc "Toggle roam buffer"             "r" #'org-roam-buffer-toggle
       :desc "Dedicated roam buffer"          "R" #'org-roam-buffer-display-dedicated
       :desc "Search notes"                   "s" #'+default/org-notes-search

       ;; TODO Optimize with proper queries when org is used
       :desc "Todo list"                      "t" #'org-todo-list

       ;; Terribly slow (based of ~org-agenda~), avoid.
       ;; :desc "Tags search"                    "m" #'org-tags-view
       ;; :desc "Search org agenda headlines"    "S" #'+default/org-notes-headlines
       ;; :desc "View search"                    "v" #'org-search-view

       :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
       :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text))

;; Assistants
;;

(after! spell-fu
  ;; Requires aspell for spell checking.
  (setq spell-fu-idle-delay 0.5))  ; default is 0.25

(after! flycheck
  (setq-default flycheck-disabled-checkers
                ;; Disable proselint for now, it's very noisy, esp. with org-mode.
                '(proselint)))

;; Accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(use-package! gptel
  :config
  (setq! gptel-api-key
         (lambda () (auth-source-pick-first-password
                     :host "openai.com"))))

;; Versioning and utilities
;;

(use-package! autosync-magit)

(use-package! logseq-org-roam
  :config
  (add-hook 'logseq-org-roam-updated-hook #'org-roam-db-sync))


;; Major modes settings
;;

(after! dap-mode
  (setq dap-python-debugger 'debugpy))
