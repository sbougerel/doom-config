;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sylvain Bougerel"
      user-mail-address "sylvain.bougerel.devel@gmail.com")

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
;;(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size (if IS-MAC 12 22))
;;      doom-variable-pitch-font (font-spec :family "Source Serif Pro" :size (if IS-MAC 12 22)))
(set-face-attribute 'default nil :height (if IS-MAC 150 110))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font")
      doom-variable-pitch-font (font-spec :family "Source Serif Pro"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Add repeat mode which is convenient for navigating through the UI.
(repeat-mode)

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

;; Org-mode configuration
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
(setq org-directory "~/Notes/")
(setq org-roam-directory (file-truename (expand-file-name "roam/" org-directory)))
(setq org-roam-dailies-directory "journals/")

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           ;; Accomodates for the fact that Logseq uses the "pages" directory
           :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           ;; Accomodates for the fact that Logseq uses underscores
           :target (file+head "%<%Y_%m_%d>.org"
                              "#+title: %<%Y-%m-%d>\n")))
        org-roam-file-exclude-regexp
        (list (expand-file-name  "logseq/" org-roam-directory))))

(after! org
  (setq org-log-done 'time)
  (setq org-log-into-drawer t))

(after! org-agenda
  (setq org-agenda-files
        (list org-directory
              org-roam-directory
              (file-truename (expand-file-name "pages/" org-roam-directory))
              (file-truename (expand-file-name "journals/" org-roam-directory)))))

;; Spell and Grammar checking
;;
;; Uses aspell for spell checking. Disable proselint for now, it's very noisy,
;; esp. with org-mode.
(after! spell-fu
  (setq spell-fu-idle-delay 0.5))  ; default is 0.25

(after! flycheck
  (setq-default flycheck-disabled-checkers '(proselint)))

;; Assistants
;;
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(use-package! auto-git-sync
  :after (magit projectile)
  :config
  (add-to-list 'auto-git-sync-dirs org-roam-directory))
