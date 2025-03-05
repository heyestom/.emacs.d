(setq image-types (cons 'svg image-types))
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Don't show the splash screen
(setq inhibit-startup-message t)

;; flash on error / warnings
(setq visible-bell t)

;; turn off some gui features
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(display-fill-column-indicator-mode -1)

;; highlight the current line
(hl-line-mode t)
;; Add some space 
(set-fringe-mode 0)

;; line numbers
;; (global-display-line-numbers-mode t)

;; show the column in mode line
(column-number-mode 1)

;; wrap lines everywhere
(global-visual-line-mode 1)

;; hide the mode line... lets try this
;; (global-hide-mode-line-mode)

;; for osx 
 (setq frame-resize-pixelwise t)

;; (setq right-margin-width 0)
;; (setq left-margin-width 0)

;; (set-window-buffer nil (current-buffer))
(set-face-foreground 'vertical-border "#3b4252")
(window-divider-mode 0)


(add-to-list 'default-frame-alist '(internal-border-width . 10))
(add-to-list 'default-frame-alist '(alpha 90 90))


;; keep buffers synced with disc 
(global-auto-revert-mode 1)

;; auto save buffers 
(auto-save-visited-mode 1)

;;  (server-start)

;; Initialize package sources and ensure use-package
(require 'package)

(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
			 ("org"    . "https://orgmode.org/elpa/")
			 ("elpa"   . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize) 

(unless (and package-archive-contents (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
  (eval-when-compile (require 'use-package))

(require 'use-package)
;; ensure all use package packages are downlaoded
(setq use-package-always-ensure t)

;; quelpa is a tool to compile and install Emacs Lisp packages locally from local or remote source code. https://github.com/quelpa/quelpa

(use-package quelpa)

(use-package monokai-theme)
;;(load-theme 'monokai t)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	    doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;  (load-theme 'doom-nord t)
;;  (load-theme 'doom-gruvbox t)

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
	      ewal-json-file "~/.cache/wallust/colors.json"
              ewal-built-in-palette "sexy-material"))

(use-package ewal-doom-themes
  :ensure t)
(load-theme 'ewal-doom-one t)

(use-package all-the-icons
  :if (display-graphic-p))

;; (all-the-icons-install-fonts t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package exec-path-from-shell
   :config (exec-path-from-shell-initialize))

(use-package counsel)
(ivy-mode 1) ;; Ivy completion everywhere

;;;; recomended defaults - https://oremacs.com/swiper/#basic-customization
(setq ivy-use-virtual-buffers "recentf")
(setq ivy-count-format "(%d/%d) ")
(setq ivy-use-selectable-prompt t)
(setq enable-recursive-minibuffers t)

;;;; recomended counsel/ivy/swiper  key bindings
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	   :map minibuffer-local-map
	   ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package which-key
  :defer 0
  :diminish which-key-modeq
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
		("C-c p" . projectile-command-map))
  :init
  (setq projectile-project-search-path '("~/Kroo/")))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
     :bind ("C-x g" . magit-status)
     :commands (magit-status magit-get-current-branch))

(use-package flycheck
  :init (global-flycheck-mode))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq ispell-dictionary "en_GB")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(use-package flyspell-correct-popup
  :after flyspell-correct)

(use-package yaml-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
	  gc-cons-threshold 100000000
	  read-process-output-max (* 1024 1024))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	   (clojure-mode . lsp)
	   (terraform-mode . lsp)
	   ;; if you want which-key integration
	   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;;  (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (lsp-install-server nil 'clojure-lsp)

(use-package yasnippet)
(use-package yasnippet-snippets)

(yas-global-mode 1)

(use-package rainbow-delimiters)
(use-package smartparens)
(use-package idle-highlight-mode) 
(use-package flycheck-clojure)
(use-package flycheck-clj-kondo)

(use-package flycheck-pos-tip
  :after flycheck)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))


(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup)               ;; autoload
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  :mode (("\\.clj\\'" . clojure-mode)
	   ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)           
  (add-hook 'clojure-mode-hook #'smartparens-mode)       
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)             
  (add-hook 'clojure-mode-hook #'idle-highlight-mode))

(use-package clj-refactor
  :defer t
  :ensure t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t                  
	  cider-repl-use-clojure-font-lock t    
	  cider-prompt-save-file-on-load 'always-save
	  cider-font-lock-dynamically '(macro core function var)
	  nrepl-hide-special-buffers t            
	  cider-overlays-use-font-lock t)
  (flycheck-clojure-setup)
  (cider-repl-toggle-pretty-printing))

;; org-mode
(use-package org)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

(setq org-todo-keywords
	'((sequence "INBOX(i)" "TODO(t)" "WAIT(w@/!)" "BLOG(b)" "PROJECT(p)" "AREA(a)" "READ(r)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-log-done 'time)

(setq org-refile-targets '(("~/org/personal/personal.org" :maxlevel . 2)))

(eval-after-load 'org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (emacs-lisp . t)
   (shell . t))))

(setq org-babel-clojure-backend 'cider)

(use-package org-auto-tangle
 :defer t
 :hook (org-mode . org-auto-tangle-mode))

(setq org-auto-tangle-default t)

;;;; org modern - clean theme 
(use-package org-modern
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; Choose some fonts
(set-face-attribute 'default nil :family "Hack Nerd Font Mono"  :height 110)
(set-face-attribute 'variable-pitch nil :family "Hack Nerd Font" :height 110)
(set-face-attribute 'org-modern-symbol nil :family "Hack Nerd Font Mono" :height 110)

(modify-all-frames-parameters '((right-divider-width . 40)))
(dolist (face '(window-divider
		window-divider-first-pixel
		window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis " ↯"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 " now ─────────────────────────────────────────────────")


(set-face-foreground 'vertical-border "#3b4252")
(window-divider-mode 0)

(setq org-image-actual-width 400)

(use-package visual-fill-column
  :hook (org-mode .  (lambda ()
			 (setq visual-fill-column-width 100
			       visual-fill-column-center-text t)
			 (visual-fill-column-mode 1)
			 )))

(use-package org-kanban)

(setq org-capture-templates
      '(("i" "Inbox" entry (file+headline "~/org/personal/personal.org" "Inbox")
	   "* INBOX %?
:PROPERTIES:
:CAPTURED: %U
:END:
  " :empty-lines 1)
	("r" "Read" entry (file+headline "~/org/personal/personal.org" "Reading List")
	 "* READ %?
:PROPERTIES:
:CAPTURED: %U
:END:
  " :empty-lines 1)
	("p" "Project [0/2]" entry (file+headline "~/org/personal/personal.org" "Projects")
	   "* PROJECT %? :projectTag: \nDEADLINE: %t \n:PROPERTIES: \n:CAPTURED: %U \n:END: \n** INBOX first task\n** INBOX second task
  " :empty-lines 1)
	  ("j" "Journal" entry (file+olp+datetree "~/org/personal/journal.org")
	   "* %?\nEntered on %U\n  %i\n" :empty-lines 1)
	  ("J" "Journal entry at time" entry (file+olp+datetree "~/org/personal/journal.org")
	   "* %T %?\n%i\n" :time-prompt t :empty-lines 1)

	  ("w" "work")
	  ("wj" "Work Journal" entry (file+olp+datetree "~/org/work/kroo-journal.org")
	   "* %?\nEntered on %U\n  %i\n" :empty-lines 1)
	  ("wJ" "Work Journal entry at time" entry (file+olp+datetree "~/org/work/kroo-journal.org")
	   "* %T %?\n%i\n%a" :time-prompt t :empty-lines 1)
	  ("wt" "Work Ticket" entry (file+headline "~/org/work/kroo-journal.org" "Tickets")
	   "* TODO  %?\nEntered on %U\n  %i\n  %a" :empty-lines 1)
    ))

;; Populates only the EXPORT_FILE_NAME property in the inserted heading.
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
	   (description (read-from-minibuffer "Post Description: ")) ;Prompt to enter the post description
	   (fname (org-hugo-slug (concat (format-time-string "%d-%m-%Y") "-" title))))
      (mapconcat #'identity
		 `(
		   ,(concat "* TODO " title)
		   ":PROPERTIES:"
		   ,(concat ":EXPORT_FILE_NAME: " fname)
		   ,(concat ":EXPORT_DESCRIPTION: " description)
		   ":END:"
		   "%?\n")          ;Place the cursor here 
		 "\n")))

  (add-to-list 'org-capture-templates
	       '("h" "Hugo post"))
  (add-to-list 'org-capture-templates    
	       '("hc" "Coding Clojure"
		 entry
		 (file+olp "~/org/blog-posts/coding-clojure/coding-clojure.org" "posts")
		 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates    
		 '("ht" "they.es"
		   entry
		   (file+olp "~/org/blog-posts/they.es/theyes-blog.org" "Blog Section")
		   (function org-hugo-new-subtree-post-capture-template)
		   :prepend t)))

;; export to hugo 
(use-package ox-hugo
  :pin melpa 
  :after ox)

(setq org-hugo-external-file-extensions-allowed-for-copying
  '("jpg" "jpeg" "tiff" "png" "svg" "gif" "bmp" "mp4" "pdf" "odt" "doc"
    "ppt" "xls" "docx" "pptx" "xlsx" "webp"))

(use-package ox-twbs)

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  (org-roam-capture-templates
   '(("d" "default" plain
	"%?"
	:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title} \n\n\n#+print_bibliography:")
	:unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	   ("C-c n f" . org-roam-node-find)
	   ("C-c n g" . org-roam-graph)
	   ("C-c n i" . org-roam-node-insert)
	   ("C-c n c" . org-roam-capture)
	   ;; Dailies
	   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;;(require 'org-roam-protocol)
  )

(use-package org-roam-ui

  :after org-roam
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
	  org-roam-ui-follow t
	  org-roam-ui-update-on-save t
	  org-roam-ui-open-on-start t))

(require 'oc-natbib)
(require 'oc-biblatex)
(setq org-cite-export-processors '((latex biblatex)
				     (t basic)))
(setq org-cite-global-bibliography '("~/org/roam/references/master-lib.bib"))

(use-package citeproc)

(use-package vterm
  :custom
  (vterm-always-compile-module t))

(use-package hide-mode-line)

(use-package multi-vterm
  :config
  (add-hook 'vterm-mode-hook
	      (lambda () 
		(hide-mode-line-mode)
		)))
