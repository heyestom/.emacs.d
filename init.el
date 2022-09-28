;; Don't show the splash screen
(setq inhibit-startup-message t)
;; flash on error / warnings
(setq visible-bell t)
;; turn off some gui features
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; highlight the current line
(hl-line-mode t)
;; Add some space 
(set-fringe-mode 10)
;; line numbers
(global-display-line-numbers-mode t)
;; show the colum in mode line
(column-number-mode 1)
;; wrap lines everywhere
(global-visual-line-mode 1)

;; Initialize package sources and ensure use-package
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize) 

(unless (and package-archive-contents (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
  (eval-when-compile (require 'use-package))

(require 'use-package)
;; ensure all use package packages are downlaoded
(setq use-package-always-ensure t)

(use-package monokai-theme)
(load-theme 'monokai t)

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

(use-package all-the-icons
  :if (display-graphic-p))

(all-the-icons-install-fonts t)

(add-hook
 'after-init-hook
 (lambda ()
   (find-file "~/org/personal/journal.org")
   ;; (toggle-frame-fullscreen)
   (split-window-right)
   (org-agenda-list)
   (other-window 1)
   ))

(use-package exec-path-from-shell
   :config (exec-path-from-shell-initialize))

(use-package counsel)
(ivy-mode 1) ;; Ivy completion everywhere

;;;; recomended defaults - https://oremacs.com/swiper/#basic-customization
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

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
  (setq projectile-project-search-path '("~/Projects/")))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status)

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

(lsp-install-server nil 'clojure-lsp)

(use-package company)
(use-package tide)
(use-package web-mode)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; tsx
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; javascript 
(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;; jsx
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
	    (when (string-equal "jsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

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

(use-package terraform-mode
    :mode (("\\.tf\\'" . terraform-mode)
	   ("\\.tfvars\\'" . terraform-mode))
    :custom (terraform-indent-level 2)
;; terraform-ls for stable language server
    :hook (terraform-mode . lsp)
    )

;; org-mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "BOOKED(b!)"  "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-log-into-drawer t)

(eval-after-load 'org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (emacs-lisp . t)
   (shell . t))))

(setq org-babel-clojure-backend 'cider)

;;;; org modern - clean theme 
(use-package org-modern
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; Choose some fonts
;; (set-face-attribute 'default nil :family "Iosevka")
;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
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
 org-ellipsis "↯"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 " now ─────────────────────────────────────────────────")

(setq org-image-actual-width nil)

(use-package visual-fill-column
  :hook (org-mode .  (lambda ()
		       (setq visual-fill-column-width 100
			     visual-fill-column-center-text t)
		       (visual-fill-column-mode 1)
		       )))

(use-package org-kanban)

(setq org-capture-templates
  '(("t" "Todo" entry (file+headline "~/org/personal/personal.org" "Todo list")
     "* TODO %?\n  %i\n  %a" :empty-lines 1)
    ("j" "Journal" entry (file+olp+datetree "~/org/personal/journal.org")
     "* %?\nEntered on %U\n  %i\n  %a" :empty-lines 1)
    ("J" "Journal entry at time" entry (file+olp+datetree "~/org/personal/journal.org")
     "* %T %?\n%i\n%a" :time-prompt t :empty-lines 1)

    ("w" "work")
    ("wj" "Work Journal" entry (file+olp+datetree "~/org/work/work-journal.org")
     "* %?\nEntered on %U\n  %i\n  %a" :empty-lines 1)
    ("wJ" "Work Journal entry at time" entry (file+olp+datetree "~/org/work/work-journal.org")
     "* %T %?\n%i\n%a" :time-prompt t :empty-lines 1)

    ))

;; Populates only the EXPORT_FILE_NAME property in the inserted heading.
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
	   (fname (org-hugo-slug title)))
      (mapconcat #'identity
		 `(
		   ,(concat "* TODO " title)
		   ":PROPERTIES:"
		   ,(concat ":EXPORT_FILE_NAME: " fname)
		   ":END:"
		   "%?\n")          ;Place the cursor here finally
		 "\n")))

  (add-to-list 'org-capture-templates
	       '("h" "Hugo post"))
  (add-to-list 'org-capture-templates    
	       '("hc" "Coding Clojure"
		 entry
		 (file+olp "~/org/blog-posts/coding-clojure/coding-clojure.org" "posts")
		 (function org-hugo-new-subtree-post-capture-template))))

;; export to hugo 
  (use-package ox-hugo
    :pin melpa 
    :after ox)
