

;; Don't show the splash screen
(setq inhibit-startup-message t)
(setq visible-bell t)

;; Defailt UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(hl-line-mode t)
(set-fringe-mode 10)

;; line numbers
(global-display-line-numbers-mode t)
(column-number-mode 1)


;; theme 
(load-theme 'modus-vivendi t)

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
(setq use-package-always-ensure t)


;; Ivy - completion - via counsel

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


;; Enable richer annotations using the Marginalia package
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

;; Which key - suggest key chord completion
(use-package which-key
  :defer 0
  :diminish which-key-modeq
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


;; projectile - https://docs.projectile.mx/projectile/index.html
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

;; magit
(use-package magit
  :commands magit-status)

;; org-mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(projectile which-key marginalia counsel use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
