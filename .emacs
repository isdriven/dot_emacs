;;-----
; Ippei Sato 's dot.Emacs
;    version : 24.5
;    compile : Max OS X
;    update  : 2015.9.1
;;-----

;;;;;;;; GENERAL ;;;;;;;;

;; base dir
(cd "~")

;; for debug
(setq init-file-debug t)

;; valriables
(defvar my-indent-settings nil "use tabs for indent?")

;; ENCODING
(set-language-environment "Japanese" ) (set-default-coding-systems 'utf-8-unix )
(set-terminal-coding-system 'utf-8-unix ) (set-buffer-file-coding-system 'utf-8-unix )
(prefer-coding-system 'utf-8-unix )

;; package
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; load-path
;(add-to-list 'load-path "~/.emacs.d/async")
;(add-to-list 'load-path "~/.emacs.d/helm")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(add-to-list 'load-path "~/.emacs.d/auto-install/")

;; colors
(add-to-list 'default-frame-alist '(foreground-color ."white"))
(add-to-list 'default-frame-alist '(background-color ."black"))
(set-frame-parameter nil 'fullscreen 'fullboth)

;; cursor
(add-to-list 'default-frame-alist '(cursor-type,'box))

;; powerline
(require 'powerline)
(powerline-default-theme)

;; migemo --off!
(setq migemo-isearch-enable-p nil)

;; window
(custom-set-variables
 '(ag-highlight-search t)
 '(ag-reuse-buffers (quote nil))
 '(ag-reuse-window (quote nil))
 '(custom-safe-themes
   (quote
    ("19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(setq frame-title-format (format "%%f- solgear" (system-name)))
;; mode-line's infomation
(line-number-mode t) (column-number-mode t)
(setq initial-scratch-message nil)
;; user space for indent, indent is 4
(setq indent-tabs-mode nil) (setq tab-width 4 )
;; display time
(display-time-mode t)
;; turn off
(tool-bar-mode 0) (setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq next-line-add-newlines nil)
;; mode-line color and region color
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "apple" :family "Menlo"))))
 '(mode-line ((t (:foreground "skyblue" :background "#222222" :box (:line-width 1 :color nil :style released-button)))))
 '(region ((t (:foreground "black" :background "skyblue")))))

;; highlight line
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "gray10"
		  :underline "gray24"))
    (((class color)
      (background light))
     (:background "ForestGreen"
		  :underline nil))
    (t ()))
  "*Face useed by hl-line")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)


;;; basic functions
(transient-mark-mode t)

;; helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "M-t")     'helm-swoop)
;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)


;; ag
(define-key global-map (kbd "M-q")     'ag)

;; make strength parent brace
(show-paren-mode t)
;; no auto-save
(setq auto-save-default nil)
;; no backup files
(setq backup-enable-predicate
      (lambda (name) nil))
;; good buffer selection
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; no kill without confirm
(defadvice kill-new (before ys:no-kill-new-duplicaties activate )
  (setq kill-ring (delete (ad-get-arg 0 ) kill-ring )))
;; ask before kill-emacs
(add-hook 'kill-emacs-query-functions
	  (lambda ()(y-or-n-p "really FINISH OFF?")))


;; linum
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d ")


;; ADJUST TO EMACS 22
(set-face-bold-p 'font-lock-warning-face nil)
(setq split-width-threshold nil)

;;;; LANGUAGE MODE ;;;;;;

;; FUNCTIONS
(defun indent-refine ()
  (interactive)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-cont-nonempty 0)
  (c-set-offset 'arglist-close 0))

;; go-mode
(require 'go-mode-autoloads)
(add-hook 'go-mode-hook 
  (lambda ()
    (setq-default) 
    (setq tab-width 2) 
    (setq standard-indent 2) 
    (setq indent-tabs-mode nil)))


;; PHP-MODE
(require 'php-mode)
(add-hook 'php-mode-hook
	  (lambda ()
	    (setq c-basic-offset 4)
	    (setq tab-width 4)
	    (indent-refine)
	    (setq indent-tabs-mode my-indent-settings)
	    )
	  )
(add-hook 'php-mode-hook 'hs-minor-mode)
(autoload 'php-mode "php-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))

;;;- hs-mode for php
(define-key php-mode-map (kbd "C-c C-o") 'hs-toggle-hiding)
(define-key php-mode-map (kbd "C-c C-f") 'hs-toggle)
(defvar my-hs-state-hide nil)

(defun hs-toggle()
  (interactive)
  (hs-toggle-mode)
  (if my-hs-state-hide
      (progn (hs-show-all) (setq my-hs-state-hide nil))
    (progn (hs-hide-array)(hs-hide-function)(setq my-hs-state-hide t))))

(defun hs-toggle-mode()
  (if (not hs-minor-mode)
      (hs-minor-mode)))

(defun hs-hide-function()
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "function.*?(.*?)" nil t)
      (if (search-forward "{" nil t )
          (hs-hide-block)))))

(defun hs-hide-array()
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "array(" nil t)
      (hs-hide-block))))

(defun next-function()
  (interactive)
  (next-line)
  (if (re-search-forward "function.*?(.*?)" nil t)
      (beginning-of-line)))

(defun prev-function()
  (interactive)
  (if (re-search-backward "function.*?(.*?)" nil t)
      (beginning-of-line)))

(define-key php-mode-map (kbd "C-c C-n") 'next-function)
(define-key php-mode-map (kbd "C-c C-p") 'prev-function)


;; js2-mode
(defun js2-indent-and-back-to-indentation()
  (interactive)
  (indent-for-tab-command)
  (let (( point-of-indentation (save-excursion (back-to-indentation) (point) ) ) )
    (skip-chars-forward "/s " point-of-indentation)))

(require 'js2-mode)
(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode my-indent-settings)
	    (setq tab-width 4)
	    (yas/minor-mode)
	    (setq c-basic-offset 4)))
(setq js2-mirror-mode nil
      js2-auto-indent-p t
					; no errors
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil
      js2-strict-trailing-comma-warning nil
      js2-strict-missing-semi-warning nil
      js2-strict-inconsistent-return-warning nil
      js2-missing-semi-one-line-override t
      js2-highlight-external-variables nil
      js2-highlight-level 3
      )
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(define-key js2-mode-map [tab] 'js2-indent-and-back-to-indentation)
(define-key js2-mode-map (kbd "C-c C-n") 'next-function)
(define-key js2-mode-map (kbd "C-c C-p") 'prev-function)


;; C-MODE
(add-hook 'c-mode-hook
	  (lambda ()
	    (setq tab-width 4)
	    (setq c-basic-offset 4)
	    (setq indent-tabs-mode my-indent-settings)
	    (indent-refine)))

;; CPP-MODE
(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq tab-width 4)
	    (setq c-basic-offset 4)
	    (setq indent-tabs-mode my-indent-settings)
	    (indent-refine)))

;; OBJECTIVE-C MODE
(add-hook 'objc-mode-hook
	  (lambda ()
	    (setq c-basic-offset 4)
	    (setq tab-width 4)
	    (indent-refine)
	    (setq indent-tabs-mode my-indent-settings)
	    )
	  )

;;;;; EXTENSIONS ;;;;;;;;

;;;;
;;;; tramp
;;;;
(require 'tramp)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete//ac-dict")
(ac-config-default)
(define-key ac-complete-mode-map [tab] 'ac-next)
(setq ac-auto-start 3)

;; yasippet
;(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")


;; eshell
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)
	    ))

(defun eshell/ccr ()
  "Clear the current buffer, leaving one prompt at the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
    (define-key eshell-mode-map (kbd "C-r") 'eshell-isearch-backward)
    ))
(setq eshell-ask-to-save-history (quote always))
(setq eshell-history-size 100000)
(setq eshell-hist-ignoredups t)

;; org-mode
(require 'org-install)
(setq org-hide-leading-stars t)

;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)



;;; MY EXTENSIONS ;;;

;; QUICK-HIGHLIGHT
(defface quick-highlight-color
  '((t (:box (:line-width 1 :color "gold") :weight bold )))
  "highlight-color for i-smart-hightlight-color")
(defvar quick-highlight-color 'quick-highlight-color)
(defun quick-highlight()
  (interactive)
  (let ( (re (or (thing-at-point 'symbol) "" )) )
    (setq font-lock-set-defaults nil)
    (font-lock-add-keywords nil (list (list re 0 quick-highlight-color t )))
    (font-lock-fontify-buffer)))
(global-set-key (kbd "C-;") 'quick-highlight)

;; QUICK-BOOKMARK
(defun quick-bookmark-set()
  "set quick bookmark"
  (interactive)
  (progn (point-to-register ?t)
         (message "mark here" )))
(defun quick-bookmark-go()
  "go quick bookmark"
  (interactive)
  (progn (jump-to-register ?t)
         (message "jump to mark!")))
(global-set-key (kbd "C-,") 'quick-bookmark-set)
(global-set-key (kbd "C-." ) 'quick-bookmark-go)
(define-key php-mode-map (kbd "C-,") 'quick-bookmark-set)
(define-key js2-mode-map (kbd "C-,") 'quick-bookmark-set)
(define-key php-mode-map (kbd "C-.") 'quick-bookmark-go)
(define-key js2-mode-map (kbd "C-.") 'quick-bookmark-go)


;;;; KEY BINDINGS ;;;

;;;; BACKWARD-KILL-WORD
(define-key global-map (kbd "C-j") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-j") 'backward-kill-word)
(define-key minibuffer-local-ns-map (kbd "C-j") 'backward-kill-word)
(define-key minibuffer-local-isearch-map (kbd "C-j") 'backward-kill-word)
(define-key minibuffer-local-filename-completion-map (kbd "C-j") 'backward-kill-word)
(define-key minibuffer-local-completion-map (kbd "C-j") 'backward-kill-word)
(define-key minibuffer-local-must-match-filename-map (kbd "C-j") 'backward-kill-word)
(define-key minibuffer-local-filename-must-match-map (kbd "C-j") 'backward-kill-word)

;;;; BACKWARD-DELETE-CHAR
(define-key global-map (kbd "C-h") 'backward-delete-char)
(define-key helm-map (kbd "C-h") 'backward-delete-char)

;; RECENTER
(global-set-key (kbd "C-l") 'recenter)

;; AUTO-INSTALL
(require 'auto-install)

;;;; OPEN-JUNK-FILE
(require 'open-junk-file)
(global-set-key (kbd "C-x C-z") 'open-junk-file)

;;;; POPWIN
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("*Backtrace*"))
(add-to-list 'popwin:special-display-config '("*Apropos*"))

;;;; sun-mode
;;(require 'sun-mode)
;;(add-to-list 'auto-mode-alist '("\\.sun$" . sun-mode))


;;;; insert date
(defun insert-date()
  (interactive)
  (insert (format-time-string "%Y-%m-%d" )))


;;;; jinja2-mode
(require 'jinja2-mode)
(add-to-list 'auto-mode-alist '("\\.twig" . jinja2-mode))

;;;; katakana
(global-set-key (kbd "C-x C-t") 'japanese-hankaku-region)

;;;
;;; original connection
;;;
;;(load-file "~/synphonie.el")
;;(load-file "~/create-on.el")
(require 'ssh-agent)

(defun c:ipsleoz()
  (interactive)
  (dired (concat "/ssh:root@ipsleoz.com#3843" ":/www/" ))
  )

;; ag-edit
(require 'ag)
 ; 現在のバッファを検索結果表示に使う
(require 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)
;; agの検索結果バッファで"r"で編集モードに。
;; C-x C-sで保存して終了、C-x C-kで保存せずに終了
(define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)
;; キーバインドを適当につけておくと便利。"\C-xg"とか
(global-set-key [(super m)] 'ag)
;; ag開いたらagのバッファに移動するには以下をつかう
(defun my/filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
(defun my/get-buffer-window-list-regexp (regexp)
  "Return list of windows whose buffer name matches regexp."
  (my/filter #'(lambda (window)
              (string-match regexp
               (buffer-name (window-buffer window))))
          (window-list)))
(global-set-key [(super m)]
                #'(lambda ()
                    (interactive)
                    (call-interactively 'ag)
                    (select-window ; select ag buffer
                     (car (my/get-buffer-window-list-regexp "^\\*ag ")))))

;; -- end of emacs --



