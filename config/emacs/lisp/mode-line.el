;;; mode-line.el --- a mode line more reminiscent of vim-airline -*- lexical-binding: t; -*-
;; 2020-7-6

;;; Commentary:

;; structure of mode-line
;; evil-module for vi mode
;; status of buffer icon with buffer name
;; name of major mode
;; percentage of where point is in buffer, current line number, total number of lines, and column number
;; battery percentage and time of day

;; TODO
;; Don't add color to modules if on inactive window

;; code to discern whether mode line is being rendered on the active
;; window taken from https://emacs.stackexchange.com/a/26345

;;; Code:

(require 'battery)
(require 'cl-lib)

(defvar tnml-selected-window nil)

(defun tnml-record-selected-window ()
  "Record current window in variable."
  (setq tnml-selected-window (selected-window)))

(defun tnml-update-all ()
  "Force mode line update."
  (force-mode-line-update t))

(add-hook 'post-command-hook 'tnml-record-selected-window)

(add-hook 'buffer-list-update-hook 'tnml-update-all)

(defun tnml/evil-module ()
  "Return current evil-state."
  (cond
   ((string-equal evil-state 'normal)
    (propertize " NORMAL "
		'face '(:background "dodger blue"
			:foreground "white"
			:weight ultra-bold)))
   ((string-equal evil-state 'insert)
    (propertize " INSERT "
		'face '(:background "yellow"
		        :foreground "gray10"
			:weight ultra-bold)))
   ((string-equal evil-state 'visual)
    (propertize " VISUAL "
		'face'(:background "MediumPurple4"
		       :foreground "seashell1"
		       :weight ultra-bold)))
   ((string-equal evil-state 'emacs)
    (propertize " EMACS "
		'face '(:background "plum1"
			:foreground "gray10"
			:weight ultra-bold)))
   ((string-equal evil-state 'replace)
    (propertize " REPLACE "
		'face '(:background "dark red"
			:foreground "white smoke"
			:weight ultra-bold)))
   ((string-equal evil-state 'motion)
    (propertize " MOTION "
		'face '(:background "navy"
			:foreground "seashell1"
			:weight ultra-bold)))
   ((string-equal evil-state 'operator)
    (propertize " OPERATOR "
		'face '(:background "SeaGreen1"
			:foreground "gray10"
			:weight ultra-bold)))))

(defun tnml/writable-module ()
  "Return a formatted string consisting of an icon representing the current buffer's status and its name."
  ;; If the condition of the alist item is true, set writable-module icon
  ;; Conditions get more specific lower in the alist
  (mapc (lambda (icon-pair)
	    (if (eval (car icon-pair))
			(setq module-icon (propertize (eval (cdr icon-pair))
										  'face `(:family ,(all-the-icons-octicon-family)
												  :height 1.0)
										  'display '(raise -0.1)))))
	  '((t . (all-the-icons-material "edit"))
		((derived-mode-p 'prog-mode) . (all-the-icons-octicon "code"))
	    (buffer-read-only . (all-the-icons-material "lock"))
	    ((buffer-modified-p) . (all-the-icons-material "save"))
	    ((derived-mode-p 'eshell-mode) . (all-the-icons-alltheicon "terminal"))))
  (setq unf-str (format " %s %s "
						module-icon
						(buffer-name)))
  (if (eq tnml-selected-window (selected-window))
	  (propertize unf-str
				   'face '(:background "dark magenta"
									   :foreground "white"))
	(propertize unf-str
				'face 'mode-line-inactive)))

(defun tnml/major-mode-module ()
  "Return the current major mode without the '-mode' suffix."
  (format " %s "
		  (let ((mode-string (symbol-name (symbol-value 'major-mode))))
			(substring mode-string
					   0
					   (string-match "-mode$"
									 mode-string)))))

(defun tnml/position-module ()
  "Return a formatted string of percentage of where point is in buffer, line number, and column number."
  (setq unf-str (format " %s%%%% %s:%s "
						(floor (* (/ (float (point))
									 (point-max))
								  100))
						(line-number-at-pos)
						(int-to-string (current-column))))
  (if (equal (selected-window) tnml-selected-window)
	  (propertize unf-str 'face '(:background "gold3"
											  :foreground "gray18"))
	(propertize unf-str 'face 'mode-line-inactive)))

(defun tnml/position-module ()
  "Return a formatted string of percentage of where point is in buffer, line number, and column number."
  (setq unf-str (format " %s%%%% %s:%s "
						(floor (* (/ (float (point))
									 (point-max))
								  100))
						(line-number-at-pos)
						(int-to-string (current-column))))
  (if (equal (selected-window) tnml-selected-window)
	  (propertize unf-str 'face '(:background "gold3"
											  :foreground "gray18"))
	(propertize unf-str 'face 'mode-line-inactive)))

(defun tnml/system-module ()
  "Return a formatted string of battery percentage and time."
  (propertize (format " %s%%%% %s "
					  (battery-format "%p"
									  (funcall battery-status-function))
					  (format-time-string "%I:%M %p")) 
			  'face '(:background "gray87"
					  :foreground "gray20"
					  :weight ultrabold)))

(defvar tnml/format-left
  '((tnml/evil-module)
	(tnml/writable-module)
	""))

(defvar tnml/format-right
  '((tnml/major-mode-module)
	(tnml/position-module)
	(if (eq tnml-selected-window (selected-window))
		(tnml/system-module)
	  " ")))

(defun tnml/format-string ()
  "Evaluate tnml/format-left and tnml/format-right, and add enough space in between to take up the entire window width."
  (setq avail-chars (window-width))
  (setq left "")
  (setq right "")
  (cl-mapcar '(lambda (l-str r-str)
				(setq new-l-module (eval l-str))
				(setq new-r-module (eval r-str))
				(if (>= (- avail-chars
						   (length new-l-module))
						0)
					(progn (setq left (concat left new-l-module))
						   (setq avail-chars (- avail-chars (length new-l-module)))))
				(if (>= (- avail-chars
						   (length new-r-module))
						0)
					(progn (setq right (concat right new-r-module))
						   (setq avail-chars (- avail-chars (length new-r-module))))))
			 tnml/format-left
			 tnml/format-right)
  (setq tnml/spaces (+ (- (window-width)
						  (length left)
						  (length right))
					   1)) ;; TODO - find number of occurences of %%
			   ;; escape characters to add lost spaces
	(concat left (make-string tnml/spaces ? ) right))

(setq-default mode-line-format
	      '(:eval (tnml/format-string)))

(provide 'mode-line)
;;; mode-line.el ends here
