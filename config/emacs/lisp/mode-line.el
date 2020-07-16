;;; mode-line.el -*- lexical-binding: t; -*-
;; 2020-7-6

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

(require 'battery)
(require 'cl-lib)

(defvar tnml-selected-window nil)

(defun tnml-record-selected-window ()
  (setq tnml-selected-window (selected-window)))

(defun tnml-update-all ()
  (force-mode-line-update t))

(add-hook 'post-command-hook 'tnml-record-selected-window)

(add-hook 'buffer-list-update-hook 'tnml-update-all)

(defun tnml/evil-module ()
  "Returns the current evil-state"
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
  "Returns a formatted string consisting of an icon representing the current buffer's status and its name"
  (mapcar (lambda (icon-pair)
	    (if (eval (car icon-pair))
		(setq module-icon (cdr icon-pair))))
	  '((t . (all-the-icons-material "edit"))
	    (buffer-read-only . (all-the-icons-material "lock"))
	    ((buffer-modified-p) . (all-the-icons-material "save"))
	    ((string-equal (symbol-name (symbol-value 'major-mode)) "eshell-mode") . (all-the-icons-alltheicon "terminal"))))
  (concat (propertize " " 'face '(:background "dark magenta"
				  :foreground "white"))
	  (propertize (eval module-icon)
		      'face `(:family ,(all-the-icons-octicon-family)
		              :height 1.0
		              :background "dark magenta"
		              :foreground "white")
		      'display '(raise -0.1))
	  (propertize (concat " " (buffer-name) " ")
		      'face '(:background "dark magenta"
			      :foreground "white"))))

(defun tnml/major-mode-module ()
  (concat " "
		  (let ((mode-string (symbol-name (symbol-value 'major-mode)))) 
			(substring mode-string 0 (string-match "-mode$" mode-string)))
		  " "))
                                                                                                                                                          
(defun tnml/position-module ()
  "Returns a formatted string of percentage of where point is in buffer, line number, and column number"
  (propertize (concat " "
		      (int-to-string (floor
				      (* (/ (float (point))
					    (point-max))
					 100)))
		      "%% "
		      (int-to-string (line-number-at-pos))
		      ":"
		      (int-to-string (current-column))
		      " ")
	      'face '(:background "gold3"
		      :foreground "gray18")))

(defun tnml/system-module ()
  "Returns a formatted string of battery percentage and time"
  (propertize (concat " "
		      (battery-format "%p"
				      (funcall battery-status-function))
		      "%% "
		      (format-time-string "%I:%M %p "))
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
