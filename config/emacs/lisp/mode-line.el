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

(custom-set-faces '(mode-line-inactive ((t ( :background "#373844"
                                             :foreground "#f8f8f2"))))
                  '(mode-line ((t ( :background "#44475a"
                                    :foreground "unspecified-fg"
                                    :inverse-video nil)))))

(defface tnml-evil-normal '((t :background "dodger blue"
                                :foreground "white"
                                :inherit 'mode-line)) nil
                                :group 'tn/mode-line)
(defface tnml-evil-insert '((t :background "yellow"
								:foreground "gray10"
                                :inherit 'mode-line)) nil
                                :group 'tn/mode-line)
(defface tnml-evil-visual '((t :background "MediumPurple4"
								:foreground "seashell1"
                                :inherit 'mode-line)) nil
                                :group 'tn/mode-line)
(defface tnml-evil-emacs '((t :background "plum1"
							   :foreground "gray10"
                               :inherit 'mode-line)) nil
                               :group 'tn/mode-line)
(defface tnml-evil-replace '((t :background "dark red"
								  :foreground "white smoke"
                                  :inherit 'mode-line)) nil
                                  :group 'tn/mode-line)
(defface tnml-evil-motion '((t :background "navy"
								:foreground "seashell1"
                                :inherit 'mode-line)) nil
                                :group 'tn/mode-line)
(defface tnml-evil-operator '((t :background "SeaGreen1"
								  :foreground "gray10"
                                  :inherit 'mode-line)) nil
                                  :group 'tn/mode-line)
(defun tnml/evil-module ()
  "Return current evil-state."
  (cond
   ((string-equal evil-state 'normal)
	(propertize " NORMAL " 'face 'tnml-evil-normal))
   ((string-equal evil-state 'insert)
	(propertize " INSERT " 'face 'tnml-evil-insert))
   ((string-equal evil-state 'visual)
	(propertize " VISUAL " 'face 'tnml-evil-visual))
   ((string-equal evil-state 'emacs)
	(propertize " EMACS " 'face 'tnml-evil-emacs))
   ((string-equal evil-state 'replace)
	(propertize " REPLACE " 'face 'tnml-evil-replace))
   ((string-equal evil-state 'motion)
	(propertize " MOTION " 'face 'tnml-evil-motion))
   ((string-equal evil-state 'operator)
	(propertize " OPERATOR " 'face 'tnml-evil-operator))))

(defface tnml-writable '((t :background "dark magenta"
						   :foreground "white"
                           :inherit 'mode-line)) nil
                           :group 'tn/mode-line)


(defun tnml/writable-module ()
  "Return a formatted string consisting of an icon representing the current buffer's status and its name."
  (let* ((selectedp (eq tnml-selected-window (selected-window)))
         (iconface (if selectedp
                       'tnml-writable
                     nil))
         (icon (if (buffer-modified-p)
                   (all-the-icons-material "save"
                                           :height 0.85
                                           :v-adjust -0.15
                                           :face iconface)
                 (all-the-icons-icon-for-mode major-mode
                                              :height 0.8
                                              :v-adjust -0.1
                                              :face iconface))))
    (format "%s%s%s"
            (propertize " " 'face iconface)
            icon
            (propertize (format " %s " (buffer-name)) 'face iconface))))

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
	unf-str))

(defun tnml/system-module ()
  "Return a formatted string of battery percentage and time."
  (propertize (format " %s%%%% %s "
					  (battery-format "%p"
									  (funcall battery-status-function))
					  (format-time-string "%I:%M %p"))
			  'face '(:background "gray87"
					              :foreground "gray20")))

(defvar tnml/format-left
  '((tnml/evil-module)
	(tnml/writable-module)
	""))

(defvar tnml/format-right
  '((tnml/major-mode-module)
	(tnml/position-module)
	(if (eq tnml-selected-window (selected-window))
		(tnml/system-module)
	  "")))

(defun tnml/format-string ()
  "Evaluate tnml/format-left and tnml/format-right, and add enough space in between to take up the entire window width."
  (setq avail-chars (window-width))
  (setq left "")
  (setq right "")
  (cl-mapc '(lambda (l-str r-str)
				(setq new-l-module (eval l-str))
				(setq new-r-module (eval r-str))
				(when (>= (- avail-chars
						   (length new-l-module))
						0)
				  (setq left (concat left new-l-module))
				  (setq avail-chars (- avail-chars (length new-l-module))))
				(when (>= (- avail-chars
						   (length new-r-module))
						0)
					(setq right (concat right new-r-module))
					(setq avail-chars (- avail-chars (length new-r-module)))))
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
