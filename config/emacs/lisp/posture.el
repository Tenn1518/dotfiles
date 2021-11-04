;;; posture.el --- Remind user to check their posture with timer

;;; Commentary:

;; Remind user to check their posture with an interval defined by
;; posture-reminder-timer-duration.  The user is asked whether they are sitting
;; straight.  If they are not, the user is reminded with tips on keeping good
;; posture.

;; posture-reminder-display-interval controls how long messages stay in the
;; minibuffer when the user answers "no" to the posture-reminder prompt.

;; TODO: Consider a variable to keep track of how many times user hasn't sit
;; straight in their chair to bully them into remembering.

;;; Code:

(defvar posture-reminder-display-interval
  3
  "When an integer, define how long reminders will stay on screen
during a call to posture-reminder.")

(defvar posture-reminder--timer
  nil
  "Timer used in (toggle-posture-reminder).")

(defvar posture-reminder-timer-duration
  900
  "Set how long posture-reminder waits in seconds before
reminding the user to check their posture.  By default, it is
15 minutes.")

(defun posture-reminder ()
  "Ask user if their posture is correct.  If not, remind them of good posture habits."
  (let ((use-dialog-box nil))
    (if (y-or-n-p "Are you sitting straight with your shoulders back? ")
      (message "Good.")
    (progn
      (mapc (lambda (str)
	      (message str)
	      (sit-for posture-reminder-display-interval))
	    '("Sit up with your back straight and your shoulders back."
	      "Distribute your body weight evenly on both hips."
	      "Keep your feet flat on the floor."
	      "Rest your elbows and arms on your chair or desk, keeping your shoulders relaxed."
	      "Bend your knees at a right angle. Do not cross your legs."
	      ""))))))

(defun toggle-posture-reminder ()
  "Toggle whether posture-reminder is active."
  (interactive)
  (if posture-reminder--timer
      (progn
	(cancel-timer posture-reminder--timer)
	(setq posture-reminder--timer nil)
	(message "Back reminder timer disabled."))
    (progn
      (setq posture-reminder--timer (run-at-time "5 sec" posture-reminder-timer-duration #'posture-reminder))
      (message "Posture reminder timer enabled."))))

(provide 'posture)
;;; posture.el ends here
