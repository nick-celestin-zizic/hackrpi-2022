;;; day-night-switch.el -- switch themes depending on
;;; the time of day.
;;;

(defvar night-theme)
(defvar day-theme)

(defvar sunset)
(defvar sunrise)

(defun maybe-change-theme ()
  (or (and (night?) (theme? day-theme)
           (set-theme night-theme t))
      (and (day?) (theme? night-theme)
           (set-theme day-theme t))))

(defun set-theme (theme &optional no-confirm no-enable)
  (and (load-theme theme no-confirm t)
       (not no-enable)
       (custom-theme-set-variables 'user
        `(custom-enabled-themes '(,theme)))))

(defun theme? (theme)
  (or (eql theme (car custom-enabled-themes))
      (null custom-enabled-themes))) ;its null on initial start

(defun night? ()
  (not (day?)))

(defun day? ()
  (time-between? sunrise (current-time) sunset))

(defun time-between? (at-least time at-most)
  (let ((at-least (get-mm at-least))
        (time (get-mm time))
        (at-most (get-mm at-most)))
    (and (< at-least time at-most))))

(defun get-mm-hh (time)
  (if (stringp time)
      (seq-subseq (parse-time-string time) 1 3)
    (seq-subseq (decode-time time) 1 3)))

(defun get-mm (time)
  (let ((mm-hh (get-mm-hh time)))
    (+ (* 60 (cadr mm-hh)) (car mm-hh))))

(run-at-time t (* 10 60) #'maybe-change-theme)

(provide 'day-night-switch)
