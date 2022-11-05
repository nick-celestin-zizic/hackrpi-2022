;; -*- lexical-binding: t; -*-
;;; pomodoro.el --
;;;

;;;###autoload

(require 'cl)

(cl-defstruct pomodoro
  seconds timer-object)

(defun pomodoro-minutes (pomodoro)
  (/ (pomodoro-seconds pomodoro) 60))

(defvar pomodoro-number-of-intervals 5)
(defvar pomodoro-work-length 1)
(defvar pomodoro-short-break-length 1)
(defvar pomodoro-long-break-length 2)

;; Extremely lazy method!!
(defun pomodoro-interval (pomodoro)
  (guess-interval (pomodoro-minutes pomodoro) 0))
(defun guess-interval (pomodoro-minute i)
  (if (>= pomodoro-minute (minutes i))
      (guess-interval pomodoro-minute (1+ i))
    (1- i)))
;; TODO: We have to figure out the inverse of this to derive better
;; method
(defun minutes (i)
  (cond ((= i 0) 0)
        ((= (mod i 4) 0)
         (+ (minutes (- i 1))
            pomodoro-work-length
            pomodoro-long-break-length))
        (t
         (+ (minutes (- i 1))
            pomodoro-work-length
            pomodoro-short-break-length))))

(defun interval-finished? (pomodoro)
  (not (= (pomodoro-interval
           (make-pomodoro :seconds (1- (pomodoro-seconds pomodoro))))
          (pomodoro-interval pomodoro))))


(defun overtime? ()
  nil)
(defun done? (pomodoro)
  (>= (pomodoro-interval pomodoro)
      pomodoro-number-of-intervals))
(defun short-break? (pomodoro)
  (not (long-break? pomodoro)))
(defun long-break? (pomodoro)
  (and (not (= (pomodoro-interval pomodoro) 0))
       (= 0 (mod (pomodoro-interval pomodoro) 4))))

(defun make-pomodoro-heart ()
  (let ((seconds-running 0)
        (timer (timer-create)))
    (timer-set-time timer 1 1)
    (timer-set-function
     timer #'(lambda ()
               (setq seconds-running
                     (1+ seconds-running))
               (pomodoro-handler
                (make-pomodoro
                 :seconds seconds-running
                 :timer-object timer))))
    timer))

(defun pomodoro-handler (pomodoro)
  (run-hook-with-args
   'pomodoro-heart-beat pomodoro)
  (when (interval-finished? pomodoro)
    ;; TODO: abstract further
    (cond ((overtime?)
           (run-hook-with-args
            'pomodoro-overtime pomodoro))
          ((done? pomodoro)
           (run-hook-with-args
            'pomodoro-done pomodoro))
          ((short-break? pomodoro)
           (run-hook-with-args
            'pomodoro-short-break pomodoro))
          ((long-break? pomodoro)
           (run-hook-with-args
            'pomodoro-long-break pomodoro)))))


(add-hook 'pomodoro-overtime
          (lambda (p) (message "overtime!")))
(add-hook !'pomodoro-done
          (lambda (p) (and (message "done!")
                      (cancel-timer (pomodoro-timer-object p)))))
(add-hook 'pomodoro-short-break
          (lambda (p) (message "short break!")))
(add-hook 'pomodoro-long-break
          (lambda (p) (message "long-break!")))
(add-hook 'pomodoro-heart-beat
          (lambda (p)
            (and (= 0 (mod (pomodoro-seconds p) 10))
                 (message "%s" p))))

(timer-activate (make-pomodoro-heart))




(defun pomodoro! ()
  (interactive "")
  (if (pomodoro-running?)
      (pomodoro :stop)
    (pomodoro :start)))

(provide 'pomodoro)
