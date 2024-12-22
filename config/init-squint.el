;; init-squint.el --- -*- lexical-binding: t; -*-


(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 200)
        (presentation . 300)))

(defun squint--height-from-label (label)
  (alist-get (intern label) squint-heights))

(defun squint--set-height (height)
  (set-face-attribute 'default nil :height height))

(defun squint--set-height-from-label (label)
  (squint--set-height (squint--height-from-label label)))

(defun squint (label)
  "Adjust font height based on predefined settings.
LABEL is a string that corresponds to a key in `squint-heights'."
  (interactive (list
                (completing-read "Desired height: " squint-heights)))
  (squint--set-height-from-label label))

;; Testing
;; (squint 'presentation)
;; (squint 'laptop)

;; (completing-read
;;  "Choose your poison: "
;;  '("arsenic" "digitalis" "strychnine" "belladonna"))

;; (consult--read)

;;
(provide 'init-squint)
