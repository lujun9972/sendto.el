;; -*- lexical-binding: t; -*-

(defun sendto--generate-menu-fn (fn)
  (lambda ()
    (interactive)
    (let* ((start (region-beginning))
           (end (region-end))
           (content (buffer-substring start end)))
      (funcall fn content))))

(defun sendto--generate-menu-item (fn)
  (vector (symbol-name fn) (sendto--generate-menu-fn fn)))

(defun sendto--generate-menu (&rest functions)
  (cons "sendto" (mapcar #'sendto--generate-menu-item functions)))

(defun sendto-file (content)
  "Append content to a file"
  (let ((file (read-file-name "append to which file: ")))
    (append-to-file content nil file)))

(defun sendto-buffer (content)
  "Append content to a buffer"
  (with-current-buffer (read-buffer "append to which buffer: ")
    (save-excursion
      (goto-char (point-max))
      (insert content))))

(defun sendto-mail (content)
  "mail the content"
  (mail)
  (goto-char (point-max))
  (insert content))

(defgroup sendto nil
  "send content of region to functions")

(defcustom sendto-function-list '(sendto-file sendto-buffer sendto-mail)
  "Functions to be send to"
  :group 'sendto
  :type '(repeat function)
  :set (lambda (item val)
         (set-default item val)
         (when (and (boundp 'sendto-mode)
                    sendto-mode)
           (easy-menu-define sendto-menu nil "Menu for sendto" (apply #'sendto--generate-menu val)))))

(defun sendto-popup-functions (&rest functions)
  (unless (and (boundp 'sendto-menu)
              (keymapp sendto-menu))
    (easy-menu-define sendto-menu nil "Menu for sendto" (apply #'sendto--generate-menu functions)))
  (popup-menu sendto-menu))

(defun sendto-popup (&rest ignore)
  "pop up a sendto menu"
  ;; (interactive)
  (apply #'sendto-popup-functions sendto-function-list))

(define-minor-mode sendto-mode "send region content to a function"
  ;; The initial value
  nil
  ;; The indicator for the mode line
  " Sendto"
  :group 'sendto
  :global t
  (if (and (boundp 'sendto-mode)
           sendto-mode)
      (advice-add #'mouse-set-region :after 'sendto-popup)
    (advice-remove #'mouse-set-region 'sendto-popup)))



(provide 'sendto)
