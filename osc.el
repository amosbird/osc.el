;;; osc.el --- Support for osc handlers
;; Package-Version: 20180907.2225
;;; Commentary:
;;; Code:
(defcustom osc-http-addr "172.26.165.60:8866"
  "Non-nil means automatically save place in each file..."
  :type 'string
  :group 'osc)

(defun osc-command (string)
  (let* ((osc-string (concat "\e]52;z;" (base64-encode-string (encode-coding-string string 'utf-8) t) "\07"))
         (escaped (if (string-match "tmux" (concat "" (getenv "TMUX"))) (concat "\033Ptmux;\033" osc-string "\033\\") osc-string)))
    (send-string-to-terminal escaped)))

;;;###autoload
(defun osc-fcitx-activate ()
    (osc-command "fcitx-remote -o"))

;;;###autoload
(defun osc-fcitx-deactivate ()
    (osc-command "fcitx-remote -c"))

;;;###autoload
(defun osc-navigate (direction)
  (osc-command "bspc node -f next.local"))

;;;###autoload
(defun osc-nav-left ()
  (interactive)
  (osc-navigate "west"))

;;;###autoload
(defun osc-nav-right ()
  (interactive)
  (osc-navigate "east"))

;;;###autoload
(defun osc-nav-up ()
  (interactive)
  (osc-navigate "north"))

;;;###autoload
(defun osc-nav-down ()
  (interactive)
  (osc-navigate "south"))

(defun i3-navigate (direction)
  (let
    ((cmd (concat "windmove-" direction)))
    (condition-case nil
      (funcall (intern cmd))
      (error
        (i3-command direction)))))

(defun i3-command (direction)
  (shell-command-to-string
    (concat "i3-msg focus " direction)))

;;;###autoload
(defun i3-nav-left ()
  (interactive)
  (i3-navigate "left"))

;;;###autoload
(defun i3-nav-right ()
  (interactive)
  (i3-navigate "right"))

;;;###autoload
(defun i3-nav-up ()
  (interactive)
  (i3-navigate "up"))

;;;###autoload
(defun i3-nav-down ()
  (interactive)
  (i3-navigate "down"))

;;;###autoload
(defun osc-select-text (string &optional replace yank-handler)
  (if (display-graphic-p)
      (gui-select-text string)
    (let ((b64-length (+ (* (length string) 3) 2)))
      (if (<= b64-length 100000)
          (let* ((osc-string (concat "\e]52;c;" (base64-encode-string (encode-coding-string string 'utf-8) t) "\07"))
                 (escaped (if (string-match "tmux" (concat "" (getenv "TMUX"))) (concat "\033Ptmux;\033" osc-string "\033\\") osc-string)))
            (send-string-to-terminal escaped))
        (message "Selection too long to send to terminal %d" b64-length)
        (sit-for 2)))))

(require 'browse-url)

;;;###autoload
(defun browse-url-osc (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((url (replace-regexp-in-string "^file://" (concat "http://" osc-http-addr) url))
         (osc-string (concat "\e]52;" (if _new-window "y" "x") ";" (base64-encode-string (encode-coding-string url 'utf-8) t) "\07"))
         (escaped (if (string-match "tmux" (concat "" (getenv "TMUX"))) (concat "\033Ptmux;\033" osc-string "\033\\") osc-string)))
    (send-string-to-terminal escaped)))

(provide 'osc)

;;; osc.el ends here
