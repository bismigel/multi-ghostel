;;; multi-ghostel.el --- Manage multiple ghostel terminals -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bismigel

;; Author: bismigel <bismigel@pm.me>
;; Assisted-by: opencode:deepseek-flash
;; URL: https://github.com/bismigel/multi-ghostel
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.52.0"))
;; Keywords: terminals, processes
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;; Ported from multi-vterm.el; see the Commentary for credits.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A ghostel port of `multi-vterm.el' (like `multi-term.el' but for
;; vterm).  The author's preferred entry point is `multi-ghostel-cwd':
;; one reusable terminal per directory.
;;
;; Ghostel already ships most of what `multi-vterm' provides, so the
;; corresponding commands are intentionally NOT reimplemented here:
;;
;;   multi-vterm-project       -> `ghostel-project'
;;   multi-vterm-next          -> `ghostel-next'
;;   multi-vterm-prev          -> `ghostel-previous'
;;   multi-vterm-get-buffer    -> `ghostel-create'
;;   multi-vterm-handle-close  -> `ghostel-kill-buffer-on-exit'
;;   multi-vterm-rename-buffer -> `rename-buffer' /
;;                                `ghostel-buffer-name-function'
;;   multi-vterm-shell-name    -> `ghostel-shell'
;;
;; A few commands have no exact equivalent:
;;
;; * `multi-vterm' always starts a NEW session in the current directory,
;;   while `ghostel' reuses its existing buffer (which keeps whatever
;;   directory it was in).  `multi-ghostel' below restores the
;;   always-new behavior.
;;
;; * Ghostel has no command that reuses a terminal by directory.
;;   `multi-ghostel-cwd' switches to the terminal whose
;;   `default-directory' matches the current one, or starts a new session
;;   there; naming is left to `ghostel-buffer-name-function'.
;;
;; * Ghostel has no *dedicated* terminal window that can be toggled open
;;   and closed independently of the terminals that participate in normal
;;   window management.  This package adds one:
;;
;;     M-x multi-ghostel-dedicated-open    open (creating if needed)
;;     M-x multi-ghostel-dedicated-close   close, keeping the buffer alive
;;     M-x multi-ghostel-dedicated-toggle  toggle
;;     M-x multi-ghostel-dedicated-select  focus the dedicated window
;;
;; The dedicated buffer is a regular ghostel buffer created with
;; `ghostel-create', so it still receives output while its window is
;; closed and reappears intact on the next `multi-ghostel-dedicated-open'.
;;
;; Suggested key bindings:
;;
;;   (global-set-key (kbd "C-c t") #'multi-ghostel-cwd)
;;   (global-set-key (kbd "C-c T") #'multi-ghostel)
;;   (global-set-key (kbd "C-`")   #'multi-ghostel-dedicated-toggle)
;;
;; Acknowledgements:
;;
;; Ported from multi-vterm.el by Minh Nguyen-Hue <minh.nh1989@gmail.com>:
;; https://github.com/suonlight/multi-vterm
;;
;; Developed with the assistance of an AI coding assistant (opencode).

;;; Code:

(require 'ghostel)
(eval-when-compile (require 'subr-x))  ; `if-let*' on Emacs < 29

(defgroup multi-ghostel nil
  "Manage multiple ghostel terminals."
  :group 'terminals
  :prefix "multi-ghostel-")

(defcustom multi-ghostel-dedicated-window-height 30
  "Height of the dedicated `multi-ghostel' window, in lines.
Ignored when `multi-ghostel-dedicated-window-height-percent' is non-nil."
  :type 'integer
  :group 'multi-ghostel)

(defcustom multi-ghostel-dedicated-window-height-percent nil
  "Height of the dedicated `multi-ghostel' window as a percentage.
When non-nil, overrides `multi-ghostel-dedicated-window-height'.  The
value is clamped to `multi-ghostel-dedicated-window-height-percent-limits'."
  :type '(choice (const :tag "Use absolute height" nil)
                 (integer :tag "Percent of the frame height"))
  :group 'multi-ghostel)

(defcustom multi-ghostel-dedicated-buffer-name "*ghostel - dedicated*"
  "Name of the dedicated `multi-ghostel' buffer."
  :type 'string
  :group 'multi-ghostel)

(defconst multi-ghostel-dedicated-window-height-percent-limits '(10 90)
  "Lower and upper bounds for the dedicated window height percentage.")

(defvar multi-ghostel-dedicated-window nil
  "The dedicated `multi-ghostel' window, or nil when none exists.")

(defvar multi-ghostel-dedicated-buffer nil
  "The dedicated `multi-ghostel' buffer, or nil when none exists.")

;;;###autoload
(defun multi-ghostel ()
  "Start a new ghostel terminal in the current directory.
Unlike `ghostel', which reuses its existing terminal (and therefore that
terminal's directory), this always creates a fresh session, mirroring
`multi-vterm'.  The new buffer is shown in the selected window and
returned."
  (interactive)
  (ghostel-create ghostel-buffer-name
                  (append display-buffer--same-window-action
                          '((category . comint)))))

;;;###autoload
(defun multi-ghostel-cwd ()
  "Switch to the ghostel terminal for the current directory.
Reuse a live ghostel buffer whose `default-directory' matches the current
directory (this follows `cd', via OSC 7); otherwise start a new session
there.  Buffer naming is left to `ghostel-buffer-name-function', e.g.

  (setq ghostel-buffer-name-function #\\='ghostel-buffer-name-by-directory)"
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (if-let* ((existing
               (seq-find (lambda (buffer)
                           (and (with-current-buffer buffer
                                  (derived-mode-p 'ghostel-mode))
                                (equal (expand-file-name
                                        (buffer-local-value 'default-directory
                                                            buffer))
                                       dir)))
                         (buffer-list))))
        (pop-to-buffer existing
                       (append display-buffer--same-window-action
                               '((category . comint))))
      (ghostel-create ghostel-buffer-name
                      (append display-buffer--same-window-action
                              '((category . comint)))))))

(defun multi-ghostel-window-exist-p (window)
  "Return non-nil when WINDOW is a live window."
  (and window (window-live-p window)))

(defun multi-ghostel-buffer-exist-p (buffer)
  "Return non-nil when BUFFER is a live buffer."
  (and buffer (buffer-live-p buffer)))

(defun multi-ghostel-dedicated-exist-p ()
  "Return non-nil when the dedicated buffer and window both exist."
  (and (multi-ghostel-buffer-exist-p multi-ghostel-dedicated-buffer)
       (multi-ghostel-window-exist-p multi-ghostel-dedicated-window)))

(defun multi-ghostel-current-window-height (&optional window)
  "Return the usable height of WINDOW in lines.
Unlike `window-height', this excludes the mode line, matching the unit
`split-window' expects.  WINDOW defaults to the selected window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun multi-ghostel-dedicated-calc-window-height ()
  "Return the requested height for the dedicated `multi-ghostel' window."
  (if multi-ghostel-dedicated-window-height-percent
      (let* ((limits multi-ghostel-dedicated-window-height-percent-limits)
             (percent (max (car limits)
                           (min multi-ghostel-dedicated-window-height-percent
                                (cadr limits)))))
        (ceiling (* (float (multi-ghostel-current-window-height))
                    (/ (float percent) 100.0))))
    multi-ghostel-dedicated-window-height))

(defun multi-ghostel-dedicated-get-buffer ()
  "Return the dedicated `multi-ghostel' buffer, creating it if needed.
The buffer is created with `ghostel-create' and is therefore a normal
ghostel terminal that keeps running while its window is closed."
  (unless (multi-ghostel-buffer-exist-p multi-ghostel-dedicated-buffer)
    (let ((existing (get-buffer multi-ghostel-dedicated-buffer-name)))
      (when (and existing
                 (not (with-current-buffer existing
                        (derived-mode-p 'ghostel-mode))))
        (setq existing nil))
      (setq multi-ghostel-dedicated-buffer
            (or existing
                (ghostel-create multi-ghostel-dedicated-buffer-name)))))
  (with-current-buffer multi-ghostel-dedicated-buffer
    (add-hook 'kill-buffer-hook
              #'multi-ghostel--dedicated-kill-buffer-hook nil t))
  multi-ghostel-dedicated-buffer)

(defun multi-ghostel--dedicated-kill-buffer-hook ()
  "Forget the dedicated buffer when it is killed."
  (when (eq (current-buffer) multi-ghostel-dedicated-buffer)
    (setq multi-ghostel-dedicated-buffer nil)))

(defun multi-ghostel-dedicated-get-window ()
  "Split off and return the dedicated `multi-ghostel' window.
Return nil, without splitting, when the selected window is too small."
  (let* ((current (multi-ghostel-current-window-height))
         (requested (multi-ghostel-dedicated-calc-window-height))
         (min window-min-height)
         (size (max min (min (- current requested)
                             (- current min))))
         (window (condition-case nil
                     (split-window (selected-window) size)
                   (error nil))))
    (setq multi-ghostel-dedicated-window window)
    window))

;;;###autoload
(defun multi-ghostel-dedicated-open ()
  "Open the dedicated `multi-ghostel' window.
Create the dedicated buffer and window when they do not exist yet."
  (interactive)
  (multi-ghostel-dedicated-get-buffer)
  (unless (multi-ghostel-window-exist-p multi-ghostel-dedicated-window)
    (multi-ghostel-dedicated-get-window))
  (if (multi-ghostel-window-exist-p multi-ghostel-dedicated-window)
      (progn
        (set-window-buffer multi-ghostel-dedicated-window
                           multi-ghostel-dedicated-buffer)
        (set-window-dedicated-p multi-ghostel-dedicated-window t)
        (select-window multi-ghostel-dedicated-window))
    (user-error "Selected window is too small for a dedicated window")))

;;;###autoload
(defun multi-ghostel-dedicated-close ()
  "Close the dedicated `multi-ghostel' window.
The dedicated buffer is left alive, so it keeps running and can be shown
again with `multi-ghostel-dedicated-open'."
  (interactive)
  (if (multi-ghostel-window-exist-p multi-ghostel-dedicated-window)
      (let ((frame (window-frame multi-ghostel-dedicated-window)))
        (if (> (length (window-list frame)) 1)
            (let ((current-window (selected-window)))
              (delete-window multi-ghostel-dedicated-window)
              (setq multi-ghostel-dedicated-window nil)
              (when (multi-ghostel-window-exist-p current-window)
                (select-window current-window)))
          (user-error "Dedicated window is the only window of its frame")))
    (message "Dedicated `multi-ghostel' window does not exist")))

;;;###autoload
(defun multi-ghostel-dedicated-toggle ()
  "Toggle the dedicated `multi-ghostel' window."
  (interactive)
  (if (multi-ghostel-dedicated-exist-p)
      (multi-ghostel-dedicated-close)
    (multi-ghostel-dedicated-open)))

;;;###autoload
(defun multi-ghostel-dedicated-select ()
  "Select the dedicated `multi-ghostel' window."
  (interactive)
  (if (multi-ghostel-dedicated-exist-p)
      (select-window multi-ghostel-dedicated-window)
    (message "Dedicated `multi-ghostel' window does not exist")))

(provide 'multi-ghostel)
;;; multi-ghostel.el ends here
