;;; ob-cereal.el --- org-babel support for serial communication
;; Copyright (C) Andreas Müller
;; Original Author and Idea and 100% credit to: Andreas Müller
;; Revision Author: elemb
;; Keywords: tools, comm, org-mode, serial, literate programming, reproducible development
;; Homepage: https://github.com/elemb/ob-cereal ;; ← update to your fork URL
;; Version: 0.1.0
;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;; Commentary:
;; Org-babel support for serial communication with embedded devices.
;; Supports Forth systems like Mecrisp and ZeptoForth.
;;
;; For Zeptoforth:
;; - Uses CRLF line endings by default.
;; - Waits for ACK (0x06) after command processing; falls back to silence detection.
;; - Detects NAK (0x15) as error.
;;; Code:
(require 'ob)

(defgroup ob-cereal nil
  "Serial communication support for org babel."
  :group 'org-babel)

(defcustom ob-cereal-debug nil
  "Whether to show debug messages for serial communication."
  :group 'ob-cereal
  :type 'boolean)

(defvar org-babel-default-header-args:cereal
  '((:port       . "/dev/ttyUSB0")
    (:speed      . 115200)
    (:bytesize   . 8)
    (:parity     . nil)
    (:stopbits   . 1)
    (:flowcontrol . nil)
    (:timeout    . 15)                  ; increased default for long-running Forth commands
    (:lineend    . "\r\n")              ; Zeptoforth prefers CRLF
    (:ienc       . "raw")
    (:oenc       . "raw"))
  "Default arguments for evaluating a cereal block.")

(defun ob-cereal--to-number (val default)
  "Convert VAL to number. If already number, return it. If string, convert. Else DEFAULT."
  (cond
   ((numberp val) val)
   ((and (stringp val) (not (string-empty-p val)))
    (let ((num (string-to-number val)))
      (if (zerop num) default num)))
   (t default)))

(defun ob-cereal--to-string (val)
  "Convert VAL to string if it's a symbol, otherwise return as-is."
  (cond
   ((stringp val) val)
   ((symbolp val) (symbol-name val))
   ((numberp val) (number-to-string val))
   (t (format "%s" val))))

(defun ob-cereal--to-symbol (val valid-syms)
  "Convert VAL to symbol if in VALID-SYMS, else nil."
  (let ((sym (cond
              ((symbolp val) val)
              ((stringp val)
               (let ((s (downcase val)))
                 (cond ((member s '("nil" "none")) nil)
                       (t (intern s)))))
              (t nil))))
    (if (memq sym valid-syms) sym nil)))

;;;###autoload
(defun org-babel-execute:cereal (body params)
  "Execute serial code block with BODY and PARAMS."
  (when ob-cereal-debug
    (message "ob-cereal executing with params: %S" params))
  (let* ((port       (ob-cereal--to-string (or (cdr (assq :port params))       "/dev/ttyUSB0")))
         (speed      (ob-cereal--to-number (cdr (assq :speed params))      115200))
         (bytesize   (ob-cereal--to-number (cdr (assq :bytesize params))   8))
         (parity     (ob-cereal--to-symbol (cdr (assq :parity params))     '(odd even)))
         (stopbits   (ob-cereal--to-number (cdr (assq :stopbits params))   1))
         (flowcontrol(ob-cereal--to-symbol (cdr (assq :flowcontrol params))'(hw sw)))
         (timeout    (ob-cereal--to-number (cdr (assq :timeout params))    15))
         (lineend    (ob-cereal--to-string (or (cdr (assq :lineend params)) "\r\n")))
         (ienc       (ob-cereal--to-string (or (cdr (assq :ienc params))   "raw")))
         (oenc       (ob-cereal--to-string (or (cdr (assq :oenc params))   "raw")))
         (proc-name  (format "ob-cereal-%s" (replace-regexp-in-string "[/:]" "-" port)))
         (proc-buf   (format " *ob-cereal-%s*" (replace-regexp-in-string "[/:]" "-" port))))
    (when ob-cereal-debug
      (message "ob-cereal: port=%s speed=%d bytesize=%d parity=%s stopbits=%d flow=%s timeout=%d lineend=%S"
               port speed bytesize parity stopbits flowcontrol timeout lineend))
    ;; Clean up stale process/buffer
    (when (get-process proc-name) (delete-process proc-name))
    (when (get-buffer proc-buf) (kill-buffer proc-buf))
    (condition-case err
        (let ((proc (make-serial-process
                     :name proc-name
                     :buffer proc-buf
                     :port port
                     :speed speed
                     :bytesize bytesize
                     :parity parity
                     :stopbits stopbits
                     :flowcontrol flowcontrol)))
          (unwind-protect
              (let ((encoded-body body))
                (when (string= ienc "hex")
                  (setq encoded-body
                        (mapconcat (lambda (hex) (char-to-string (string-to-number hex 16)))
                                   (split-string body "[ \t\n]+" t) "")))
                ;; Send command
                (process-send-string proc (concat encoded-body lineend))
                ;; Wait for ACK (0x06) or NAK (0x15), with silence fallback
                (let* ((output "")
                       (ack-seen nil)
                       (nak-seen nil)
                       (start (float-time))
                       (last-recv (float-time))
                       (quiet-sec 0.0)
                       (get-output (lambda () (with-current-buffer proc-buf (buffer-string)))))
                  (while (and (process-live-p proc)
                              (< (- (float-time) start) timeout)
                              (not ack-seen)
                              (not nak-seen))
                    (accept-process-output proc 0.1 nil t)
                    (let ((new-output (funcall get-output)))
                      (when (> (length new-output) (length output))
                        (setq last-recv (float-time)
                              quiet-sec 0.0
                              output new-output))
                      ;; Scan for ACK/NAK
                      (when (string-match "\006" output) (setq ack-seen t))
                      (when (string-match "\025" output) (setq nak-seen t))))
                    ;; Silence fallback
                    (setq quiet-sec (if (> (- (float-time) last-recv) 1.0)
                                        (+ quiet-sec 0.1)
                                      0.0))
                    (when (> quiet-sec 4.0)
                      (when ob-cereal-debug
                        (message "ob-cereal: No ACK/NAK, but quiet >4s — assuming done"))
                      (setq ack-seen t)))
                  ;; Annotate errors/timeouts
                  (when nak-seen
                    (setq output (concat output "\n[NAK received - possible command error]")))
                  (unless (or ack-seen nak-seen)
                    (setq output (concat output "\n[Timeout - no ACK/NAK received]")))
                  ;; Cleanup & process result
                  (setq result output)))
            ;; Always clean up
            (when (process-live-p proc) (delete-process proc))
            (when (get-buffer proc-buf) (kill-buffer proc-buf)))
          ;; Strip unwanted control chars (keep \n \t \r for readability)
          (setq result (replace-regexp-in-string "[\x00-\x08\x0B-\x1E\x7F]" "" result))
          ;; Optional hex output
          (cond
           ((string= oenc "hex")
            (setq result (mapconcat (lambda (c) (format "%02x" (string-to-char c))) result " ")))
           ((string= oenc "HEX")
            (setq result (mapconcat (lambda (c) (format "%02X" (string-to-char c))) result " "))))
          result)
      (error (format "Error in serial communication: %s" (error-message-string err))))))

(provide 'ob-cereal)
;;; ob-cereal.el ends here
