;;; ob-uart.el --- org-babel support for UART communication

;; Copyright (C) Andreas Müller

;; Author: Andreas Müller
;; Revision Author: elemb
;; Keywords: tools, comm, org-mode, UART, literate programming, reproducible development
;; Homepage: https://github.com/elemb/ob-uart
;; Version: 0.1.0

;; code inspired by ob-restclient.el - https://github.com/alf/ob-restclient.el

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package provides org-babel support for UART communication.
;; It allows executing UART commands directly in org-mode code blocks.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(defgroup ob-uart nil
  "UART support for org babel."
  :group 'org-babel)

(defcustom ob-uart-debug t
  "Whether to show debug messages for UART communication."
  :package-version '(ob-uart . "0.1.0")
  :group 'ob-uart
  :type 'boolean)

(defun ob-uart-detect-default-port ()
  "Detect default serial port based on operating system."
  (cond
   ((eq system-type 'darwin)
    ;; macOS - look for USB serial devices
    (let ((ports (append (directory-files "/dev" nil "cu\\.usbmodem.*")
                        (directory-files "/dev" nil "cu\\.usbserial.*"))))
      (if ports
          (concat "/dev/" (car ports))
        "/dev/cu.usbmodem")))
   ((eq system-type 'gnu/linux)
    ;; Linux
    (let ((ports (append (directory-files "/dev" nil "ttyUSB.*")
                        (directory-files "/dev" nil "ttyACM.*"))))
      (if ports
          (concat "/dev/" (car ports))
        "/dev/ttyUSB0")))
   (t
    ;; Default fallback
    "/dev/ttyUSB0")))

(defvar org-babel-default-header-args:uart
  `((:ienc . "raw")
    (:oenc . "raw")
    (:port . ,(ob-uart-detect-default-port))
    (:speed . 115200)
    (:bytesize . 8)
    (:parity . "none")
    (:stopbits . 1)
    (:flowcontrol . "none")
    (:timeout . 2)
    (:lineend . "\n"))
  "Default arguments for evaluating a UART block.")

(defun ob-uart--ensure-number (val default)
  "Ensure VAL is a number. If string, convert. If nil or invalid, use DEFAULT."
  (cond
   ((numberp val) val)
   ((stringp val) (string-to-number val))
   (t default)))

(defun ob-uart--ensure-symbol (val valid-symbols)
  "Ensure VAL is a symbol from VALID-SYMBOLS or nil.
VAL can be a string or symbol. Returns symbol or nil."
  (let ((sym (cond
              ((symbolp val) val)
              ((stringp val) (intern (downcase val)))
              (t nil))))
    (if (or (null sym)
            (memq sym valid-symbols)
            (member (symbol-name sym) '("none" "nil")))
        (if (member (symbol-name sym) '("none" "nil"))
            nil
          sym)
      nil)))

;;;###autoload
(defun org-babel-execute:uart (body params)
  "Execute a block of UART code with org-babel.
This function is called by `org-babel-execute-src-block'
Argument BODY content to send.
Argument PARAMS UART communication parameters."
  (message "executing UART source code block")
  (let* ((ienc (let ((i (or (cdr (assoc :ienc params)) "raw")))
                 (if (symbolp i) (symbol-name i) i)))
         (oenc (let ((o (or (cdr (assoc :oenc params)) "raw")))
                 (if (symbolp o) (symbol-name o) o)))
         (port (let ((p (or (cdr (assoc :port params)) (ob-uart-detect-default-port))))
                 (if (symbolp p) (symbol-name p) p)))
         (speed (ob-uart--ensure-number (cdr (assoc :speed params)) 115200))
         (bytesize (ob-uart--ensure-number (cdr (assoc :bytesize params)) 8))
         (parity (ob-uart--ensure-symbol (cdr (assoc :parity params)) 
                                          '(odd even)))
         (stopbits (ob-uart--ensure-number (cdr (assoc :stopbits params)) 1))
         (flowcontrol (ob-uart--ensure-symbol (cdr (assoc :flowcontrol params))
                                               '(hw sw hardware software)))
         (timeout (ob-uart--ensure-number (cdr (assoc :timeout params)) 2))
         (lineend (let ((le (or (cdr (assoc :lineend params)) "\n")))
                    (if (symbolp le) (symbol-name le) le)))
         (process-name (format "ob-uart-%s" (replace-regexp-in-string "/" "-" port)))
         (process-buffer (format "*ob-uart-%s*" (replace-regexp-in-string "/" "-" port))))
    
    ;; Clean up any existing process and buffer
    (when (get-process process-name)
      (delete-process process-name))
    (when (get-buffer process-buffer)
      (kill-buffer process-buffer))
    
    ;; Create the serial process
    (condition-case err
        (let ((proc (make-serial-process
                     :name process-name
                     :buffer process-buffer
                     :port port
                     :speed speed
                     :bytesize bytesize
                     :parity parity
                     :stopbits stopbits
                     :flowcontrol flowcontrol
                     :filter 'ob-uart-listen-filter)))
          
          ;; Process input encoding
          (when (string= "hex" ienc)
            (setq body (mapconcat 
                       (lambda (x) 
                         (char-to-string (string-to-number x 16))) 
                       (split-string body) "")))
          
          ;; Send the command
          (process-send-string proc (concat body lineend))
          
          ;; Clear buffer and wait for response
          (with-current-buffer process-buffer
            (erase-buffer))
          
          ;; Use accept-process-output instead of sleep-for
          (let ((start-time (current-time)))
            (while (and (< (float-time (time-since start-time)) timeout)
                       (process-live-p proc))
              (accept-process-output proc 0.1 nil t)))
          
          ;; Get result
          (let ((result ""))
            (with-current-buffer process-buffer
              (setq result (buffer-string)))
            
            ;; Clean up
            (when (process-live-p proc)
              (delete-process proc))
            (when (get-buffer process-buffer)
              (kill-buffer process-buffer))
            
            ;; Process output encoding
            (when (string= "hex" oenc)
              (setq result (mapconcat (lambda (x) (format "%02x" x)) 
                                     (string-to-list result) " ")))
            (when (string= "HEX" oenc)
              (setq result (mapconcat (lambda (x) (format "%02X" x)) 
                                     (string-to-list result) " ")))
            
            ;; Format result
            (with-temp-buffer
              (insert result)
              (when (not (string= "raw" oenc))
                (fill-region (point-min) (point-max)))
              (buffer-string))))
      (error
       (format "Error opening serial port %s: %s" port (error-message-string err))))))

(defun ob-uart-listen-filter (proc string)
  "Filter to process response.
Argument PROC process.
Argument STRING response string."
  (when ob-uart-debug
    (message (format "ob-uart got %d bytes" (length string))))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert string))))

(provide 'ob-uart)
;;; ob-uart.el ends here
