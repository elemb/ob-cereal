;;; ob-uart.el --- org-babel support for UART communication
;; Copyright (C) Andreas Müller
;; Author: Andreas Müller
;; Keywords: tools, comm, org-mode, UART, literate programming, reproducible development
;; Homepage: https://www.0x7.ch
;; Version: 0.0.2
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
  :package-version '(ob-uart . "0.0.2")
  :group 'ob-uart
  :type 'boolean)

(defun ob-uart-detect-default-port ()
  "Detect default serial port based on operating system."
  (condition-case nil
      (cond
       ((eq system-type 'darwin)
        (let ((ports (append (file-expand-wildcards "/dev/cu.usbmodem*")
                            (file-expand-wildcards "/dev/cu.usbserial*"))))
          (if ports (car ports) "/dev/cu.usbmodem01")))
       ((eq system-type 'gnu/linux)
        (let ((ports (append (file-expand-wildcards "/dev/ttyUSB*")
                            (file-expand-wildcards "/dev/ttyACM*"))))
          (if ports (car ports) "/dev/ttyUSB0")))
       (t "/dev/ttyUSB0"))
    (error "/dev/ttyUSB0")))

(defvar org-babel-default-header-args:uart
  `((:ienc . "raw")
    (:oenc . "raw")
    (:port . "/dev/cu.usbmodem01")
    (:speed . "9600")
    (:bytesize . "8")
    (:parity . "")
    (:stopbits . "1")
    (:flowcontrol . "")
    (:timeout . "2")
    (:lineend . "\n"))
  "Default arguments for evaluating a UART block.")

(defun ob-uart-safe-string-to-number (val default)
  "Safely convert VAL to number, return DEFAULT if conversion fails."
  (condition-case nil
      (cond
       ((numberp val) val)
       ((and (stringp val) (not (string-empty-p val)))
        (string-to-number val))
       (t default))
    (error default)))

(defun ob-uart-safe-string (val default)
  "Safely convert VAL to string, return DEFAULT if nil or empty."
  (condition-case nil
      (cond
       ((and (stringp val) (not (string-empty-p val))) val)
       ((numberp val) (number-to-string val))
       (t default))
    (error default)))

(defun ob-uart-cleanup-process (process-name process-buffer)
  "Clean up existing UART process and buffer safely."
  (condition-case nil
      (progn
        (when-let ((proc (get-process process-name)))
          (when (process-live-p proc)
            (delete-process proc))
          (sit-for 0.1))
        (when-let ((buf (get-buffer process-buffer)))
          (kill-buffer buf)))
    (error nil)))

;;;###autoload
(defun org-babel-execute:uart (body params)
  "Execute a block of UART code with org-babel.
This function is called by `org-babel-execute-src-block'
Argument BODY content to send.
Argument PARAMS UART communication parameters."
  (message "executing UART source code block")
  (condition-case err
      (let* ((processed-params (org-babel-process-params params))
             (ienc (ob-uart-safe-string (cdr (assoc :ienc processed-params)) "raw"))
             (oenc (ob-uart-safe-string (cdr (assoc :oenc processed-params)) "raw"))
             (port (ob-uart-safe-string (cdr (assoc :port processed-params)) "/dev/cu.usbmodem01"))
             (speed (ob-uart-safe-string-to-number (cdr (assoc :speed processed-params)) 9600))
             (bytesize (ob-uart-safe-string-to-number (cdr (assoc :bytesize processed-params)) 8))
             (parity-str (ob-uart-safe-string (cdr (assoc :parity processed-params)) ""))
             (parity (cond ((string= parity-str "odd") 'odd)
                          ((string= parity-str "even") 'even)
                          (t nil)))
             (stopbits (ob-uart-safe-string-to-number (cdr (assoc :stopbits processed-params)) 1))
             (flowcontrol-str (ob-uart-safe-string (cdr (assoc :flowcontrol processed-params)) ""))
             (flowcontrol (cond ((string= flowcontrol-str "hw") 'hw)
                               ((string= flowcontrol-str "sw") 'sw)
                               (t nil)))
             (timeout (ob-uart-safe-string-to-number (cdr (assoc :timeout processed-params)) 2))
             (lineend (ob-uart-safe-string (cdr (assoc :lineend processed-params)) "\n"))
             (safe-port (replace-regexp-in-string "[^a-zA-Z0-9]" "-" port))
             (process-name (format "ob-uart-%s" safe-port))
             (process-buffer (format "*ob-uart-%s*" safe-port)))
        
        ;; Clean up any existing process and buffer
        (ob-uart-cleanup-process process-name process-buffer)
        
        ;; Validate port exists (on macOS)
        (when (and (eq system-type 'darwin) (not (file-exists-p port)))
          (error "Serial port %s does not exist" port))
        
        ;; Process input encoding
        (when (string= "hex" ienc)
          (setq body (mapconcat 
                     (lambda (x) 
                       (char-to-string (string-to-number x 16))) 
                     (split-string body) "")))
        
        ;; Create the serial process with error handling
        (let ((proc (condition-case proc-err
                        (make-serial-process
                         :name process-name
                         :buffer process-buffer
                         :port port
                         :speed speed
                         :bytesize bytesize
                         :parity parity
                         :stopbits stopbits
                         :flowcontrol flowcontrol
                         :noquery t
                         :filter 'ob-uart-listen-filter)
                      (error
                       (error "Failed to create serial process: %s" (error-message-string proc-err))))))
          
          ;; Give process time to initialize
          (sit-for 0.1)
          
          ;; Clear buffer before sending
          (with-current-buffer process-buffer
            (erase-buffer))
          
          ;; Send the command
          (process-send-string proc (concat body lineend))
          
          ;; Wait for response with proper timeout handling
          (let ((start-time (current-time))
                (result ""))
            (while (and (< (float-time (time-subtract (current-time) start-time)) timeout)
                       (process-live-p proc))
              (sit-for 0.1))
            
            ;; Get result
            (condition-case nil
                (with-current-buffer process-buffer
                  (setq result (buffer-string)))
              (error (setq result "")))
            
            ;; Clean up process and buffer
            (ob-uart-cleanup-process process-name process-buffer)
            
            ;; Process output encoding
            (when (string= "hex" oenc)
              (setq result (mapconcat (lambda (x) (format "%02x" x)) 
                                     (string-to-list result) " ")))
            (when (string= "HEX" oenc)
              (setq result (mapconcat (lambda (x) (format "%02X" x)) 
                                     (string-to-list result) " ")))
            
            ;; Return formatted result
            (if (string-empty-p result)
                "No response received"
              result))))
    (error
     (format "UART execution error: %s" (error-message-string err)))))

(defun ob-uart-listen-filter (proc string)
  "Filter to process response.
Argument PROC process.
Argument STRING response string."
  (when ob-uart-debug
    (message "ob-uart received: %d bytes" (length string)))
  (condition-case nil
      (when (process-buffer proc)
        (with-current-buffer (process-buffer proc)
          (goto-char (point-max))
          (insert string)))
    (error nil)))

;; Ensure uart is in the supported languages list
(eval-after-load 'org
  '(add-to-list 'org-babel-load-languages '(uart . t)))

(provide 'ob-uart)
;;; ob-uart.el ends here
