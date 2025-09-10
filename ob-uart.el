;;; ob-uart.el --- org-babel support for UART communication
;; Copyright (C) Andreas Müller
;; Author: Andreas Müller
;; Keywords: tools, comm, org-mode, UART, literate programming, reproducible development
;; Homepage: https://www.0x7.ch
;; Version: 0.0.2

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;; This package provides org-babel support for UART communication.
;; It allows executing UART commands directly in org-mode code blocks.

;;; Code:
(require 'ob)

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
    (:speed . 9600)
    (:bytesize . 8)
    (:parity . nil)
    (:stopbits . 1)
    (:flowcontrol . nil)
    (:timeout . 1)
    (:lineend . "\n"))
  "Default arguments for evaluating a UART block.")

(defun ob-uart-safe-number (value default)
  "Safely convert VALUE to number, using DEFAULT if conversion fails."
  (cond
   ((numberp value) value)
   ((stringp value) (string-to-number value))
   (t default)))

(defun ob-uart-safe-string (value default)
  "Safely convert VALUE to string, using DEFAULT if nil."
  (cond
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t default)))

;;;###autoload
(defun org-babel-execute:uart (body params)
  "Execute a block of UART code with org-babel.
This function is called by `org-babel-execute-src-block'
Argument BODY content to send.
Argument PARAMS UART communication parameters."
  (message "executing UART source code block")
  (let* ((ienc (ob-uart-safe-string (cdr (assoc :ienc params)) "raw"))
         (oenc (ob-uart-safe-string (cdr (assoc :oenc params)) "raw"))
         (port (ob-uart-safe-string (cdr (assoc :port params)) (ob-uart-detect-default-port)))
         (speed (ob-uart-safe-number (cdr (assoc :speed params)) 9600))
         (bytesize (ob-uart-safe-number (cdr (assoc :bytesize params)) 8))
         (parity-val (cdr (assoc :parity params)))
         (parity (cond ((equal parity-val "odd") 'odd)
                       ((equal parity-val "even") 'even)
                       ((eq parity-val 'odd) 'odd)
                       ((eq parity-val 'even) 'even)
                       (t nil)))
         (stopbits (ob-uart-safe-number (cdr (assoc :stopbits params)) 1))
         (flowcontrol-val (cdr (assoc :flowcontrol params)))
         (flowcontrol (cond ((equal flowcontrol-val "hw") 'hw)
                           ((equal flowcontrol-val "sw") 'sw)
                           ((eq flowcontrol-val 'hw) 'hw)
                           ((eq flowcontrol-val 'sw) 'sw)
                           (t nil)))
         (timeout (ob-uart-safe-number (cdr (assoc :timeout params)) 1))
         (lineend (ob-uart-safe-string (cdr (assoc :lineend params)) "\n"))
         (process-name (format "ob-uart-%s" (replace-regexp-in-string "/" "-" port)))
         (buffer-name (format "*ob-uart-%s*" (replace-regexp-in-string "/" "-" port))))
    
    ;; Debug output
    (when ob-uart-debug
      (message "UART params: port=%s speed=%s timeout=%s" port speed timeout))
    
    ;; Clean up any existing process and buffer
    (when (get-process process-name)
      (delete-process process-name)
      (sit-for 0.1))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    ;; Create the serial process
    (condition-case err
        (let ((proc (make-serial-process
                     :name process-name
                     :buffer buffer-name
                     :port port
                     :speed speed
                     :bytesize bytesize
                     :parity parity
                     :stopbits stopbits
                     :flowcontrol flowcontrol
                     :noquery t
                     :filter (lambda (proc string)
                               (when ob-uart-debug
                                 (message "ob-uart got %d bytes" (length string)))
                               (with-current-buffer (process-buffer proc)
                                 (insert string))))))
          
          ;; Process input encoding
          (when (string= "hex" ienc)
            (setq body (mapconcat 
                       (lambda (x) 
                         (char-to-string (string-to-number x 16))) 
                       (split-string body) "")))
          
          ;; Send the command
          (process-send-string proc (concat body lineend))
          
          ;; Clear buffer and wait for response
          (with-current-buffer buffer-name
            (erase-buffer))
          (sit-for timeout)
          
          ;; Get result
          (let ((result ""))
            (when (get-buffer buffer-name)
              (with-current-buffer buffer-name
                (setq result (buffer-string))))
            
            ;; Clean up
            (when (process-live-p proc)
              (delete-process proc))
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name))
            
            ;; Process output encoding
            (when (string= "hex" oenc)
              (setq result (mapconcat (lambda (x) (format "%02x" x)) 
                                     (string-to-list result) " ")))
            (when (string= "HEX" oenc)
              (setq result (mapconcat (lambda (x) (format "%02X" x)) 
                                     (string-to-list result) " ")))
            
            ;; Return result
            (if (string-empty-p result)
                "No response"
              result)))
      (error
       (format "UART Error: %s" (error-message-string err))))))

(provide 'ob-uart)
;;; ob-uart.el ends here
