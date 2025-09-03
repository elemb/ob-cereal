;;; ob-uart.el --- org-babel support for UART communication
;; Copyright (C) Andreas Müller
;; Author: Andreas Müller
;; Keywords: tools, comm, org-mode, UART, literate programming, reproducible development
;; Homepage: https://www.0x7.ch
;; Version: 0.0.1
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
  :package-version '(ob-uart . "0.0.1")
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

;;;###autoload
(defun org-babel-execute:uart (body params)
  "Execute a block of UART code with org-babel.
This function is called by `org-babel-execute-src-block'
Argument BODY content to send.
Argument PARAMS UART communication parameters."
  (message "executing UART source code block")
  (let* ((processed-params (org-babel-process-params params))
         (ienc (cdr (assoc :ienc processed-params)))
         (oenc (cdr (assoc :oenc processed-params)))
         (port (cdr (assoc :port processed-params)))
         (speed (string-to-number (or (cdr (assoc :speed processed-params)) "9600")))
         (bytesize (string-to-number (or (cdr (assoc :bytesize processed-params)) "8")))
         (parity (let ((p (cdr (assoc :parity processed-params))))
                   (cond ((string= p "odd") 'odd)
                         ((string= p "even") 'even)
                         (t nil))))
         (stopbits (string-to-number (or (cdr (assoc :stopbits processed-params)) "1")))
         (flowcontrol (let ((fc (cdr (assoc :flowcontrol processed-params))))
                        (cond ((string= fc "hw") 'hw)
                              ((string= fc "sw") 'sw)
                              (t nil))))
         (timeout (string-to-number (or (cdr (assoc :timeout processed-params)) "1")))
         (lineend (or (cdr (assoc :lineend processed-params)) "\n"))
         (process-name (format "ob-uart-%s" (replace-regexp-in-string "/" "-" port)))
         (process-buffer (format "*ob-uart-%s*" (replace-regexp-in-string "/" "-" port))))
    
    ;; Clean up any existing process and buffer
    (when (get-process process-name)
      (delete-process process-name))
    (when (get-buffer process-buffer)
      (kill-buffer process-buffer))
    
    ;; Create the serial process
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
      (sleep-for timeout)
      
      ;; Get result
      (let ((result ""))
        (with-current-buffer process-buffer
          (setq result (buffer-string)))
        
        ;; Clean up
        (delete-process proc)
        (kill-buffer process-buffer)
        
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
          (buffer-string))))))

(defun ob-uart-listen-filter (proc string)
  "Filter to process response.
Argument PROC process.
Argument STRING response string."
  (when ob-uart-debug
    (message (format "ob-uart got %d bytes" (length string))))
  (with-current-buffer (process-buffer proc)
    (insert string)))

;; Add uart to the list of supported languages
(add-to-list 'org-babel-load-languages '(uart . t))

(provide 'ob-uart)
;;; ob-uart.el ends here
