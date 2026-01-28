;;; ob-uart.el --- org-babel support for UART communication

;; Copyright (C) Andreas Müller

;; Author: Andreas Müller
;; Revision Author: elemb
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
;;

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(defgroup ob-uart nil
  "UART support for org babel."
  :group 'org-babel)

(defcustom ob-uart-debug t
  "Whether to notify in case of new commits."
  :package-version '(ob-uart . "0.0.1")
  :group 'ob-uart
  :type 'boolean)

(require 'cl-lib)
(require 'ob)

(defvar org-babel-default-header-args:uart
  '((:port . "/dev/ttyUSB0")
    (:speed . 115200)
    (:bytesize . 8)
    (:parity . "none")
    (:stopbits . 1)
    (:flowcontrol . "none")
    (:timeout . 2)
    (:lineend . "\r")
    (:wait-for . "ok")
    (:ienc . "raw")
    (:oenc . "raw")))

(defun ob-uart--normalize-params (params)
  (cl-labels
      ((sym (v)
            (pcase (and v (downcase v))
              ((or "none" "nil") nil)
              ("even" 'even)
              ("odd"  'odd)
              ("hardware" 'hardware)
              ("software" 'software)
              (_ nil))))
    (list
     :port (cdr (assoc :port params))
     :speed (string-to-number (cdr (assoc :speed params)))
     :bytesize (string-to-number (cdr (assoc :bytesize params)))
     :parity (sym (cdr (assoc :parity params)))
     :stopbits (string-to-number (cdr (assoc :stopbits params)))
     :flowcontrol (sym (cdr (assoc :flowcontrol params)))
     :timeout (string-to-number (cdr (assoc :timeout params)))
     :lineend (cdr (assoc :lineend params))
     :wait-for (cdr (assoc :wait-for params))
     :ienc (cdr (assoc :ienc params))
     :oenc (cdr (assoc :oenc params)))))

(defun ob-uart--open (p)
  (make-serial-process
   :name "ob-uart"
   :buffer (generate-new-buffer " *ob-uart*")
   :port (plist-get p :port)
   :speed (plist-get p :speed)
   :bytesize (plist-get p :bytesize)
   :parity (plist-get p :parity)
   :stopbits (plist-get p :stopbits)
   :flowcontrol (plist-get p :flowcontrol)))

(defun ob-uart--encode (s enc)
  (pcase enc
    ("raw" s)
    ("hex"
     (apply #'unibyte-string
            (mapcar (lambda (x) (string-to-number x 16))
                    (split-string s "[ \t\n]+" t))))
    (_ s)))

(defun ob-uart--read (proc timeout wait-for)
  (let ((buf (process-buffer proc))
        (start (float-time))
        done)
    (with-current-buffer buf (erase-buffer))
    (while (and (process-live-p proc)
                (< (- (float-time) start) timeout)
                (not done))
      (accept-process-output proc 0.1)
      (when wait-for
        (with-current-buffer buf
          (when (string-match-p wait-for (buffer-string))
            (setq done t)))))
    (with-current-buffer buf
      (buffer-string))))

(defun ob-uart--decode (s enc)
  (pcase enc
    ("raw" s)
    ("hex"
     (mapconcat (lambda (b) (format "%02x" b))
                (string-to-list s) " "))
    (_ s)))

;;;###autoload
(defun org-babel-execute:uart (body params)
  (let* ((p (ob-uart--normalize-params params))
         (proc (ob-uart--open p)))
    (unwind-protect
        (progn
          (process-send-string
           proc
           (concat (ob-uart--encode body (plist-get p :ienc))
                   (plist-get p :lineend)))
          (ob-uart--decode
           (ob-uart--read proc
                          (plist-get p :timeout)
                          (plist-get p :wait-for))
           (plist-get p :oenc)))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer (process-buffer proc)))))
