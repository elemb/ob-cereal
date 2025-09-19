;;; ob-uart.el --- org-babel support for UART communication

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:uart
  '((:port . "/dev/cu.usbmodem01")
    (:speed . "115200")
    (:timeout . "1")
    (:lineend . "\n"))
  "Default arguments for UART blocks.")

(defun org-babel-execute:uart (body params)
  "Execute UART command BODY with PARAMS."
  (let* ((port (or (cdr (assoc :port params)) "/dev/cu.usbmodem01"))
         (speed (string-to-number (or (cdr (assoc :speed params)) "115200")))
         (timeout (string-to-number (or (cdr (assoc :timeout params)) "1")))
         (lineend (or (cdr (assoc :lineend params)) "\n"))
         (proc-name (format "uart-%d" (random 10000)))
         (buffer-name (format "*%s*" proc-name)))
    
    ;; Clean up existing process/buffer
    (when (get-process proc-name) (delete-process proc-name))
    (when (get-buffer buffer-name) (kill-buffer buffer-name))
    
    ;; Create serial process
    (let ((proc (make-serial-process
                 :name proc-name
                 :buffer buffer-name
                 :port port
                 :speed speed)))
      
      ;; Send command
      (process-send-string proc (concat body lineend))
      
      ;; Wait for response
      (sleep-for timeout)
      
      ;; Get result
      (let ((result ""))
        (when (buffer-live-p (get-buffer buffer-name))
          (with-current-buffer buffer-name
            (setq result (buffer-string))))
        
        ;; Clean up
        (when (process-live-p proc) (delete-process proc))
        (when (get-buffer buffer-name) (kill-buffer buffer-name))
        
        result))))

(provide 'ob-uart)
;;; ob-uart.el ends here
