;;; ob-cereal.el --- org-babel support for serial communication

;; Copyright (C) Loren Maxwell Butler
;; Original Author and Idea and 100% credit to: Andreas Müller
;; Revision Author: elemb
;; Keywords: tools, comm, org-mode, serial, literate programming, reproducible development
;; Homepage: https://github.com/elemb/ob-cereal   ;; ← update to your fork URL
;; Version: 0.1.0

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
;; Org-babel support for serial communication with embedded devices.
;; Supports Forth systems like Mecrisp and ZeptoForth.

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
  '((:port . "/dev/ttyUSB0")
    (:speed . 115200)
    (:bytesize . 8)
    (:parity . nil)
    (:stopbits . 1)
    (:flowcontrol . nil)
    (:timeout . 2)
    (:lineend . "\n")
    (:ienc . "raw")
    (:oenc . "raw")
    (:wrap . "example"))
  "Default arguments for evaluating a cereal block.

:wrap defaults to \"example\" so multi-line results render as a
#+begin_example/#+end_example fence rather than Org's default
per-line \": \" fixed-width prefix, which is a pain to copy-paste
out of. Override per-block with :wrap nil if you want the old
behavior for some reason.")

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

(defun ob-cereal--count-occurrences (needle haystack)
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (let ((count 0) (start 0) (re (regexp-quote needle)))
    (while (string-match re haystack start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

;;;###autoload
(defun org-babel-execute:cereal (body params)
  "Execute serial code block with BODY and PARAMS."
  (when ob-cereal-debug
    (message "ob-cereal executing with params: %S" params))

  (let* ((port      (ob-cereal--to-string (or (cdr (assq :port      params)) "/dev/ttyUSB0")))
         (speed     (ob-cereal--to-number (cdr (assq :speed     params)) 115200))
         (bytesize  (ob-cereal--to-number (cdr (assq :bytesize  params)) 8))
         (parity    (ob-cereal--to-symbol (cdr (assq :parity    params)) '(odd even)))
         (stopbits  (ob-cereal--to-number (cdr (assq :stopbits  params)) 1))
         (flowcontrol (ob-cereal--to-symbol (cdr (assq :flowcontrol params)) '(hw sw)))
         (timeout   (ob-cereal--to-number (cdr (assq :timeout   params)) 2))
         (lineend   (ob-cereal--to-string (or (cdr (assq :lineend   params)) "\n")))
         (ienc      (ob-cereal--to-string (or (cdr (assq :ienc      params)) "raw")))
         (oenc      (ob-cereal--to-string (or (cdr (assq :oenc      params)) "raw")))
         (proc-name (format "ob-cereal-%s" (replace-regexp-in-string "[/:]" "-" port)))
         (proc-buf  (format " *ob-cereal-%s*" (replace-regexp-in-string "[/:]" "-" port))))

    (when ob-cereal-debug
      (message "ob-cereal: port=%s speed=%d bytesize=%d parity=%s stopbits=%d flow=%s timeout=%d"
               port speed bytesize parity stopbits flowcontrol timeout))

    ;; Clean up any stale process/buffer
    (when (get-process proc-name)   (delete-process proc-name))
    (when (get-buffer proc-buf)     (kill-buffer proc-buf))

    (let (proc result)
      (condition-case err
          (unwind-protect
              (progn
                (setq proc (make-serial-process
                            :name proc-name
                            :buffer proc-buf
                            :port port
                            :speed speed
                            :bytesize bytesize
                            :parity parity
                            :stopbits stopbits
                            :flowcontrol flowcontrol))

                ;; Encode input if hex mode
                (let ((encoded-body body))
                  (when (string= ienc "hex")
                    (setq encoded-body
                          (mapconcat (lambda (hex) (char-to-string (string-to-number hex 16)))
                                     (split-string body "[ \t\n]+" t) "")))

                  ;; Send
                  (process-send-string proc (concat encoded-body lineend))

                  ;; Expected prompt count: zeptoforth's REPL prints a
                  ;; literal " ok\n" (do-prompt, in outer.s) once per input
                  ;; line it finishes processing -- including blank lines,
                  ;; confirmed on hardware. So the number of terminators we
                  ;; can hope to see back is the number of newlines in what
                  ;; we just sent. This is an upper bound, not a guarantee:
                  ;; a parse error (unknown word) makes the REPL `abort'
                  ;; past do-prompt for that one line, silently eating its
                  ;; terminator -- confirmed on hardware, see the
                  ;; bogus-word-that-does-not-exist test. So this count is
                  ;; used only as a fast-path early exit below; it is never
                  ;; the sole exit condition, and the idle-timeout loop
                  ;; after it is what guarantees we always return even when
                  ;; the count is never reached.
                  (let ((expected-oks
                         (let ((sent (concat encoded-body lineend)) (n 0) (i 0))
                           (while (setq i (string-match "\n" sent i))
                             (setq n (1+ n) i (1+ i)))
                           n)))

                    ;; Wait (non-blocking), idle-reset style: `timeout' is
                    ;; how long the port must go QUIET before we give up,
                    ;; not a fixed wall-clock budget for the whole block.
                    ;; Every time new bytes show up we push the deadline
                    ;; back out, so a device mid-loop that's still
                    ;; producing output doesn't get cut off just because
                    ;; the block has been running a while. A genuinely
                    ;; dead port still closes in `timeout' seconds, same as
                    ;; before, since silence means the buffer size never
                    ;; changes.
                    ;;
                    ;; On top of that: after every poll we also count
                    ;; literal " ok\n" terminators seen so far and break
                    ;; immediately once that count reaches expected-oks --
                    ;; the common, no-error case then returns as soon as
                    ;; the block is actually done instead of waiting out a
                    ;; full idle window afterward. If a parse error ate a
                    ;; terminator, expected-oks is never reached and we
                    ;; simply fall through to the idle-timeout above, which
                    ;; still guarantees termination.
                    ;;
                    ;; Caveat: no absolute ceiling. A device stuck emitting
                    ;; output forever (runaway loop) will hold the block
                    ;; open forever too -- there's no backstop timer here
                    ;; yet. Known tradeoff, not an oversight; add one if it
                    ;; bites.
                    (let ((idle-deadline (+ (float-time) timeout))
                          (last-size (buffer-size (get-buffer proc-buf)))
                          (ok-count 0)
                          (done nil))
                      (while (and (not done)
                                  (process-live-p proc)
                                  (< (float-time) idle-deadline))
                        (accept-process-output proc 0.1 nil t)
                        (let ((cur-size (buffer-size (get-buffer proc-buf))))
                          (when (/= cur-size last-size)
                            (setq last-size cur-size)
                            (setq idle-deadline (+ (float-time) timeout))
                            (setq ok-count
                                  (let ((s (with-current-buffer proc-buf (buffer-string)))
                                        (n 0) (i 0))
                                    (while (setq i (string-match " ok\n" s i))
                                      (setq n (1+ n) i (1+ i)))
                                    n))
                            (when (>= ok-count expected-oks)
                              (setq done t)))))))

                  ;; Capture result
                  (setq result (with-current-buffer proc-buf (buffer-string)))

                  ;; Strip ANSI CSI escape sequences (e.g. color codes from
                  ;; zeptoforth's error console: ESC [ 31 ; 1 m ... ESC [ 0 m).
                  ;; Must run before the generic control-char strip below,
                  ;; which only eats the lone ESC byte and would otherwise
                  ;; leave the printable payload ("[31;1m") behind as junk.
                  (setq result (replace-regexp-in-string "\x1B\\[[0-9;]*[a-zA-Z]" "" result))

                  ;; Strip remaining control chars (keep \n and \t)
                  (setq result (replace-regexp-in-string "[\x00-\x08\x0B-\x1F\x7F]" "" result))

                  ;; Optional hex output decoding
                  (cond
                   ((string= oenc "hex")
                    (setq result (mapconcat (lambda (c) (format "%02x" c)) result " ")))
                   ((string= oenc "HEX")
                    (setq result (mapconcat (lambda (c) (format "%02X" c)) result " "))))))

            ;; Cleanup always runs, success or error, because this is
            ;; the unwind-protect's cleanup form, not the body's tail.
            (when (and proc (process-live-p proc)) (delete-process proc))
            (when (get-buffer proc-buf) (kill-buffer proc-buf)))

        (error (setq result (format "Error in serial communication: %s" (error-message-string err)))))

      result)))

(provide 'ob-cereal)
;;; ob-cereal.el ends here
