;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;                        TypeRex OCaml Studio                            ;
;                                                                        ;
;                           Tiphaine Turpin                              ;
;                                                                        ;
;  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           ;
;  All rights reserved.  This file is distributed under the terms of     ;
;  the GNU Public License version 3.0.                                   ;
;                                                                        ;
;  TypeRex is distributed in the hope that it will be useful,            ;
;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;  GNU General Public License for more details.                          ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs lisp implementation of our RPC protocol.

;; Usage:

;; - Connect to the listening server using by evaluating
;;     (start-connection port-number)
;;   or setup a server with
;;     (start-rpc-server)
;;   The protocol is the one implemented by class
;;     Server.tagged_connection

;; - Then use
;;     (ocp-rpc-string-command command)
;;   which behaves as method send_tagged_command, to execute requests.

;; - Callbacks are processed by calling the function ocp-rpc-process-callback
;;   on the buffer which contains the contents. Contents should be read
;;   from the point. The ocp-rpc-process-callback function is responsible for
;;   error encoding.

;; - Accepting requests is not implemented, since we don't use it in
;;   TypeRex because Emacs initiates the requests.

(defconst ocp-rpc-end-of-message "END_OF_MESSAGE"
  "mark for the client to detect the end of the answer")

(defconst ocp-rpc-request-start "REQUEST_START"
  "mark for the client to detect the beginning of a request")

(defvar ocp-rpc-connection-buffer nil
  "all input received from ocp-wizard goes here")

(defvar ocp-rpc-current-callback-start nil
  "start of a callback, if any")

(defvar ocp-rpc-current-answer-start nil
  "start of an answer, if any")

(defvar ocp-rpc-next-message-start 1
  "start of an answer, if any")

(defvar ocp-rpc-last-answer nil
  "region containing the last answer")

(defvar ocp-rpc-connection nil
  "connection process, (used, e.g. for sending data)")

(defvar ocp-rpc-connection-server nil
  "server process")

(defun ocp-rpc-connection-filter (proc string)
;;  (message "inserting %d chars" (length string))
  (with-current-buffer ocp-rpc-connection-buffer
;; always go to the end BEFORE (in case the user moves the pointer)
    (goto-char (point-max))
    (insert string)
;; try to determine the nature of the next message
    (if (not (eq ocp-rpc-next-message-start nil))
        (progn
;;        (message "message starts at %s" ocp-rpc-next-message-start)
          (goto-char ocp-rpc-next-message-start)
          (if (< (line-number-at-pos ocp-rpc-next-message-start)
                 (line-number-at-pos (point-max)))
              (progn
                (end-of-line)
                (let ((next-line (buffer-substring ocp-rpc-next-message-start (point))))
;;                (message "next line complete")
                  (if (string= next-line ocp-rpc-request-start)
                      (setq ocp-rpc-current-callback-start (+ (point) 1))
                    (setq ocp-rpc-current-answer-start ocp-rpc-next-message-start))
                  (setq ocp-rpc-next-message-start nil))))
          (goto-char (point-max)))))
;; if we are receiving a callback
  (if (not (eq ocp-rpc-current-callback-start nil))
      (let ((complete
             (with-current-buffer ocp-rpc-connection-buffer
               (forward-line -1)
               (let ((bol (point)))
                 (end-of-line)
                 (let ((last-line (buffer-substring bol (point))))
                   (if (string= last-line ocp-rpc-end-of-message)
                       (progn
                         (goto-char ocp-rpc-current-callback-start)
                         (setq ocp-rpc-current-callback-start nil)
                         (setq ocp-rpc-next-message-start (point-max))
;;                    (message "received message %s" (buffer-substring (point) (point-max)))
                         t)
                     nil))))))
        (if complete
            (let ((result (ocp-rpc-process-callback ocp-rpc-connection-buffer)))
              (process-send-string ocp-rpc-connection
                                   (concat result "\n" ocp-rpc-end-of-message "\n"))
              )))
;; if we are receiving an answer
    (if (not (eq ocp-rpc-current-answer-start nil))
        (with-current-buffer ocp-rpc-connection-buffer
;;            (message "reading answer from %d" ocp-rpc-current-answer-start)
          (forward-line -1)
          (let ((bol (point)))
            (end-of-line)
            (let ((last-line (buffer-substring bol (point))))
              (if (string= last-line ocp-rpc-end-of-message)
                  (progn
;;                      (message "answer ends at %d" (- bol 1))
                    (setq ocp-rpc-last-answer `(,ocp-rpc-current-answer-start ,(- bol 1)))
                    (setq ocp-rpc-current-answer-start nil)
                    (setq ocp-rpc-next-message-start (point-max))))))))))

(defun ocp-rpc-get-answer ()
  "get an expected answer"
  (let ((quit-inhibited inhibit-quit))
    (setq inhibit-quit t)
    (while (and (eq ocp-rpc-last-answer nil) (eq (process-status ocp-rpc-connection) 'open))
      (accept-process-output ocp-rpc-connection 1 0))
    (unless (eq (process-status ocp-rpc-connection) 'open)
      (signal 'error '("Error: connection closed")))
    (let ((answer ocp-rpc-last-answer))
      (setq ocp-rpc-last-answer nil)
;;    (message "last answer = [%d, %d[" (car answer) (cadr answer))
      (setq inhibit-quit quit-inhibited)
      answer))
  )

(defun ocp-rpc-delete-until (p)
  (let ((shift (- p (point-min))))
    (with-current-buffer ocp-rpc-connection-buffer
      (delete-region (point-min) p))
    (unless (eq ocp-rpc-next-message-start nil)
      (setq ocp-rpc-next-message-start (- ocp-rpc-next-message-start shift)))
    (unless (eq ocp-rpc-current-answer-start nil)
      (setq ocp-rpc-current-answer-start (- ocp-rpc-current-answer-start shift)))
    (unless (eq ocp-rpc-current-callback-start nil)
      (setq ocp-rpc-current-callback-start (- ocp-rpc-current-callback-start shift)))))

(defvar ocp-pending-commands 0
  "number of commands that are currently being sent")

(defun ocp-rpc-string-command (c)
  (setq ocp-pending-commands (+ ocp-pending-commands 1))
;;  (message "sending command %s" c)
  (process-send-string ocp-rpc-connection
                       (concat ocp-rpc-request-start "\n" c "\n" ocp-rpc-end-of-message "\n"))
;;  (message "command sent:   %s" c)
  (let*
      ((answer-region (ocp-rpc-get-answer))
       (answer
        (with-current-buffer ocp-rpc-connection-buffer
          (buffer-substring (car answer-region) (cadr answer-region)))))
;;    (message "received answer %s" answer)
    (ocp-rpc-delete-until (cadr answer-region))
  (setq ocp-pending-commands (- ocp-pending-commands 1))
    answer))

(defun ocp-rpc-start-connection (port)
  "start connection by listening on specified port"
  (setq max-lisp-eval-depth 10000)
  (setq ocp-rpc-connection-buffer (get-buffer-create-clear " *output-from-ocp-wizard*"))
  (setq ocp-rpc-next-message-start 1)
  (setq ocp-rpc-current-answer-start nil)
  (setq ocp-rpc-current-callback-start nil)
  (setq ocp-rpc-last-answer nil)
  (setq ocp-rpc-connection
        (open-network-stream "ocp-wizard-client" nil 'local port))
  (set-process-filter ocp-rpc-connection 'ocp-rpc-connection-filter)
  (set-process-query-on-exit-flag ocp-rpc-connection nil)
  )

(defun ocp-rpc-connection-sentinel (process event)
  (if (string= (substring event 0 4) "open")
      (progn
;;        (message "connected")
        (setq ocp-rpc-connection process)
        (set-process-query-on-exit-flag ocp-rpc-connection nil))
    ))

(defun ocp-rpc-start-server ()
  "start a server connection and return the listening port"
  (setq max-lisp-eval-depth 10000)
  (setq ocp-rpc-connection-buffer
        (get-buffer-create-clear " *output-from-ocp-wizard*"))
  (setq ocp-rpc-next-message-start 1)
  (setq ocp-rpc-current-answer-start nil)
  (setq ocp-rpc-current-callback-start nil)
  (setq ocp-rpc-last-answer nil)
  (setq ocp-rpc-connection nil)
  (setq ocp-rpc-connection-server
        (make-network-process
         :name "ocp-connection"
         :server 0 :host "127.0.0.1" :family 'ipv4 :service t :reuseaddr))
  (set-process-sentinel ocp-rpc-connection-server 'ocp-rpc-connection-sentinel)
  (set-process-filter ocp-rpc-connection-server 'ocp-rpc-connection-filter)
  (set-process-query-on-exit-flag ocp-rpc-connection-server nil)
;;  (set-process-coding-system ocp-rpc-connection-server 'emacs-mule 'emacs-mule)
  (set-process-coding-system ocp-rpc-connection-server 'emacs-internal 'emacs-internal)
;; From the emacs-devel list:
;; The byte sequence of a buffer after decoded is
;; always in emacs-mule (in emacs-unicode-2 branch, it's
;; utf-8).  So, changing buffer-file-coding-system or any other
;; coding-system-related variables doesn't affects
;; position-bytes.
;; However, emacs-mule is broken with 3byte utf8 chars, while emacs-internal works
  (process-contact ocp-rpc-connection-server :service)
  )
