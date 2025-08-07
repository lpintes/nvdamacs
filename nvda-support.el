(require 'bindat)
(defvar eval-server--process nil)
(defvar eval-server--buffer "*eval-server*")
(defvar eval-server--port-file (expand-file-name ".eval-server-port" user-emacs-directory))

(defun eval-server--log (msg &rest args)
  (with-current-buffer (get-buffer-create eval-server--buffer)
    (goto-char (point-max))
    (insert (apply #'format (concat msg "\n") args))))

(defun eval-server--write-port (port)
  (with-temp-file eval-server--port-file
    (insert (number-to-string port))))

(defun eval-server--read-expr (proc)
  "Read a message from PROC, evaluate it, and send back the result."
  (let ((len-bytes (process-get proc :pending-bytes)))
    (if (not len-bytes)
        (progn
          ;; Read length first (4 bytes)
          (when (>= (length (process-get proc :buffer)) 4)
            (let* ((buf (process-get proc :buffer))
                   (len (bindat-get-field (bindat-unpack '((:len u32)) buf) :len)))
              (process-put proc :pending-bytes len)
              (process-put proc :buffer (substring buf 4))
              (eval-server--read-expr proc))))
      ;; Now read the expression string
      (let* ((buf (process-get proc :buffer)))
        (when (>= (length buf) len-bytes)
          (let* ((expr-str (decode-coding-string (substring buf 0 len-bytes) 'utf-8))
                 (rest (substring buf len-bytes))
                 (result (condition-case err
                              (let ((val (eval (read expr-str))))
                                (format "%s" val))
                            (error (format "Error: %s" err))))
                 (response (encode-coding-string result 'utf-8))
                 (resp-len (bindat-pack '((:len u32)) `((:len . ,(length response)))))
                 (final-msg (concat resp-len response)))
            (process-send-string proc final-msg)
            (process-put proc :pending-bytes nil)
            (process-put proc :buffer rest)
            (eval-server--read-expr proc)))))))

(defun eval-server--process-filter (proc string)
  (let ((buf (or (process-get proc :buffer) "")))
    (process-put proc :buffer (concat buf string))
    (eval-server--read-expr proc)))

(defun start-eval-server ()
  "Start the TCP eval server."
  (interactive)
  (when (process-live-p eval-server--process)
    (user-error "Server is already running"))

  (let* ((server (make-network-process
                  :name "eval-server"
                  :buffer eval-server--buffer
                  :family 'ipv4
                  :service 0 ;; use random port
                  :server t
                  :filter 'eval-server--process-filter
                  :sentinel (lambda (proc event)
                              (eval-server--log "Connection event: %s %s" proc event))
                  :plist '(:buffer ""))))
    (setq eval-server--process server)
    (let ((port (process-contact server :service)))
      (eval-server--log "Server started on port %d" port)
      (eval-server--write-port port)
      (message "Eval server started on port %d" port))))

(defun stop-eval-server ()
  "Stop the TCP eval server."
  (interactive)
  (when (process-live-p eval-server--process)
    (delete-process eval-server--process)
    (setq eval-server--process nil)
    (delete-file eval-server--port-file)
    (message "Eval server stopped")))
