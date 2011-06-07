(defconst test-server-port 20000
  "port of the test server")

(defvar test-server-clients '() 
  "Alist where KEY is a client process and VALUE is the string")

(setq test-server-buffer (get-buffer-create "test-server-output"))

(defun test-server-start nil
  "Starts an emacs test server"
  (interactive)
  (unless (process-status "test-server")
    (setq test-server-proc (make-network-process 
			    :name "test-server" 
			    :buffer "*test-server*" 
			    :family 'ipv4 
			    :service test-server-port 
			    :sentinel 'test-server-sentinel 
			    :filter 'test-server-filter 
			    :server 't) 
	  )))

  
(defun test-server-sentinel (proc msg))

(defun test-server-filter (proc string)
  (with-current-buffer test-server-buffer
    ;(sleep-for 0.0001)
    (goto-char (point-max))
    (insert (format "%d\n" (length string)))
    ))


(test-server-start)
(delete-process test-server-proc)


(setq s "abc")
(aset s 0 ?A)
(store-substring s 0 "BC")