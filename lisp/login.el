;; Attempt at providing some login stuff.
;; We'll see if it sticks...


(defvar mds-login-passkey-alist nil
  "Alist with each entry (userid . passkey).
Both userid and passkey are strings.  Passkey
is the sha1 transformation of the actual password.")

(defun mds-login-get-passkey (userid)
  (let ((passkey (assoc userid mds-login-passkey-alist)))
    (if passkey (cdr passkey)
      (error "No passkey for user %s" userid))))
  

(defun mds-login-generate-challenge (userid)
  "Generate a challenge, which consists of a cons-cell,
(random-string . encrypted-random-string).  Each string
is 40 characters long."
  (let ((rand (sha1 (format "%d" (random t)))))
    (cons rand . (sha1 (concat rand (mds-login-get-passkey userid)))))) 

(defun mds-login-verify-response (userid challenge response)
  "Verify the RESPONSE from USERID to a CHALLENGE.  Return non-nil
if the challeng is correct, nil otherwise."
  (let ((passkey (mds-login-get-passkey userid)))
    (string= response (sha1 (concat (mds-login-get-passkey userid) (car rand))))))

(md5sum "hello")