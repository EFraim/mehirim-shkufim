;; (defpackage :com.e5fraim6.mehirim-shkufim.puller
;;   (:use
;;    :common-lisp
;;    :json
;;    :drakma
;;    :cxml-stp
;;    :local-time
;;    :trivial-shell
;;    :chtml))
(ql:quickload 'cl-json)
(ql:quickload 'drakma)
(ql:quickload 'cxml-stp)
(ql:quickload 'local-time)
(ql:quickload 'trivial-shell)
(ql:quickload 'closure-html)

(defun most-recent-file (files)
  (cond
    ((null files) nil)
    ((null (rest files)) (cdr (assoc :VALUE (first files))))
    (t (cdr (assoc :VALUE
		   (reduce (lambda (file-x file-y)
			     (if (local-time:timestamp>
				  (local-time:parse-timestring (cdr (assoc :TIME file-x)))
				  (local-time:parse-timestring (cdr (assoc :TIME file-y))))
				 file-x file-y)) files)
		   )))))

(defun starts-with (str prefix)
  (and (stringp str) (stringp prefix) (<= (length prefix) (length str)) (string-equal (subseq str 0 (length prefix)) prefix)))

(defun download-file (url local cookies)
  (ensure-directories-exist local)
  (let* ((download-file-name local))
    (with-open-file (out download-file-name
			 :direction :output
			 :element-type '(unsigned-byte 8)
			 :if-exists :supersede)
      (write-sequence (drakma:http-request url
					   :cookie-jar cookies
					   :force-binary 't)
		      out))))

(defun snapshot-published-prices (localdir user password)
  (let* ((cookie-jar (make-instance 'drakma:cookie-jar))
	 (login-page (drakma:http-request "https://url.publishedprices.co.il/login"
					  :cookie-jar cookie-jar))
	 (login-doc (chtml:parse login-page (cxml-stp:make-builder)))
	 (csrftoken
	   (stp:attribute-value
	    (stp:find-recursively "csrftoken" login-doc
				  :key (lambda (node)
					 (if (and (typep node 'stp:element)
						  (equal (stp:local-name node) "input"))
					     (stp:attribute-value node "name")
					     ""))
				  :test 'string-equal) "value"))
	 (login-resp (drakma:http-request "https://url.publishedprices.co.il/login/user"
					  :method :post
					  :parameters `(("username" . ,user)
							("password" . ,password)
							("csrftoken" . ,csrftoken))
					  :cookie-jar cookie-jar))
	 (file-list (drakma:http-request "https://url.publishedprices.co.il/file/ajax_dir?cd=/&iDisplayStart=0&iDisplayLength=9999"
					 :cookie-jar cookie-jar))
	 (files (assoc :AA-DATA (with-input-from-string (s file-list) (json:decode-json s))))
	 (prices-full (remove-if-not (lambda (x) (and (listp x) (starts-with (cdr (assoc :NAME x)) "PriceFull"))) files))
	 (promos-full (remove-if-not (lambda (x) (and (listp x) (starts-with (cdr (assoc :NAME x)) "PromoFull"))) files))
	 (prices (remove-if-not (lambda (x) (and (listp x) (starts-with (cdr (assoc :NAME x)) "Price"))) files))
	 (promos (remove-if-not (lambda (x) (and (listp x) (starts-with (cdr (assoc :NAME x)) "Promo"))) files))
	 (stores (remove-if-not (lambda (x) (and (listp x) (starts-with (cdr (assoc :NAME x)) "Stores"))) files)))
    (dolist (file (remove-duplicates (append prices promos stores prices-full promos-full)))
      (download-file (concatenate 'string "https://url.publishedprices.co.il/file/d/" (drakma:url-encode (cdr (assoc :VALUE file)) :latin-1)) (concatenate 'string localdir "/" (cdr (assoc :NAME file))) cookie-jar))
    ))

(defun snapshot-nibit (localdir)
  (trivial-shell:shell-command (format nil "bash /home/evgeny/projects/mehirimShkufim/pullerMatrix.sh ~a" localdir))
)  

(defparameter *snapshot* (format nil "~a" (local-time:today)))

(defvar *sources*
  '(
    ((:name . "TivTaam") (:username . "TivTaam") (:password . "") (:type . published-prices))
    ((:name . "doralon") (:username . "doralon") (:password . "") (:type . published-prices))
    ((:name . "osherad") (:username . "osherad") (:password . "") (:type . published-prices))
    ((:name . "HaziHinam") (:username . "HaziHinam") (:password . "") (:type . published-prices))
    ((:name . "Keshet") (:username . "Keshet") (:password . "") (:type . published-prices))
    ((:name . "RamiLevi") (:username . "RamiLevi") (:password . "") (:type . published-prices))
    ((:name . "SuperDosh") (:username . "SuperDosh") (:password . "") (:type . published-prices))
    ((:name . "yohananof") (:username . "yohananof") (:password . "") (:type . published-prices))
	;;((:name . "freshmarket") (:username . "freshmarket_sn") (:password . "f_efrd") (:type . published-prices))
    ((:name . "Nibit") (:type . nibit))
    ))

(dolist (src *sources*)
  (cond
    ((eq (cdr (assoc :type src)) 'published-prices)
     (snapshot-published-prices (concatenate 'string *snapshot* "/" (cdr (assoc :name src)))
				(cdr (assoc :username src)) (cdr (assoc :password src))))
    ((eq (cdr (assoc :type src)) 'nibit)
     (snapshot-nibit (concatenate 'string *snapshot* "/Nibit")))
    (t (error "Unknown source type"))))
