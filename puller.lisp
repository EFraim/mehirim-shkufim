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

(defun download-file (url local cookies)
  (ensure-directories-exist local)
  (let* ((gzipped (string-equal ".gz"
				(subseq url (- (length url) 3))))
	 (download-file-name (if gzipped
				 (concatenate 'string local ".gz")
				 local)))
    (with-open-file (out download-file-name
			 :direction :output
			 :element-type '(unsigned-byte 8)
			 :if-exists :supersede)
      (write-sequence (drakma:http-request url
					   :cookie-jar cookies
					   :force-binary 't)
		      out))
    (when gzipped (trivial-shell:shell-command (concatenate 'string "gunzip " download-file-name)))))

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
	 (prices (remove-if-not (lambda (x) (and (listp x) (string-equal (subseq (cdr (assoc :NAME x)) 0 9) "PriceFull"))) files))
	 (promos (remove-if-not (lambda (x) (and (listp x) (string-equal (subseq (cdr (assoc :NAME x)) 0 9) "PromoFull"))) files))
	 (stores (remove-if-not (lambda (x) (and (listp x) (string-equal (subseq (cdr (assoc :NAME x)) 0 6) "Stores"))) files))
	 (last-price-file (most-recent-file prices))
	 (last-promo-file (most-recent-file promos))
	 (last-store-file (most-recent-file stores)))
    (unless (null last-price-file)
      (download-file (concatenate 'string "https://url.publishedprices.co.il/file/d/" (drakma:url-encode last-price-file :latin-1)) (concatenate 'string localdir "/last-price-file.xml") cookie-jar))
    (unless (null last-promo-file)
      (download-file (concatenate 'string "https://url.publishedprices.co.il/file/d/" (drakma:url-encode last-promo-file :latin-1)) (concatenate 'string localdir "/last-promo-file.xml") cookie-jar))
    (unless (null last-store-file)
      (download-file (concatenate 'string "https://url.publishedprices.co.il/file/d/" (drakma:url-encode last-store-file :latin-1)) (concatenate 'string localdir "/last-store-file.xml") cookie-jar))
    ))

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
					;((:name . "freshmarket") (:username . "freshmarket_sn") (:password . "f_efrd") (:type . published-prices)))
    ))

(dolist (src *sources*)
  (cond
    ((eq (cdr (assoc :type src)) 'published-prices)
     (snapshot-published-prices (concatenate 'string *snapshot* "/" (cdr (assoc :name src)))
				(cdr (assoc :username src)) (cdr (assoc :password src))))
    (t (error "Unknown source type"))))
