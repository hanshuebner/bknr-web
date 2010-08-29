(in-package :cl-user)

(defpackage :leech
  (:use :common-lisp)
  (:export #:job-queue
	   #:job-queue-add-job
	   #:job-queue-print
	   #:job-queue-idle-p
	   #:job-queue-destroy
	   #:job-queue-start

	   #:schedule-job
	   #:execute-job
	   #:execute-leech-job
	   #:job-queue-add-job
	   #:job-error-handler
	   #:job-queue-print
	   #:job-queue-job-count
	   #:job-queue-idle-p
	   #:job-queue-destroy
	   #:job-queue-start

	   #:queue-cookie-jar
	   #:queue-proxy
	   #:queue-errors
	   #:queue-jobs

	   #:pleech-queue
	   #:pleech-queues
	   #:pleech-proxies
	   #:leech-error
	   #:load-proxies

	   #:execute-job
	   #:execute-leech-job

	   #:leech-url
	   #:leech-job
	   #:leech-file-job))

	   