;; -*- coding: utf-8; no-byte-compile: t; lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 1000 1000 8)
		  gc-cons-percentage 0.1)))
