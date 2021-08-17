;;; package --- Summary

;; -*- coding: utf-8; lexical-binding: t -*

;;; Commentary:

;; Emacs configuration of Christian van Onzenoodt.

;;   ___  _ __   ___ _ __ ___   __ _  ___ ___
;;  / _ \| '_ \ / __| '_ ` _ \ / _` |/ __/ __|
;; | (_) | | | | (__| | | | | | (_| | (__\__ \
;;  \___/|_| |_|\___|_| |_| |_|\__,_|\___|___/

;;; Code:


;;; ---------
;;; Constants
;;; ---------

(defconst onc/dotemacs-orgfile (file-truename (expand-file-name "~/.emacs.d/dotemacs.org")))


;;; --------
;;; GC stuff
;;; --------

;; GC tweaking according to
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; this also makes init.el load faster
(setq gc-cons-threshold most-positive-fixnum)

(defun my-minibuffer-setup-hook ()
  "Crank up the Garbage Collection threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Put Garbage Collection back to default value."
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; If this is non-nil, then when both .el and .elc
;; versions of a file exist, and the caller did not explicitly specify
;; which one to load, then the newer file is loaded.
(setq load-prefer-newer t)

;;; ----------------------------
;;; Bootstrap package management
;;; ----------------------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package org
  :ensure t)

(org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
(garbage-collect)

;; Make FlyCheck happy...
;;; init.el ends here
