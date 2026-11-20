;;; coverage.el --- Measure test coverage of lyrion -*- lexical-binding: t -*-

;; SPDX-License-Identifier: CC0-1.0

;;; Commentary:

;; Loaded by `make coverage' before the tests. It installs undercover
;; into .deps on first use, then instruments lyrion.el so the test run
;; writes an lcov report to coverage/lcov.info.

;;; Code:

(require 'package)

(setq package-user-dir (expand-file-name ".deps")
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'undercover)
  (package-refresh-contents)
  (package-install 'undercover))

(require 'undercover)

;; Otherwise undercover only measures on a CI server.
(setq undercover-force-coverage t)
(make-directory "coverage" t)
(undercover "lyrion.el"
            (:report-format 'lcov)
            (:report-file "coverage/lcov.info")
            (:send-report nil))

;;; coverage.el ends here
