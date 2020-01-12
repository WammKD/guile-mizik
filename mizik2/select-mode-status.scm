#!/usr/bin/guile
!#
(use-modules (srfi srfi-9))

(define-record-type <select-mode-status>
  (make-select-mode-status isInSelectMode currentSelectionIndex sortedIndex)
  select-mode-status?
  (isInSelectMode        active?      active-set!)
  (currentSelectionIndex index        index-set!)
  (sortedIndex           sorted-index sorted-index-set!))

(define (initialize-select-mode-status)
  (make-select-mode-status #f #f #f))
