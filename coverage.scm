;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010, 2013 Free Software Foundation, Inc.
;;;               2019       Peter Elliott.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
(define-module (guest coverage)
  #:use-module (system vm coverage)
  #:use-module (srfi srfi-11)
  #:export (coverage-data->lcov))

;; this function is from (system vm coverage),
;; but modified to support filtering by files.

(define* (coverage-data->lcov data port #:key files)
  "Traverse code coverage information DATA, as obtained with
`with-code-coverage', and write coverage information in the LCOV format to PORT.
The report will include all the modules loaded at the time coverage data was
gathered, even if their code was not executed."

  ;; FIXME: Re-enable this code, but using for-each-elf-symbol on each source
  ;; chunk.  Use that to build a map of file -> proc-addr + line + name.  Then
  ;; use something like procedure-execution-count to get the execution count.
  #;
  (define (dump-function proc)
    ;; Dump source location and basic coverage data for PROC.
    (and (or (program? proc))
         (let ((sources (program-sources* data proc)))
           (and (pair? sources)
                (let* ((line (source:line-for-user (car sources)))
                       (name (or (procedure-name proc)
                                 (format #f "anonymous-l~a" line))))
                  (format port "FN:~A,~A~%" line name)
                  (and=> (procedure-execution-count data proc)
                         (lambda (count)
                           (format port "FNDA:~A,~A~%" count name))))))))

  ;; Output per-file coverage data.
  (format port "TN:~%")
  (for-each (lambda (file)
              (let ((path (search-path %load-path file)))
                (if (string? path)
                    (begin
                      (format port "SF:~A~%" path)
                      #;
                      (for-each dump-function procs)
                      (for-each (lambda (line+count)
                                  (let ((line  (car line+count))
                                        (count (cdr line+count)))
                                    (format port "DA:~A,~A~%"
                                            (+ 1 line) count)))
                                (line-execution-counts data file))
                      (let-values (((instr exec)
                                    (instrumented/executed-lines data file)))
                        (format port "LH: ~A~%" exec)
                        (format port "LF: ~A~%" instr))
                      (format port "end_of_record~%"))
                    (begin
                      (format (current-error-port)
                              "skipping unknown source file: ~a~%"
                              file)))))
            (or files
                (instrumented-source-files data))))
