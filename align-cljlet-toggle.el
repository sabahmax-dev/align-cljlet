;;; align-cljlet-toggle.el --- Toggle align/collapse for align-cljlet -*- lexical-binding: t; -*-

(require 'align-cljlet)
(require 'advice)

(defun align-cljlet--find-form-start (pt)
  "Return the start position of the nearest enclosing supported form."
  (save-excursion
    (goto-char pt)
    (catch 'found
      (let ((limit (point-min)))
        (while (> (point) limit)
          (backward-sexp)
          (when (looking-at-p
                 "(\\(?:when-let\\|if-let\\|let\\|condp\\|cond\\|binding\\|loop\\|with-open\\|defroute\\)\\b[ \t]*")
            (throw 'found (point))))
        nil))))

(defun align-cljlet--form-end (start)
  "Return the end position of the Lisp form starting at START."
  (save-excursion
    (goto-char start)
    (forward-sexp)
    (point)))

(defun align-cljlet--collapse-whitespace (start end)
  "Collapse runs of internal horizontal whitespace in region START-END."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (search-forward-regexp "[ \t]\\{2,\\}" end t))
      (let ((b (match-beginning 0))
            (e (match-end 0)))
        (save-excursion
          (goto-char b)
          (let ((ppss (syntax-ppss))
                (cb (char-before b))
                (ca (char-after e)))
            (when (and cb ca
                       (not (memq cb '(?\n ?\r)))
                       (not (memq ca '(?\n ?\r)))
                       (not (nth 1 ppss))
                       (not (nth 3 ppss)))
              (delete-region b (1- e)))))
        (goto-char e)))))

(defun align-cljlet--toggle-advice (original-fn)
  "Align with ORIGINAL-FN, or collapse if it does not change the buffer."
  (let ((before (buffer-string)))
    (funcall original-fn)
    (when (string= before (buffer-string))
      (let ((start (align-cljlet--find-form-start (point)))
            (end nil))
        (when start
          (setq end (align-cljlet--form-end start))
          (align-cljlet--collapse-whitespace start end))))))

(advice-add 'align-cljlet :around #'align-cljlet--toggle-advice)
