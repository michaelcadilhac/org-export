;; Convenience functions to export org documents.

(require 'org)
(require 'ox)
(require 'htmlize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Add the IGNORE property tag.
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Add the EXPORT property tag.
(add-to-list 'org-export-options-alist
             '(:export "EXPORT" nil nil t))

(defvar my/org-export-fmt nil)

(defun my/org-export-to-default ()
  (interactive)
  (let ((k (or (and my/org-export-fmt
		    (or (and (functionp (intern my/org-export-fmt))
			     my/org-export-fmt)
			(concat "org-" my/org-export-fmt "-export-to-" my/org-export-fmt)))
	       (plist-get (org-export--get-inbuffer-options) :export))))
    (unless k
      (error "No EXPORT property found."))
    (funcall (intern k))))

(defun my/org-export-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (set-buffer-modified-p nil)
    (setq buffer-file-name (expand-file-name filename))
    (org-mode)
    (my/org-export-to-default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Use correct quotes.
(setq org-export-with-smart-quotes t)

;; Make sure SRC blocks use HTML properties if specified.
(require 'ox-html)



(defun org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
  CONTENTS holds the contents of the item.  INFO is a plist holding
  contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
           (code (org-html-format-code src-block info))
           (extra-class (let ((ec (org-export-read-attribute :attr_html src-block :class)))
                          (if ec (concat " " ec) "")))
           (attributes (org-html--make-attribute-string
                        (plist-put (org-export-read-attribute :attr_html src-block) :class nil)))
           (label (let ((lbl (and (org-element-property :name src-block)
                                  (org-export-get-reference src-block info))))
                    (if lbl (format " id=\"%s\"" lbl) "")))
           (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
                                           "ruby" "scheme" "clojure" "php" "html")))))
      (if (not lang) (format "<pre class=\"example%s\"%s>\n%s</pre>" label code)
        (format "<div class=\"org-src-container%s\"%s>\n%s%s\n</div>"
                extra-class
                (if (equal attributes "") "" (concat " " attributes))
                ;; Build caption.
                (let ((caption (org-export-get-caption src-block)))
                  (if (not caption) ""
                    (let ((listing-number
                           (format
                            "<span class=\"listing-number\">%s </span>"
                            (format
                             (org-html--translate "Listing %d:" info)
                             (org-export-get-ordinal
                              src-block info nil #'org-html--has-caption-p)))))
                      (format "<label class=\"org-src-name\">%s%s</label>"
                              listing-number
                              (org-trim (org-export-data caption info))))))
                ;; Contents.
                (if klipsify
                    (format "<pre><code class=\"src src-%s\"%s>%s</code></pre>"
                            lang
                            label
                            (if (string= lang "html")
                                " data-editor-type=\"html\""
                              "")
                            code)
                  (format "<pre class=\"src src-%s\"%s>%s</pre>"
                          lang label code)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Bolden the $-starting lines in code,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; as they probably are shell prompts.
(defun my/bold-prompt (orig-fun &rest args)
  (let ((code (car args))
        (lang (cadr args)))
    (if (and code (not lang))
        (let (s)
          (setq s (replace-regexp-in-string "\\\\\n" "@@LF@@" (org-html-encode-plain-text code)))
          (setq s (replace-regexp-in-string "^!BL!\\(.*\\)$" "<b>\\1</b>" s))
          (setq s (replace-regexp-in-string "^\\(\\$ \\|\\$$\\|(gdb)\\).*$" "<b>\\&</b>" s))
          (setq s (replace-regexp-in-string "@@LF@@" "\\\\\n" s)))
      (apply orig-fun args))))

(defun my/add-prism-code (orig-fun &rest args)
  (let ((code (car args))
        (lang (cadr args)))
    (if (and code (not lang)
             (string-prefix-p "$ " code))
        (concat "<code class=\"language-shell-session\" style=\"padding-left: 0em\">"
                (org-html-encode-plain-text code)
                "</code>")
      (apply orig-fun args))))


;(advice-add 'org-html-fontify-code :around #'my/bold-prompt)
(advice-add 'org-html-fontify-code :around #'my/add-prism-code)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Convert pipes in inline code blocks.
(defun my/convert-entities (orig-fun &rest args)
  (let* ((code (car args))
         (codetxt (org-element-property :value code))
         (newcode (replace-regexp-in-string "\\\\vert\\b\\({}\\|\\)" "|" codetxt)))
    (apply orig-fun `(,(org-element-put-property code :value newcode) . ,(cdr args)))))


(defun my/add-manpage (orig-fun &rest args)
  (let* ((codetxt (org-element-property :value (car args))))
    (if (file-exists-p (concat "/usr/share/man/man1/"
                               codetxt ".1.gz"))
        (org-html-link
         (with-temp-buffer
           (insert "[[https://man7.org/linux/man-pages/man1/"
                   codetxt ".1.html][=" codetxt "(1)=]]")
           (goto-char 0)
           (org-element-link-parser))
         (concat "<code>" codetxt "(1)</code>")
         (nth 3 args))
      (apply orig-fun args))))

(advice-add 'org-html-code :around #'my/convert-entities)
(advice-add 'org-html-verbatim :around #'my/add-manpage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; No postamble
(setq org-html-postamble nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Use CSS for HTMLIZE, as we're in batch-mode.
(setq org-html-htmlize-output-type 'css)

(provide 'org-export)
