;;; org-parallel.el --- Creating parallel text for translate -*- coding: utf-8; lexical-binding: t -*-

;; Author: DKrivets
;; Created: 07 Jan 2019
;; Version: 0.0.1
;; Keywords: parallel, text, languages, translating
;; Homepage: https://github.com/dkrivets/org-parallel
;; Package-Require: ((emacs "24.3")(org)(subr-x)(thingatpt))

;;; Commentary:
;; as
;;; Code:
(require 'subr-x)
(require 'thingatpt)

(defgroup org-parallel nil
  "Simple way to create parallel texts."
  :group 'applications)


(defcustom org-parallel-last-finish-language nil
  "Native language.  Last finish language."
  :type 'string
  :group 'org-parallel)


;; Markup for table
(defvar org-parallel-line-splitter "\n"   "Get lines from file by splitter.")
(defvar org-parallel-file-ext      ".org" "File extension.")
(defvar org-parallel-col-splitter  "@@@"  "Column splitter.")
(defvar org-parallel-row-splitter  " ;\n" "Row splitter.")

;; Languages
(defvar org-parallel--from nil "Translate from language.")
(defvar org-parallel--to   nil "Translate to language.")
(defvar org-parallel-languages
 '("af(Afrikaans)"     "id(Bahasa Indonesia)" "ru(Русский)"        "am(አማርኛ)"
   "ig(Igbo)"          "sd(ﻱﮅﻨﺳ)"             "ar(ﺔﻴﺑﺮﻌﻟﺍ)"           "is(Íslenska)"
   "si(සිංහල)"          "az(Azərbaycanca)"     "it(Italiano)"       "sk(Slovenčina)"
   "ba(башҡорт теле)"  "ja(日本語)"           "sl(Slovenščina)"     "be(беларуская)"
   "jv(Basa Jawa)"     "sm(Gagana Sāmoa)"    "bg(български)"        "ka(ქართული)"
   "sn(chiShona)"      "bn(বাংলা)"            "kk(Қазақ тілі)"        "so(Soomaali)"
   "bs(Bosanski)"      "km(ភាសាខ្មែរ)"          "sq(Shqip)"           "ca(Català)"
   "kn(ಕನ್ನಡ)"           "sr-Cyrl(српски)"     "ceb(Cebuano)"        "ko(한국어)"
   "sr-Latn(srpski)"    "co(Corsu)"          "ku(Kurdî)"           "st(Sesotho)"
   "cs(Čeština)"       "ky(Кыргызча)"        "su(Basa Sunda)"      "cy(Cymraeg)"
   "la(Latina)"        "sv(Svenska)"         "da(Dansk)"            "lb(Lëtzebuergesch)"
   "sw(Kiswahili)"     "de(Deutsch)"         "lo(ລາວ)"              "ta(தமிழ்)"
   "el(Ελληνικά)"      "lt(Lietuvių)"        "te(తెలుగు)"            "emj(Emoji)"
   "lv(Latviešu)"      "tg(Тоҷикӣ)"          "en(English)"          "mg(Malagasy)"
   "th(ไทย)"           "eo(Esperanto)"       "mhr(Олык марий)"      "tl(Tagalog)"
   "es(Español)"       "mi(Māori)"           "tlh(tlhIngan Hol)"    "et(Eesti)"
   "mk(Македонски)"    "eu(Euskara)"         "ml(മലയാളം)"           "to(Lea faka-Tonga)"
   "fa(ﯽﺳﺭﺎﻓ)"          "mn(Монгол)"          "tr(Türkçe)"           "fi(Suomi)"
   "mr(मराठी)"           "tt(татарча)"         "fj(Vosa Vakaviti)"     "mrj(Кырык мары)"
   "ty(Reo Tahiti)"    "fr(Français)"         "ms(Bahasa Melayu)"    "udm(удмурт)"
   "fy(Frysk)"         "mt(Malti)"           "uk(Українська)"         "ga(Gaeilge)"
   "mww(Hmoob Daw)"    "ur(ﻭُﺩﺭُﺍ)"              "gd(Gàidhlig)"         "my(မြန်မာစာ)"
   "uz(Oʻzbek tili)"   "gl(Galego)"          "ne(नेपाली)"              "vi(Tiếng Việt)"
   "gu(ગુજરાતી)"         "nl(Nederlands)"      "xh(isiXhosa)"          "ha(Hausa)"
   "no(Norsk)"         "yi(שידִיי)"             "haw(ʻŌlelo Hawaiʻi)"  "ny(Nyanja)"
   "yo(Yorùbá)"        "he(תיִרְבִע)"            "otq(Hñąñho)"           "yua(Màaya T'àan)"
   "hi(हिन्दी)"           "pa(ਪੰਜਾਬੀ)"             "yue(粵語)"             "hmn(Hmoob)"
   "pap(Papiamentu)"   "zh-CN(简体中文)"       "hr(Hrvatski)"          "pl(Polski)"
   "zh-TW(正體中文)"    "ht(Kreyòl Ayisyen)"   "ps(ﻮﺘښﭘ)"              "zu(isiZulu)"
   "hu(Magyar)"        "pt(Português)"        "hy(Հայերեն)"           "ro(Română)"
   )
 )


(defvar org-parallel-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") #'org-parallel-translate)
    map)
  "Keymap for org-parallel: to translate word at point: `\\[org-parallel-translate]'.")


(defun org-parallel--slurp (file)
  "Read data to temp buffer from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties
       (point-min)
       (point-max))))


(defun org-parallel--template ()
  "Template which wrap text from source file."
  (format " %s %s %s"
	  org-parallel-col-splitter
	  org-parallel-col-splitter
	  org-parallel-row-splitter))


(defun org-parallel--buffer-p (buffer-name)
  "Check is BUFFER-NAME exists?"
  (if (not (eql (get-buffer buffer-name) nil))
      t
    nil))


(defun org-parallel--file-base (file)
  "Get file base from FILE."
  (file-name-sans-versions (file-name-nondirectory file)))


(defun org-parallel--get-buff-name(file)
  "Create buffer and finished file name from source FILE."
  (format "%s%s" (org-parallel--file-base file) org-parallel-file-ext))


(defun org-parallel--get-file ()
  "Get file for transformation and translating."
  (interactive)
  "~/Documents/PROJECTS/cpp/tagfile/makefile"
  (read-file-name "Choose file: "))


(defun org-parallel--lang-completion (helper)
  "Check language to translate from/to with HELPER as prompt."
  (interactive)
  (let ((prompt helper))
    (if (bound-and-true-p ido-mode)
	(ido-completing-read prompt org-parallel-languages)
      "en")))


(defun org-parallel--get-lang-from ()
  "Check language to translate to."
  (org-parallel--lang-completion "Translate from: "))


(defun org-parallel--get-lang-to ()
  "Check language to translate to."
  (let* ((prompt-part "Translate to")
	 (nl org-parallel-last-finish-language)
	 (prompt-def  (if (< 0 (length nl))
			  (format "[%s]:" nl)
			(format ":"))))
    ;; Orginize output and check default value
    (let ((res (org-parallel--lang-completion (concat prompt-part prompt-def))))
      (if (< 0 (length res))
	  ;; TODO return last value
	  ;; (progn
	  ;;  (setq org-parallel-last-finish-language res)
	  ;;  res)
	  res
	nl))) )


(defun org-parallel--format (text)
  "Format shell output data string as TEXT."
  ;;(setq alist '())
  (let ((str text)
	(alist '()))
    (let ((alist2 (split-string str "\n"))
	  (z 0))
      (while (< z (length alist2))
	(let ((x (car alist2)))
	(if (and ( > (length x) 4)
		  (string= (substring x 0 4) "    ")
		  (not (string= (substring x 4 5) " ")) )
	    (push (string-trim (substring x 4 (length x))) alist)
	  ))
	(setq alist2 (cdr alist2))
	(setq z (1+ z))
	)
      (reverse alist) )))


(defun org-parallel--get-target ()
  "Get target: a word or phrase."
  (interactive)
  (if (use-region-p)
      (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
	(if (= (length selection) 0)
	    ""
	  selection))
    (thing-at-point 'word 'no-properties) ) )


(defun org-parallel--call-trans (target postfix)
  "Run trans as cmd using TARGET POSTFIX."
  (make-thread
  (let* ((alist ())
	 (cmd-1 (format "trans %s:%s %s %s" org-parallel--from org-parallel--to target  postfix))
	 (cmd-2 (concat cmd-1
			" -show-languages n"
			" -show-original-phonetics n"
			" -show-translation-phonetics n"
			" -show-prompt-message n"
			" -show-original n"
			" -show-translation n"
			" -show-alternatives n"))
	 )
    (let ((result (concat (shell-command-to-string cmd-2) "\n")))
      (if (and (< 0 (length result)) (= 0 (length postfix)))
	  ;; Format for translation and dictionary data
	  (setq alist (org-parallel--format result))
	result)
      (ido-completing-read (format "%s: " target) alist)
      ))))


;;;###autoload
(defun org-parallel-dict ()
  "Show info about word/sentence like in dictionary."
  (org-parallel--call-trans (org-parallel--get-target) "-d"))


;;;###autoload
(defun org-parallel-translate ()
  "Show translation of word/sentence, choose it and copy to clipboard."
  (kill-new (org-parallel--call-trans (org-parallel--get-target) "")))


;;;###autoload
(defun org-parallel-create ()
  "Create buffer with FILE context."
  (interactive)
  ;; Get interactive parameters
  (let* ((file      (org-parallel--get-file))
	 (lang-from (org-parallel--get-lang-from))
	 (lang-to   (org-parallel--get-lang-to))
	 (buff-name (org-parallel--get-buff-name file)))
    ;; If not exists
    (if (eql nil (org-parallel--buffer-p buff-name))
	;;    
	(let* ((base (org-parallel--file-base file))
	       (buff (get-buffer-create buff-name))
	       (col-width (- (/ (- (window-width) (% (window-width) 2)) 2) 5)))
	  ;; Show buffer
	  (switch-to-buffer buff)
	  ;; Load data from file by line with markup
	  (mapc
	   (lambda(x) (insert (concat x (org-parallel--template))))
	   (split-string (org-parallel--slurp file) org-parallel-line-splitter t))
      
	  ;; Create table by markup
	  (table-capture (point-min)
			 (point-max)
			 org-parallel-col-splitter
			 org-parallel-row-splitter
			 nil
			 col-width)
	  ;; To start
	  (goto-char (point-min))
	  ;; Set name of file
	  (insert (concat "#+TITLE: "    base "\n"))
	  (insert (concat "#+AUTHOR: "   (user-full-name) "\n"))
	  (insert (concat "#+LANG_FROM:" lang-from "\n" ))
	  (insert (concat "#+LANG_TO:"   lang-to "\n" ))

	  (setq org-parallel--from lang-from)
	  (setq org-parallel--to lang-to)
	  ;; End
	  )
      (switch-to-buffer buff-name))))


(define-minor-mode org-parallel-mode
  "Parallel text node."
  :group 'org-parallel
  :require 'org-parallel
  :lighter " OrgP"
  :keymap org-parallel-map
  :global t
  
  ;; Table markup
  (make-local-variable org-parallel-line-splitter)
  (make-local-variable org-parallel-file-ext)
  (make-local-variable org-parallel-col-splitter)
  (make-local-variable org-parallel-row-splitter)

  ;; Languages
  (make-local-variable org-parallel--from)
  (make-local-variable org-parallel--to)
  (make-local-variable org-parallel-languages)

  ;; Key bindings
  (make-local-variable org-parallel-map)
  )

(provide 'org-parallel)

;;; org-parallel.el ends here
