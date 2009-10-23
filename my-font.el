(defun my-set-font ()
  "set font when gui started

default font size must available, `adobe courier' has no size 13, if we set 
default font to `-adobe-courier-medium-r-*-*-13-*-*-*-*-*-fontset-ifre', it 
does not work. try to use scalable fonts (PIXEL has value 0, such as
  -bitstream-bitstream vera sans mono-medium-r--0-0-0-0-0-0-0-iso8859-1
while set `scalable-fonts-allowed' to t does now work."
  (interactive)
  (create-fontset-from-fontset-spec
   (let (
;;          (wenquanyifont "-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-*-*-*-p-*-iso10646-1")
         ;; (wenquanyifont "-wenquanyi-wenquanyi bitmap song-medium-r-normal--12-120-75-75-p-80-iso10646-1")
         (wenquanyifont "-wenquanyi-wenquanyi bitmap song-medium-r-normal--13-130-75-75-p-80-iso10646-1")
;;          (wenquanyifont "-wenquanyi-wenquanyi bitmap song-medium-r-normal--15-150-75-75-p-80-iso10646-1")
;;          (wenquanyifont "-wenquanyi-wenquanyi bitmap song-medium-r-normal--16-160-75-75-p-80-iso10646-1")
         (monofont "-adobe-courier-medium-r-*-*-14-*-*-*-*-*-fontset-ifree")
;;          (monofont "-misc-fixed-medium-r-semicondensed--13-*-*-*-c-60-fontset-ifree")
;;          (monofont "-misc-fixed-medium-r-normal--13-*-*-*-c-70-fontset-ifree")
;;          (monofont "-misc-dejavu sans mono-medium-r-*--13-*-*-*-m-*-fontset-ifree")
;;          (monofont "-*-bitstream vera sans mono-medium-r-*-*-*-*-*-*-c-*-fontset-ifree")
           )
     (concat
      monofont
      ",chinese-gb2312:" wenquanyifont
      ",chinese-big5-1:" wenquanyifont
      ",chinese-big5-2:" wenquanyifont
      ",chinese-sisheng:" wenquanyifont
      ",chinese-cns11643-3:" wenquanyifont
      ",chinese-cns11643-4:" wenquanyifont
      ",chinese-cns11643-5:" wenquanyifont
      ",chinese-cns11643-6:" wenquanyifont
      ",chinese-cns11643-7:" wenquanyifont
      ",japanese-jisx0208-1978:" wenquanyifont
      ",japanese-jisx0208:" wenquanyifont
      ",japanese-jisx0212:" wenquanyifont
      ",japanese-jisx0213-1:" wenquanyifont
      ",japanese-jisx0213-2:" wenquanyifont
      )
     )
    )

    (setq scalable-fonts-allowed t)
    (set-default-font "fontset-ifree")

    (add-to-list 'after-make-frame-functions
                 (lambda (new-frame) (select-frame new-frame)
                   (set-default-font "fontset-ifree")))
    )

(defun my-set-font1 ()
  "set font when gui started.
this approach seems works: `describe-char' show that ascii characters are
displayed with `-adobe-courier-medium-r-normal--12-*-iso8859-1' and 
chinese fonts are displayed with `wenquanyi bitmap song'.
problem: can not set font size of `wenquanyi' font."
  (interactive)
  (let (
        (wenquanyifont '("wenquanyi bitmap song" . nil))
;;         (wenquanyifont '("-wenquanyi-wenquanyi bitmap song-medium-r-normal--15-150-75-75-p-80-iso10646-1" . nil))
        )
    (setq scalable-fonts-allowed t)
    (set-default-font "Bitstream Vera Sans Mono")
    (dolist (elt `((chinese-gb2312 . ,wenquanyifont)
                   (chinese-big5-1 . ,wenquanyifont)
                   (chinese-big5-2 . ,wenquanyifont)
                   (chinese-sisheng . ,wenquanyifont)
                   (chinese-cns11643-3 . ,wenquanyifont)
                   (chinese-cns11643-4 . ,wenquanyifont)
                   (chinese-cns11643-5 . ,wenquanyifont)
                   (chinese-cns11643-6 . ,wenquanyifont)
                   (chinese-cns11643-7 . ,wenquanyifont)
                   (japanese-jisx0208-1978 . ,wenquanyifont)
                   (japanese-jisx0208 . ,wenquanyifont)
                   (japanese-jisx0212 . ,wenquanyifont)
                   (japanese-jisx0213-1 . ,wenquanyifont)
                   (japanese-jisx0213-2 . ,wenquanyifont)
                   (korean-ksc5601 . ,wenquanyifont)
                   ))
      (set-fontset-font "fontset-default" (car elt) (cdr elt))))
  )
(provide 'my-font)
;;; end
