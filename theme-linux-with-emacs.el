(require 'color)


(defvar themelinux-theming-method 'ansi
  "How to extract terminal colours from Emacs.

Possible values are `ansi' and `fonts'.

`ansi'  - Use the ansi colors defined in
          `ansi-color-names-vector'. This is the preferred
          method, but it will fail if the theme failed to define
          ansi colors.

`fonts' - Try to extract a nice color palette from inbuilt fonts.
          Legacy method.")


(defvar linuxtheme--preferred-fonts
  '(
    ;; Color0 uses the background of the default face so we don't include it.
    nil                                 ; Color0
    font-lock-string-face               ; Color1
    font-lock-doc-face                  ; Color2
    font-lock-keyword-face              ; Color3
    font-lock-preprocessor-face         ; Color4 - is this right?
    error                               ; Color5
    font-lock-function-name-face        ; Color6
    default                             ; Color7 - foreground
    ;; Colours 8-15 are alternate colours. Normally this seems to mean they are
    ;; "pale versions" of the main 8 colours, except colour 8, which is halfway
    ;; between the background and foreground. Pywal doesn't seem to use pale
    ;; colours here - it just adds a shadow for color 8, then defers back to the
    ;; first 8 colours.
    shadow                              ; Color8 - pale background.
    ;; Defer to normal colours for colors 9-15.
    font-lock-string-face               ; Color9
    font-lock-doc-face                  ; Color10
    font-lock-keyword-face              ; Color11
    font-lock-preprocessor-face         ; Color12
    error                               ; Color13
    font-lock-function-name-face        ; Color14
    default                             ; Color15 - foreground pale
    )
  ;; TODO: Docstring
  "List of fonts to extract the 16 terminal colours from.")


(defun themelinux--color-name-to-hex (color-name)
  "Convert a `color-name' into a 6-digit hex value.

E.g. \"Orange\" -> \"#FFA500\"."
  ;; Upcase result to make it neat.
  (upcase
   ;; Have to convert to rgb first, *then* convert back to hex.
     (apply
      'color-rgb-to-hex
      (append (color-name-to-rgb
               color-name)
              ;; We have to specify "2" as the fourth argument
              '(2)))))


(defun themelinux--extract-background-color ()
  "Extract the background color from the default font."
  (themelinux--color-name-to-hex
   (face-background 'default)))


(defun themelinux--extract-shadow-color ()
  (themelinux--color-name-to-hex
   (face-foreground 'shadow)))


(defun themelinux--extract-font-colors ()
  "Extract 16 terminal colours from inbuilt fonts."
  ;; TODO: When duplicate colors are found, iterate over fallback fonts.

  ;; First colour is just the background color of the default face.
  (mapcar 'themelinux--color-name-to-hex
          (cons
           (themelinux--extract-background-color)
           (mapcar (lambda (font)
                     (face-foreground
                      font))
                   ;; Ignore the first font - it should be nil because we get
                   ;; the background color a different way.
                   (cdr linuxtheme--preferred-fonts)))))


(defun themelinux--apply-colors-with-pywal (colors)
  ;; TODO: Check pywal is installed
  ;; TODO: Integrate this python colors script
  (message "Applying colors: %s" colors)
  (if (eq 0 (apply 'call-process (append
                                  '("~/.scripts/python/wal_change_colors.py" nil nil nil)
                                  colors)))
      (message "Successfully applied colors!")
    (user-error "There was an error applying the colors.")))


(defun themelinux--16-colors-from-ansi ()
  (let* ((ansi-colors-vector
          ;; Duplicate the 8 basic ansi colors to get a 16-color palette.
          (vconcat ansi-color-names-vector
                   ansi-color-names-vector)))
    ;; Ansi colors are inconsistent. The first of the 8 ansi colors may be the
    ;; background color, but it might also be the shadow color. We modify them
    ;; manually to ensure consistency.
    (aset ansi-colors-vector 0 (themelinux--extract-background-color))
    (aset ansi-colors-vector 8 (themelinux--extract-shadow-color))
    (seq-into ansi-colors-vector 'list)))


(defun themelinux-theme-from-ansi ()
  (interactive)
  (themelinux--apply-colors-with-pywal
   (themelinux--16-colors-from-ansi)))


(defun themelinux-theme-from-fonts ()
  (interactive)
  (themelinux--apply-colors-with-pywal
   (themelinux--extract-font-colors)))


(defun themelinux-theme-from-emacs ()
  "Theme the rest of Linux based on the Emacs theme."
  (interactive)
  (cond ((eq themelinux-theming-method 'ansi)
         (themelinux-theme-from-ansi))
        ((eq themelinux-theming-method 'fonts)
         (themelinux-theme-from-fonts))
        (t (user-error (format "Unknown theming method: '%s'"
                               themelinux-theming-method)))))


(provide 'theme-linux-with-emacs)
;;; theme-linux-with-emacs.el ends here
