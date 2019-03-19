(require 'color)


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
    ;; "pale versions" of the main 6 colours, except colour 8, which is halfway
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


(defun themelinux--extract-font-colors ()
  "Extract 16 terminal colours from inbuilt fonts."
  ;; First colour is just the background color of the default face.
  (mapcar 'themelinux--color-name-to-hex
          (cons
           (face-background 'default)
           (mapcar (lambda (font)
                     (face-foreground
                      font))
                   ;; Ignore the first font - it should be nil because we get
                   ;; the background color a different way.
                   (cdr linuxtheme--preferred-fonts)))))


(defun themelinux--apply-colors-with-pywal (colors)
  ;; TODO: Check pywal is installed
  ;; TODO: Integrate this python colors script
  (message "colors: %s" colors)
  (apply 'call-process (append
                        '("~/.scripts/python/wal_change_colors.py" nil nil nil)
                        colors)))



(defun themelinux-theme-from-emacs ()
  "Theme the rest of Linux based on the Emacs theme."
  (interactive)
  (themelinux--apply-colors-with-pywal
   (themelinux--extract-font-colors)))


(provide 'theme-linux-with-emacs)
;;; theme-linux-with-emacs.el ends here
