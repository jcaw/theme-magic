;;; theme-magic.el --- Apply your Emacs theme to the rest of Linux  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author: GitHub user "jcaw" <40725916+jcaw@users.noreply.github.com>
;; URL: https://github.com/jcaw/theme-magic.el
;; Keywords: unix, faces, terminals, extensions
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") color ansi-color font-lock)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; What's the point in an Emacs theme if the rest of Linux looks different?
;;
;; Just call `theme-magic-theme-from-emacs' and your Emacs theme will be applied
;; to your entire Linux session. Now all your colors match!
;;
;; `theme-magic' uses pywal to set its themes. Pywal must be installed
;; separately. When you log out, the theme will be reset to normal. To restore
;; your theme, call "wal -R" in a shell. To reload it whenever you log in, add
;; "pywal -R" to your .xprofile (or whatever file you use to initialise programs
;; when logging in graphically).
;;
;; See the documentation of Pywal for more information:
;; https://github.com/dylanaraps/pywal
;;
;; Please note that pywal version 1.0.0 or greater is required.


;;; Code:


(require 'color)
(require 'font-lock)
(require 'ansi-color)


(defvar theme-magic-theming-method 'ansi
  "How to extract terminal colours from Emacs.

Possible values are `ansi' and `fonts'.

`ansi'  - Use the ansi colors defined in
          `ansi-color-names-vector'. This is the preferred
          method, but it will fail if the theme failed to define
          ansi colors.

`fonts' - Try to extract a nice color palette from inbuilt fonts.
          Legacy method.")


(defvar theme-magic--preferred-fonts
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


(defvar theme-magic--scripts-directory
  (concat (file-name-directory
           (or
            ;; `load-file-name' should point to this file when loading.
            load-file-name
            ;; For debugging/development: if not loaded as a package, use the
            ;; buffer for this file instead.
            buffer-file-name))
          "python/")
  "Directory where the Python scripts for manipulating pywal should be.")


(defvar theme-magic--pywal-python-script
  (concat theme-magic--scripts-directory "wal_change_colors.py")
  "Path to the Python script that sets the theme from 16 colours.")


(defun theme-magic--color-name-to-hex (color-name)
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


(defun theme-magic--extract-background-color ()
  "Extract the background color from the default font."
  (theme-magic--color-name-to-hex
   (face-background 'default)))


(defun theme-magic--extract-shadow-color ()
  "Extract the color of the shadow face, in hex."
  (theme-magic--color-name-to-hex
   (face-foreground 'shadow)))


(defun theme-magic--extract-font-colors ()
  "Extract 16 terminal colours from the inbuilt fonts.

This is designed to give a wide range of colors set by the
current theme."
  ;; TODO: When duplicate colors are found, iterate over fallback fonts.

  ;; First colour is just the background color of the default face.
  (mapcar 'theme-magic--color-name-to-hex
          (cons
           (theme-magic--extract-background-color)
           (mapcar (lambda (font)
                     (face-foreground font))
                   ;; Ignore the first font - it should be nil because we get
                   ;; the background color a different way.
                   (cdr theme-magic--preferred-fonts)))))


(defun theme-magic--check-dependencies ()
  "Ensure dependencies are installed. Throws an error if not.

Specifically, this checks that both python and pywal are
installed and accessible from the user's home dir."
  ;; If we're in a pyenv directory, we might accidentally run the virtual
  ;; version of Python instead of the user's root version. To fix this, we
  ;; temporarily change to the user's dir.
  (let ((default-directory "~/"))
    (unless (executable-find "wal")
      (user-error (concat "Could not find 'wal' executable. "
                          "Is Pywal installed and on the path?")))
    ;; TODO: Check wal is up-to-date enough to use, and the python implementation.
    (unless (executable-find "python")
      (user-error (concat "Could not find 'python' executable. "
                          "Is Python installed and on the path?")))))


(defun theme-magic--call-pywal-process (colors)
  "Call the script that sets the theme with pywal.

This just calls the python script from the home directory. It
doesn't provide any wrapper feedback to the user."
  (let (
        ;; If we're in a pyenv directory, we might accidentally run the virtual
        ;; version of Python instead of the user's root version. To fix this, we
        ;; temporarily change to the user's dir.
        (default-directory "~/")
        ;; The color modification script will work with python 2 or 3, so just
        ;; use the default Python.
        (python-executable "python")
        (theming-script theme-magic--pywal-python-script)
        )
    ;; We have to use apply here to expand the list of colors.
    (apply 'call-process
           (append
            ;; Append the first arguments to the colors list to create one long
            ;; list of arguments.
            (list
             python-executable
             ;; These are the positional arguments that `call-process' takes.
             nil nil nil
             theming-script)
            ;; Now we expand the list of colors
            colors))))


(defun theme-magic--apply-colors-with-pywal (colors)
  "Change the linux theme to the 16 colors in `colors' (using pywal).

Provides some wrapper feedback to the user, plus some error
handling."
  (message "Applying colors: %s" colors)
  (if (eq 0 (theme-magic--call-pywal-process colors))
      (message "Successfully applied colors!")
    (user-error "There was an error applying the colors.")))


(defun theme-magic--16-colors-from-ansi ()
  "Construct a set of 16 terminal colors from the current ansi colors."
  (let* ((ansi-colors-vector
          ;; Duplicate the 8 basic ansi colors to get a 16-color palette.
          (vconcat ansi-color-names-vector
                   ansi-color-names-vector)))
    ;; Ansi colors are inconsistent. The first of the 8 ansi colors may be the
    ;; background color, but it might also be the shadow color. We modify them
    ;; manually to ensure consistency.
    (aset ansi-colors-vector 0 (theme-magic--extract-background-color))
    (aset ansi-colors-vector 8 (theme-magic--extract-shadow-color))
    (seq-into ansi-colors-vector 'list)))


(defun theme-magic-theme-from-emacs-ansi ()
  "Theme Linux based on the Emacs theme, using `ansi-color-names-vector'."
  (interactive)
  (theme-magic--check-dependencies)
  (theme-magic--apply-colors-with-pywal
   (theme-magic--16-colors-from-ansi)))


(defun theme-magic-theme-from-emacs-fonts ()
  "Theme Linux based on the Emacs theme, extracting colors from fonts."
  (interactive)
  (theme-magic--check-dependencies)
  (theme-magic--apply-colors-with-pywal
   (theme-magic--extract-font-colors)))


(defun theme-magic-theme-from-emacs ()
  "Theme the rest of Linux based on the Emacs theme."
  (interactive)
  ;; This will actually check dependencies twice, but that's fine - it's cheap
  ;; we want to do it up front.
  (theme-magic--check-dependencies)
  (cond ((eq theme-magic-theming-method 'ansi)
         (theme-magic-theme-from-ansi))
        ((eq theme-magic-theming-method 'fonts)
         (theme-magic-theme-from-fonts))
        (t (user-error (format "Unknown theming method: '%s'"
                               theme-magic-theming-method)))))


(provide 'theme-magic)
;;; theme-magic.el ends here
