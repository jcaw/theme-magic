;;; theme-magic.el --- Apply your Emacs theme to the rest of Linux  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author: GitHub user "jcaw" <40725916+jcaw@users.noreply.github.com>
;; URL: https://github.com/jcaw/theme-magic.el
;; Keywords: unix, faces, terminals, extensions
;; Version: 0.1.4
;; Package-Requires: ((emacs "24.4") (seq "1.8"))

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
;; Just call `theme-magic-from-emacs' and your Emacs theme will be applied
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
(require 'seq)


(defvar theme-magic--theming-functions
  '(
    load-theme
    ;; When these are enabled, changing the theme calls wal multiple times.
    ;; Might be fixable by running wal with an idle timer, but the updates would
    ;; be less synchronised. Note that without these, disabling a theme will not
    ;; trigger a wal update.
    ;;
    ;; enable-theme
    ;; disable-theme
    )
  "Functions that should trigger an update of the linux theme.

\(Iff auto-updating is enabled.\)")


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


(defvar theme-magic--pywal-buffer-name "*pywal*"
  "Name to use for pywal's output buffer.")


(defun theme-magic--color-name-to-hex (color-name)
  "Convert a `COLOR-NAME' into a 6-digit hex value.

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


(defun theme-magic--extract-default-color ()
  "Extract the foreground color of the default face, in hex."
  (theme-magic--color-name-to-hex
   (face-foreground 'default)))


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


(defun theme-magic--erase-pywal-buffer ()
  "Erase the contents of the pywal output buffer iff it exists."
  (when (get-buffer theme-magic--pywal-buffer-name)
    (with-current-buffer theme-magic--pywal-buffer-name
      (erase-buffer))))


(defun theme-magic--call-pywal-process (colors)
  "Call the Python script that sets the theme with pywal.

`COLORS' should be the 16 hexadecimal colors to use as the theme.

This just calls the python script from the home directory. It
doesn't provide any wrapper feedback to the user."
  ;; Kill pywal buffer if it already exists
  (theme-magic--erase-pywal-buffer)
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
             nil theme-magic--pywal-buffer-name t
             theming-script)
            ;; Now we expand the list of colors
            colors))))


(defun theme-magic--apply-colors-with-pywal (colors)
  "Change the linux theme to use the 16 `COLORS' (using pywal).

`COLORS' should be a list of 16 hexadecimal terminal colors.

Provides some wrapper feedback to the user, plus some error
handling."
  (message "Applying colors: %s" colors)
  (if (eq 0 (theme-magic--call-pywal-process colors))
      (message "Successfully applied colors!")
    (user-error "There was an error applying the colors. See buffer \"*pywal*\" for details.")))


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
    ;; Some themes mess up the foreground color (seen in `material-theme').
    ;; Foreground color is very important anyway, and should match Emacs even if
    ;; it deviates from the Ansi palette. Manually fix it.
    (aset ansi-colors-vector 7 (theme-magic--extract-default-color))
    ;; Finally, we ensure each color is hexadecimal. (We also want to output a
    ;; list - this will also serve that purpose.)
    (mapcar 'theme-magic--color-name-to-hex
            ansi-colors-vector)))


;;;###autoload
(defun theme-magic-from-emacs ()
  "Theme the rest of Linux based on the Emacs theme."
  (interactive)
  ;; This will actually check dependencies twice, but that's fine - it's cheap
  ;; we want to do it up front.
  (theme-magic--check-dependencies)
  (theme-magic--apply-colors-with-pywal
   (theme-magic--16-colors-from-ansi)))


(defun theme-magic-from-emacs--wrapper (&rest _)
  "Wrapper for `theme-magic-from-emacs' to be used as advice."
  (theme-magic-from-emacs))


;;;###autoload
(defun theme-magic-enable-auto-update ()
  "Enable automatic Linux theme updating.

The Linux theme will be updated whenever the Emacs theme is
changed."
  (interactive)
  ;; TODO: Maybe swap this to a minor mode?
  (mapc (lambda (func)
          (advice-add func :after 'theme-magic-from-emacs--wrapper))
        theme-magic--theming-functions))


;;;###autoload
(defun theme-magic-disable-auto-update ()
  "Disable automatic Linux theme updating.

The Linux theme will need to be updated manually with
`theme-magic-from-emacs'."
  (interactive)
  (mapc (lambda (func)
          (advice-remove func 'theme-magic-from-emacs--wrapper))
        theme-magic--theming-functions))


(provide 'theme-magic)
;;; theme-magic.el ends here
