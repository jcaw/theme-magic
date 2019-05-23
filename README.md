<p align="center">
          <img src="media/logo.png" alt="theme-magic logo" />
</p>

<p align="center">
          What's the point in an Emacs theme if the rest of Linux looks different?
</p>

<p align="center">
          Apply your Emacs theme to the rest of Linux, using magic.
</p>

---

<p align="center">
          <!-- FIXME: Gif is resized. Looks terrible. -->
          <img src="media/theming-linux-demo.gif" alt="Demonstration of applying the theme to Linux with theme-magic" />
</p>

## Usage

Just call `M-x` `theme-magic-from-emacs`. theme-magic will extract the colors from your Emacs theme and apply them to the rest of Linux with [Pywal](https://github.com/dylanaraps/pywal).

If you want the Linux theme to update automatically whenever the Emacs theme is changed, enable the global minor mode `theme-magic-export-theme-mode`. For example:

```emacs-lisp
(require 'theme-magic)
(theme-magic-export-theme-mode)
```

You can disable auto-updating by disabling the minor mode.

## Installation

### Dependencies

First, you must install [Pywal](https://github.com/dylanaraps/pywal) as a dependency. Check if it's installed by calling `wal` in a shell. Make sure Python is installed too.

### Installing `theme-magic` from MELPA

`theme-magic` is [available](http://melpa.org/#/theme-magic) on MELPA. Follow the [instructions](https://melpa.org/#/getting-started) to set up MELPA.

Install `theme-magic` with `M-x package-install RET theme-magic RET`.

---

## Footnotes

### Restoring Your Theme

[Pywal](https://github.com/dylanaraps/pywal) only applies your theme to the current session. See its documentation for details. To restore the last theme, call `wal -R` in the shell. To restore your theme automatically, add the following to your `.xprofile` (or whichever dotfile is loaded automatically once your desktop starts up):

```shell
wal -R
```

### Setting Your Wallpaper

Pywal was designed to generate a color scheme that matches your wallpaper. Because of some quirks in how Pywal works, you have to set the wallpaper before exporting a theme from Emacs, or it will not be saved. Call this command in a shell:

```shell
wal -i "path/to/wallpaper.png"
```

Pywal will set your wallpaper and save it in its cache. Now, you can apply your Emacs theme:

```emacs
M-x theme-magic-from-emacs
```

Now, when you call `wal -R`, both the wallpaper and the theme will be set.

### MacOS

Theoretically, `theme-magic` should work on MacOS, but I don't have the ability to test it. If anyone has access to a Mac and can test, please [let me know](https://github.com/jcaw/theme-magic/issues/11) the results!
