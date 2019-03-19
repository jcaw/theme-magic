#!/usr/bin/env python3

import sys
import re

import pywal_utils


def ensure_color_is_hex(color_string):
    # This regex should match a hex color.
    match = re.search(r'^#(?:[0-9a-fA-F]{3}){1,2}$', color_string)
    return bool(match)


def replace_color(index, color, config):
    """Replace one color in the `config` dict."""
    assert 0 <= index <= 15
    ensure_color_is_hex(color)
    color_name = "color{}".format(index)
    config["colors"][color_name] = str(color)


def replace_colors(colors):
    """Replace the colors in the config dict with `colors`."""
    config = pywal_utils.load_config()
    # Ensure colors dict exists.
    if "colors" not in config:
        config["colors"] = {}
    # Now replace each color.
    for i, color in enumerate(colors):
        replace_color(i, color, config)
    # Also replace the special colors.
    if "special" not in config:
        config["special"] = {}
    config["special"]["background"] = colors[0]
    config["special"]["foreground"] = colors[7]
    config["special"]["cursor"] = colors[7]
    pywal_utils.save_config(config)
    pywal_utils.reload_theme()


if __name__ == "__main__":
    if len(sys.argv) != 17:
        raise ValueError("Please specify 16 colours as the command line "
                         "arguments. No more, no less. You gave "
                         "{}.".format(len(sys.argv) - 1))
    colors = sys.argv[1:16]
    replace_colors(colors)

