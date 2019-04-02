#!/usr/bin/env python3

import sys
import re
import os
import shutil
import json
from subprocess import Popen, PIPE


def expandpath(path):
    path = os.path.expanduser(path)
    path = os.path.expandvars(path)
    path = os.path.abspath(path)
    return path


WAL_DIR = expandpath("~/.cache/wal")
CONFIG_FILE_PATH = expandpath("~/.cache/wal/colors.json")
WAL_FILE_PATH = expandpath("~/.cache/wal/wal")


def create_wal_cache():
    """Create wal's .cache folder (if it doesn't exist)."""
    if not os.path.isdir(WAL_DIR):
        os.mkdir(WAL_DIR)


def empty_config():
    """Construct an empty pywal config."""
    return {
        "colors": {},
    }


def load_config():
    """Load the current pywal configuration."""
    if not config_exists():
        create_wal_cache()
        return empty_config()
    with open(CONFIG_FILE_PATH, "r") as f:
        return json.load(f)


def save_config(config_dict):
    """Save the current pywal configuration."""
    with open(CONFIG_FILE_PATH, "w") as f:
        json.dump(config_dict, f)


def rewrite_wal_file(new_wallpaper):
    """Adjust the `wal` file to hold the new wallpaper."""
    with open(WAL_FILE_PATH, "w") as f:
        # The `wal` file should just contain the wallpaper path - nothing else.
        f.write(new_wallpaper)


def call_process(args):
    """Call an external process, with reasonable error handling."""
    process = Popen(args, stdout=PIPE, stderr=PIPE)
    stdout, stderr = process.communicate()
    # We want to print the output to track the underlying process.
    print(stdout.decode("utf-8"))
    return_code = process.returncode
    if return_code != 0:
        stderr_string = stderr.decode(encoding="utf-8")
        raise RuntimeError(
            "Subprocess {} failed with return code {}. Error message:"
            "\n{}".format(args, return_code, stderr_string))


def refresh_wal():
    """Refresh the wal display (call `wal -R`)."""
    call_process(["wal", "-R"])


def reload_theme():
    """Set the wal theme from the colors.json file.

    This is intended to ensure manual changes propogate out properly, i.e. that
    caches are rebuilt. Just reloading the last config (`wal -R`) might lead to
    out-of-date caches from the old config.

    """
    call_process(["wal", "--theme", CONFIG_FILE_PATH])


def call_normally(image_path):
    """Call wal as normal - set the entire theme from an image."""
    call_process(["wal", "-i"])


def copy_config(destination):
    """Copy the config file to another destination."""
    shutil.copy(CONFIG_FILE_PATH, expandpath(destination))


def config_exists():
    """Does the config file exist?"""
    return os.path.isfile(CONFIG_FILE_PATH)


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
    config = load_config()
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
    save_config(config)
    reload_theme()


if __name__ == "__main__":
    if len(sys.argv) != 17:
        raise ValueError("Please specify 16 colours as the command line "
                         "arguments. No more, no less. You gave "
                         "{}.".format(len(sys.argv) - 1))
    colors = sys.argv[1:17]
    replace_colors(colors)
