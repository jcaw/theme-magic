import os
import shutil
import json

from utils import expandpath


WAL_DIR = expandpath("~/.cache/wal")
CONFIG_FILE_PATH = expandpath("~/.cache/wal/colors.json")
WAL_FILE_PATH = expandpath("~/.cache/wal/wal")


def create_wal_cache():
    """Create wal's .cache folder (if it doesn't exist)."""
    if not os.path.isdir(WAL_DIR):
        os.mkdir(WAL_DIR)


def load_config():
    """Load the current pywal configuration."""
    if not config_exists():
        create_wal_cache()
        return {}
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


def refresh_wal():
    """Refresh the wal display (call `wal -R`)."""
    os.system("wal -R")


def reload_theme():
    """Set the wal theme from the colors.json file.

    This is intended to ensure manual changes propogate out properly, i.e. that
    caches are rebuilt. Just reloading the last config (`wal -R`) might lead to
    out-of-date caches from the old config.

    """
    os.system('wal --theme "{}"'.format(CONFIG_FILE_PATH))


def call_normally(image_path):
    """Call wal as normal - set the entire theme from an image."""
    os.system("wal -i {}".format(image_path))


def copy_config(destination):
    """Copy the config file to another destination."""
    shutil.copy(CONFIG_FILE_PATH,
                expandpath(destination))


def config_exists():
    """Does the config file exist?"""
    return os.path.isfile(CONFIG_FILE_PATH)
