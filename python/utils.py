#!/usr/bin/env python3

import os


def expandpath(path):
    path = os.path.expanduser(path)
    path = os.path.expandvars(path)
    path = os.path.abspath(path)
    return path
