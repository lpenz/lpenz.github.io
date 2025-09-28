"""Misc functions"""

import os
import subprocess
from contextlib import contextmanager


@contextmanager
def chdir(d):
    old = os.getcwd()
    os.chdir(d)
    yield
    os.chdir(old)


@contextmanager
def bgprocess(cmd):
    r = subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        yield
    finally:
        r.kill()
        r.wait()
