"""Create gnupg logs for gpg article"""


import sys
import re
from contextlib import contextmanager
import pexpect
from io import StringIO


class Session(pexpect.spawn):
    def __init__(self, env=None, preproc=None, data=None):
        env_ = {"PS1": "\\$ ", "PS2": "=>"}
        env_.update(env or {})
        pexpect.spawn.__init__(
            self,
            "bash",
            ["--norc"],
            encoding="utf-8",
            timeout=30,
            # echo=False,
            env=env_,
        )
        self.preproc = preproc
        self.data = data
        self._prompt = u"[#$] $"
        self._logfd = None
        self._pending = ""
        self.waitready()
        self.sendwait('export PS1="$ "')
        self.sendwait("stty cols 9999")
        self.sendwait("set -e")

    def log(self, s):
        if self.preproc:
            s = self.preproc(s)
        sys.stdout.write(s)
        if self._logfd:
            self._logfd.write(s)

    def process(self):
        self.log(self._pending)
        self._pending = ""
        m = self.match
        i1, i2 = m.span()
        self.log(m.string[0:i1])
        self._pending += m.string[i1:]

    def waitready(self, prompt=None):
        prompt = prompt or self._prompt
        try:
            self.expect(prompt)
        except pexpect.TIMEOUT:
            print(self._pending)
            raise
        return self.process()

    @contextmanager
    def logging(self, key=None):
        fd = StringIO()
        self._logfd = fd
        yield fd
        self._logfd = None
        if self.data is not None and key:
            self.data[key] = fd.getvalue()
        fd.close()

    @contextmanager
    def prompt(self, prompt):
        oldprompt = self._prompt
        self._prompt = prompt
        yield
        self._prompt = oldprompt

    def sendwait(self, s, prompt=None, invisible=False):
        # ini/end_bold smuggles html <b> through pandoc
        self._pending += "{b}%s{/b}" % s
        pexpect.spawn.send(self, s + "\r")
        if invisible:
            self._pending += "\n"
        else:
            self.expect(re.escape(s) + "\r")
        return self.waitready(prompt=prompt)

    def sendpass(self, s):
        return self.sendwait(s, invisible=True)

    def done(self):
        pexpect.spawn.send(self, "exit\r")
        self.expect(pexpect.EOF)
        self.wait()
        self.close()
        assert self.exitstatus == 0
