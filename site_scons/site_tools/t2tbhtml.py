import SCons.Action
import SCons.Builder
import SCons.Util
import SCons.Scanner
import SCons.Tool
import os
import re

pjoin = os.path.join

cre = re.compile(r"^%!include:\s*(.*)$", re.M)
ids = {"`": "verb", '"': "raw", "'": "passthru"}


def t2tbhtmlSourceScan(node, env):
    dirname = str(node.dir)
    candidates = cre.findall(node.get_contents())
    includes = []
    for f in candidates:
        mark = f[0]
        if mark in ids.keys():
            if f[:2] == f[-2:] == mark * 2:
                f = f[2:-2]
        else:
            fileobj = env.File(pjoin(dirname, f))
            subincludes = t2tbhtmlSourceScan(fileobj, env)
            includes.extend(subincludes)
        includes.append(env.File(pjoin(dirname, f)))
    includes.append(env.File(os.path.join(env["TOP"], "t2tconfig")))
    return includes


def t2tbhtmlSourceScanner(node, env, path):
    return t2tbhtmlSourceScan(node, env)


def t2tbhtmlTargetScanner(node, env, path):
    return [os.path.join(env["TOP"], env["T2TBHTML"])]


def generate(env):
    """Add Builders and construction variables for t2tbhtml to an
    Environment."""
    T2tbhtmlTargetScanner = SCons.Scanner.Base(
        name="t2tbhtmlTargetScanner", function=t2tbhtmlTargetScanner
    )
    env["BUILDERS"]["T2TBHTML"] = SCons.Builder.Builder(
        action="$T2TBHTML $SOURCE $TARGET",
        suffix=".bhtml",
        src_suffix=".t2t",
        source_scanner=SCons.Tool.SourceFileScanner,
        target_scanner=T2tbhtmlTargetScanner,
    )
    env["T2TBHTML"] = "tools/t2tbhtml"
    T2tbhtmlSourceScanner = SCons.Scanner.Base(
        name="t2tbhtmlSourceScanner",
        function=t2tbhtmlSourceScanner,
        skeys=[".t2t"],
        recursive=True,
    )
    SCons.Tool.SourceFileScanner.add_scanner(".t2t", T2tbhtmlSourceScanner)


def exists(env):
    return env.Detect("t2tbhtml")


# vim: ft=scons
