
import SCons.Action
import SCons.Builder
import SCons.Util
import SCons.Scanner
import SCons.Tool
import yaml
import os
import re


cre = re.compile('^%!include:\s*(.*)$', re.M)
ids = {'`':'verb', '"':'raw', "'":'passthru' }

def t2tbhtmlSourceScanner(node, env, path):
    candidates = cre.findall(node.get_contents())
    includes = []
    for f in candidates:
        mark = f[0]
        if mark in ids.keys():
            if f[:2] == f[-2:] == mark*2:
                f = f[2:-2]
        else:
            includes.extend(t2tbhtmlScanFile(env.File(f)))
        includes.append(f)
    return includes


def generate(env):
    """Add Builders and construction variables for t2tbhtml to an Environment."""
    env['BUILDERS']['T2TBHTML'] = SCons.Builder.Builder(\
            action = 'txt2tags -q -H -t html -i $SOURCE -o $TARGET'
            , suffix = '.bhtml'
            , src_suffix = '.t2t'
            , source_scanner = SCons.Tool.SourceFileScanner)
    T2tbhtmlSourceScanner = SCons.Scanner.Base(name = "t2tbhtmlSourceScanner", function = t2tbhtmlSourceScanner, skeys = ['.t2t'], recursive = True)
    SCons.Tool.SourceFileScanner.add_scanner('.t2t', T2tbhtmlSourceScanner)


def exists(env):
    return env.Detect('t2tbhtml')

# vim: ft=scons


