
import re
import os

import SCons.Action
import SCons.Builder
import SCons.Util

output_re = [
        re.compile(r'''png\('([^']+)'\)''', re.M)
        , re.compile(r'''save\(.*file\s*=\s*'([^']+)'\s*[),]''', re.M)
        ]

def rEmitter(target, source, env):
    target = []
    for s in source:
        sdir = os.path.dirname(str(s))
        deps = rSearchDeps(s, env)
        deps.append(s)
        for d in deps:
            contents = d.get_contents()
            for r in output_re:
                for t in r.findall(contents):
                    target.append(os.path.join(sdir, t))
    return target, source


source_re = [
        re.compile(r'''source\('([^']+)'\)''', re.M)
        , re.compile(r'''load\('([^']+)'\)''', re.M)
        ]

def rSearchDeps(node, env):
    contents = node.get_contents()
    rv = []
    for r in source_re:
        for d in r.findall(contents):
            dpath = os.path.join(os.path.dirname(str(node)), d)
            df = env.File(dpath)
            rv.append(df)
            rv.extend(rSearchDeps(df, env))
    return rv


def rScannerFunc(node, env, path):
    return rSearchDeps(node, env)


def generate(env):
    """Add Builders and construction variables for R to an Environment."""

    rAction = SCons.Action.Action('$RCOM', chdir=1)

    rScanner = SCons.Scanner.Base(
            name = 'rScanner',
            function = rScannerFunc,
            skeys = ['.R'],
            path_function = SCons.Scanner.FindPathDirs('RPATH'),
            recursive = True)

    bld = SCons.Builder.Builder(
            action = rAction,
            src_suffix = '.R',
            emitter = rEmitter,
            source_scanner = rScanner)

    env['BUILDERS']['R'] = bld
    env['RPATH'] = ['.']
    env['RFLAGS'] = SCons.Util.CLVar('')
    env['RCOM']   = './${SOURCE.file} $RFLAGS'


def exists(env):
    return env.Detect('R')


# vim: ft=scons

