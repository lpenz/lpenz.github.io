
import re

import SCons.Action
import SCons.Builder
import SCons.Util

main_re = re.compile(r'^main\s*::\s*IO\s+\(\)', re.M)

def haskellExecutableEmitter(target, source, env):
    base = SCons.Util.splitext(str(source[0]))[0]
    contents = source[0].get_contents()
    target.append(base + '.o')
    target.append(base)
    return target, source

def haskellAutoEmitter(target, source, env):
    base = SCons.Util.splitext(str(source[0]))[0]
    contents = source[0].get_contents()
    target.append(base + '.o')
    if main_re.findall(contents):
        target.append(base)
    return target, source

import_re = [
        re.compile(r'^import\s+qualified\s+(\S+)', re.M),
        re.compile(r'^import\s+(\S+)', re.M),
        ]

def haskellSearchDeps(root, node, env, path):
    contents = node.get_contents()
    rv = []
    for r in import_re:
        for d in r.findall(contents):
            dbasenamehs = d + '.hs'
            for dbasenameobj in [ d + '.o', d + '.hi' ]:
                dpathobj = SCons.Node.FS.find_file(dbasenameobj, path)
                if dpathobj:
                    rv.append(dpathobj)
            dpathhs = SCons.Node.FS.find_file(dbasenamehs, path)
            if dpathhs:
                rv.extend(haskellSearchDeps(root, env.File(dpathhs), env, path))
    return rv

def haskellScannerFunc(node, env, path):
    return haskellSearchDeps(node, node, env, path)

def generateAuto(env):
    """Add Builders and construction variables for HASKELL to an Environment."""

    HaskellAction = SCons.Action.Action('$HASKELLCOM', '$HASKELLCOMSTR')

    HaskellScanner = SCons.Scanner.Base(
            name = 'haskellScanner',
            function = haskellScannerFunc,
            skeys = ['.hs'],
            path_function = SCons.Scanner.FindPathDirs('HASKELLPATH'),
            recursive = True)

    bld = SCons.Builder.Builder(
            action = HaskellAction,
            src_suffix = '.hs',
            suffix = '.hi',
            emitter = haskellAutoEmitter,
            source_scanner = HaskellScanner)

    env['BUILDERS']['HASKELL'] = bld

    env['HASKELLBIN']      = 'ghc'
    env['HASKELLPATH']  = []
    env['HASKELLINCPREFIX']  = '-i'
    env['HASKELLINCSUFFIX']  = ''
    env['HASKELLINCFLAGS'] = '$( ${_concat(HASKELLINCPREFIX, HASKELLPATH, HASKELLINCSUFFIX, __env__, RDirs, TARGET, SOURCE)} $)'
    env['HASKELLFLAGS'] = SCons.Util.CLVar('--make -cpp -fglasgow-exts -Wall -Werror ')
    env['HASKELLCOM']   = '$HASKELLBIN $HASKELLFLAGS $HASKELLINCFLAGS $SOURCE'

def generateExecutable(env):
    """Add Builders and construction variables for HASKELL to an Environment."""

    HaskellAction = SCons.Action.Action('$HASKELLCOM', '$HASKELLCOMSTR')

    HaskellScanner = SCons.Scanner.Base(
            name = 'haskellScanner',
            function = haskellScannerFunc,
            skeys = ['.hs'],
            path_function = SCons.Scanner.FindPathDirs('HASKELLPATH'),
            recursive = True)

    bld = SCons.Builder.Builder(
            action = HaskellAction,
            src_suffix = '.hs',
            suffix = '.hi',
            emitter = haskellExecutableEmitter,
            source_scanner = HaskellScanner)

    env['BUILDERS']['HASKELL_EXECUTABLE'] = bld

def generate(env):
    generateAuto(env)
    generateExecutable(env)



def exists(env):
    return env.Detect('HASKELL')

# vim: ft=scons

