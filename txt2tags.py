
import SCons.Action
import SCons.Builder
import SCons.Util
import SCons.Scanner
import re

cre = re.compile('^%!include:\s*(.*)$', re.M)
ids = {'`':'verb', '"':'raw', "'":'passthru' }

def txt2tagsScanFile(node, env):
    candidates = cre.findall(node.get_contents())
    includes = []
    for file in candidates:
        mark = file[0]
        if mark in ids.keys():
            if file[:2] == file[-2:] == mark*2:
                file = file[2:-2]
        else:
            includes.extend(txt2tagsScanFile(env.File(file)))
        includes.append(file)
    return includes

def txt2tagsScannerFunc(node, env, path):
    return txt2tagsScanFile(node, env)

def generate(env):
    """Add Builders and construction variables for txt2tags to an Environment."""

    Txt2tagsAction = SCons.Action.Action('$TXT2TAGSCOM', '$TXT2TAGSCOMSTR')
    Txt2tagsScanner = SCons.Scanner.Base(name = "txt2tagsScanner", function = txt2tagsScannerFunc, skeys = ['.t2t'], recursive = True)
    bld = SCons.Builder.Builder(action = Txt2tagsAction, src_suffix = '.t2t', source_scanner = Txt2tagsScanner)

    env['BUILDERS']['TXT2TAGS'] = bld

    env['SCANNERS'].append(Txt2tagsScanner)

    env['TXT2TAGS']      = 'txt2tags'
    env['TXT2TAGSFLAGS'] = SCons.Util.CLVar('')
    env['TXT2TAGSCOM']   = '$TXT2TAGS $TXT2TAGSFLAGS -t${TARGET.get_suffix()[1:]} -i $SOURCE -o $TARGET'

def exists(env):
    return env.Detect('txt2tags')

# vim: ft=scons

