
import SCons.Action
import SCons.Builder
import SCons.Util
import SCons.Scanner
import SCons.Tool
import os
import re


cre = re.compile('^%!include:\s*(.*)$', re.M)
ids = {'`':'verb', '"':'raw', "'":'passthru' }

def renderSourceScanner(node, env, path):
    candidates = cre.findall(node.get_contents())
    includes = []
    for file in candidates:
        mark = file[0]
        if mark in ids.keys():
            if file[:2] == file[-2:] == mark*2:
                file = file[2:-2]
        else:
            includes.extend(renderScanFile(env.File(file)))
        includes.append(file)
    return includes


def renderTargetScanner(node, env, path):
    return [os.path.join(env['TOP'], env['RENDER'])]


def generate(env):
    """Add Builders and construction variables for render to an Environment."""
    RenderAction = SCons.Action.Action('$RENDERCOM', '$RENDERCOMSTR')
    RenderTargetScanner = SCons.Scanner.Base(name = "renderTargetScanner", function = renderTargetScanner, skeys = ['.t2t'])
    env['BUILDERS']['RENDER'] = SCons.Builder.Builder(action = RenderAction, src_suffix = '.t2t', source_scanner = SCons.Tool.SourceFileScanner, target_scanner = RenderTargetScanner)
    env['RENDER']      = 'tools/render'
    env['RENDERTEMPLATE'] = 'layouts/default.st'
    env['RENDERCOM']   = '$RENDER $RENDERTEMPLATE $SOURCE $TARGET'
    RenderSourceScanner = SCons.Scanner.Base(name = "renderSourceScanner", function = renderSourceScanner, skeys = ['.t2t'], recursive = True)
    SCons.Tool.SourceFileScanner.add_scanner('.t2t', RenderSourceScanner)


def exists(env):
    return env.Detect('render')

# vim: ft=scons

