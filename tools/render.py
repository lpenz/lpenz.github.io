
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

def renderSourceScanner(node, env, path):
    candidates = cre.findall(node.get_contents())
    includes = []
    for f in candidates:
        mark = f[0]
        if mark in ids.keys():
            if f[:2] == f[-2:] == mark*2:
                f = f[2:-2]
        else:
            includes.extend(renderScanFile(env.File(f)))
        includes.append(f)
    return includes


def renderTargetScanner(node, env, path):
    rv = [os.path.join(env['TOP'], env['RENDER'])]
    yfilename = os.path.join(os.path.dirname(str(node)), 'info.yaml')
    if os.path.isfile(yfilename):
        y = yaml.load(open(yfilename).read())
        if y.has_key('template'):
            rv.append(os.path.join(env['TOP'], 'templates', y['template'] + '.html'))
        else:
            rv.append(os.path.join(env['TOP'], 'templates', 'base.html'))
    return rv


def generate(env):
    """Add Builders and construction variables for render to an Environment."""
    RenderAction = SCons.Action.Action('$RENDERCOM', '$RENDERCOMSTR')
    RenderTargetScanner = SCons.Scanner.Base(name = "renderTargetScanner", function = renderTargetScanner, skeys = ['.t2t'])
    env['BUILDERS']['RENDER'] = SCons.Builder.Builder(action = RenderAction, suffix = '.html', src_suffix = '.t2t', source_scanner = SCons.Tool.SourceFileScanner, target_scanner = RenderTargetScanner)
    env['RENDER']      = 'tools/render'
    env['RENDERCOM']   = '$RENDER $SOURCE $TARGET'
    RenderSourceScanner = SCons.Scanner.Base(name = "renderSourceScanner", function = renderSourceScanner, skeys = ['.t2t'], recursive = True)
    SCons.Tool.SourceFileScanner.add_scanner('.t2t', RenderSourceScanner)


def exists(env):
    return env.Detect('render')

# vim: ft=scons


