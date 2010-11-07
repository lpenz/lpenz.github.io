
import SCons.Action
import SCons.Builder
import SCons.Util
import SCons.Scanner
import SCons.Tool
import yaml
import os
import re


def makohtmlTargetScanner(node, env, path):
    rv = [os.path.join(env['TOP'], f) for f in [env['MAKOHTML'], 'infotree.yaml']]
    yfilename = os.path.join(os.path.dirname(str(node)), 'info.yaml')
    if os.path.isfile(yfilename):
        y = yaml.load(open(yfilename).read())
        if y.has_key('template'):
            rv.append(os.path.join(env['TOP'], 'templates', y['template'] + '.html'))
        else:
            rv.append(os.path.join(env['TOP'], 'templates', 'base.html'))
    return rv


def generate(env):
    """Add Builders and construction variables for makohtml to an Environment."""
    MakohtmlTargetScanner = SCons.Scanner.Base(name = "makohtmlTargetScanner", function = makohtmlTargetScanner)
    env['BUILDERS']['MAKOHTML'] = SCons.Builder.Builder(\
            action = '$MAKOHTML $MAKOFLAGS $SOURCE $TARGET'
            , suffix = '.html'
            , src_suffix = '.bhtml'
            , source_scanner = SCons.Tool.SourceFileScanner
            , src_builder = [env['BUILDERS']['T2TBHTML']]
            , target_scanner = MakohtmlTargetScanner)
    env['MAKOHTML'] = 'tools/makohtml'


def exists(env):
    return env.Detect('makohtml')

# vim: ft=scons


