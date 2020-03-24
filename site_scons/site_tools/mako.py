import SCons.Action
import SCons.Builder
import SCons.Util
import SCons.Scanner
import SCons.Tool
import yaml
import os


def makoTargetScanner(node, env, path):
    rv = [os.path.join(env['TOP'], f) for f in [env['MAKO'], 'infotree.yaml']]
    yfilename = os.path.join(os.path.dirname(str(node)), 'info.yaml')
    if os.path.isfile(yfilename):
        y = yaml.load(open(yfilename).read())
        if 'template' in y:
            rv.append(
                os.path.join(env['TOP'],
                             'templates',
                             y['template'] + '.html.mako'))
        else:
            rv.append(
                os.path.join(env['TOP'], 'templates', 'htmlpage.html.mako'))
        rv.append(os.path.join(env['TOP'], 'templates', 'htmlbase.html.mako'))
    return rv


def generate(env):
    """Add Builders and construction variables for mako to an Environment."""
    MakoTargetScanner = SCons.Scanner.Base(
        name="makoTargetScanner", function=makoTargetScanner)
    env['BUILDERS']['MAKO'] = SCons.Builder.Builder(
        action='$MAKO $MAKOFLAGS $SOURCE $TARGET',
        suffix='.html',
        src_suffix='.bhtml',
        source_scanner=SCons.Tool.SourceFileScanner,
        src_builder=[env['BUILDERS']['T2TBHTML'],
                     env['BUILDERS']['PANDOC']],
        target_scanner=MakoTargetScanner)
    env['MAKO'] = 'tools/mako'


def exists(env):
    return env.Detect('mako')

# vim: ft=scons
