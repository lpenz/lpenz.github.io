import SCons.Action
import SCons.Builder
import SCons.Util
import SCons.Scanner
import SCons.Tool


def generate(env):
    """Add Builders and construction variables for pandoc to an Environment."""
    env['BUILDERS']['PANDOC'] = SCons.Builder.Builder(
        action='$PANDOC $PANDOCFLAGS -o $TARGET $SOURCES',
        suffix='.bhtml',
        src_suffix='.md')
    env['PANDOC'] = 'pandoc'


def exists(env):
    return env.Detect('pandoc')

# vim: ft=scons
