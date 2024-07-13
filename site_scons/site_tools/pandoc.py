import SCons.Action
import SCons.Builder
import SCons.Util
import SCons.Scanner
import SCons.Tool


def generate(env):
    """Add Builders and construction variables for pandoc to an Environment."""
    env["BUILDERS"]["PANDOC"] = SCons.Builder.Builder(
        action="$PANDOC $PANDOCFLAGS -o $TARGET $SOURCES"
    )
    env["PANDOC"] = "pandoc"


def exists(env):
    return env.Detect("pandoc")


# vim: ft=scons
