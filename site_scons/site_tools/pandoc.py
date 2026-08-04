import SCons.Action
import SCons.Builder
import SCons.Util
import SCons.Scanner
import SCons.Tool


def pandocTemplateFlag(target, source, env, for_signature):
    if env.get("PANDOC_TEMPLATE"):
        return "--template=$PANDOC_TEMPLATE"
    return ""


def pandocEmitter(target, source, env):
    if env.get("PANDOC_TEMPLATE"):
        env.Depends(target, env["PANDOC_TEMPLATE"])
    return target, source


def generate(env):
    """Add Builders and construction variables for pandoc to an Environment."""
    env["BUILDERS"]["PANDOC"] = SCons.Builder.Builder(
        action="$PANDOC $PANDOCFLAGS $PANDOC_TEMPLATEFLAG -o $TARGET $SOURCES",
        emitter=pandocEmitter,
    )
    env["PANDOC"] = "pandoc"
    env["PANDOC_TEMPLATEFLAG"] = pandocTemplateFlag


def exists(env):
    return env.Detect("pandoc")


# vim: ft=scons
