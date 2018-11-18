import os
import glob

if False:
    Environment = None
env = Environment(ENV=os.environ, TOP=os.path.abspath(
    os.path.curdir), toolpath=['tools/scons', '/usr/lib/scons/SCons/Tool'])
env.HTMLSITEFILES = set()
env.Export('env')

for t in ['haskell', 't2tbhtml', 'mako', 'R', 'gcc']:
    env.Tool(t)

# infotree:


def infotreeProcDir(d, l):
    if not os.path.isdir(d):
        return
    i = os.path.join(d, 'info.yaml')
    if os.path.isfile(i):
        l.append(os.path.relpath(i))
    for s in glob.glob(os.path.join(d, '*')):
        infotreeProcDir(s, l)


infofiles = []
infotreeProcDir('.', infofiles)
env.Command('infotree.yaml', infofiles, 'tools/infotreebuild $TARGET $SOURCES')
env.Depends('infotree.yaml', 'tools/infotreebuild')

# Main page:
env.Command('index.t2t', 'index.bt2t', 'tools/mako $SOURCE $TARGET')
env.Depends('index.t2t', 'infotree.yaml')
env.MAKO('index.t2t', MAKOFLAGS='-t htmlpage')
env.HTMLSITEFILES.add('index.html')

# Logo:
env.Command('media/logo-black.png', 'media/logo-black.svg',
            'inkscape -z -e $TARGET -w 14 -h 14 $SOURCE')
env.HTMLSITEFILES.add('media/logo-white.png')
env.Command('media/logo-white.png', 'media/logo-white.svg',
            'inkscape -z -e $TARGET -w 14 -h 14 $SOURCE')
env.HTMLSITEFILES.add('media/logo-black.png')


# About me:
env.SConscript('about/SConscript')


# Articles:
env.SConscript('articles/SConscript')


# Debian:
env.SConscript('debian/SConscript')


# Feeds:
env.SConscript('feeds/SConscript')


# Final touches:
env.Command('_linkchecker_ok.txt', list(env.HTMLSITEFILES),
            'linkchecker -flinkcheckerrc '
            'index.html && md5sum $SOURCES > $TARGET')
env.Command('sitemap.xml', list(env.HTMLSITEFILES),
            'tools/sitemapper $TARGET $SOURCES')
env.Depends('sitemap.xml', 'tools/sitemapper')
