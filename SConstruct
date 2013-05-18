
import os
import glob
import re

env = Environment(ENV = os.environ
    , TOP = os.path.abspath(os.path.curdir)
    , toolpath = ['tools/scons', '/usr/lib/scons/SCons/Tool'])
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

htmlsitefiles = set()
env.Export('htmlsitefiles')

# Main page:
env.Command('index.t2t', 'index.bt2t', 'tools/mako $SOURCE $TARGET')
env.Depends('index.t2t', 'infotree.yaml')
env.MAKO('index.t2t', MAKOFLAGS='-t base')
htmlsitefiles.add('index.html')


# About me:
env.SConscript('about/SConscript', )


# Articles:
env.SConscript('articles/SConscript')


# Debian:
env.SConscript('debian/SConscript')


# Feeds:
env.SConscript('feeds/SConscript')


# Final touches:
env.Command('sitemap.xml', list(htmlsitefiles), 'tools/sitemapper $TARGET $SOURCES')
env.Depends('sitemap.xml', 'tools/sitemapper')

