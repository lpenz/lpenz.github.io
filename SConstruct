
import os
import glob
import re

env = Environment(ENV = os.environ
    , TOP = os.path.abspath(os.path.curdir)
    , toolpath = ['tools/scons']
    , tools = ['haskell', 't2tbhtml', 'mako'])
env.Export('env')


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


# Haskell config:
env.Append(HASKELLPATH='tools')
env.HASKELL('tools/RenderLib.hs')


# Main page:
env.HASKELL('tools/topbuild.hs')
env.Command('index.t2t', 'tools/topbuild', 'tools/topbuild index.t2t')
env.MAKO('index.t2t', MAKOFLAGS='-t base')


# About me:
env.SConscript('aboutme/SConscript')


# Articles:
env.SConscript('articles/SConscript')


# Debian:
env.SConscript('debian/SConscript')


# Feeds:
env.SConscript('feeds/SConscript')


