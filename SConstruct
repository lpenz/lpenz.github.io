
import os
import glob
import re

env = Environment(ENV = os.environ
    , TOP = os.path.abspath(os.path.curdir)
    , toolpath = ['tools/scons']
    , tools = ['haskell', 'render'])
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


# Haskell config:
env.Append(HASKELLPATH='tools')
env.HASKELL('tools/RenderLib.hs')


# Main page:
env.HASKELL('tools/topbuild.hs')
env.Command('index.t2t', 'tools/topbuild', 'tools/topbuild index.t2t whatsnew0.xml')
env.SideEffect('whatsnew0.xml', 'index.t2t')
env.Command('whatsnew.xml', 'whatsnew0.xml', 'xmllint --format --output $TARGET $SOURCE')
env.RENDER('index.t2t')

# Article feed:
articles = []
for i in glob.glob('articles/*'):
    if os.path.isdir(i):
        articles.append(os.path.join(i, 'index.t2t'))
env.HASKELL('tools/articlefeed.hs')
env.Command('articles0.xml', articles, 'tools/articlefeed $TARGET $SOURCES')
env.Depends('articles0.xml', 'tools/articlefeed')
env.Command('articles.xml', 'articles0.xml', 'xmllint --format --output $TARGET $SOURCE')


# About me:
env.SConscript('aboutme/SConscript')


# Articles:
env.SConscript('articles/SConscript')


# Debian:
env.SConscript('debian/SConscript')


