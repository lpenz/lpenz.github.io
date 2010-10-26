
import os
import glob
import re

env = Environment(ENV = os.environ
    , TOP = os.path.abspath(os.path.curdir)
    , toolpath = ['tools']
    , tools = ['haskell', 'txt2tags', 'render'])
env.Export('env')


# Renderer:
env.Append(HASKELLPATH='tools')
env.HASKELL('tools/RenderLib.hs')
env.HASKELL('tools/render.hs')


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


