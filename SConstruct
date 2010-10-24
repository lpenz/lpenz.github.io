
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
env.Command('index.t2t', 'tools/topbuild', 'tools/topbuild index.t2t feed.xml')
env.SideEffect('feed.xml', 'index.t2t')
env.RENDER('index.t2t')


# About me:
env.SConscript('aboutme/SConscript')


# Articles:
env.SConscript('articles/SConscript')


# Debian:
env.SConscript('debian/SConscript')


