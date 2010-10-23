
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
env.HASKELL('indexbuild.hs')
env.Command('index.t2t', 'indexbuild', './indexbuild $TARGET')
env.RENDER('index.t2t')


# About me:
env.SConscript('aboutme/SConscript')


# Articles:
env.SConscript('articles/SConscript')


# Debian:
env.SConscript('debian/SConscript')


