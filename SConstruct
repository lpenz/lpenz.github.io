
import os
import glob
import re

env = Environment(ENV = os.environ
    , toolpath = ['tools']
    , tools = ['haskell', 'txt2tags'])
env.Export('env')

top = os.path.abspath(os.path.curdir)
env.Export('top')


# Renderer:
env.Append(HASKELLPATH='tools')
env.HASKELL('tools/RenderLib.hs')
env.HASKELL('tools/render.hs')
renderer = os.path.join('tools', 'render')
env.Export('renderer')


def render(f):
    t = os.path.splitext(f)[0] + '.html'
    env.Command(t, f, 'tools/render ${SOURCE} ${TARGET}')
    env.Depends(t, os.path.join(top, renderer))
    env.Depends(t, os.path.join(top, 'layouts/default.st'))
env.Export('render')


# Top texts:
for f in glob.glob('*.t2t'):
    render(f)


# Articles:
env.SConscript('articles/SConscript')


# Debian:
env.SConscript('debian/SConscript')


