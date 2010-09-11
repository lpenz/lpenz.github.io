
import os
import glob
import re

env = Environment(ENV = os.environ
    , toolpath = ['tools']
    , tools = ['haskell', 'txt2tags', 'render'])
env.Export('env')

top = os.path.abspath(os.path.curdir)
env.Export('top')
env['TOP'] = top


# Renderer:
env.Append(HASKELLPATH='tools')
env.HASKELL('tools/RenderLib.hs')
env.HASKELL('tools/render.hs')


def render(f):
    t = os.path.splitext(f)[0] + '.html'
    env.RENDER(t, ['$TOP/layouts/default.st', f])
env.Export('render')


# Top texts:
for f in glob.glob('*.t2t'):
    render(f)


# Articles:
env.SConscript('articles/SConscript')


# Debian:
env.SConscript('debian/SConscript')


