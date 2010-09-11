
import os
import glob
import re

env = Environment(ENV = os.environ)
env.Export('env')

top = os.path.abspath(os.path.curdir)
env.Export('top')


# Renderer:
env.Command('tools/render', 'tools/render.hs', 'ghc --make -itools -o $TARGET $SOURCE')
env.SideEffect('tools/render.o',  'tools/render')
env.SideEffect('tools/render.hi', 'tools/render')
env.Depends('tools/render', 'tools/RendererLib.hs')
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


