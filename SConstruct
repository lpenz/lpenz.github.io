
import os
import glob
import re

env = Environment(ENV = os.environ)
env.Export('env')

top = os.path.abspath(os.path.curdir)
env.Export('top')


# Renderer:
env.Command('tools/render', 'tools/render.hs', 'ghc --make -o $TARGET $SOURCE')
env.SideEffect('tools/render.o',  'tools/render')
env.SideEffect('tools/render.hi', 'tools/render')
renderer = os.path.join('tools', 'render')
env.Export('renderer')


def render(f):
    t = os.path.splitext(f)[0] + '.html'
    env.Command(t, f, '%s ${SOURCE} ${TARGET}' % renderer)
    env.Depends(t, os.path.join(top, renderer))
    env.Depends(t, os.path.join(top, 'layouts/default.st'))
env.Export('render')


# Top texts:
for f in glob.glob('*.t2t'):
    render(f)


# Articles:
render('articles/debianization-with-git.t2t')
env.SConscript('articles/df0pred-1/SConscript')


# Debian:
env.SConscript('debian/SConscript')


