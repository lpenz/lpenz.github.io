
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
render = os.path.join(top, 'tools', 'render')
env.Export('render')


# Top texts:
for f in glob.glob('*.t2t'):
    (bn, be) = os.path.splitext(f)
    t = bn + '.html'
    env.Command(t, f, 'tools/render %s 0 $SOURCE $TARGET' % top)
    env.Depends(t, 'tools/render')
    env.Depends(t, 'layouts/default.st')


env.SConscript('articles/df0pred-1/SConscript')


#def proc(path):
#    if os.path.isdir(path):
#        for p in glob.glob(os.path.join(path, '*')):
#            if os.path.basename(p)[0] == '_':
#                continue
#            proc(p)
#        return
#    env.Command(os.path.relpath(path, '_srcs'), path, 'cp $SOURCE $TARGET')
#
#proc('_srcs')

#for p in glob.glob('posts/*'):
#    b = os.path.basename(p)
#    (bn, be) = os.path.splitext(b)
#    if be == '.t2t':
#        t = os.path.join('_posts', bn + '.textile')
#        env.Command(t, p, 'tools/yamlt2t $SOURCE $TARGET')
#    else:
#        t = os.path.join('_posts', b)
#        env.Command(t, p, 'cp $SOURCE $TARGET')
#
#env.Command('aboutme.textile', 'aboutme.t2t', 'tools/yamlt2t $SOURCE $TARGET')
#
#env.SConscript('df0pred-1/SConscript')
#env.Command('_posts/2010-08-15-df0pred-1.textile', 'df0pred-1/main.textile', 'cp $SOURCE $TARGET')


