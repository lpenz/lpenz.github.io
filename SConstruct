
import os
import glob
import re

env = Environment(ENV = os.environ)

for f in glob.glob('_srcs/top/*'):
    env.Command(os.path.basename(f), f, 'cp $SOURCE $TARGET')

env.Command('_srcs/tools/render', '_srcs/tools/render.hs', 'ghc --make -o $TARGET $SOURCE')

for f in glob.glob('_srcs/pages/*.t2t'):
    b = os.path.basename(f)
    env.Command(b, f, '_srcs/tools/render $SOURCE $TARGET')
    env.Depends(b, '_srcs/tools/render')

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


