
import os
import glob
import re

env = Environment(ENV = os.environ)

for p in glob.glob('posts/*'):
    b = os.path.basename(p)
    (bn, be) = os.path.splitext(b)
    if be == '.t2t':
        t = os.path.join('_posts', bn + '.textile')
        env.Command(t, p, 'tools/yamlt2t $SOURCE $TARGET')
    else:
        t = os.path.join('_posts', b)
        env.Command(t, p, 'cp $SOURCE $TARGET')

env.Command('aboutme.textile', 'aboutme.t2t', 'tools/yamlt2t $SOURCE $TARGET')


