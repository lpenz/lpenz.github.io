
import os
import glob
import re

env = Environment(ENV = os.environ)

debiantaget = 'debian/db/packages.db'

def debdo():
    allchanges = []
    debre = re.compile('debian/incoming/([^_]+)_([0-9.]+)-([0-9]+)_(i386|all).deb')
    ipath = os.path.join('debian', 'incoming')
    for f in glob.glob(os.path.join(ipath, '*.deb')):
        m = debre.match(f)
        if not m:
            continue
        full = m.groups()
        pack, ov, dv, arch = full
        firstletter = pack[0]
        tpath = os.path.join('debian', 'pool', 'main', firstletter, pack)
        changes = (os.path.join(ipath, '%s_%s-%s_i386.changes' % (pack, ov, dv)))
        debsources = [
            os.path.join(ipath, '%s_%s-%s_%s.deb' % (pack, ov, dv, arch)),
            changes,
            os.path.join(ipath, '%s_%s-%s_i386.build' % (pack, ov, dv)),
            os.path.join(ipath, '%s_%s-%s.diff.gz' % (pack, ov, dv)),
            os.path.join(ipath, '%s_%s-%s.dsc' % (pack, ov, dv)),
            os.path.join(ipath, '%s_%s.orig.tar.gz' % (pack, ov)),
            os.path.join(tpath, '%s_%s.orig.tar.gz' % (pack, ov)),
            ]
        for s in debsources:
            env.Depends(debiantaget, debsources)
        for s in [
            '%s_%s-%s_%s.deb' % full,
            '%s_%s-%s.diff.gz' % (pack, ov, dv),
            '%s_%s-%s.dsc' % (pack, ov, dv),
            ]:
            env.SideEffect(os.path.join(tpath, s), debiantaget)
        for s in [
            'checksums.db',
            'contents.cache.db',
            'references.db',
            'release.caches.db',
            'version',
            ]:
            env.SideEffect(os.path.join('debian', 'db', s), debiantaget)
        for s in ['Release.gpg', 'Release', 'InRelease']:
            env.SideEffect(os.path.join('debian', 'dists', 'lpenz', s), debiantaget)
        for d in ['main', 'contrib', 'non-free']:
            for j in [
                os.path.join('source', 'Release'),
                os.path.join('source', 'Sources.gz'),
                os.path.join('binary-i386', 'Packages'),
                os.path.join('binary-i386', 'Release'),
                os.path.join('binary-i386', 'Packages.gz'),
                ]:
                env.SideEffect(os.path.join('debian', 'dists', 'lpenz', d, j), debiantaget)
        env.Command(os.path.join(tpath, '%s_%s.orig.tar.gz' % (pack, ov)),
            os.path.join(ipath, '%s_%s.orig.tar.gz' % (pack, ov)),
            'cp $SOURCE $TARGET')
        allchanges.append(changes)
    return allchanges


allchanges = debdo()
env.Command('debian/index.t2t', ['debian/lpenz.list'] + allchanges, 'debian/indexbuild $TARGET $SOURCES')
env.Command('debian/index.html', 'debian/index.t2t', 'txt2tags -t html -i $SOURCE -o $TARGET')
env.Command(debiantaget, allchanges, 'debian/repbuild $SOURCES')

env.Command('index.html', 'index.t2t', 'txt2tags -t html -i $SOURCE -o $TARGET')
env.Command('aboutme.html', 'aboutme.t2t', 'txt2tags -t html -i $SOURCE -o $TARGET')


