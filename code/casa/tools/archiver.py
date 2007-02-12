def make_archive(target, source, env):
    from distutils.archive_util import make_tarball
    make_tarball(str(target[0]), str(source[0]), compress='bzip2')
    return 0

def make_archive_str(target, source, env):
    return "Creating archive %s.tar.bz2 from %s" % (target[0], source[0])

def generate(env):
    archiver_builder = env.Builder(
        action = env.Action(make_archive,make_archive_str),
        source_factory=env.fs.Entry,
        target_factory=env.fs.Entry,
        multi=0
    )

    env.Append(BUILDERS = {
        'Archiver': archiver_builder
    })

    env.AppendUnique(
        ARCHIVER = 'archiver',
    )

def exists(env):
   return True
