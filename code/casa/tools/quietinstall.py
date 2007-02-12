import SCons

def no_output(target, source, env):
    return ""

def generate(env):

    def QInstall(dir, source):
        dnodes = env.arg2nodes(dir, env.fs.Dir)
        sources = env.arg2nodes(source, env.fs.File)
        tgt = []
        for dnode in dnodes:
            for src in sources:
                target = env.fs.File(src.name, dnode)
                tgt.extend(env.QuietInstaller(target, src))
        return tgt
    env.QInstall = QInstall
    
    def QInstallAs(target, source):
        result = []
        sources = env.arg2nodes(source, env.fs.File)
        targets = env.arg2nodes(target, env.fs.File)
        for src, tgt in map(lambda x, y: (x, y), sources, targets):
            result.extend(env.QuietInstaller(tgt, src))

    env.QInstallAs = QInstallAs
    
    quietinstaller_builder = env.Builder(
        action = env.Action(SCons.Environment.installFunc, no_output),
        multi=1
    )

    env.Append(BUILDERS = {
        'QuietInstaller': quietinstaller_builder
    })

def exists(env):
   return True
