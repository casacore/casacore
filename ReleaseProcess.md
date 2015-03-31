# Introduction #
  1. svn tag trunk to casacore-x.y.z
  1. amend version in doxygen file
  1. verify build at ASTRON, CASS (and NRAO) by adding entry to wikie page ReleaseVersionXY (e.g. ReleaseVersion14)
  1. get agreement to tag new release
  1. svn export casacore-x.y.z and tar jcf as casacore-x.y.z.tar.bz2 and add as featured new upload. Deprecate previous upload
  1. update BuildInstructions and/or CmakeInstructions
  1. build doxygen documentation from tag
  1. build notes if necessary.