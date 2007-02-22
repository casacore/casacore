"""engine.SCons.Tool.gfortan

Tool-specific initialization for the generic Posix f95 Fortran compiler.

There normally shouldn't be any need to import this module directly.
It will usually be imported through the generic SCons.Tool.Tool()
selection method.

"""

#
# Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007 The SCons Foundation
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
# KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
# WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#

__revision__ = "/home/scons/scons/branch.0/branch.96/baseline/src/engine/SCons/Tool/f95.py 0.96.94.D001 2007/01/07 20:56:14 knight"

import SCons.Defaults
import SCons.Tool
import SCons.Util
from SCons.Tool import fortran

compilers = ['gfortran']

#
GFORTRANSuffixes = ['.f']
GFORTRANPPSuffixes = []
if SCons.Util.case_sensitive_suffixes('.f', '.F'):
    GFORTRANPPSuffixes.append('.f')
else:
    GFORTRANSuffixes.append('.F')

#
GFORTRANScan = SCons.Scanner.Fortran.FortranScan("GFORTRANPATH")

for suffix in GFORTRANSuffixes + GFORTRANPPSuffixes:
    SCons.Tool.SourceFileScanner.add_scanner(suffix, GFORTRANScan)

#
fVLG = fortran.VariableListGenerator

GFORTRANGenerator = fVLG('GFORTRAN', 'FORTRAN', '_FORTRAND')
GFORTRANFlagsGenerator = fVLG('GFORTRANFLAGS', 'FORTRANFLAGS')
GFORTRANCommandGenerator = fVLG('GFORTRANCOM', 'FORTRANCOM', '_GFORTRANCOMD')
GFORTRANCommandStrGenerator = fVLG('GFORTRANCOMSTR', 'FORTRANCOMSTR', '_GFORTRANCOMSTRD')
GFORTRANPPCommandGenerator = fVLG('GFORTRANPPCOM', 'FORTRANPPCOM', '_GFORTRANPPCOMD')
GFORTRANPPCommandStrGenerator = fVLG('GFORTRANPPCOMSTR', 'FORTRANPPCOMSTR', '_GFORTRANPPCOMSTRD')
ShGFORTRANGenerator = fVLG('SHGFORTRAN', 'SHFORTRAN', 'GFORTRAN', 'FORTRAN', '_FORTRAND')
ShGFORTRANFlagsGenerator = fVLG('SHGFORTRANFLAGS', 'SHFORTRANFLAGS')
ShGFORTRANCommandGenerator = fVLG('SHGFORTRANCOM', 'SHFORTRANCOM', '_SHGFORTRANCOMD')
ShGFORTRANCommandStrGenerator = fVLG('SHGFORTRANCOMSTR', 'SHFORTRANCOMSTR', '_SHGFORTRANCOMSTRD')
ShGFORTRANPPCommandGenerator = fVLG('SHGFORTRANPPCOM', 'SHFORTRANPPCOM', '_SHGFORTRANPPCOMD')
ShGFORTRANPPCommandStrGenerator = fVLG('SHGFORTRANPPCOMSTR', 'SHFORTRANPPCOMSTR', '_SHGFORTRANPPCOMSTRD')

del fVLG

#
GFORTRANAction = SCons.Action.Action('$_GFORTRANCOMG ', '$_GFORTRANCOMSTRG')
GFORTRANPPAction = SCons.Action.Action('$_GFORTRANPPCOMG ', '$_GFORTRANPPCOMSTRG')
ShGFORTRANAction = SCons.Action.Action('$_SHGFORTRANCOMG ', '$_SHGFORTRANCOMSTRG')
ShGFORTRANPPAction = SCons.Action.Action('$_SHGFORTRANPPCOMG ', '$_SHGFORTRANPPCOMSTRG')

def add_to_env(env):
    """Add Builders and construction variables for gfortran to an Environment."""
    env.AppendUnique(FORTRANSUFFIXES = GFORTRANSuffixes + GFORTRANPPSuffixes)

    static_obj, shared_obj = SCons.Tool.createObjBuilders(env)

    for suffix in GFORTRANSuffixes:
        static_obj.add_action(suffix, GFORTRANAction)
        shared_obj.add_action(suffix, ShGFORTRANAction)
        static_obj.add_emitter(suffix, fortran.FortranEmitter)
        shared_obj.add_emitter(suffix, fortran.ShFortranEmitter)

    for suffix in GFORTRANPPSuffixes:
        static_obj.add_action(suffix, GFORTRANPPAction)
        shared_obj.add_action(suffix, ShGFORTRANPPAction)
        static_obj.add_emitter(suffix, fortran.FortranEmitter)
        shared_obj.add_emitter(suffix, fortran.ShFortranEmitter)

    env['_GFORTRANG']           = GFORTRANGenerator
    env['_GFORTRANFLAGSG']      = GFORTRANFlagsGenerator
    env['_GFORTRANCOMG']        = GFORTRANCommandGenerator
    env['_GFORTRANCOMSTRG']     = GFORTRANCommandStrGenerator
    env['_GFORTRANPPCOMG']      = GFORTRANPPCommandGenerator
    env['_GFORTRANPPCOMSTRG']   = GFORTRANPPCommandStrGenerator

    env['_SHGFORTRANG']         = ShGFORTRANGenerator
    env['_SHGFORTRANFLAGSG']    = ShGFORTRANFlagsGenerator
    env['_SHGFORTRANCOMG']      = ShGFORTRANCommandGenerator
    env['_SHGFORTRANCOMSTRG']   = ShGFORTRANCommandStrGenerator
    env['_SHGFORTRANPPCOMG']    = ShGFORTRANPPCommandGenerator
    env['_SHGFORTRANPPCOMSTRG'] = ShGFORTRANPPCommandStrGenerator

    env['_GFORTRANINCFLAGS'] = '$( ${_concat(INCPREFIX, GFORTRANPATH, INCSUFFIX, __env__, RDirs, TARGET, SOURCE)} $)'

    env['_GFORTRANCOMD']     = '$_GFORTRANG -o $TARGET -c $_GFORTRANFLAGSG $_GFORTRANINCFLAGS $_FORTRANMODFLAG $SOURCES'
    env['_GFORTRANPPCOMD']   = '$_GFORTRANG -o $TARGET -c $_GFORTRANFLAGSG $CPPFLAGS $_CPPDEFFLAGS $_GFORTRANINCFLAGS $_FORTRANMODFLAG $SOURCES'
    env['_SHGFORTRANCOMD']   = '$_SHGFORTRANG -o $TARGET -c $_SHGFORTRANFLAGSG $_GFORTRANINCFLAGS $_FORTRANMODFLAG $SOURCES'
    env['_SHGFORTRANPPCOMD'] = '$_SHGFORTRANG -o $TARGET -c $_SHGFORTRANFLAGSG $CPPFLAGS $_CPPDEFFLAGS $_GFORTRANINCFLAGS $_FORTRANMODFLAG $SOURCES'

def generate(env):
    fortran.add_to_env(env)

    from SCons.Tool import f77
    f77.add_to_env(env)

    from SCons.Tool import f90
    f90.add_to_env(env)

    add_to_env(env)

    env['_FORTRAND']        = env.Detect(compilers) or 'gfortran'

def exists(env):
    return env.Detect(compilers)
