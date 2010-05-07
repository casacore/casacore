""" installer

This module defines a minimal installer for scons build scripts.  It is aimed
at *nix like systems, but I guess it could easily be adapted to other ones as
well.
"""
import sys
import fnmatch, os, os.path
import platform
import SCons.Defaults
from SCons.Script import AddOption

sys.path.insert(0, ".")
import lib64linux

ARCHLIBDIR=lib64linux.get_libdir()

PREFIX = "prefix"
EPREFIX = "eprefix"
BINDIR = "bindir"
LIBDIR = "libdir"
INCLUDEDIR = "includedir"
SHAREDIR = "sharedir"

def generate(env):
    class Installer:
        """ A basic installer. """
        def __init__( self):
            """ Initialize the installer.
            
            @param configuration A dictionary containing the configuration.
            @param env The installation environment.
            """
            self._prefix = os.path.abspath(os.path.expanduser(os.path.expandvars(env.get(PREFIX))))

            self._bindir = env.get(BINDIR) \
                           or os.path.join(self._prefix, "bin")
            self._libdir = env.get( LIBDIR ) \
                           or os.path.join(self._prefix, ARCHLIBDIR)
            self._includedir = env.get( INCLUDEDIR ) \
                           or os.path.join(self._prefix, "include")
            self._sharedir = env.get( SHAREDIR ) \
                           or os.path.join(self._prefix, "share")

            env.Alias( "install", env.Dir(self._bindir) )
            env.Alias( "install", env.Dir(self._libdir) )
            env.Alias( "install", env.Dir(self._includedir ) )
            env.Alias( "install", env.Dir(self._sharedir ) )
            self._env = env
	
        def Add( self, destdir, name, basedir="", perm=None ):
            destination = os.path.join( destdir, basedir )
            obj = self._env.Install( destination, name )
            for i in obj:
                if perm:
                    self._env.AddPostAction(i,
                                            SCons.Defaults.Chmod(str(i),
                                                                 perm))
	
        def AddProgram( self, program ):
	        """ Install a program.
                
	        @param program The program to install.
	        """
	        self.Add( self._bindir, program, perm=0755 )
                
        def AddLibrary( self, library ):
	        """ Install a library.
	
	        @param library the library to install.
	        """
	        self.Add( self._libdir, library )

        def AddShare( self, header, basedir="" ):
	        self.Add( self._sharedir, header, basedir )

        def AddShares( self, parent , pattern, basedir="", recursive=False):
	        for entry in os.listdir( parent ):
	            entrypath = os.path.join( parent, entry )
	            if os.path.isfile( entrypath ) and  \
			   fnmatch.fnmatch( entry, pattern ):
	                self.AddShare( entrypath, basedir )
	            elif os.path.isdir( entrypath ) and recursive:
	                self.AddShares( entrypath, pattern,
					 os.path.join( basedir, entry ),
					 recursive )


        def AddHeader( self, header, basedir="" ):
	        self.Add( self._includedir, header, basedir )
	
        def AddHeaders( self, parent, pattern, basedir="", recursive=False, 
                        exclude_tests=True ):
	        """ Installs a set of headers.
	
	        @param parent The parent directory of the headers.
	        @param pattern A pattern to identify the files that
                       are headers.
	        @param basedir The subdirectory in which to install the
                       headers.
	        @param recursive Search recursively for headers.
	        """
                parentlist = os.path.split(parent)
                if ".svn" in parentlist:
                    return
                relpath = parent.replace(str(env.Dir("#")), "")
                if exclude_tests and relpath.find("test") > -1:
                    return
	        for entry in os.listdir( parent ):
	            entrypath = os.path.join( parent, entry )
	            if os.path.isfile( entrypath ) and  \
			   fnmatch.fnmatch( entry, pattern ):
	                self.AddHeader( entrypath, basedir )
	            elif os.path.isdir( entrypath ) and recursive:
	                self.AddHeaders( entrypath, pattern,
					 os.path.join( basedir, entry ),
					 recursive )
    env.Installer = Installer

def exists(env):
    return True
