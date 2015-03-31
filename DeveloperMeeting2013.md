# Introduction #

The meeting is designed to bring together the major contributors to casacore.

# Attendees #

Ger van Diepen,
Jim Jacobs,
Malte Marquarding,
Dirk Petry,
Wim Brouw (partly)

# Venue & Time #

ASTRON
25-26 April 2013

# Agenda item suggestions #

  * (re)integration of casacore into casapy (and vice-versa)
  * Single repository or keeps NRAO its own?
  * Also brief discussion of pyrap?
  * Support for multithreading; here's an interesting related article call [Just Enough Thread Safety](http://www.drdobbs.com/just-enough-thread-safety/184401851).  We might want to expand the library to not only work with multithreaded code but to actually support it (e.g., add thread objects, process objects, interprocess communication, synchronization, etc.); although it could be that users would prefer to use an existing concurrency package.
  * Are there regions of casacore that should be migrated out (e.g., ImageAnalysis, etc.)?
  * Use of external packages (e.g., boost, gsl, etc.)
  * Platforms and compilers supported; C++11?
  * Current and future user base.
  * Module "maintainers"; depending on the amount of work that is likely to require, it might require approval of their host organization.
  * Establish procedures for the orderly modification of casacore.
  * Review coding, testing standards, and continuous integration
  * Review documentation
  * Update file headers (at least remove AIPS++)
  * Replace Map, List, etc. by STL counterparts
  * High-level plans for the future
  * Release management
  * Virtual Observatory: The NRAO going to build some support to make it easy to get data from the Virtual Observatory.  If there's interest, that could be put into casacore for others to use.
  * Create a small steering committee (maybe 3 people?) to keep track of the general direction of the package as well as serve as a place to handle problems that cannot be resolved between a developer and the module supervisor.  I hope that this feature is used infrequently, but it provides a way to avoid dragging in higher-level management whenever a disagreement occurs.
  * Licensing review/repos clean out. We had request re debian packaging which needs these resolved.
  * Code base location/management (optional discussion item). For a shared project svn can be cumbersome and googlecode is a fairly barebones system. Consider thinking going to e.g. bitbucket/github. Drawback - git as a new system to learn for developers
  * Big data: does casacore need to change to support "big" data?
  * measuresdata collection mechanisms.