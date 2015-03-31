# Introduction #

It is possible to define runtime variables (e.g. the path of the Measures data files) by means of so-called casarc (aipsrc) resource files, which resemble a file like `.XDefaults`.

# Details #

Class Aipsrc searches variables in the resource files until a match is found.
The resource files to be looked at can be defined in the environment
variable CASARCFILES. If undefined, the resource files searched are (in the
following order):
  * ~/.casarc
  * ~/.aipsrc
  * $AIPSROOT/.aipsrc
  * $AIPSHOST/aipsrc
  * $AIPSSITE/aipsrc
  * $AIPSARCH/aipsrc
The aipsrc files are still present to support the old **aips++** way used by  [casapy](http://casa.nrao.edu).

The variables in these files are defined as a **keyword: value** pair per line, where the keyword can be a dot-separated name. Each part of the name can be wildcarded using an asterisk.

The Measures module is the greatest user of these variables, in particular to know where the various tables it needs are located.
A typical $HOME/.casarc file could look like:

```
measures.directory: $HOME/measures_data
```

> or explicitly for each table

```
measures.DE200.directory: $HOME/measures_data/ephemerides
measures.DE405.directory: $HOME/measures_data/ephemerides
measures.line.directory: $HOME/measures_data/ephemerides
measures.sources.directory: $HOME/measures_data/ephemerides
measures.comet.directory: $HOME/measures_data/ephemerides
measures.ierseop97.directory: $HOME/measures_data/geodetic
measures.ierspredict.directory: $HOME/measures_data/geodetic
measures.tai_utc.directory: $HOME/measures_data/geodetic
measures.igrf.directory: $HOME/measures_data/geodetic
measures.observatory.directory: $HOME/measures_data/geodetic
```