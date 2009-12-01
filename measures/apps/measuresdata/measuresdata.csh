#!/bin/tcsh
# 
# measuresdata.csh
# Must be executable. Call it with all defaults or arguments for measuresdata
# It is a test script to enable writing python (or other) versions
#
# Set connection type (maybe should be ftp, not pftp in some cases)
#
set pftp = pftp
#
# get possible arguments
set mdarg = ''
while ( $# > 0)
  set mdarg = "$mdarg $1"
  shift
end
#
# Call measuresdata
#
while ( 1 == 1 )
  echo Calling measuresdata $mdarg
  if ( ! { measuresdata $mdarg } ) then
    echo Severe error calling measuresdata with $mdarg
    exit 1
  endif
#
# Check and analyse call-back
#
  if ( ! -e measuresdata.rc) then
    echo Severe error: no measuresdata.rc file returned
    exit 1
  endif
  set mdata = (`cat measuresdata.rc`)
  if ( $#mdata < 2 ) then
    echo Severe program error: measuresdata.rc has not enough fields
    exit 1
  endif

  echo Called by measuresdata $mdata

  if ( "$mdata[1]" != "status:" ) then
    echo Severe: no status given in measuresdata.rc
    exit 1
  endif
  shift mdata
  if ( "$mdata[1]" == "end" ) then
    echo measuresdata.csh finished normally
    exit 0
  endif
  if ( "$mdata[1]" != "cont" ) then
    echo Severe: unknown statuss given in measuresdata.rc
    exit 1
  endif
  shift mdata

  set arg = ""
  unset ftp
  unset http

  while ( $#mdata > 1 ) 
    if ( "$mdata[1]" == "ftp:" ) then
      set ftp = $mdata[2]
    else if ( "$mdata[1]" == "http:" ) then
      set http = $mdata[2]
    else if ( "$mdata[1]" == "data:" ) then
      set data = $mdata[2]
      if ( "$data" != "ascii" ) then
        echo Severe: only ascii data protocol supported
        exit 1
      endif
    else if ( "$mdata[1]" == "dir:" ) then
      set dir = $mdata[2]
    else if ( "$mdata[1]" == "file:" ) then
      set file = $mdata[2]
    else if ( "$mdata[1]" == "arg:" ) then
      shift mdata
      while ( $#mdata > 0 )
        set arg = "$arg $mdata[1]"
        shift mdata
      end
    endif
    if ( $#mdata > 1 ) then
      shift mdata
      shift mdata
    endif
  end
  if ( ! ( $?ftp || $?http ) || ! $?dir || ! $?file || ! $?arg ) then
    echo Severe: missing ftp/http, dir, file or arg data
    exit 1
  endif
#
# Obtain ftp/http
#
  if ($?http) then
    lynx -dump -dont_wrap_pre 'http://'$http'/'$dir'/'$file >! $file
  else
    $pftp -n -v -i $ftp <<_EOD_
    quote user ftp
    quote pass brouw@astron.nl
    ascii
    cd $dir
    get $file
    quit
_EOD_
  endif
#
# Call back
#
  set mdarg = "$arg"
end
#
exit 0
#
