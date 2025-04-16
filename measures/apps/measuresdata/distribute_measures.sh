#!/bin/bash -x
#
cd /data/IERS/daily
curdate=`date +%Y%m%d-%H%M%S`
ftp_name=WSRT_Measures_${curdate}.ztar
# Update the date of tai-utc.dat to prevent automatic deletion after 6 months (default behaviour of ASTRON FTP server)
ssh iers@ftp.astron.nl touch /ftp/pub/anonymous/outgoing/Measures/tai-utc.dat || echo $curdate ": Failed to update /ftp/pub/anonymous/outgoing/Measures/tai-utc.dat"
# Copy latest IERS tables to ftp
push_to_ftp()
{
  scp iers.tar.gz iers@ftp.astron.nl:/ftp/pub/anonymous/outgoing/Measures/${ftp_name} && \
  scp iers.tar.gz iers@ftp.astron.nl:/ftp/pub/anonymous/outgoing/Measures/WSRT_Measures.ztar
  return $?
}

# Copy latest IERS tables to webpages.astron.nl
push_to_web()
{
  scp iers.tar.gz iers@webpages.astron.nl:iers/${ftp_name} && \
  scp iers.tar.gz iers@webpages.astron.nl:iers/WSRT_Measures.ztar
  return $?
}

if push_to_ftp; then
  echo $curdate": copied to ftp.astron.nl:/ftp/pub/anonymous/outgoing/Measures"
else
  echo $curdate": Failed to copy to ftp.astron.nl:/ftp/pub/anonymous/outgoing/Measures"
fi

if push_to_web; then
  echo $curdate": copied to webpages.astron.nl:/iers"
else
  echo $curdate": Failed to copy to webpages.astron.nl:/iers"
fi
