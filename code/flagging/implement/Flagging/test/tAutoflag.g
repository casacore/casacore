print ">>>"
include 'autoflag.g'
af:=autoflag('tAutoflag_tmp.MS2')
shell("sleep 5")  # give all servers a chance to log startup messages
print "<<<"
af.settimemed()
af.setfreqmed()
af.setsprej(chan=[5,30])
af.setuvbin(plotchan=100,thr=.04)
af.setselect(ant="RT6",fq=[806,808])
af.setselect(clip=[expr="ABS I",max=10])
af.run(trial=T,reset=T,plotscr=F,devfile='tAutoflag_tmp_report.ps/ps',assaying=T)
exit
