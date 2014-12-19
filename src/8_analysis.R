

### SIMULATE MODEL  
# fix this query for clipped/uclipped
dex<- sqlFetch(com, "Adult Counts (Dexter dam)")
dex[dex==-99]<- 0
dex$clip<- dex$FEM_TOTAL-dex$FEM_NON_CLIP + 
	dex$MALE_TOTAL-dex$MALE_NON_CLIP  +
	dex$JACK_TOTAL-dex$JACK_NON_CLIP 
dex$uclip<- dex$FEM_NON_CLIP+dex$MALE_NON_CLIP+dex$JACK_NON_CLIP 
dex$year<- dex$YR
dex$date<- dex$DATE
dex<- dex[order(dex$year, dex$date),]

yr<- 2011
dd_index<- yr-2001 # only 2002 to 2012 for water temps

dex<-subset(dex,year==yr)
dex$doy<- as.numeric(format(dex$date,"%j"))

dat<- aggregate(cbind(clip,uclip)~doy,dex,sum)
dat$prev_tend<- c(min(dat$doy)-7, dat$doy[-nrow(dat)])# assumes trap opened 7 days before
dat$clip<- rpois(nrow(dat),20)
clip<- cbind(origin="clip",as.data.frame(lapply(dat,function(x) rep(x,dat$clip))))
if(sum(dat$uclip>0)) {
	uclip<- cbind(origin="uclip",as.data.frame(lapply(dat,function(x) rep(x,dat$uclip))))
	dat<-rbind(clip, uclip)
	} else{dat<- clip}
dat$tmp<- 1

# ASSIGN DURATION IN MS, TRIB AND TAILRACE
dat$ms_duration<-rgamma(nrow(dat),shape=resTimes[resTimes$Segment=="Mainstem (tag to WMF)",]$Shape, 
	rate=resTimes[resTimes$Segment=="Mainstem (tag to WMF)",]$Rate) 
dat$trib_dur<- rgamma(nrow(dat),shape=resTimes[resTimes$Segment=="Tributary (WMF to DXT)",]$Shape, 
	rate=resTimes[resTimes$Segment=="Tributary (WMF to DXT)",]$Rate) 
dat$tail_dur<-rgamma(nrow(dat),shape=resTimes[resTimes$Segment=="Mainstem (tag to WMF)",]$Shape, 
	rate=resTimes[resTimes$Segment=="Mainstem (tag to WMF)",]$Rate) 
dat$tot_dur<-dat$ms_dur + dat$trib_dur + dat$tail_dur

# STOCHASTICALLY ASSIGN DAY FISH ENTERED THE TRAP
dat$doy_tend<- dat$doy
dat$doy_cap<- sapply(1:nrow(dat),function(x) sample(dat$prev_tend[x]:dat$doy_tend[x],1,replace=TRUE))
dat$tail_doy_entry<- floor(dat$doy_cap-dat$tail_dur)
dat$trib_doy_entry<- floor(dat$tail_doy_entry-dat$trib_dur)
dat$ms_doy_entry<- floor(dat$trib_doy_entry-dat$ms_duration)

# LOOKUP DEGREE DAYS FOR EACH SEGMENT
ms<- dd_ms[,,dd_index]
tr<- dd_trib[,,dd_index]
tailr<- dd_trib[,,dd_index]
dat$ms_dd<- ms[cbind(dat$ms_doy_entry,dat$trib_doy_entry)]
dat$trib_dd<- tr[cbind(dat$trib_doy_entry,dat$tail_doy_entry)]
dat$tail_dd<- tailr[cbind(dat$tail_doy_entry,dat$doy_cap)]
dat$trap_dd<- tailr[cbind(dat$doy_cap,dat$doy_tend)]




# day 244 for holding (sept 1)

dd_out_mf
dd_out_nfmf









