
# [1] SIMULATE TEH NUMBER OF FISH ESCAPING WILLAMETTE FALLS
#     RETURNS THE NUMBER OF CLIPPED AND UCLIPPED FISH ESCAPING 
#     WILLAMETTE FALLS
sim_out<- list(wf_escapement=sim_wf_escapement(nreps=1))

# [2] 
sim_out$trapped<- sim_trapped(wf_escapement = sum(sim_out$wf_escapement$count))

# [3] SIMULATE THE NUMBER OF FISH TRAPPED DAILY AT 
#     MAJOR UWR LOCATIONS [N. SANTIAM, S. SANTIAM, FALL CREE, DEXTER]
#     RETURNS:  AT DATA.FRAME WITH FIELDS
#		1) DAY OF YEAR 
#		2) LOCATION
#		3) COUNT OF CLIPPED FISH
#     	4) COUNTS OF UNCLIPPED FISH
sim_out$daily<- sim_daily_trap(trapped=sim_out$trapped)

tend_day<-seq(0,365,7)
sim_out$daily$tend<-cut(sim_out$daily$doy,tend_day,labels=tend_day[-1])

trapped<-aggregate(cbind(counts_clipped ,counts_unclipped)~location+tend,sim_out$daily,sum)

