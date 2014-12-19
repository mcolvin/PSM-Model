
sim_wf_escapement<- function(nreps=1,...) 
	{
	out<-round(quantile(wf_escapement$spring_chinook_total, runif(nreps)),0)
	clipped<- rbinom(nreps,out,0.75)
	return(data.frame(type=c("clipped", "unclipped"),
		count=c(as.numeric(clipped),as.numeric(out-clipped))))
	}

sim_trapped<- function(wf_escapement,...)
	{
	# THIS FUNCTION SIMULATES THE NUMBER OF SPRING CHINOOK TRAPPED/PASSED AT LOCATIONS OF INTEREST
	# THOROUGH OUT THE UPPER WILLMETTE RIVER.
	# THE INPUT IS THE ANNUAL COUNT OF SPRING CHINOOK ESCAPING WILLAMETTE FALLS
	minto<- round(exp(6.954+0.0117*wf_escapement/1000+ rnorm(1,0,0.5684)),0)
	foster<- round(exp(7.4611+0.0197*wf_escapement/1000+ rnorm(1,0,0.404)),0)
	mckenzie<- round(exp(7.6563+0.0141*wf_escapement/1000+ rnorm(1,0,0.2171)),0)
	leaburg<- round(exp(6.8826+0.0227*wf_escapement/1000+ rnorm(1,0,0.3111)),0)
	fallcreek<- round(exp(5.7225+0.004*wf_escapement/1000+ rnorm(1,0,1.4639)),0)
	dexter<- round(exp(8.2835+0.0131*wf_escapement /1000+ rnorm(1,0,0.3599)),0)
	out<-data.frame(location=c("Minto Dam","Foster Dam","Leaburg Dam", "Mckenzie Hatchery","Fall Creek Dam", "Dexter Dam"),
		trapped=c(minto, foster,leaburg, mckenzie,fallcreek,dexter))
	out$p_unclipped<-c(rbeta(1,shape1=4.66, shape2=21.63),# MINTO
		rbeta(1,shape1=4.39, shape2=28.86),#FOSTER
		rbeta(1,shape1=4.39, shape2=21.63),# LEABURG ######### NEED TO ESTIMATE
		rbeta(1,shape1=29.54, shape2=1198.30),# MCKENZIE
		rbeta(1,shape1=3.80, shape2=3.00),# FALL CREEK
		rbeta(1,shape1=1.99, shape2=43.66))# DEXTER DAM
	out$trapped_unclipped<- rbinom(nrow(out),out$trapped,out$p_unclipped)
	out$trapped_clipped<- out$trapped-out$trapped_unclipped
	return(out)		
	}
	
sim_daily_trap<- function(trapped,...)
	{
	# MINTO (N. SANTIAM)
	year<- sample(unique(trap_timing[trap_timing$location=="Minto Dam",]$year),1)
	minto<- trap_timing[trap_timing$year==year & trap_timing$location=="Minto Dam",c(4,5)] 
	
	# FOSTER (S. SANTIAM)
	year<- sample(unique(trap_timing[trap_timing$location=="Foster Dam",]$year),1)
	foster<- trap_timing[trap_timing$year==year & trap_timing$location=="Foster Dam",c(4,5)] 
	
	# FALL CREEK
	year<- sample(unique(trap_timing[trap_timing$location=="Fall Creek Dam",]$year),1)
	fc<- trap_timing[trap_timing$year==year & trap_timing$location=="Fall Creek Dam",c(4,5)] 	
	# MCKENZIE
	
	# COUGAR
	
	# DEXTER
	year<- sample(unique(trap_timing[trap_timing$location=="Dexter Dam",]$year),1)
	dexter<- trap_timing[trap_timing$year==year & trap_timing$location=="Dexter Dam",c(4,5)] 
	out<- data.frame(doy=rep(c(1:365),4),
		location=c(rep("Minto Dam",365), rep("Foster Dam",365), rep("Fall Creek Dam",365), rep("Dexter Dam",365)),
		counts_clipped=c(
			rmultinom(1,trapped[trapped$location=="Minto Dam","trapped_clipped"],minto[,2]),
			rmultinom(1,trapped[trapped$location=="Foster Dam","trapped_clipped"],foster[,2]),
			rmultinom(1,trapped[trapped$location=="Fall Creek Dam","trapped_clipped"],fc[,2]),
			rmultinom(1,trapped[trapped$location=="Dexter Dam","trapped_clipped"],dexter[,2])),
		counts_unclipped=c(
			rmultinom(1,trapped[trapped$location=="Minto Dam","trapped_unclipped"],minto[,2]),
			rmultinom(1,trapped[trapped$location=="Foster Dam","trapped_unclipped"],foster[,2]),
			rmultinom(1,trapped[trapped$location=="Fall Creek Dam","trapped_unclipped"],fc[,2]),
			rmultinom(1,trapped[trapped$location=="Dexter Dam","trapped_unclipped"],dexter[,2])))
	return(out)
	}
	
sim_ind_duration<- function(ZZ, ...)
	{
	# N. SANTIAM
		
	# S. SANTIAM
	inds<- subset(sim_out$daily,location=="Foster Dam" & counts>0)
	inds$id<- c(1:nrow(inds))
	inds<- inds[rep(inds$id,inds$count),]
	vals<- resTimes[resTimes$Tributary.population=="South Santiam River",]
	inds$ms<- rgamma(nrow(inds), shape=vals[1,"Shape"],rate=vals[1,"Rate"])
	inds$trib<- rgamma(nrow(inds), shape=vals[2,"Shape"],rate=vals[2,"Rate"])
	inds$tailrace<- rgamma(nrow(inds), shape=vals[3,"Shape"],rate=vals[3,"Rate"])
	out<- inds
	
	# FALL CREEK
	
	# DEXTER
	inds<- subset(sim_out$daily,location=="Dexter Dam" & counts>0)
	inds$id<- c(1:nrow(inds))
	inds<- inds[rep(inds$id,inds$count),]
	vals<- resTimes[resTimes$Tributary.population=="Middle Fork Willamette River",]
	inds$ms<- rgamma(nrow(inds), shape=vals[1,"Shape"],rate=vals[1,"Rate"])
	inds$trib<- rgamma(nrow(inds), shape=vals[2,"Shape"],rate=vals[2,"Rate"])
	inds$tailrace<- rgamma(nrow(inds), shape=vals[3,"Shape"],rate=vals[3,"Rate"])	
	
	out<- rbind(out,inds)
	
	out$total<-out$ms + out$trib + out$tailrace
	return(out)	
	}
	
	
	
	
	
	