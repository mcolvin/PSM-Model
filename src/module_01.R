# this module takes an input of daily counts from Willamette Falls and expands them to individual fish
# and stochastically assigns them a clipped or unclipped status.  
module_01<- function(year=2013,doy, inn,...)
	{
	# Inputs:
		# inn: a vector of daily counts
		# doy: day of year
	# Outputs: a  data frame containing 
		# doy: doy of escapement
		# count: number of fish escaping that day
	inds<- data.frame(doy=doy, count=inn)
	inds<-as.data.frame(lapply(inds,function(x) rep(x,inds$count)))
	inds$count<- 1
	# merge a month to doy
	# assign clipped and uncliped by month to match cumulative marking rates
	inds$m<- cut(inds$doy, breaks=doy_months$doy,labels=c(1:11))
	inds_monthly<- aggregate(count~m,inds, sum)
	inds_monthly$cum_p_clipped<- marking_rate_fun(year, inds_monthly$m) 
	inds_monthly$cum_adults<- cumsum(inds_monthly$count)
	inds_monthly$cum_clipped<- round(inds_monthly$cum_adults*inds_monthly$cum_p_clipped,0)
	inds_monthly$cum_unclipped<- inds_monthly$cum_adults-inds_monthly$cum_clipped
	inds_monthly$unclipped<- c(inds_monthly$cum_unclipped[1], 
		inds_monthly$cum_unclipped[-1]-inds_monthly$cum_unclipped[-nrow(inds_monthly)])
	inds_monthly$clipped<- c(inds_monthly$cum_clipped[1], 
		inds_monthly$cum_clipped[-1]-inds_monthly$cum_clipped[-nrow(inds_monthly)])
	inds_monthly$monthly_p_marked<- inds_monthly$clipped/inds_monthly$count
	inds<- merge(inds, inds_monthly[,c("m","monthly_p_marked")])
	inds$clipped<- rbinom(nrow(inds),1,inds$monthly_p_marked)
	return(inds[,c("m","doy","count","clipped")])
	}
	

	
	
	