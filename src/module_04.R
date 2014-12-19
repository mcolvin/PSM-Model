# TRAPPING OPERATIONS
module_04<- function(B=1,X=1,dat=sim,man_alt=1,...)
	{ T
	
	if(man_alt==1)
		{# BUISNESS AS USUAL		
		dat$cum_count<- unlist(tapply(dat$count,dat$stockId,cumsum))
		dat$fate<- "outplant"
		dat[dat$cum_count<=2500,]$fate<- "broodstock"
		}
	if(man_alt==2)
		{# PROPORTIONAL OUTPLANTING
		outplant<- rbinom(nrow(dat),1,0.25)
		dat$fate<- ifelse(outplant==0,"broodstock","outplant")
		}
	if(man_alt==3)
		{
		}	
	if(man_alt==4)
		{
		}
	if(man_alt==5)
		{
		}
	if(man_alt==6)
		{
		}
	if(man_alt==7)
		{
		}
	return(dat$fate)
	}