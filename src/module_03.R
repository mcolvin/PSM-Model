# THIS IS THE TRAPPING MODULE THAT SIMULATES TRAPPING FOR 


module_03<- function(B0=c(0,0,0,0,0,0), B1=c(0,0,0,0,0,0),endDOY=356,p_capture=rep(0.1,6), dat,...)
	{
	# The module accepts dam specfice B and B1 to feed a 
	# logit linear model.  
	# DOE is a matrix of 0s and 1s with column indicating the segment
	# clipped is a vector indicating if a fish was clipped (1) or unclipped (0)
	
	yyy <- sapply(1:nrow(dat), function(x)
		{	
		for(dur in 1:100)
			{
			# IS A FISH CAPTURED
			is_captured<- rbinom(1,1,p_capture[dat$stockId[x]])
			# IF CAPTURED HOW LONG?
			yy<- B0[dat$stockId[x]] + B1[dat$stockId[x]]*dur
			cap<- rbinom(1,1,inv_logit(yy)*is_captured)
			yy<- (dat$arrive[x]+dur)*cap
			if(cap==1 | dur+dat$arrive[x]==endDOY | is_captured == 0) break
			}
		return(yy)
		})
	return(data.frame(capture_doy=yyy))
	}




#module_03<- function(B0=c(0,0,0,0,0,0), B1=c(0,0,0,0,0,0),dat,...)
#	{
#	# The module accepts dam specfice B and B1 to feed a 
#	# logit linear model.  
#	# DOE is a matrix of 0s and 1s with column indicating the segment
#	# clipped is a vector indicating if a fish was clipped (1) or unclipped (0)
#	X<- model.matrix(~as.factor(stockId)+arrive:as.factor(stockId)-1,dat)	
#	betas<- c(B0,B1)	
#	Y<- X %*% betas
#	Y<- inv_logit(Y)
#	Y_out<- data.frame(trapped=rbinom(length(Y),1,Y))
#	return(Y_out)
#	}