# p is a 356 row 5 column vector for each major stream segment node
# inn is the daily number of fish escaping Willamette falls

# OUPUT: AN ARRAY WITH THE FOLLOWING
	# yy[NFISH,NSEGS(1:11),1]: SEGEMENT PRESENCE
	# yy[NFISH,NSEGS(1:11),2]: SEGEMENT ENTRY (DOY)
	# yy[NFISH,NSEGS(1:11),3]: SEGEMENT EXIT (DOY)
		# NOTE THAT DURATION IN TERMINAL SEGEMENTS REFLECTS THE ARRIVAL IN THE TAILRACE
# SOME ASSUMPTIONS
	# NO WANDERING 
	# NO MORTALITY
	
	
	
	
	
	

	
module_02<- function(B0=c(0,0,0,0,0), B1=c(0,0,0,0,0),escaped,ln_mu=rep(3.13,11), cv=rep(0.1,11),d=rep(10,11),...)
	{
	n<- length(escaped)
	yy<-array(0,c(n,ncol=11,3))# [,,1] segment, [,,2] in doy, [,,3] out doy 	
	yy[,1,1]<- 1
	yy[,1,2]<- escaped
	yy[,1,3]<- yy[,1,2] + d[1]/rlnorm(n,ln_mu[1],cv[1])# segment 1

	# SEGMENT 1 TO SEGMENT 2 (MS-Santiam River)
	p_1_2<-inv_logit(y= B0[1] + B1[1]*yy[,1,3])
	yy[,2,1]<- rbinom(n,1,p_1_2)# DOES A FISH GO INTO THE SEGMENT  
	yy[,2,2]<- yy[,1,3]*yy[,2,1]
	yy[,2,3]<- yy[,2,2] + d[2]/rlnorm(n,ln_mu[2],cv[2])# segment 2
	
	# SEGMENT 1 TO SEGMENT 5 (MS-MS)
	yy[,5,1]<- (1-yy[,2,1])*yy[,1,1]
	yy[,5,2]<- yy[,1,3]*yy[,3,1]
	yy[,5,3]<- yy[,3,2] + d[5]/rlnorm(n,ln_mu[5],cv[5])# segment 5
	
	# SEGMENT 2 TO  3 (SR-SSR): TERMINAL SEGEMENT
	p_2_3<- inv_logit(y= B0[2] + B1[2]*yy[,2,3])
	yy[,3,1]<- rbinom(n,1,p_2_3*yy[,2,1])# THIS IS CONDITIONAL ON BEING AVAILABLE TO MOVE INTO SEGEMENT
	yy[,3,2]<- yy[,2,3]*yy[,3,1] # ASSIGN DAY LEAVING SEGEMENT TO DAY ENTERING SEGEMENT
	yy[,3,3]<- yy[,3,2] + d[3]/rlnorm(n,ln_mu[3],cv[3])	
	
	# SEGMENT 2 TO  4 (SR-NSR): TERMINAL SEGEMENT
	yy[,4,1]<- (1-yy[,3,1])*yy[,2,1]
	yy[,4,2]<- yy[,2,3]*yy[,4,1] # ASSIGN DAY LEAVING SEGEMENT TO DAY ENTERING SEGEMENT
	yy[,4,3]<- yy[,4,2] + d[4]/rlnorm(n,ln_mu[4],cv[4])		

	# SEGMENT 5 TO SEGMENT 6 (MS-MS)
	p_5_6<- inv_logit(y= B0[3] + B1[3]*yy[,5,3])
	yy[,6,1]<- rbinom(n,1,p_5_6*yy[,5,1])
	yy[,6,2]<- yy[,5,3]*yy[,6,1]
	yy[,6,3]<- yy[,6,2] + d[6]/rlnorm(n,ln_mu[6],cv[6])
	
	# SEGMENT 5 TO SEGMENT 7 (MS-MCK)
	yy[,7,1]<- (1-yy[,6,1])*yy[,5,1]
	yy[,7,2]<- yy[,5,3]*yy[,7,1]
	yy[,7,3]<- yy[,7,2] + d[7]/rlnorm(n,ln_mu[7],cv[7])
	
	# SEGMENT 7 TO SEGMENT 8 (MCK-SFMCK): TERMINAL SEGEMENT
	p_7_8<- inv_logit(y= B0[4] + B1[4]*yy[,7,3])
	yy[,8,1]<- rbinom(n,1,p_7_8*yy[,7,1])
	yy[,8,2]<- yy[,7,3]*yy[,8,1]
	yy[,8,3]<- yy[,8,2]+ d[8]/rlnorm(n,ln_mu[8],cv[8])
	
	# SEGMENT 7 TO SEGMENT 9 (MCK-MCK): TERMINAL SEGEMENT
	yy[,9,1]<- (1-yy[,8,1])*yy[,7,1]
	yy[,9,2]<- yy[,6,3]*yy[,9,1]
	yy[,9,3]<- yy[,9,2] + d[9]/rlnorm(n,ln_mu[9],cv[9])
	
	# SEGMENT 6 TO SEGMENT 10 (MS-MF): TERMINAL SEGEMENT
	p_6_10<- inv_logit(y= B0[5] + B1[5]*yy[,6,3])
	yy[,10,1]<- rbinom(n,1,p_6_10*yy[,6,1])
	yy[,10,2]<- yy[,6,3]*yy[,10,1]
	yy[,10,3]<- yy[,10,2]+ d[10]/rlnorm(n,ln_mu[10],cv[10])
	
	# SEGMENT 6 TO SEGMENT 11 (MS-FC): TERMINAL SEGEMENT
	yy[,11,1]<- (1-yy[,10,1])*yy[,6,1]
	yy[,11,2]<- yy[,6,3]*yy[,11,1]
	yy[,11,3]<- yy[,11,2]+ d[11]/rlnorm(n,ln_mu[11],cv[11])
	
	# SUMMARIZE DATA IN LONG FORMAT TO MAKE USE OF MODEL.MATRIX
	stock<- data.frame(stockId=apply(yy[,c(4,3,9,8,11,10),1],1,which.max))
	stocks<- data.frame(stockId=c(1:6),stock=c("N. Santiam","S. Santiam", "McKenzie R.",
		"S. McKenzie R.", "Fall Creek","MF Willamette R."))
	stock<- stocks[c(stock$stockId),]
	stock$duration<- apply(yy[,,3]-yy[,,2],1,sum)
	# stock$escape<- yy[,1,2]
	stock$arrive<- floor(apply(yy[,,3],1,max))
	return(stock)
	}
	

