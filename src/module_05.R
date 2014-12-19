# TRANSPORT MORTALITY MODULE
module_05<- function(B0=c(0,0,0,0,0,0),B1=c(0,0,0,0,0,0),dat=sim,...)
	{
	# module requires input of fish outplanted
	# B IS A MATRIX OF BETAS WITH EACH ROW FOR DAM
	# X IS A MATRIX OF COVARIATES
	# FISH MAT IS A DATAFRAME OUTPUT FROM MODULE 3
	# transport mortality	
	# GENERATE MODEL MATRIX
	XX<- model.matrix(~as.factor(stockId) + as.factor(stockId):arrive-1,dat)
	betas<- c(B0,B1)
	y<- XX %*% betas
	y<-inv_logit(y)
	transport_mort<- rbinom(nrow(y),1,y)
	return(transport_mort)
	}