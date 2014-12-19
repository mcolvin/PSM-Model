
#load("degreeDayLookup_new.RData")
resTimes<- read.csv("./dat/residenceTimes.csv")
	
# DAYS WHEN WATER TEMPERATURE EXCEEDS 13 C IN OUTPLANT STREAM (MF WR) 
# NEED TO ADD MF WR 
	temp_excedance<- data.frame(matrix(c(
		2002 , 155,
		2003 , 155,
		2004 , 168,
		2005 , 147,
		2006 , 135,
		2007 , 152,
		2008 , 136,
		2009 , 149,
		2010 , 175,
		2011 , 183,
		2012 , 169), ncol=2, byrow=TRUE))

	# THIS FUNCTION STOCHASTICALLY SIMULATES THE NUMBER OF SPRING CHINOOK ESCAPING WILLMATTE FALLS
	wf_escapement<- sqlFetch(com, "Adult Counts (Annual Willamette Falls Totals)")
	wf_escapement<- wf_escapement[,c("YEAR","SPRING_CHINOOK_TOTAL")]
	names(wf_escapement)<- tolower(names(wf_escapement))
	
	# PROBABILITIES OF BEING TRAPPED ON A GIVEN DAY OF YEAR
	trap_timing<- read.csv("./dat/traptiming.csv")
	
	
	
	
	