

juv_maturity<-approxfun(x=c(0,8,41), y=c(0,1,1),method="constant")


year0<- function(S0=0.051)
	{# THIS FUNCTION RETURNS A VECTOR SURVIVALS FOR THE 3 STAGES THAT
	# THAT OCCUR IN YEAR 0 
	# DEFAULT 0.051 IS FROM STEPHENSON ET AL...
	repeat{
		x<- runif(2, 0.001,0.3)
		if (prod(x)>S0){break}
		}
	x<- c(x, 
	a<- log(S0)-(log(x[1])+log(x[2]))
	
	exp(a)
	prod(x,exp(a))
	return(x)
	}


eggs_S<- function(val, surv, stoch=FALSE,...)
	{
	# INPUTS
	# val: NUMBER OF EGGS AT PREVIOUS DAY
	# surv: DAILY SURVIVAL RATE TO APPLY TO EGGS
	# stoch: SHOULD OUTPUT BE STOCHASTIC?  DEFAULTS TO FALSE
	# eggs_out: THIS IS THE VALUE RETURNED BY THE FUNCTION
	eggs<- val
	eggs_out<- ifelse(stoch==FALSE, eggs*surv, rpois(eggs*surv))
	return(eggs_out)	
	}
	
embryo_S<- function(val, surv, stoch=FALSE,...)
	{
	# INPUTS
	# val: NUMBER OF EMBRYOS AT PREVIOUS DAY
	# surv: DAILY SURVIVAL RATE TO APPLY TO EMBRYOS
	# stoch: SHOULD OUTPUT BE STOCHASTIC?  DEFAULTS TO FALSE
	# embryo_out: THIS IS THE VALUE RETURNED BY THE FUNCTION
	embryo<- val
	embryo_out<- ifelse(stoch==FALSE, embryo*surv, rpois(embryo*surv))
	return(embryo_out)	
	}

exogenously_feeding_embryo_S<- function(val, surv, stoch=FALSE,...)
	{
	# INPUTS
	# val: NUMBER OF EXOGENOUSLY FEEDING EMBRYOS AT PREVIOUS DAY
	# surv: DAILY SURVIVAL RATE TO APPLY TO EXOGENOUSLY FEEDING 
	# stoch: SHOULD OUTPUT BE STOCHASTIC?  DEFAULTS TO FALSE
	# ef_embryo_out: THIS IS THE VALUE RETURNED BY THE FUNCTION
	ef_embryo<- val
	ef_embryo_out<- ifelse(stoch==FALSE, ef_embryo*surv, rpois(ef_embryo*surv))
	return(ef_embryo_out)	
	}
	
juveniles_S<- function(val, surv, stoch=FALSE,...)
	{
	# INPUTS
	# val: NUMBER OF JUVENILES AT PREVIOUS DAY
	# surv: DAILY SURVIVAL RATE TO APPLY TO JUVENILES
	# stoch: SHOULD OUTPUT BE STOCHASTIC?  DEFAULTS TO FALSE
	# juvenile_out: THIS IS THE VALUE RETURNED BY THE FUNCTION
	juvenile<- val
	juvenile_out<- ifelse(stoch==FALSE, juvenile*surv, rpois(juvenile*surv))
	return(juvenile_out)	
	}	
	
adults_S<- function(val, surv, stoch=FALSE,...)
	{
	# INPUTS
	# val: NUMBER OF ADULTS AT PREVIOUS DAY
	# surv: DAILY SURVIVAL RATE TO APPLY TO ADULTS
	# stoch: SHOULD OUTPUT BE STOCHASTIC?  DEFAULTS TO FALSE
	# adult_out: THIS IS THE VALUE RETURNED BY THE FUNCTION
	adult<- val
	adult_out<- ifelse(stoch==FALSE, adult*surv, rpois(adult*surv))
	return(adult_out)	
	}	
	
spawning_adults_S<- function(val, surv, stoch=FALSE,...)
	{
	# INPUTS
	# val: NUMBER OF ADULTS AT PREVIOUS DAY
	# surv: DAILY SURVIVAL RATE TO APPLY TO SPAWNING ADULTS
	# stoch: SHOULD OUTPUT BE STOCHASTIC?  DEFAULTS TO FALSE
	# spawning_adults_out: THIS IS THE VALUE RETURNED BY THE FUNCTION
	spawning_adults<- val
	spawning_adults_out<- ifelse(stoch==FALSE, spawning_adults*surv, rpois(spawning_adults*surv))
	return(spawning_adults_out)	
	}	
	
	
