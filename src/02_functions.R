


eggs<- function(val, surv, stoch=FALSE,...)
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
	
embryo<- function(val, surv, stoch=FALSE,...)
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

exogenously_feeding_embryo<- function(val, surv, stoch=FALSE,...)
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
	
juveniles<- function(val, surv, stoch=FALSE,...)
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
	
adults<- function(val, surv, stoch=FALSE,...)
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
	
spawning_adults<- function(val, surv, stoch=FALSE,...)
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
	
	
