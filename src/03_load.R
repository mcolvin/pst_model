
# POPULATION CHARACTERISTICS
maxAge				<- 41
fecundity			<- 4000
ageAtMaturity		<- 8 # split difference for males and females (S1002)
fecundity			<- 19064 # S1001
spawningInterval	<- 2.5 # split difference for males and females (S1002)
sex_ratio			<- 0.33


# POPULATION DEMOGRAPHIC RATES
baseline<- 0.051 # age 1 from Stephenson

# SET UP STAGES
# Gametes, Gametes and Developing Embryo, Free Embryo, Exogenously feeding larvae & age-0
S<- rep(0, 9)
S[5]<- 0.686 # Juvenile (age 1-age -2)  
S[6]<- 0.992 # Juvenile (age 2 to age at maturity)
S[7]<- 0.992 # Adult > age at maturity (8 to age 41)
S[8]<- 1 # Spawning adult
S[9]<- 0.992 # Recrudescent Adult
# END SURVIVALS

nyears=50


# DPH 0-365
## GAMETES AND DEVELOPING EMBRYOS
gade<- matrix(0,nrow=nyears,ncol=2)
## FREE EMBRYOS
fe<- matrix(0,nrow=nyears,ncol=2)
## EXOGENOUSLY FEED LARVAE AND AGE-0
efl<- matrix(0,nrow=nyears,ncol=2)


# AGE 1-SEXUAL MATURITY
## JUVENILES (1-9 years old)
juv<-array(0,dim=c(nyears,maxAge,2))#[year,age,1=natural & 2=hatchery]

# SEXUAL MATURITY TO MAX AGE
## ADULT
recadult1<-array(0,dim=c(nyears,maxAge,2))# [year,age,1=natural & 2=hatchery]
recadult2<-array(0,dim=c(nyears,maxAge,2))# [year,age,1=natural & 2=hatchery]
recadult3<-array(0,dim=c(nyears,maxAge,2))# [year,age,1=natural & 2=hatchery]
spawners<- matrix(0, nrow=maxAge, ncol=1)
# END SETUP

# INITIALIZE POPULATION

## INITIALIZE JUVENILES
juv_ini_n<- 5000
juv_ini_h<- 32000

juv[1,,1]<- rmultinom(1,juv_ini_n,c(rep(1/length(1:8),length(1:8)),rep(0,33)))
juv[1,,2]<- rmultinom(1,juv_ini_h,c(rep(1/length(1:8),length(1:8)),rep(0,33)))

## INITIALIZE ADULTS
adults_ini_n<- 5000
adults_ini_h<- 32000

adults_ini_n<- rmultinom(1,adults_ini_n,prob=c(rep(1/3,3)))
recadult1[1,,1]<- rmultinom(1,adults_ini_n[1],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))
recadult2[1,,1]<- rmultinom(1,adults_ini_n[2],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))
recadult3[1,,1]<-rmultinom(1,adults_ini_n[3],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))

adults_ini_h<- rmultinom(1,adults_ini_h,prob=c(rep(1/3,3)))
recadult1[1,,2]<- rmultinom(1,adults_ini_h[1],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))
recadult2[1,,2]<- rmultinom(1,adults_ini_h[2],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))
recadult3[1,,2]<-rmultinom(1,adults_ini_h[3],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))







