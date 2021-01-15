#################################################################################################################
###                                                                                                           ###
###                           demo script for recruitment testing                                             ###
###                                                                                                           ###
###   stockassessment package version:https://github.com/elisvb/SAM                                           ###
###   -> tiny modifications to allow forecasts to start prior to the last 2 years                             ###
### modifications to original: https://github.com/elisvb/SAM/commit/5f4020480ab82a860e35f3bb2029adb4cefb76d8  ###
#################################################################################################################

library(stockassessment)

###-----------------------------------------------------------------------------
### Forecast parameters
###-----------------------------------------------------------------------------
y <- tail(nscodData$years,1)           # last year assessment
i <- y-10                              # year from which to start simulating a forecast
p <- 3                                 # duration of forecast    

###-----------------------------------------------------------------------------
### Get one operating model (test-bed)
###-----------------------------------------------------------------------------
fit <- sam.fit(nscodData,nscodConf,nscodParameters,saveState = c(y,i))     # save state is simply so SAM will not just save the last years' state
fit
plot(fit)

###-----------------------------------------------------------------------------
### Explore forecast function
###-----------------------------------------------------------------------------
## 1) two identical runs
f1 <- forecast(fit,catchval=rep(50000,p+1),year.base = y)
plot(f1)
ssbplot(f1)
f1

f2 <- forecast(fit,catchval=rep(50000,p+1),year.base = y)
ssbplot(f2)
f2

identical(f1,f2)

## 2) Don't want performance of forecast to depend on stochasticity: set seed (here a bit naively)
set.seed(123)
f3 <- forecast(fit,catchval=rep(50000,p+1),year.base = max(fit$data$years))
f3

set.seed(123)
f4 <- forecast(fit,catchval=rep(50000,p+1),year.base = max(fit$data$years))
f4

identical(f3,f4)

###-----------------------------------------------------------------------------
### ASSESS SKILL OF FORECAST
## -> compare 'perfection' to 'attempt at perfection' 
###-----------------------------------------------------------------------------

# Get'true' catch for the period of the forecast
catch <- catchtable(fit)
id <- which(as.numeric(rownames(catch))==i)
truecatch <- catch[id:(id+p),1]

# 1) 'our attempt to get perfection'
set.seed(123)
fperf <- my.forecast(fit,catchval.exact=truecatch,year.base = i,rec.meth=0) # rec.meth=0 = perfect recruitment
recplot(fperf)
ssbplot(fperf)

skill(fperf)  # note the small difference between OM SSB and 'perfect' prediction of SSB caused by stochastic elements

# 1) attempt to get at the truth
# -> Pretend we started the forecast earlier and knew everything perfectly but recruitment (+ stochasticity) to exclude other sources of error
set.seed(123)
fmeth1 <- my.forecast(fit,catchval.exact=truecatch,year.base = i,rec.meth=1,rec.years=i+(-9:0)) # rec.year = years to sample from (default SAM method)
recplot(fmeth1)
ssbplot(fmeth1)

skill(c(fperf,fmeth1))

set.seed(123)
fmeth2 <- my.forecast(fit,catchval.exact=truecatch,year.base = i,rec.meth=2,rec.years=i+(-9:0)) # sample same pool from an emperical cumulative distributino function
recplot(fmeth2)
ssbplot(fmeth2)

skill(c(fperf,fmeth2))

## compare 
par(mfrow=c(3,1))
ssbplot(fperf)
ssbplot(fmeth1)
ssbplot(fmeth2)


