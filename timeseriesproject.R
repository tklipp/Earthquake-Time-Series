library(astsa)
library(forecast)

earthquake<-read.table("earthquake.csv",sep=",",header=FALSE,skip=1,nrows=99)
earth.ts<-ts(earthquake[,2])
ts.plot(earth.ts,xlab="Years since 1900",ylab="Number of Earthquakes",col="red")
acf(earth.ts)
pacf(earth.ts)
acf2(earth.ts)
hist(earth.ts,breaks=30,main="Histogram of Earthquake Time Series Prior to Transformation",col="blue")
auto.arima(earth.ts)

t=1:length(earth.ts)
bcTransform = boxcox(earth.ts ~ t)
bcTransform
lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))] #box cox transformation
earth.bc = (1/lambda)*(earth.ts^lambda-1)
ts.plot(earth.bc,xlab="Years since 1900",ylab="Number of Earthquakes",main="Earthquake Time Series after Box Cox",col="green")
hist(earth.bc,breaks=30,main="Histogram of Earthquake Time Series After Box-Cox ",col="green")

earth.diff1=diff(earth.ts) #first order difference
acf2(earth.diff1) #acf and pacf of differenced ts
acf(earth.diff1,lag.max=100)
ts.plot(earth.diff1,col="brown",main="Earthquake Time Series After Differening",xlab="Years Since 1900",ylab="Number of Earthquakes") #time series plot

fit1<-sarima(earth.ts,p=0,d=1,q=1) #ARIMA(0,1,1)
fit2<-sarima(earth.ts,p=1,d=1,q=1) #ARIMA(1,1,1)
fit3<-sarima(earth.ts,p=0,d=1,q=2) #ARIMA(0,1,2)
fit4<-sarima(earth.ts,p=1,d=1,q=2) #ARIMA(1,1,2)
fit_best <- fit1 #best model 
lower <- fit_best$fit$coef - 1.96*sqrt(diag(fit_best$fit$var.coef)) # the lower bounds
upper <- fit_best$fit$coef + 1.96*sqrt(diag(fit_best$fit$var.coef)) # the upper bounds
rbind(lower,upper) #confidence interval of coefficients 

forecast<-sarima.for(earth.ts,p=0,d=1,q=1,n.ahead=10) #forecasting
lower.pred <- forecast$pred - 1.96*forecast$se #lower bound
upper.pred <- forecast$pred + 1.96*forecast$se #upper bound
rbind(lower.pred, upper.pred) #prediction interval
forecast$pred #individual predictions

