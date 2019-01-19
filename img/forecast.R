## Task 1
folder = "/Users/xfang7/Google\ Drive/Courses/CSC591-004/hw/ForecastingProject/"
data = read.csv(paste(folder,"xfang7.csv", sep=''),header = FALSE)
myts = ts(data[[1]])
plot(myts,main = "Time Series", xlab="time",ylab="value")

## Task2
library(tseries)
adf.test(myts)
myts.train = subset(myts,end = length(myts) - 500)
myts.test =  subset(myts,start = length(myts) - 499)
rmse <- function(error)
{
  sqrt(mean(error^2))
}

doSma <- function(train,k){
  library(TTR)
  sma = SMA(train,n=k)
 # plot(sma,main=paste("SMA",k,sep=""),xlab ="time",ylab = "vaule")
  cActrual = c(train[(k+1):length(train)])
  cFitted =  c(sma[(k+1):length(sma)])
  error = cActrual - cFitted
  return (rmse(error))
}
rmseResults = c()
for(i in 1 : 50){
  rmseResults = c(rmseResults,doSma(myts.train,i))
}
print(rmseResults)
plot(rmseResults,main = "RMSE vs k", xlab = "k",ylab = "RMSE")


## Task 3
doEsModel <- function(p){
  plot(myts.train,main=paste("exponential smoothing model,alpha=",p,sep=""),ylab = "Value")
  myts.es = HoltWinters(myts.train,alpha =p ,beta = F,gamma = F)
  lines(myts.es$fitted[,1],col="green")
   return (sqrt(myts.es$SSE/length(myts.es$fitted[,1])))
}

es.rmses = c()
for( a in 1:9){
  es.rmses = c(es.rmses,doEsModel(a/10))
}
print(es.rmses)
plot(es.rmses,main = "RMSE vs alpha in Exponential Smoothing Model", xlab = "k",ylab = "RMSE")

## TASK 4
myts.ar = ar(myts.train,order.max=2,aic=FALSE)
myts.ar.fitted = myts.train - myts.ar$resid
plot(myts.ar.fitted,myts.train,main = "Fitted Value vs Original Value",xlab = "Fitted Value",ylab = "Original Value",col="red")
residUsed = myts.ar$resid[3:1500]
ar.rmse =rmse(residUsed)
print(ar.rmse)
qqnorm(residUsed)
qqline(residUsed)

hist(residUsed,main = "Histogram of residual", xlab = "residual")
chisq.test(residUsed^2)

plot(residUsed,main = "Plot of residual", ylab = "residual",xlab = "")
abline(0,0)

## Task 5
library(TTR)
library(forecast)
testData = myts.test
sma = SMA(myts.train,n=1)
fore.sma= sma %>%forecast(h=500)
fore.sma%>% autoplot(main="Forecasts From SMA",ylab="value") + autolayer(testData) 
accuracy(fore.sma,testData)


es = HoltWinters(myts.train,alpha =0.1 ,beta = F,gamma = F)
fore.es=es %>%forecast(h=500)
fore.es%>%autoplot(main="Forecasts From Exponential Smoothing Model",ylab="value") + autolayer(testData)
accuracy(fore.es,testData)

ar = ar(myts.train,order.max=2,aic=FALSE)
fore.ar = ar %>%forecast(h=500) 
fore.ar %>%autoplot(ylab="value") + autolayer(testData)
accuracy(fore.ar,testData)





