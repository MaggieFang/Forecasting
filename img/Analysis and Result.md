[TOC]

## Task 1

The plot of time series are shown as follow.

![tsRplot](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/tsRplot.png)Visually, we draw preliminary conclusion that it is stationary. We can also use R function `adf.test` to test it.  p-value < 0.05 indicates the time series is stationary. We test at various lags and the result is shown below, p -value is 0.01, we can draw the time series is stationary. So we don't need to do data transformation here. 

![adf](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/adf.png)

Generally, if the time series have non-stationary feature. We can use Differencing, Transformations, Seasonal differencing to remove the non-stationary feature. There are R package to do that. For example, the `nsdiffs ` and `ndiffs` from `forecast` package can help to find out how many seasonal differencing and regular differencing respectively, which are needed to make the series stationary.

## Task 2

### 2.1

Firstly, we partition the train data and test data. We selected the data from 1 to 1500 be the train data, and data from 1501 to 2000 be the test data.  As shown below

```R
myts.train = subset(myts,end = length(myts) - 500)
myts.test =  subset(myts,start = length(myts) - 499)
```

Then we use `SMA` to do simple moving average model on the train data. We select k = 1 here and plot the result as follows.

![SMA25Rplot](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/SMA1.png)

### 2.2

Assume preditted values and actual values are stored in v1 and v2 perspectively, calculate the error by v2 -v1 to get the error series, and can plot them, as shown below.

![ErrorSMA1](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/ErrorSMA1.png)

we define a function to calculate the RMSE and put the error series into it to get the RMSE = 7.157443e-13

```R
rmse <- function(error)
{
  sqrt(mean(error^2))
}
```

### 2.3 

Here, we abstract a function doSMA to do model and return RMSE as follows

```R
doSma <- function(train,k){
  library(TTR)
  sma = SMA(train,n=k)
  plot(sma,main=paste("SMA",k,sep=""),xlab ="time",ylab = "vaule")
  
  cActrual = c(train[(k+1):length(train)])
  cFitted =  c(sma[(k+1):length(sma)])
  error = cActrual - cFitted
  # plot(error,main=paste("Error of SMA",k,sep=""),xlab = "time",ylab = "difference")
  return (rmse(error))
}
```

So we can call it in a loop varies k from 1 to 50 and save the RMSEs in a vector. 

```R
 rmseResults = c()
for(i in 1 : 50){
  rmseResults = c(rmseResults,doSma(myts.train,i))
}
print(rmseResults)
```

and we can get  RMSEs  when k varies from 1 to 50  as follows.

![rmses](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/rmses.png)

From the above results. we know that RMSE increases as k increases, particularly at the beginning.

Selectedly, we print some SMA models shown below when k = 10,20,30,40,50.

![SMA1](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/SMA10.png)

![SMA1](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/SMA20.png)

![SMA1](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/SMA30.png)

![SMA1](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/SMA40.png)

![SMA200](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/SMA50.png)

### 2.4

Plot RMSE vs k as follows

![RMSEvsK](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/RMSEvsK.png)

To plot the Fitted Value vs Original Value, we choose Fitted Value as x axis and  Original Value y asix. We know  RMSE = 4.798373e-13 is very small, so the the plots shows nearly like a line with slope 1, shown as follows.

![FitvsOriginal](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/FitvsOriginal.png)



### 2.5

From this task, we know that modeling using simple moving average model, the k value selection affects the result of modeling. we need to select the best value for k and we use RMSE (Routed mean squared error)  method for it. In this case, we conclude that, RMSE increases as k increases firstly and at some point of k, the RMSE not change much . And when k =1 ,we can get the best modeling result with  RMSE   4.798373e-13

## Task 3

### 3.1

Use `HoltWinters` to do the exponential smoothing model and get the result as follows.

![SmothModel](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/SmothModel.png)

### 3.2

For  exponential smoothing model , we just get 1499 fitted values from the 1500 observations.  And the first value in fitted value vector is the predit value when t = 2. so when calculating the error series. we need to choose the train data from 2 to 1500 compared with the fitted values.We plot the error series as follows 

![esError](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/esError.png)

The result of `HoltWinters` contains information of $SSE, the sum of squared errors for the in-sample forecast error. So we can use  SSE divide the length of data length and then sqrt to get RMSE = 1.640987 (Same with the result we use the calculation function rmse in 2.2 ). 

Selectedly, we plots the models when $\alpha = 0.4 $  and 0.9

![ES4](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/ES4.png)

![ES9](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/ES9.png)

### 3.3

we abstrat function `doEsModel` to do exponential smoothing model and return RMSE . Let alpha be the param of the function.

```R
doEsModel <- function(p){
  plot(myts.train,main=paste("exponential smoothing model,alpha=",p,sep=""),ylab = "Value")
  myts.es = HoltWinters(myts.train,alpha =p ,beta = F,gamma = F)
  lines(myts.es$fitted[,1],col="green")
   return (sqrt(myts.es$SSE/length(myts.es$fitted[,1]))) 
}
```

then we use a loop to do the modeling varying alpha from 0.1 to 0.9 and save RMSEs in a vector. 

```R
es.rmses = c()
for( a in 1:9){
  es.rmses = c(es.rmses,doEsModel(a/10))
}
print(es.rmses)
```

we get RMSEs as follows

![esrmse](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/esrmse.png)

### 3.4

Plot RMSE vs the $\alpha$  as follows.We knows that when $\alpha = 0.1$ the RMSE is lowest. 

![errorES](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/errorES.png)

### 3.5

when $\alpha = 0.1$ plot the predicted values vs the original values shown as follows.

![fittedES](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/fittedES.png)

Visually, the fitted data are centered around 161-162, It is much smoother than the time series of the original data here. Also, from the RMSE above, we know that the RMSE is minimum when $\alpha = 0.1$. As $\alpha$ increases, the lag become more and more distinctive and the RMSE increases. 

### 3.6

From this task, we know that modeling using exponential smoothing model, the $\alpha$  value selection affect the result of modeling. we need to select the best value for  $\alpha$  and we use RMSE (Routed mean squared error)  method for it. In this case, the RMSE is minimum  when $\alpha = 0.1$. Maybe it is a little hard for us to visually see how fit the $\alpha$ affect.  But we can draw and enlarge part of the plots of modeling. For example, the two images shown below enlarge the first 100 points of model when $\alpha = 0.1$ and 0.9. Now we can see when $\alpha = 0.9$ . Lags are distinctive which will enlarge the error. So modeling when $\alpha = 0.9$  is not as good as modeling when  $\alpha = 0.1$

![Enlarge1](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/Enlarge1.png)

![Enlarge](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/Enlarge.png)

## Task 4

 ### 4.1

we can use function `pacf` to plot PACF. as shown below, we should select p =2 

![pacf](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/Pace.png)

### 4.2

we use  `ar` function to model AR(p)  and get the estimate params of 0.6819 and -0.7130

![image-20181111000649593](/var/folders/6n/bdq9817j37zdmgs7vvwzdp9c0000gn/T/abnerworks.Typora/image-20181111000649593.png)

There is no fitted values in the output of `ar` but there are resid. So we can use the orginalData - resid to get the fitted value and plot them.  We use the function rmse we define above to compure RMSE = 1.014927

```R
myts.ar = ar(myts.train,order.max=2,aic=FALSE)
myts.ar.fitted = myts.train - myts.ar$resid
plot(myts.ar.fitted,myts.train,main = "Fitted Value vs Original Value",xlab = "Fitted Value",ylab = "Original Value",col="red")
residUsed = myts.ar$resid[3:1500]
ar.rmse =rmse(residUsed)
print(ar.rmse)
```

![arFitted](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/arFitted.png)

### 4.3

a.

![4QQplot](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/4QQplot.png)

![4hisRes](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/4hisRes.png)

we carried out χ2 test by the following code. From the reslut  p-value < 2.2e-16, we can reject null hypothesis, means we have strong confidence that  residual follows the normal distribution N(0,$s^2$ )

> residUsed = myts.ar$resid[3:1500]			

> chisq.test(residUsed^2).          

> data:  residUsed^2
> X-squared = 3456.7, df = 1497, p-value < 2.2e-16

b.

the residual scatter plot drawn as follows. Conclusion: residuals have no correlation trends

![4scatter](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/4scatter.png)

### 4.4

From this task, we know that modeling using AR(p) model. the p value selection affect the result of modeling. We use PACF to determine the lag k at which PACF cuts off to select the order p. After p selection, we can do AR modeling to estimate the params and get the resid. 

From the residuals analysis, we draw histogram and carry out χ2  test and Q-Q plot, leading to the conclusion that residual follows the normal distribution N(0, $s^2$).  From the scatter plot of residuals, we conclude residuals have no trends.

## Task 5

We use `forecast` function to forecast the next 500 step and then plot together with the test dataset. Then we use `accuracy(f,x)`, in which  input param `f` is the forecast result and `x` is the test DataSet to calculate the accuracy.

```R
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
```

plot results are shown below for SMA, Exponential Smoothing Model,AR(2) perspectively. 

![foreSMA](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/foreSMA.png)

![foreSMA](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/foreES.png)

![foreSMA](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/foreAR.png)

The accuracy between forecast models and testDate is shown below.

![foreSMA](/Users/xfang7/Google Drive/Courses/CSC591-004/hw/ForecastingProject/foreacc.png)

Result interpretation:

 The forecasts are shown as a blue line, with the 80% prediction intervals as an darker shaded area, and the 95% prediction intervals as a  lighter shaded area. Original test data are shown as the red line.mAnd we check the preditted effect by `accuracy` function in which inputs are forecast values and original test values. From the RMSE, MAE, MPE, MAPE for each one, we know that AR(2) is the best model we can use.

















