#!/usr/bin/env Rscript

Problem2 <- function(x,prd){
	#ACF of data
	#tsx = ts(x)
	#acfx = acf(tsx)
	#acfx = acf(tsx,type="partial")

	#ACF after differenced
	#d = arima(x,order=c(0,1,0))
	#acf(d$residual,lag=30)
	#acf(d$residual,type="partial",lag=30)

	#ACF after seasonal differenced 1 at 4
	#d = arima(x,order=c(0,0,0),seasonal=list(order=c(0,1,0),period=4))
	#acf(d$residual,lag=30)

	#ACF of D1 and seasonal D1 at 4
	#d = arima(x,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=12))
	#acf(d$residual,lag=30)

	#PACF of D1 and seasonal D1 at 4
	#acf(d$residual,type="partial",lag=30)

	model = arima(x,order=c(0,0,0),seasonal=list(order=c(0,0,0),period=prd))
	bic = BIC(model)
	param = c(0,0,0,0,0,0,prd)
	#Determine the order
	for(p in 0:5){
		for(d in 0:3){
			for(q in 0:5){
				for(P in 0:5){
					for(D in 0:3){
						for(Q in 0:5){
							result = tryCatch({
								temp = arima(x,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=prd))
								if(BIC(temp) < bic)
									model = temp
									bic = BIC(model)
									param = c(p,d,q,P,D,Q,prd)
							},
								error = function(e) {
								message(e) 
								return(NULL)
							})
							print(c(p,d,q,P,D,Q,bic))
						}
					}
				}
			}
		}
	}
	print(model)
	print(param)
	Ret$model = model
	Ret$param = param
	return(Ret)
}
x = read.csv("data/2-1_pe.csv",header=F,sep=",")
Ret <- Problem2(x,4)
param1 = Ret$param
model1 = Ret$model
png(filename="pe_ACF.png")
acf(model1$residual,lag=30)
dev.off()
png(filename="pe_PACF.png")
acf(model1$residual,type="partial",lag=30)
dev.off()

x = read.csv("data/2-2_rsp.csv",header=F,sep=",")
Ret <- Problem2(x,12)
param2 = Ret$param
model2 = Ret$model
png(filename="rsp_ACF.png")
acf(model2$residual,lag=30)
dev.off()
png(filename="rsp_PACF.png")
acf(model2$residual,type="partial",lag=30)
dev.off()
print(model1)
print(param1)
print(model2)
print(param2)
