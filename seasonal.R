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

	#Determine the order
	d1 <- arima(x,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=prd))
	d2 <- arima(x,order=c(1,1,1),seasonal=list(order=c(1,1,0),period=prd))
	d3 <- arima(x,order=c(0,1,2),seasonal=list(order=c(1,1,0),period=prd))
	d4 <- arima(x,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=prd))
	d5 <- arima(x,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=prd))
	d6 <- arima(x,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=prd))
	ARIMApool = list(d1,d2,d3,d4,d5,d6)
	BICscore = c()
	for(i in seq_along(ARIMApool)) BICscore <- c(BICscore,BIC(ARIMApool[[i]]))
	d = ARIMApool[BICscore==min(BICscore)][[1]]
	print(d)
}
x = read.csv("data/2-1_pe.csv",header=F,sep=",")
d <- Problem2(x,4)
png(filename="pe_ACF.png")
acf(d$residual,lag=30)
dev.off()
png(filename="pe_PACF.png")
acf(d$residual,type="partial",lag=30)
dev.off()

x = read.csv("data/2-2_rsp.csv",header=F,sep=",")
d <- Problem2(x,12)
png(filename="rsp_ACF.png")
acf(d$residual,lag=30)
dev.off()
png(filename="rsp_PACF.png")
acf(d$residual,type="partial",lag=30)
dev.off()
