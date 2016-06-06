#!/usr/bin/env Rscript
library("MuMIn")
library("utils")
n.sel = 100; 
beta0 = c( 0.1, -0.12, 0.25, -0.21 ); 
t.x = 1 : n.sel
yy = cbind(1,t.x,sin(t.x),cos(3*t.x))%*%beta0 + arima.sim(list(order=c(1,0,2),ar=c(0.2),ma=c(0.1,0.3)),sd=0.1,n=n.sel)
plot(yy,type="l",ylab="Data",xlab="Periods",col="blue",main="Observations-Time(Sample)",lwd=2);
points(yy, col="red",pch=20, lwd=5)
#dredge( , rank="" );
#arima( , order = c( p ,0, q ) )
