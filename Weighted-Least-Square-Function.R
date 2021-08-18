#Weighted Least Square to estimate nonlinear parameter function

library(MASS)
set.seed(100)
n=30
ev=10
j=rep(1:n)
MAPD=0.9
bb=ev*(1-MAPD)
ba=ev*(1+MAPD)
diagsig=runif(n,bb,ba)
x=abs(rnorm(n,7,5))
f=2.578+(1.567*x)
sigma=matrix(rep(0,(((n)^2))),(n),(n))
	for(i in 1:n){sigma[i,i]=diagsig[i]}
k=rep(1:n)
enol=k*0
e=mvrnorm(1,enol,sigma)
y=abs(f+e)
#regresi x ke y#
model1=lm(y~x)
summary(model1)
yhat=model1$coefficients[1]+(model1$coefficients[2]*x)
residual=y-yhat
plot(residual,yhat)
for (i in 1:n){
if (x.urut=sort(x)
x.urut
data=data.frame(cbind(x,y))
data.urut.x=data[with(data,order(x,y)),]
yy=data.urut.x$y
xbar.neig1=mean(x.urut[1:2])
xbar.neig2=mean(x.urut[3:4])
xbar.neig3=mean(x.urut[5:6])
xbar.neig4=mean(x.urut[7:9])
xbar.neig5=mean(x.urut[10:11])
xbar.neig6=mean(x.urut[12:16])
xbar.neig7=mean(x.urut[17:20])
xbar.neig8=mean(x.urut[21:22])
xbar.neig9=mean(x.urut[23:25])
xbar.neig10=mean(x.urut[26:27])
var.neig1=var(yy[1:2])
var.neig2=var(yy[3:4])
var.neig3=var(yy[5:6])
var.neig4=var(yy[7:9])
var.neig5=var(yy[10:11])
var.neig6=var(yy[12:16])
var.neig7=var(yy[17:20])
var.neig8=var(yy[21:22])
var.neig9=var(yy[23:25])
var.neig10=var(yy[26:27])
xbar.neighbor=c(xbar.neig1,xbar.neig2,xbar.neig3,xbar.neig4,xbar.neig5,xbar.neig6,xbar.neig7,xbar.neig8,xbar.neig9,xbar.neig10)
var.neighbor=c(var.neig1,var.neig2,var.neig3,var.neig4,var.neig5,var.neig6,var.neig7,var.neig8,var.neig9,var.neig10)
modelneig=lm(var.neighbor~xbar.neighbor)
bobot=modelneig$coefficients[1]+(modelneig$coefficients[2]*x)
w=1/bobot
W=matrix(rep(0,(((n)^2))),(n),(n))
	for(i in 1:n){W[i,i]=w[i]}
x0=rep(1:1,n)
X=cbind(x0,x)
Y=as.matrix(y)
#WLS#
beta=solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y
betaaa=lm(y~x,weights=w)
