library(MASS)
data<-read.csv(file.choose(),header=T) #choose logisticregression_data.csv
data
y<-data$y
y
x1<-data$x1
x1
x2<-data$x2
x2
inisial.b=function(x1,x2,y)
{
  y=y
  n<-length(y) 
  x=as.matrix(cbind(rep(1:1,n),data[,2],data[,3]))
  inisial.b=solve(t(x)%*%x)%*%t(x)%*%y
  print(inisial.b)
}
beta.inisialisasi=inisial.b(x1,x2,y)
NewtonRaphson <- function(data,inisial.b){
x<-as.matrix(cbind(rep(1:1,n),data[,2],data[,3]))
x1<-x[,2]
x2<-x[,3]
y<-as.matrix(data[,1])
n<-length(y)
beta.old<-inisial.b
p=ncol(x)
beta.new<-numeric(p)
delta <- 1
iterasi <- 0
pi<-numeric(n)
while(delta > 0.0000000001)
{
 iterasi<-iterasi+1
 pi<-exp(x%*%beta.old)/(1+exp(x%*%beta.old))
 for (i in 1:n){
 g1<-sum(y[i]-pi[i])
 g2<-sum((y[i]-pi[i])*x1[i])
 g3<-sum((y[i]-pi[i])*x2[i])
 h11<-(-sum(pi[i]-(1-pi[i])))
 h12<-(-sum(x1[i]*(pi[i]-(1-pi[i]))*x1[i]))
 h13<-(-sum(x2[i]*(pi[i]-(1-pi[i]))*x2[i]))
 h21<-h12
 h22<-(-sum((x1[i]^2)*(pi[i]-(1-pi[i]))*(x1[i]^2)))
 h23<-(-sum(x1[i]*x2[i]*(pi[i]-(1-pi[i]))*x1[i]*x2[i]))
 h31<-h13
 h32<-h23
 h33<-(-sum((x2[i]^2)*(pi[i]-(1-pi[i]))*(x2[i]^2)))
 }
 gx<-rbind(g1,g2,g3)
 hx<-rbind(c(h11,h12,h13),c(h21,h22,h23),c(h31,h32,h33))
 beta.new<-beta.old-ginv(hx)%*%gx
 delta<-sum((beta.old-beta.new)^2)
 beta.old<-beta.new
}
list(estimasi.parameter=rbind(b0duga=beta.old[1],b1duga=beta.old[2],b2duga=beta.old[3]),iterasi=iterasi,delta=delta)
}
estimasi.NewtonRaphson=NewtonRaphson(data,beta.inisialisasi)
