library(Deriv)
data<-read.csv(file.choose(),header=T) #logisticregression_data.csv
data
y<-data$y
x1<-data$x1
x2<-data$x2
inisial.b=function(x1,x2,y)
{
  y=y
  n<-length(y) 
  x=as.matrix(cbind(rep(1:1,n),data[,2],data[,3]))
  inisial.b=solve(t(x)%*%x)%*%t(x)%*%y
  print(inisial.b)
}
beta.inisialisasi=inisial.b(x1,x2,y)
GradienMethod <- function(x1,x2,y,b0,b1,b2){
n<-length(y)
beta.old<-c(b0,b1,b2)
dg1 <-1
dg2 <-1
dg3 <-1
delta<-1
iterasi <- 0
p<-numeric(n)
#function cari r (root) fungsi nonlinier metode iterasi numerik#
 root<-function(f,f1,x0,num=100,eps=1e-05,eps1=1e-05){
       a=x0
       b=a-f(a)/f1(a)
       i=0
       while((abs(b-a)>eps)||(i<num)){
       a=b
       b=a-f(a)/f1(a)
       i=i+1
       }
  	if (abs(f(b))<eps1){
  	list(b,f(b))
	}
  }
while(dg1>0.000001||dg2>0.000001||dg3>0.000001)
{
 iterasi<-iterasi+1
 b0<-beta.old[1]
 b1<-beta.old[2]
 b2<-beta.old[3]
 for (i in 1:n){
 lnL<-sum((y[i]*(b0+b1*x1[i]+b2*x2[i]))-log(1+exp(b0+b1*x1[i]+b2*x2[i])))
 p[i]<-exp(b0+b1*x1[i]+b2*x2[i])/(1+exp(b0+b1*x1[i]+b2*x2[i]))
 g1<-sum(y[i]-p[i])
 g2<-sum((y[i]-p[i])*x1[i])
 g3<-sum((y[i]-p[i])*x2[i])
 hr<-function(r) sum((y[i]*(b0+r*g1+(b1+r*g2)*x1[i]+(b2+r*g3)*x2[i]))-log(1+exp(b0+r*g1+(b1+r*g2)*x1[i]+(b2+r*g3)*x2[i])))
 }
 dhr<-Deriv(hr)
 r1<-root(hr,dhr,1)
 r<-r1[[1]]
 b0duga=g1
 b1duga=g2
 b2duga=g3
 dfb<-as.matrix(rbind(b0duga,b1duga,b2duga))
 beta.new<-beta.old+r*dfb
 dg1<-abs(g1)
 dg2<-abs(g2)
 dg3<-abs(g3)
 beta.old<-beta.new
}
list(estimasi.parameter=beta.old,iterasi=iterasi,delta.gradien=rbind(g1=g1,g2=g2,g3=g3))
}
estimasi.GradienMethod=GradienMethod(x1,x2,y,beta.inisialisasi[1],beta.inisialisasi[2],beta.inisialisasi[3])
