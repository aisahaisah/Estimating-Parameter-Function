soal1<-read.csv(file.choose(),header=T) #choose file soal1.csv
soal1
x<-soal1$t
x
y<-soal1$y1
y

LMsoal1 <- function(x,y,b0,b1){
beta.old <-c(b0,b1)
delta <- 1
iterasi <- 0
lambda <- 1
while(delta > 0.0000001)
{
 iterasi=iterasi+1
 b0<-beta.old[1]
 b1<-beta.old[2]
 jxx0 <- -100*exp(-b1*x)/((1+b0*exp(-b1*x))^2)
 jxx1 <- 100*x/((1+b0*exp(-b1*x))^2)
 jx <- cbind(jxx0,jxx1)
 yhat <- 100/(1+(b0*exp(-b1*x)))
 error.old <- y-yhat
 sse.old <- sum(error.old^2)
 beta.new<-beta.old+solve(t(jx)%*%jx+lambda*diag(t(jx)%*%jx))%*%t(jx)%*%error.old
 b01<-beta.new[1]
 b11<-beta.new[2]
 yhat1 <- 100/(1+(b01*exp(-b11*x)))
 error.new <- y-yhat1
 sse.new <- sum(error.new^2)
 delta <- sum((beta.old-beta.new)^2)
 if (sse.new < sse.old){
   lambda <- lambda/10
   b0=b01
   b1=b11
 }
 else {
   lambda <- lambda*10
   b0=b0
   b1=b1
 }
 beta.old<-beta.new
}
R.square = 1-(sum(error.new^2)/(sum((y-mean(yhat1))^2)))
list(estimasi.parameter=beta.old, Rsquare = R.square, iterasi=iterasi)
}
solusiLM.soal1<-LMsoal1(x,y,0.15,0.5)
