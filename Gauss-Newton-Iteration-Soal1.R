soal1<-read.csv(file.choose(),header=T) #choose file soal1.csv
soal1
x<-soal1$t
x
y<-soal1$y1
y
GNsoal1 <- function(x,y,b0,b1){
beta.old <-c(b0,b1)
delta <- 1
iterasi <- 0
while(delta > 0.0000001)
{
 iterasi=iterasi+1
 b0<-beta.old[1]
 b1<-beta.old[2]
 jxx0 <- -100*exp(-b1*x)/((1+b0*exp(-b1*x))^2)
 jxx1 <- 100*x/((1+b0*exp(-b1*x))^2)
 jx <- cbind(jxx0,jxx1)
 yhat <- 100/(1+(b0*exp(-b1*x)))
 error <- y-yhat
 beta.new<-beta.old+solve(t(jx)%*%jx)%*%t(jx)%*%error
 delta <- sum((beta.old-beta.new)^2)
 beta.old<-beta.new
}
R.square = 1-(sum(error^2)/(sum((y-mean(yhat))^2)))
list(estimasi.parameter=beta.old, Rsquare = R.square, iterasi=iterasi)
}
solusi.soal1<-GNsoal1(x,y,0.1,0.2)
