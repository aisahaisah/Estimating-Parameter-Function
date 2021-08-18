soal2<-read.csv(file.choose(),header=T) #choose file soal2.csv
soal2
x1<-soal2$xx1
x2<-soal2$xx2
x3<-soal2$xx3
y<-soal2$y2

GNsoal2 <- function(x1,x2,x3,y,b0,b1,b2,b3){
beta.old <-c(b0,b1,b2,b3)
delta <- 1
iterasi <- 0
while(delta > 0.0000001)
{
 iterasi=iterasi+1
 b0<-beta.old[1]
 b1<-beta.old[2]
 b2<-beta.old[3]
 b3<-beta.old[4]
 jxx0 <- (x1^b1)*(x2^b2)*(x3^b3)
 jxx1 <- log(x1)*b0*(x1^b1)*(x2^b2)*(x3^b3)
 jxx2 <- log(x2)*b0*(x1^b1)*(x2^b2)*(x3^b3)
 jxx3 <- log(x3)*b0*(x1^b1)*(x2^b2)*(x3^b3)
 jx <- cbind(jxx0,jxx1,jxx2,jxx3)
 yhat <- b0*(x1^b1)*(x2^b2)*(x3^b3)
 error <- y-yhat
 beta.new<-beta.old+solve(t(jx)%*%jx)%*%t(jx)%*%error
 delta <- sum((beta.old-beta.new)^2)
 beta.old<-beta.new
}
R.square = 1-(sum(error^2)/(sum((y-mean(yhat))^2)))
list(estimasi.parameter=beta.old, Rsquare = R.square, iterasi=iterasi)
}
solusiGN.soal2<-GNsoal2(x1,x2,x3,y,0.3,0.3,0.3,0.3)
