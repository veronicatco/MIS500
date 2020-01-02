movie_data<-read.csv('HollywoodsMostProfitableStories.csv')
ascrfit<-lm(Worldwide.Gross~Audience..score..,data=movie_data)
summary(ascrfit)

plot(movie_data$Audience..score..,movie_data$Worldwide.Gross,
     xlab='Audience score',ylab='Worldwide gross (millions of $)')
abline(ascrfit,lwd=2)
legend('topleft',legend=c('linear model'),lwd=c(2))

###############################################################
#extra code, not used in report

#determine max and min audience score
min_scr<-min(movie_data$Audience..score..,na.rm=TRUE)
max_scr<-max(movie_data$Audience..score..,na.rm=TRUE)

#determine confidence and prediction intervals
xseq<-data.frame(Audience..score..=seq(min_scr,max_scr,length=100))
ci.band<-predict(ascrfit,newdata=xseq,interval='confidence',
                 level=.95)
pi.band<-predict(ascrfit,newdata=xseq,interval='prediction',
                 level=.95)

#add confidence interval to plot
lines(xseq[,1],ci.band[,2],lty=2)
lines(xseq[,1],ci.band[,3],lty=2)
#add prediction interval to plot
lines(xseq[,1],pi.band[,2],lty=2,col='gray')
lines(xseq[,1],pi.band[,3],lty=2,col='gray')

#residuals normal?
hist(ascrfit$residuals)

qqnorm(ascrfit$residuals)
qqline(ascrfit$residuals)

with(ascrfit,{
  plot(fitted.values,residuals)
  points(c(min(fitted.values),max(fitted.values)),
         c(0,0),type='l')})
