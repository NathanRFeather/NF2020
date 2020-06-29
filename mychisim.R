#' Chi-square statistic
#'
#' @description This is a chi-square statistic for a single population
#'
#' @param n1
#' @param sigma1
#' @param mean1
#' @param iter
#' @param ymax
#' @param ...
#'
#' @return A plot showing a single population chi-square statistic. This uses a histogram with a two trendlines.
#' @export
#'
#' @examples
mychisim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,...){
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)

  ssq1=apply(data1.mat,2,var)

  w=(n1-1)*ssq1/sigma1^2

  hist(w,freq=FALSE, ylim=c(0,ymax),
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
  lines(density(w),col="Red",lwd=3)
  curve(dchisq(x,n1-1),add=TRUE,col="Green",lty=2,lwd=3)
  title=expression(chi^2==frac((n[1]-1)*s^2,sigma^2))
  legend(-5,c("Simulated","Theoretical"),col=c("Red","Green"),lwd=4,lty=1:2,bty="n",title=title)
  #return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq"))
}
