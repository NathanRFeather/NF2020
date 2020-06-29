#' T-Simulation: two populations
#'
#' @description This creates a histogram and two trendlines for a sampling distribution from the T distribution.
#'Two populations
#'
#' @param n1
#' @param n2
#' @param sigma1
#' @param sigma2
#' @param mean1
#' @param mean2
#' @param iter
#' @param ymax
#' @param ...
#'
#' @return A Histogram and trendlines of a T Distribution
#' @export
#'
#' @examples
myTsim2<-function(n1=10,n2=14,sigma1=3,sigma2=3,mean1=5,mean2=10,iter=1000,ymax=0.5,...){
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)
  y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)
  data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
  ssq1=apply(data1.mat,2,var)
  ybar1= apply(data1.mat,2,mean)
  ssq2=apply(data2.mat,2,var)
  ybar2=apply(data2.mat,2,mean)
  spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2)
  w=((ybar1-ybar2)-(mean1-mean2))/sqrt(spsq*(1/n1+1/n2))
  hist(w,freq=FALSE, ylim=c(0,ymax),
       main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",T)),
       xlab=paste(" T Statistic",sep=""), las=1)
  lines(density(w),col="Cyan",lwd=3)
  curve(dt(x,n1+n2-2),add=TRUE,col="Orange",lty=2,lwd=3)
  title=expression(T==frac((bar(Y)[1]-bar(Y)[2])-(mu[1]-mu[2]),S[p]*sqrt(frac(1,n[1])+frac(1,n[2]))))
  legend(2,0.2,c("Simulated","Theoretical"),col=c("Cyan","Orange"),lwd=4,lty=1:2,bty="n",title=title)
  #return(list(w=w,summary=summary(w),sdw=sd(w),fun="T")) # some output to use if needed
}
