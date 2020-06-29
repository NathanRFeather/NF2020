#' T simulator
#'
#' @description This creates a histogram and two trendlines for a sampling distribution from the T distribution.
#'Single Population
#'
#' @param n1
#' @param sigma1
#' @param mean1
#' @param iter
#' @param ymax
#' @param ...
#'
#' @return A Histogram and trendlines of a T Distribution
#' @export
#'
#' @examples
myTsim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,...){
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)

  sd1=apply(data1.mat,2,sd)
  ybar=apply(data1.mat,2,mean)

  w=(ybar-mean1)/(sd1/sqrt(n1))

  hist(w,freq=FALSE, ylim=c(0,ymax),
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",T," iterations= ",iter)),
       xlab=expression(paste(T, "Statistic",sep=" ")), las=1)
  lines(density(w),col="Purple",lwd=3)
  curve(dt(x,n1-1),add=TRUE,col="Dark Orange",lty=2,lwd=3)
  title=expression(T==frac((bar(y)-mu),s/sqrt(n1)))
  legend(-5,c("Simulated","Theoretical"),col=c("Purple","Dark Orange"),lwd=4,lty=1:2,bty="n",title=title)
  #return(list(w=w,summary=summary(w),sd=sd(w),fun="T"))
}
