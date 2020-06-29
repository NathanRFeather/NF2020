#' Chi-Squared Statistic: two populations
#'
#' @description This is a chi-square statistic for two populations
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
#' @return A plot showing a two populations chi-square statistic. This uses a histogram with a two trendlines.
#' @export
#'
#' @examples
mychisim2<-function(n1=10,n2=14,sigma1=3,sigma2=3,mean1=5,mean2=10,iter=1000,ymax=0.07,...){
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)
  y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)
  data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
  ssq1=apply(data1.mat,2,var)
  ssq2=apply(data2.mat,2,var)
  spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2)
  w=(n1+n2-2)*spsq/(sigma1^2)
  hist(w,freq=FALSE, ylim=c(0,ymax),
       main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
  lines(density(w),col="Blue",lwd=3)
  curve(dchisq(x,n1+n2-2),add=TRUE,col="Red",lty=2,lwd=3)
  title=expression(chi^2==frac((n[1]+n[2]-2)*S[p]^2,sigma^2))
  legend(-5,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)
  #return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq"))
}
