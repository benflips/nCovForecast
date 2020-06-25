# 
## ---------------------------
##
## Script name: BPfunctions.R
##
## Purpose of script: 4 functions for back projection and forward projection analyses
##
## Author: Ian Marschner
##
## Date Created: 2020-06-20
##
## Email: ian.marschner@ctc.usyd.edu.au
##
## ---------------------------
## ---------------------------

# Back projection to estimate the total number of infections
BackProj <- function(cases,dist,pre.smooth=TRUE,post.smooth=TRUE,
                     pre.df=8,post.df=8,constr.final=TRUE,plot.results=FALSE,plot.start=1) {
  
  # cases is a series of cumulative cases (diagnosis numbers)
  # dist is output from function incubation and is a 2-column matrix with col1=days and col2=Pr( diagnosis - infection = days )
  # pre.smooth determines whether to smooth the cases data first
  # post.smooth determines whether to smooth the infection estimates at the end
  # pre.df is the effective degrees of freedom for the pre-smooth
  # post.df is the effective degrees of freedom for the post-smooth
  # constr.final determines whether to constrain final 2 infection estimates to be the same
  # plot.results determines whether a plot of the analyses is produced
  # plot.start is the time point to start the plots
  
  F<-design.BP(length(cases),dist)
  prob<-dist[,2]
  T<-length(cases)
  I.max<-length(prob)-1
  T.days<-T+I.max
  Cases<-c(rep(0,I.max),diff(c(0,as.numeric(cases)))) # daily new cases
  Cases[Cases<0]<-0 # just in case the data have negative diagnoses e.g. due to re-adjustments
  Cases.fit<-Cases
  if (pre.smooth) {
    pre.fit <- suppressWarnings(gam(Cases~s(1:T.days,df=pre.df),
                                    family=poisson(link="log"))$fitted.values)
    Cases.fit <- round(pre.fit) #non-integers causes problems in nnpois
  }  
  
  F.full<-F
  if (constr.final) {
    F[,(T.days-1)]<-F[,(T.days-1)]+F[,T.days]
    F<-F[,-T.days]
  } 
  cf<-as.numeric(constr.final)
  
  # linear Poisson fit (the initial estimates and control parameters are currently hardcoded) 
  res<-nnpois(Cases.fit,F,rep(1,T.days),rep(0,T.days),rep(100,(T.days-cf)),
              control=addreg.control(epsilon=1e-07,maxit=1000000))
  if (res$conv==FALSE) {
    warning("linear Poisson fit did not converge - may need more iterations")
    infections.est <- rep(0, T.days)
  } else {
    infections.est<-res$coef
  }
  if (constr.final) {
    infections.est[T.days-1]<-infections.est[T.days-1]/2
    infections.est[T.days]<-infections.est[T.days-1]
  }
  infections.est<-round(infections.est)
  if (post.smooth) infections.est <- suppressWarnings(gam(infections.est~s(1:T.days,df=post.df),
                                                          family=poisson(link="log"))$fitted.values)
  fits<-F.full%*%infections.est
  
  if (plot.results) {
    par(mfrow=c(2,1))
    ymax1<-sum(infections.est)
    ymax2<-max(c(infections.est,Cases.fit,Cases))
    plot(plot.start:T,cumsum(infections.est)[-c(1:I.max)][plot.start:T],type="l",xlab="day",ylab="cumumlative",lwd=2,main="diagnosed (black) and all (red) infections",col=50,ylim=c(0,ymax1))
    points(plot.start:T,cases[plot.start:T],pch=16)
    plot(plot.start:T,infections.est[-c(1:I.max)][plot.start:T],type="l",lwd=2,xlab="day",ylab="daily",col=50,ylim=c(0,ymax2))
    lines(plot.start:T,Cases.fit[-c(1:I.max)][plot.start:T],lwd=2)
    points(plot.start:T,Cases[-c(1:I.max)][plot.start:T],lwd=2,pch=16)
  }
  
  # output the smoothed daily case series and the estimated daily infections 
  out<-cbind(1:T,Cases[-c(1:I.max)],Cases.fit[-c(1:I.max)],fits[-c(1:I.max)],infections.est[-c(1:I.max)])
  colnames(out)<-c("day","observed","smoothed","fitted","infections")
  out
}

#-------------------------------------------------------------------------

# Forward projection to predict the number of cases in the future
ForwardProj <- function(BP,dist,proj.days=5,extrap.method="last",extrap.user=NULL,
                        plot.results=FALSE,plot.start=1) {
  
  # BP is an output object from function BackProj
  # dist is output from function incubation and is a 2-column matrix with col1=days and col2=Pr( diagnosis - infection = days )
  # proj.days is the number of days into the future to project cases
  # extrap.method is the method used to extrapolate the infection curve in BP[,5]
  #               "last" means extrapolate the last infection number 
  #               "lower" means extrapolate zero so that the projection is a lower bound
  #               "user" means extrapolate a user defined value
  # extrap.user is the user defined value if extrap.method="user" (othersise ignored)
  # plot.results determines whether a plot of the analyses is produced
  # plot.start is the time point to start the plots
  
  I.max<-length(dist[,1])-1
  T<-length(BP[,1])
  T.days<-T+I.max
  if (extrap.method=="last") extrap.inf<-rep(BP[T,5],proj.days)
  if (extrap.method=="lower") extrap.inf<-rep(0,proj.days)
  if (extrap.method=="user") extrap.inf<-extrap.user
  infections<-c(rep(0,I.max),BP[,5],extrap.inf)
  F<-design.BP(T+proj.days,dist)
  cases<-F%*%infections
  projs<-cbind(c((T+1):(T+proj.days)),cases[-c(1:T.days)])
  colnames(projs)<-c("day","forecast")
  
  if (plot.results==TRUE) {
    par(mfrow=c(2,1))
    fit.final<-cumsum(BP[,4])[T]
    projs.plot<-rbind(c(T,fit.final),projs)
    projs.plot[,2]<-cumsum(projs.plot[,2])
    plot(c(plot.start:T),cumsum(BP[,2])[plot.start:T],pch=16,ylab="cumulative cases",xlab="day",xlim=c(plot.start,T+proj.days),ylim=c(0,projs.plot[(proj.days+1),2]))
    lines(c(plot.start:T),cumsum(BP[,4])[plot.start:T],lwd=2)
    lines(projs.plot,lwd=2,col=50)
    abline(v=T)
    
    plot(c(plot.start:T),BP[plot.start:T,2],pch=16,ylab="daily cases",xlab="day",xlim=c(plot.start,T+proj.days))
    lines(c(plot.start:T),BP[plot.start:T,4],lwd=2)
    lines(projs,lwd=2,col=50)
    lines(c(T,(T+1)),c(BP[T,4],projs[1,2]),lwd=2,col=50)
    abline(v=T)
    
  }
  
  projs
}

#-----------------------------------------------------------------------

# incubation distribution
incubation <- function(mean.inc=5.2,perc95=12.5,I.max=30,mean.test=2) {
  
  # distribution of the period between infection and diagnosis, possibly including for a testing delay
  # mean = mean in days of the incubation period
  # perc95 = 95% percentile of the incubation period
  # I.max = maximum number of days for the incubation period
  # mean.test = mean delay from symptoms to diagnosis due to test result delay
  # plot.dist determines whether to plot the distribution
  
  # solve for parameters of the log-normal incubation distribution
  a<-(1/2)
  b<- -qnorm(0.95)
  c<-log(perc95/mean.inc)
  sigma<-(-b-sqrt(b**2-4*a*c))/(2*a)
  mu<-log(5.2)-a*sigma**2
  days<-seq(0,(I.max+1),.01)
  days.int<-c(1:(I.max+1))
  inc.dens<- (1/(days*sigma*sqrt(2*pi)))*exp(-(1/(2*sigma**2))*(log(days)-mu)**2)
  inc.dens[1]<-0
  inc.cdf<-pnorm((log(days)-mu)/sigma)
  inc.cdf.int<-pnorm((log(days.int)-mu)/sigma)
  prob.inc<-diff(c(0,inc.cdf.int))
  prob.inc<-prob.inc/sum(prob.inc) # just in case not exactly equal to 1
  out<-cbind((days.int-1),prob.inc)
  colnames(out)<-c("day","probability function")
  
  # convolve with testing delay
  if (mean.test>0) {
    shape=1.2
    rate=shape/mean.test
    test.dens<-dgamma(days,shape=shape,rate=rate)
    test.cdf<-pgamma(days,shape=shape,rate=rate)
    count<-0
    conv.cdf<-0
    for (d in days) {
      count<-count+1
      conv.cdf[count]<-sum(inc.dens[1:count]*test.cdf[count:1])*0.01
    }
    out<-cbind(c(0:I.max),diff(c(0,conv.cdf[seq(101,(I.max+1)*100+1,100)]))/conv.cdf[(I.max+1)*100+1])
    colnames(out)<-c("day","probability function")
  }
  
  out
}

#------------------------------------------------------------------------

# function to construct design matrix in linear model relating infections and diagnoses
design.BP <- function(T,dist) {
  
  # T = length of case series
  # dist = output from function incubation giving the distribution of time between infection and diagnosis 
  
  prob<-dist[,2]
  I.max<-length(prob)-1
  T.days<-T+I.max # to allow for incubation period of first cases
  F<-matrix(0,T.days,T.days)
  for (d in 1:T.days) {
    start<-max(0,(d-I.max))+1
    length.f<-length(c(start:d))
    F[d,c(start:d)]<-prob[length.f:1]
  }
  F
}  


