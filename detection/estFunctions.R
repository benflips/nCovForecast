## ---------------------------
##
## Script name: estFunctions.R
##
## Purpose of script: functions needed for estimation of cumulative infections and projections of new cases
##
## Author: Ian Marschner (using some code from the addreg package)
##
## Date Created: 2020-03-30
##
## Email: ian.marschner@ctc.usyd.edu.au
##
## ---------------------------
## ---------------------------
##

#design matrix for linear Poisson regression
design <- function(T,inc.dist) {
	prob.inc<-inc.dist[,2]
	I.max<-length(prob.inc)-1
	T.days<-T+I.max # to allow for incubation period of first cases
	F<-matrix(0,T.days,T.days)
	for (d in 1:T.days) {
		start<-max(0,(d-I.max))+1
		length.f<-length(c(start:d))
		F[d,c(start:d)]<-rev(prob.inc[1:length.f])
	}
	F
}

#estimate of cumulative number of infections
#cases is a series of cumulative daily diagnosis counts
infect.est <- function(cases,inc.dist,F,min.cases=0) {
	prob.inc<-inc.dist[,2]
	T<-length(cases)
	I.max<-length(prob.inc)-1
	T.days<-T+I.max
	Cases<-c(rep(0,I.max),diff(c(0,as.numeric(cases)))) # daily new cases
	Cases[Cases<0]<-0
	inf.est<-rep(0,T)
	if (sum(Cases)>=min.cases) {
		inf.est<-rep(-1,T)
		res<-nnpois.smooth(Cases,F,rep(1,T.days),rep(0,T.days),rep(100,T.days),control=addreg.control(epsilon=1e-07,maxit=1000000))
		if (res$conv)inf.est<-round(cumsum(res$coef))[-c(1:I.max)]
	}
	inf.est
}

#projections
project <- function(infect,inc.dist,F,low=FALSE,proj.days=5,inf.extrap=20) {
out<-rep(max(infect),proj.days)
if (max(infect) > 0) {
	prob.inc<-inc.dist[,2]
	I.max<-length(inc.dist)-1
	T<-length(infect)
	infect.extrap<-infect[(T-inf.extrap+1):T]
	y<-log(infect.extrap[infect.extrap>0])
	x<-c((T-inf.extrap+1):T)[infect.extrap>0]
	extrap.coef<-lm(y~x)$coef
	inf.linear<-c(infect,exp(extrap.coef[1]+extrap.coef[2]*c((T+1):(T+proj.days))))
	inf.low<-c(infect,rep(infect[T],proj.days))
	if ((extrap.coef[2]<0)|(inf.linear[T+1]<inf.linear[T])) inf.linear<-inf.low
	inf.daily<-diff(c(0,inf.linear))
	inf.daily.low<-diff(c(0,inf.low))
	proj<-0
	proj.low<-0
	for (i in 1:proj.days) {
		proj[i]<-sum(rev(prob.inc)*inf.daily[(T-I.max+i):(T+i)])
		proj.low[i]<-sum(rev(prob.inc)*inf.daily.low[(T-I.max+i):(T+i)])
	}
	out<-round(cumsum(proj))	
	if (low) {
		out<-cbind(round(cumsum(proj)),round(cumsum(proj.low)))
		colnames(out)<-c("projection","lower bound")
	}
}
out
}

#plot cumulative infection estimates
plot.inf <- function(cases,inc.dist,xstart=0,main=" ",xaxt="s",xlab="days since first diagnosis") {
	T=length(cases)
	designF<-design(T,inc.dist)
	inf<-infect.est(cases,inc.dist,designF,min.cases=0)
	plot(inf,xlim=c(xstart,T),cex=1.5,pch=16,
		xlab=xlab,ylab="cumulative infections",main=main,xaxt=xaxt)
	lines(inf,col=50,lwd=2)
}

#plot cumulative diagnoses and projections
plot.proj <-function(cases,inc.dist,low=FALSE,proj.days=5,inf.extrap=20,xstart=0,main=" ",xaxt="s",xlab="days since first diagnosis") {
	T=length(cases)
	designF<-design(T,inc.dist)
	inf<-infect.est(cases,inc.dist,designF,min.cases=0)
	proj<-project(inf,inc.dist,designF,low=low,proj.days=proj.days,inf.extrap=inf.extrap)
	plot(cases,xlim=c(xstart,T+proj.days),ylim=c(0,cases[T]+sum(proj[,1])),
		type="n",xlab=xlab,ylab="cumulative diagnoses",main=main,xaxt=xaxt)
	lines(c(T:(T+proj.days)),cumsum(c(cases[T],proj[,1])),col=50,lwd=2)
	if (low) lines(c(T:(T+proj.days)),cumsum(c(cases[T],proj[,2])),col=50,lwd=2,lty=3)
	points(cases,pch=16)
}

# This is a version of the nnpois function from the addreg package with a smoothing step added
nnpois.smooth <- function (y, x, standard, offset, start, control = addreg.control(), 
    accelerate = c("em", "squarem", "pem", 
        "qn"), control.method = list()) 
{
    control <- do.call("addreg.control", control)
    accelerate <- match.arg(accelerate)
    x <- as.matrix(x)
    xnames <- dimnames(x)[[2L]]
    ynames <- if (is.matrix(y)) 
        rownames(y)
    else names(y)
    if (any(x < 0)) 
        stop("x must be non-negative")
    if (any(apply(x, 2, function(col) all(col == 0)))) 
        stop("x contains column with all 0")
    nvars <- ncol(x)
    nobs <- NROW(y)
    fam <- poisson(link = identity)
    eval(fam$initialize)
    mu.eta <- fam$mu.eta
    linkinv <- fam$linkinv
    dev.resids <- fam$dev.resids
    aic <- fam$aic
    weights <- rep(1, nobs)
    if (is.null(standard)) 
        standard <- rep.int(1, nobs)
    if (is.null(offset)) 
        offset <- rep.int(0, nobs)
    if (any(offset < 0)) 
        stop("offset must be non-negative")
    converged <- FALSE
    coefold <- if (!is.null(start)) {
        if (length(start) != nvars) 
            stop(gettextf("length of 'start' should equal %d and correspond to initial coefs for %s", 
                nvars, paste(deparse(xnames), collapse = ", ")), 
                domain = NA)
        else if (any(start <= control$bound.tol)) 
            stop("'start' is on our outside the boundary of the parameter space (consider 'bound.tol')", 
                domain = NA)
        else start
    }
    else {
        simple <- mean(y/standard)/colMeans(x) + 2 * control$bound.tol
        trymat <- tryCatch(as.vector(solve(t(x) %*% x) %*% t(x) %*% 
            (y)) + 2 * control$bound.tol, error = function(e) NULL)
        if (is.null(trymat)) 
            simple
        else if (any(trymat < control$bound.tol)) 
            simple
        else trymat
    }
    fixptfn <- function(p, y, n, x, o, div, fam, bound.tol) {
        eta <- drop(x %*% p) + o
        y.over.fits <- y/fam$linkinv(eta)
        y.over.fits[fam$linkinv(eta) == 0] <- 0
        pnew <- p * colSums(y.over.fits * x) * div
        pnew[pnew <= 0] <- bound.tol/2
        return(pnew)
    }
    objfn <- function(p, y, n, x, o, div, fam, bound.tol) {
        eta <- drop(x %*% p) + o
        mu <- n * fam$linkinv(eta)
        negll <- -sum(dpois(y, mu, log = TRUE))
        return(negll)
    }
    validparams <- function(p) return(all(p >= 0))
    conv.user <- function(old, new) return(conv.test(old, new, 
        tol))
    std.div <- 1/colSums(standard * x)
    res <- turboEM::turboem(par = coefold, fixptfn = fixptfn, 
        objfn = objfn, method = accelerate, pconstr = validparams, 
        y = y, n = standard, x = x, o = offset, div = std.div, 
        fam = fam, bound.tol = control$bound.tol, control.run = list(convtype = "parameter", 
            tol = control$epsilon, stoptype = "maxiter", 
            maxiter = control$maxit, convfn.user = conv.user, 
            trace = control$trace), control.method = list(control.method))
    if (res$fail[1]) 
        stop(res$errors[1])
    coefnew <- res$pars[1, ]

#---------------------------------------------------------------
# Smoothing step:
# Simple moving average of width 9 (4 in each direction)
# At later times the moving average downweights the final time
# (because the estimate at the final time is the most unstable)
#---------------------------------------------------------------
coefnew1 <- c(coefnew[2:(length(coefnew)-1)],coefnew[(length(coefnew)-1)],coefnew[(length(coefnew)-1)])
coefnew2 <- c(coefnew[1],coefnew[1:(length(coefnew)-1)])
coefnew3 <- c(coefnew[3:(length(coefnew)-2)],coefnew[(length(coefnew)-2)],coefnew[(length(coefnew)-2)],coefnew[(length(coefnew)-2)],coefnew[(length(coefnew)-2)])
coefnew4 <- c(coefnew[1],coefnew[1],coefnew[1:(length(coefnew)-2)])
coefnew5 <- c(coefnew[4:(length(coefnew)-3)],coefnew[(length(coefnew)-3)],coefnew[(length(coefnew)-3)],coefnew[(length(coefnew)-3)],
								coefnew[(length(coefnew)-3)],coefnew[(length(coefnew)-3)],coefnew[(length(coefnew)-3)])
coefnew6 <- c(coefnew[1],coefnew[1],coefnew[1],coefnew[1:(length(coefnew)-3)])
coefnew7 <- c(coefnew[5:(length(coefnew)-4)],coefnew[(length(coefnew)-4)],coefnew[(length(coefnew)-4)],coefnew[(length(coefnew)-4)],coefnew[(length(coefnew)-4)],
								coefnew[(length(coefnew)-4)],coefnew[(length(coefnew)-4)],coefnew[(length(coefnew)-4)],coefnew[(length(coefnew)-4)])
coefnew8 <- c(coefnew[1],coefnew[1],coefnew[1],coefnew[1],coefnew[1:(length(coefnew)-4)])
coefnew<-colMeans(rbind(coefnew,coefnew1,coefnew2,coefnew3,coefnew4,coefnew5,coefnew6,coefnew7,coefnew8))

#------------------------------------
    names(coefnew) <- xnames
    eta <- drop(x %*% coefnew) + offset
    mu <- standard * linkinv(eta)
    residuals <- (y - mu)/mu.eta(eta)
    names(y) <- names(mu) <- names(eta) <- names(residuals) <- ynames
    dev.new <- sum(dev.resids(y, mu, weights))
    aic.model <- aic(y, nobs, mu, weights, dev.new) + 2 * nvars
    aic.c.model <- aic.model + 2 * nvars * (nvars + 1)/(nobs - 
        nvars - 1)
    wtdmu <- standard * sum(weights * y/standard)/sum(weights)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    nulldf <- nobs - 1
    resdf <- nobs - nvars
    boundary <- any(coefnew < control$bound.tol)
    list(coefficients = coefnew, residuals = residuals, fitted.values = mu, 
        rank = nvars, family = fam, linear.predictors = eta, 
        deviance = dev.new, aic = aic.model, aic.c = aic.c.model, 
        null.deviance = nulldev, iter = res$itr[1], weights = weights, 
        prior.weights = weights, standard = standard, df.residual = resdf, 
        df.null = nulldf, y = y, converged = res$convergence[1], 
        boundary = boundary, loglik = -res$value.objfn[1], nn.design = x)
}

# end of nnpois.smooth

