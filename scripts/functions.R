#functions for project
sumfun <- function(x, ...){
  c(m=mean(x, ...),l=length(x))
}

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
mean2 <- function(x) mean(x, na.rm=TRUE)

###3 functions (fitgam, addpoly, predline, and smooth plot) ----------------------------------------------------------

###fitgam
fitgam <- function(X,Y,dfr, k=-1, R=NULL){
  dfr$Y <- dfr[,Y]
  dfr$X <- dfr[,X]
  if(!is.null(R)){
    dfr$R <- dfr[,R]
    model <- 2
  } else model <- 1
  dfr <- droplevels(dfr)
  
  
  if(model ==1){
    g <- gam(Y ~ s(X, k=k), data=dfr)
  }
  if(model ==2){
    g <- gamm(Y ~ s(X, k=k), random = list(R=~1), data=dfr)
  }
  
  return(g)
}


####addpoly
addpoly <- function(x,y1,y2,col=scales::alpha("lightgrey",0.75),...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}


####predline
predline <- function(fit, from=NULL, to=NULL, ...){
  
  if(is.null(from))from <- min(fit$model[,2], na.rm=TRUE)
  if(is.null(to))to <- max(fit$model[,2], na.rm=TRUE)
  
  newdat <- data.frame(X = seq(from,to, length=101))
  
  nm <- names(coef(fit))
  names(newdat)[1] <- nm[length(nm)]
  
  pred <- as.data.frame(predict(fit, newdat, se.fit=TRUE, interval="confidence")$fit)
  
  addpoly(newdat[[1]], pred$lwr, pred$upr)
  ablinepiece(fit, from=from, to=to, ...)
}


#' Plot a generalized additive model
#' @param x Variable for X axis (unquoted)
#' @param y Variable for Y axis (unquoted)
#' @param data Dataframe containing x and y
#' @param kgam the \code{k} parameter for smooth terms in gam.
#' @param R An optional random effect (quoted)
#' @param log Whether to add log axes for x or y (but no transformations are done).
#' @param fitoneline Whether to fit only 
smoothplot <- function(x,y,g=NULL,data,
                       fittype=c("gam","lm"),
                       kgam=4,
                       R=NULL,
                       randommethod=c("lmer","aggregate"),
                       log="",
                       axes=TRUE,
                       fitoneline=FALSE,
                       pointcols=NULL,
                       linecols=NULL, 
                       xlab=NULL, ylab=NULL,
                       polycolor=scales::alpha("lightgrey",0.75),
                       plotit=TRUE, add=FALSE,
                       pch=16,
                       ...){
  
  fittype <- match.arg(fittype)
  randommethod <- match.arg(randommethod)
  if(log != "")require(magicaxis)
  
  if(!is.null(substitute(g))){
    data$G <- as.factor(eval(substitute(g),data))
  } else {
    fitoneline <- TRUE
    data$G <- 1
  }
  data$X <- eval(substitute(x),data)
  data$Y <- eval(substitute(y),data)
  data <- droplevels(data)
  
  data <- data[!is.na(data$X) & !is.na(data$Y) & !is.na(data$G),]
  
  if(class(data$X) == "Date"){
    xDate <- TRUE
    data$X <- as.numeric(data$X)
  } else {
    xDate <- FALSE
  }
  
  if(is.null(pointcols))pointcols <- palette()
  if(is.null(linecols))linecols <- palette()
  
  if(is.null(xlab))xlab <- substitute(x)
  if(is.null(ylab))ylab <- substitute(y)
  
  # If randommethod = aggregate, average by group and fit simple gam.
  if(!is.null(R) && randommethod == "aggregate"){
    data$R <- data[,R]
    
    data <- summaryBy(. ~ R, FUN=mean, na.rm=TRUE, keep.names=TRUE, data=data,
                      id=~G)
    R <- NULL
  }
  
  if(!fitoneline){
    
    d <- split(data, data$G)
    
    if(fittype == "gam"){
      fits <- lapply(d, function(x)try(fitgam("X","Y",x, k=kgam, R=R)))
      if(!is.null(R))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- lapply(d, function(x)lm(Y ~ X, data=x))
    }
    hran <- lapply(d, function(x)range(x$X, na.rm=TRUE))
  } else {
    if(fittype == "gam"){
      fits <- list(fitgam("X","Y",data, k=kgam, R=R))
      if(!is.null(R))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- list(lm(Y ~ X, data=data))
    }
    hran <- list(range(data$X, na.rm=TRUE))
    
  }
  
  if(plotit){
    if(xDate){
      data$X <- as.Date(data$X, origin="1970-1-1")
    }
    
    if(!add){
      with(data, plot(X, Y, axes=FALSE, pch=pch, col=pointcols[G],
                      xlab=xlab, ylab=ylab, ...))
    } else {
      with(data, points(X, Y, pch=pch, col=pointcols[G],
                        ...))
    }
    
    if(!add && axes){
      if(log=="xy")magaxis(side=1:2, unlog=1:2)
      if(log=="x"){
        magaxis(side=1, unlog=1)
        axis(2)
        box()
      }
      if(log=="y"){
        magaxis(side=2, unlog=2)
        axis(1)
        box()
      }
      if(log==""){
        if(xDate)
          axis.Date(1, data$X)
        else
          axis(1)
        axis(2)
        box()
      }
    }
    
    for(i in 1:length(fits)){
      
      if(fittype == "gam"){
        nd <- data.frame(X=seq(hran[[i]][1], hran[[i]][2], length=101))
        if(!inherits(fits[[i]], "try-error")){
          p <- predict(fits[[i]],nd,se.fit=TRUE)
          addpoly(nd$X, p$fit-2*p$se.fit, p$fit+2*p$se.fit, col=polycolor)
          lines(nd$X, p$fit, col=linecols[i], lwd=2)
        }
      }
      if(fittype == "lm"){
        pval <- summary(fits[[i]])$coefficients[2,4]
        LTY <- if(pval < 0.05)1 else 5
        predline(fits[[i]], col=linecols[i], lwd=2, lty=LTY)
      }
    }
  }
  return(invisible(fits))
}


###remkos ablinepiece function
ablinepiece <- function(a=NULL,b=NULL,reg=NULL,from,to,...){
  
  # Borrowed from abline
  if (!is.null(reg)) a <- reg
  
  if (!is.null(a) && is.list(a)) {
    temp <- as.vector(coefficients(a))
    
    if (length(temp) == 1) {
      a <- 0
      b <- temp
    }
    else {
      a <- temp[1]
      b <- temp[2]
    }
  }
  
  segments(x0=from,x1=to,
           y0=a+from*b,y1=a+to*b,...)
  
}


###redo addpoly and predline for eps with no transparency
####addpoly
addpoly2 <- function(x,y1,y2,col="lightgrey",...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}


####predline
predline2 <- function(fit, from=NULL, to=NULL, ...){
  
  if(is.null(from))from <- min(fit$model[,2], na.rm=TRUE)
  if(is.null(to))to <- max(fit$model[,2], na.rm=TRUE)
  
  newdat <- data.frame(X = seq(from,to, length=101))
  
  nm <- names(coef(fit))
  names(newdat)[1] <- nm[length(nm)]
  
  pred <- as.data.frame(predict(fit, newdat, se.fit=TRUE, interval="confidence")$fit)
  
  addpoly2(newdat[[1]], pred$lwr, pred$upr)
  ablinepiece(fit, from=from, to=to, ...)
}

