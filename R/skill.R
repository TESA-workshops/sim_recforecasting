##' skill of forecasts
##' @param x sam forecast or forecast set
##' @param what variable (ssb, rec, fbar, catch)
##' @param average logical. if false only the final year will be used
##' @param trans function to be applied before skill is estimated
##' @param data logical. return data used to calculate skill
##' @export
##' @details Gives statistics of skill (see below) when forecasts were performed over the estimated time period. When two forecasts are provided, they are compared with each other
#' \itemize{
#'   \item mase - mean absolute scaled error
#'   \item mape - mean absolute percentage error
#'   \item mpe - mean percentage error
#'   \item smape - symmetric mean absolute percentage error
#'   \item cor - correlation
#' }
skill <- function(x, what=c('ssb','rec','fbar','catch'),average=FALSE,metric=c('mape','mase','mpe','smape','cor'),trans=function(x)x, data = TRUE){
    UseMethod("skill")
}

##' @rdname skill
##' @method skill samforecast
##' @export
skill.samforecast <- function(x, what=c('ssb','rec','fbar','catch'),average=FALSE,metric=c('mape','mpe','mase','smape','cor'),trans=function(x)x, data = TRUE){
    
    if(missing(x)) stop('forecast?')
    what <- match.arg(what)
    metric <- match.arg(metric)
    
    mpe <- function(x,xref){mean((xref-x)*100/xref)}
    mape <- function(x,xref){mean(abs((xref-x)*100/xref))}
    smape <- function(x,xref){mean((2*abs((x-xref)))*100/(abs(x)+abs(xref)))}
    mase <- function (x,xref){
        start <- 2
        n <- length(xref)
        end <- n - 1
        esum <- sum(abs((xref-x)))
        enaive <- sum(abs((xref[start:n]- xref[1:end])))
        esum*100/(n * enaive/end)
    }
    
    fit <- attr(x,'fit')
    idx <- tolower(names(fit$sdrep$value))==paste0('log',ifelse(what=='rec','r',what) )   # naming fit and forecast is not consistent
    est <- exp(fit$sdrep$value[idx])
    estci <- est*exp(fit$sdrep$sd[idx]%o%c(-2,2))
    
    proj <- attr(x,'tab')
    idx <- gsub(":.*","",colnames(proj))==what
    cast <- proj[,idx]

    m <- merge(cbind(year=fit$data$years,
                     est=est),
                     #estlow=estci[,1],
                     #esthigh=estci[,2],
               cbind(year=as.numeric(rownames(cast)),
                     cast=cast[,1],
                     castlow=cast[,2],
                     casthigh=cast[,3]),all=T)
    m[,-1] <- trans(m[,-1])
    
    dat <- na.omit(m)[-1,]                                                  
    if(nrow(dat)==0) stop('forecast does not overlap with estimates')
    
    if(!average) d <- tail(dat,1) else d <- dat
    
    ret <- sapply(metric,function(x){f <- get(x);with(d,f(cast,est))})
    attr(ret,'what') <- what
    attr(ret,'average') <- average
    if(data) attr(ret,'data') <- dat
    return(ret)
}

##' @rdname skill
##' @method skill forecastset
##' @export
skill.forecastset <- function(x, what=c('ssb','rec','fbar','catch'),average=FALSE,metric=c('mape','mpe','mase','smape','cor'),trans=function(x)x, data = TRUE){
    if(missing(x)) stop('forecast?')
    if(length(x)!=2) stop('Need two forecasts')
    what <- match.arg(what)
    metric <- match.arg(metric)
    
    mpe <- function(x,xref){mean((xref-x)*100/xref)}
    mape <- function(x,xref){mean(abs((xref-x)*100/xref))}
    smape <- function(x,xref){mean((2*abs((x-xref)))*100/(abs(x)+abs(xref)))}
    mase <- function (x,xref){
        start <- 2
        n <- length(xref)
        end <- n - 1
        esum <- sum(abs((xref-x)))
        enaive <- sum(abs((xref[start:n]- xref[1:end])))
        esum*100/(n * enaive/end)
    }
    
    m <- lapply(1:2,function(y){
        proj <- attr(x[[y]],'tab')
        idx <- gsub(":.*","",colnames(proj))==what
        cast <- proj[,idx]
        cast <- cbind(year=as.numeric(rownames(cast)),v=cast[,grep("median",colnames(cast))])
        colnames(cast)[2] <- paste0('f',y)
        return(cast)
    })
    dat <- Reduce(merge,m)
    dat[,-1] <- trans(dat[,-1])
                                              
    if(nrow(dat)==0) stop('forecasts do not overlap')
    
    if(!average) d <- tail(dat,1) else d <- dat
    
    ret <- sapply(metric,function(x){f <- get(x);with(d,f(f1,f2))})
    attr(ret,'what') <- what
    attr(ret,'average') <- average
    if(data) attr(ret,'data') <- dat
    return(ret)}




