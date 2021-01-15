##' Collect sam forecast objects 
##' @method c sam
##' @param  ... one or more sam forecasts 
##' @importFrom graphics par
##' @details ...
##' @export
c.samforecast<-function(...){
    ret<-list(...)
    class(ret)<-"forecastset"
    ret
}