
# posted by Trevor L. Davis in ggplot2 discussion group:
# http://groups.google.com/d/msg/ggplot2/YfpSEqsKlDU/toKlGB1x910J

autoplot.zoo <- function(x, ...) { 
    if (is.null(ncol(x))) { 
        qplot(as.Date(time(x)), coredata(x), geom="line") + xlab("") + ylab(substitute(x))
    } else if(ncol(x) == 1) {
        qplot(as.Date(time(x)), as.vector(coredata(x)), geom="line") + xlab("") + ylab(substitute(x))
    } else {
        df <- data.frame(time=as.Date(time(x)), coredata(x))
        dfm <- reshape2::melt(df, id.vars="time", variable.name="series")
        qplot(time, value, data=dfm, geom="line", group=series, colour=series, linetype=series)
    }
}


