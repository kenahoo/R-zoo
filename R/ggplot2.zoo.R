# Copyright (c) 2012 Trevor L. Davis
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' Convenience functions for plotting zoo objects with ggplot2
#'
#' \code{fortify.zoo} takes a zoo object and converts it into a data frame (intended for ggplot2), 
#' \code{autoplot.zoo} takes a zoo object and returns a ggplot2 object.
#' @name ggplot2.zoo
#' @aliases fortify.zoo
#' @param model Zoo object to be converted to data frame, \code{time(model)} must be convertible to Dates using \code{as.Date}
#' @param data Not used (required by generic fortify method)
#' @param melt Should \code{fortify.zoo} use reshape2 to melt 
#'            the data in a format friendly for graphing multivariate series at same time
#' @param ... \code{fortify.zoo}: not used (required by generic fortify method), \code{autoplot.zoo}: passed to qplot
#' @return \code{fortify.zoo} returns a data frame with a time variable equivalent to \code{as.Date(time(model))}
#'    and if \code{melt=FALSE}, additional columns equivalent to \code{coredata(model)} whereas
#'    if \code{melt=TRUE} additional columns value (of time series values) and series (factors representing the different series)
#' @method fortify zoo
#' @export fortify.zoo
#' @seealso \code{\link[ggplot2]{fortify}}
#' @examples
#' require("zoo")
#' require("ggplot2")
#' ## example data
#' x.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-"))
#' x <- zoo(rnorm(5), x.Date)
#' xlow <- x - runif(5)
#' xhigh <- x + runif(5)
#' z <- cbind(x, xlow, xhigh)
#' 
#' ## univariate plotting
#' autoplot(x)
#' ggplot(aes(x=time, y=value), data=fortify(x, melt=TRUE)) + geom_line() + xlab("") + ylab("x") 
#' last_plot() + geom_line(aes(x=time, y=xlow), colour="red", data=fortify(xlow)) # add series one at a time
#' ggplot(aes(x=time, y=x, ymin=xlow, ymax=xhigh), data=fortify(x)) + geom_ribbon(fill="yellow") + geom_line()
#' 
#' ## multivariate plotting
#' autoplot(z)
#' ggplot(aes(x=time, y=value, group=series, colour=series, linetype=series), data=fortify(z, melt=TRUE)) + 
#'       geom_line() + xlab("") + ylab("")
#' autoplot(z, geom="point") 
#' autoplot(z) + geom_point()
#' autoplot(z) + scale_colour_grey() + theme_bw()
#' autoplot(z, facets = series ~ .) 
#' autoplot(z) + facet_grid(series ~ .) # same plot, explicitly use facet_grid
#'
#' ## suppress automatic colours, linetypes by using geom="blank" and then explicit geoms
#' autoplot(z, facets = series ~ ., geom="blank") + geom_line(colour="black", linetype="solid") 
#' @rdname ggplot2.zoo
#' @author Trevor L Davis \email{trevor.l.davis@@gmail.com}
fortify.zoo <- function(model, data, melt=FALSE, ...) {
    df <- data.frame(time=as.Date(time(model)), coredata(model))
    if(ncol(df) == 2) {
        names(df) <- c("time", substitute(model))
    } 
    if (melt) {
        df <- reshape2::melt(df, id.vars="time", variable.name="series")
    }
    return(df)
}

#' @param object zoo object to be plotted
#' @param geom  which ggplot2 geom to use to plot series
#' @rdname ggplot2.zoo
#' @return \code{autoplot.zoo} returns a ggplot object.
#' @method autoplot zoo
#' @export autoplot.zoo
#' @details  \code{autoplot.zoo} uses \code{fortify.zoo} (with melt=TRUE) to convert the zoo object into a data frame.
#'        For multivariate plots it effectively does the following mapping
#'        \code{aes(x=time, y=value, group=series, colour=series, linetype=series, shape=series)}
#' @seealso \code{\link[ggplot2]{autoplot}}
autoplot.zoo <- function(object, ..., geom="line") {
    require("ggplot2")
    dfm <- fortify.zoo(object, melt=TRUE)
    if(nlevels(dfm$series) == 1)
        gg <- qplot(time, value, data=dfm, geom=geom, ...) + ylab(substitute(object)) + xlab("")
    else
        gg <- qplot(time, value, data=dfm, group=series, colour=series, 
            linetype=series, shape=series, geom=geom, ...) + ylab("") + xlab("")
    return(gg)
}
