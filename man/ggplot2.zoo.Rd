\name{ggplot2.zoo}
\alias{autoplot.zoo}
\alias{fortify.zoo}
\alias{ggplot2.zoo}
\title{Convenience functions for plotting zoo objects with ggplot2}
\usage{
  \method{fortify}{zoo} (model, data, melt = FALSE, ...)

  \method{autoplot}{zoo} (object, ..., geom = "line")
}
\arguments{
  \item{model}{Zoo object to be converted to data frame,
  \code{time(model)} must be convertible to Dates using
  \code{as.Date}}

  \item{data}{Not used (required by generic fortify
  method)}

  \item{melt}{Should \code{fortify.zoo} use reshape2 to
  melt the data in a format friendly for graphing
  multivariate series at same time}

  \item{...}{\code{fortify.zoo}: not used (required by
  generic fortify method), \code{autoplot.zoo}: passed to
  qplot}

  \item{object}{zoo object to be plotted}

  \item{geom}{which ggplot2 geom to use to plot series}
}
\value{
  \code{fortify.zoo} returns a data frame with a time
  variable equivalent to \code{as.Date(time(model))} and if
  \code{melt=FALSE}, additional columns equivalent to
  \code{coredata(model)} whereas if \code{melt=TRUE}
  additional columns value (of time series values) and
  series (factors representing the different series)

  \code{autoplot.zoo} returns a ggplot object.
}
\description{
  \code{fortify.zoo} takes a zoo object and converts it
  into a data frame (intended for ggplot2),
  \code{autoplot.zoo} takes a zoo object and returns a
  ggplot2 object.
}
\details{
  \code{autoplot.zoo} uses \code{fortify.zoo} (with
  melt=TRUE) to convert the zoo object into a data frame.
  For multivariate plots it effectively does the following
  mapping \code{aes(x=time, y=value, group=series,
  colour=series, linetype=series, shape=series)}
}
\examples{
require("zoo")
require("ggplot2")
## example data
x.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-"))
x <- zoo(rnorm(5), x.Date)
xlow <- x - runif(5)
xhigh <- x + runif(5)
z <- cbind(x, xlow, xhigh)

## univariate plotting
autoplot(x)
ggplot(aes(x=time, y=value), data=fortify(x, melt=TRUE)) + geom_line() + xlab("") + ylab("x")
last_plot() + geom_line(aes(x=time, y=xlow), colour="red", data=fortify(xlow)) # add series one at a time
ggplot(aes(x=time, y=x, ymin=xlow, ymax=xhigh), data=fortify(x)) + geom_ribbon(fill="yellow") + geom_line()

## multivariate plotting
autoplot(z)
ggplot(aes(x=time, y=value, group=series, colour=series, linetype=series), data=fortify(z, melt=TRUE)) +
      geom_line() + xlab("") + ylab("")
autoplot(z, geom="point")
autoplot(z) + geom_point()
autoplot(z) + scale_colour_grey() + theme_bw()
autoplot(z, facets = series ~ .)
autoplot(z) + facet_grid(series ~ .) # same plot, explicitly use facet_grid

## suppress automatic colours, linetypes by using geom="blank" and then explicit geoms
autoplot(z, facets = series ~ ., geom="blank") + geom_line(colour="black", linetype="solid")
}
\author{
  Trevor L Davis \email{trevor.l.davis@gmail.com}
}
\seealso{
  \code{\link[ggplot2]{fortify}}

  \code{\link[ggplot2]{autoplot}}
}

