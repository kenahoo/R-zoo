\name{ggplot2.zoo}
\alias{autoplot.zoo}
\alias{fortify.zoo}
\alias{ggplot2.zoo}

\title{Convenience Functions for Plotting zoo Objects with ggplot2}

\details{
  Convenience interface for visualizing zoo objects with ggplo2.
  \code{autoplot.zoo} uses \code{fortify.zoo} (with \code{melt = TRUE})
  to convert the zoo object into a data frame and then uses a suitable
  \code{aes()} mapping to visiualize the series.
}

\usage{
  \method{autoplot}{zoo}(object, \dots, geom = "line")
  \method{fortify}{zoo}(model, data, melt = FALSE, \dots)
}

\arguments{
  \item{object}{an object of class \code{"zoo"}.}
  \item{\dots}{further arguments passed to \code{\link[ggplot2]{qplot}}
    for \code{autplot} (and not used for \code{fortify}).}
  \item{geom}{character specifying which \code{geom} to use
    in \code{qplot}.}  
  \item{model}{an object of class \code{"zoo"} to be converted to
    a \code{"data.frame"}. The associated time index \code{time(model)}
    must be convertible to dates using \code{as.Date}.}
  \item{data}{not used (required by generic \code{\link[ggplot2]{fortify}}
    method).}
  \item{melt}{Should \code{fortify} use reshape2 to melt the data in a
    format friendly for graphing multivariate series at the same time.}
}

\description{
  \code{fortify.zoo} takes a zoo object and converts it into a data frame
  (intended for ggplot2). \code{autoplot.zoo} takes a zoo object and returns a
  ggplot2 object. For multivariate series it effectively uses the following mapping
  \code{aes(x = time, y = value, group = series, colour = series, linetype = series, shape = series)}.
}

\value{
  \code{fortify.zoo} returns a \code{data.frame} either in long format
  (\code{melt = TRUE}) or in wide format (\code{melt = FALSE}). The
  long format has three columns: the \code{time} index, a
  factor indicating the \code{series}, and the corresponding \code{value}.
  The wide format simply has the \code{time} index plus all columns
  of \code{coredata(model)}.
  
  \code{autoplot.zoo} returns a \code{ggplot} object.
}

\author{
  Trevor L Davis \email{trevor.l.davis@gmail.com}, Achim Zeileis
}

\seealso{
  \code{\link[ggplot2]{autoplot}}, \code{\link[ggplot2]{fortify}}, \code{\link[ggplot2]{qplot}}
}

\examples{
if(require("ggplot2")) {
## example data
x.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-"))
x <- zoo(rnorm(5), x.Date)
xlow <- x - runif(5)
xhigh <- x + runif(5)
z <- cbind(x, xlow, xhigh)

## univariate plotting
autoplot(x)
ggplot(aes(x = time, y = value), data = fortify(x, melt = TRUE)) + geom_line() + xlab("") + ylab("x")
last_plot() + geom_line(aes(x = time, y = xlow), colour = "red", data = fortify(xlow)) # add series one at a time
ggplot(aes(x = time, y = x, ymin = xlow, ymax = xhigh), data = fortify(x)) + geom_ribbon(fill = "darkgray") + geom_line()

## multivariate plotting
autoplot(z)
ggplot(aes(x = time, y = value, group = series, colour = series, linetype = series), data = fortify(z, melt = TRUE)) +
  geom_line() + xlab("") + ylab("")
autoplot(z, geom = "point")
autoplot(z) + geom_point()
autoplot(z) + scale_colour_grey() + theme_bw()
autoplot(z, facets = series ~ .)
autoplot(z) + facet_grid(series ~ .) # same plot, explicitly use facet_grid

## suppress automatic colours, linetypes by using geom="blank" and then explicit geoms
autoplot(z, facets = series ~ ., geom = "blank") + geom_line(colour = "black", linetype = "solid")
}
}
