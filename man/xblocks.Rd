\name{xblocks}
\alias{xblocks}
\alias{xblocks.default}
\alias{xblocks.ts}
\alias{xblocks.zoo}
\title{
  Plot contiguous blocks along x axis.
}
\description{
  Plot contiguous blocks along x axis. A typical use would be to
  highlight events or periods of missing data.
}
\usage{
xblocks(x, ...)

\S3method{xblocks}{default}(x, y, ..., col = NULL, border = NA, 
        ybottom = par("usr")[3], ytop = ybottom + height,
        height = diff(par("usr")[3:4]),
        last.step = median(diff(tail(x))))

\S3method{xblocks}{ts}(x, y = NULL, ...)
}
\arguments{
  \item{x, y}{
    In the default method, \code{x} gives the ordinates along the x axis and
    must be in increasing order. \code{y} gives the color values to plot as
    contiguous blocks. If \code{y} is numeric, data coverage is plotted,
    by converting it into a logical (\code{!is.na(y)}). Finally, if
    \code{y} is a function, it is applied to \code{x} (\code{time(x)} in
    the time series methods). 
    
    If \code{y} has character (or factor) values, these are interpreted
    as colors -- and should therefore be color names or hex
    codes. Missing values in \code{y} are not plotted. The default color
    is taken from \code{palette()[1]}. If \code{col} is given, this
    over-rides the block colors given as \code{y}.

    The \code{ts} method plots values against their \code{time()},
    or \code{y} against \code{time(x)} if \code{y} is given.
  }
  \item{\dots}{
    In the default method, further arguments are graphical parameters
    passed on to \code{\link[grid]{gpar}}.
  }
  \item{col}{
    if \code{col} is specified, it determines the colors of the blocks
    defined by \code{y}. If multiple colors are specified they will be
    repeated to cover the total number of blocks.
  }
  \item{border}{
    border color.
  }
  \item{ybottom, ytop, height}{
    y axis position of the blocks. The default it to fill the whole plot
    region, but by setting these values one can draw blocks along the
    top of bottom of the plot. Note that \code{height} is not used
    directly, it only sets the default value of \code{ytop}.
  }
  \item{last.step}{
    width (in native units) of the final block. Defaults to the median of
    the last 5 time steps (assuming steps are regular).
  }
}
\details{
  Blocks are drawn forward in "time" from the specified x locations,
  up until the following value. Contiguous blocks are calculated using
  \code{\link{rle}}.
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\seealso{
  \code{\link[latticeExtra]{panel.xblocks}},
  \code{\link{rect}}
}
\examples{
## example time series:
set.seed(0)
flow <- ts(filter(rlnorm(200, mean = 1), 0.8, method = "r"))

## highlight values above and below thresholds.
## this draws on top using semi-transparent colors.
plot(flow)
xblocks(flow > 30, col = "#ff000044") ## high values red
xblocks(flow < 15, col = "#0000ff44") ## low value blue
xblocks(flow >= 15 & flow <= 30, col = "#88888844") ## grey

## same thing:
plot(flow)
xblocks(time(flow), cut(flow, c(0,15,30,Inf),
  labels = c("#0000ff44", "#88888844", "#ff000044")))

## another approach is to plot blocks underneath without transparency.
plot(flow)
## note that 'ifelse' keeps its result as class 'ts'
xblocks(ifelse(flow < mean(flow), "grey", "yellow"))
## need to redraw data series on top:
lines(flow)
box()

## for single series only: plot.default has a panel.first argument
plot(time(flow), flow, type = "l",
  panel.first = xblocks(flow > 20, col = "lightgrey"))
## (see also the 'panel' argument for use with multiple series, below)

## insert some missing values
flow[c(1:10, 50:80, 100)] <- NA

## the default plot shows data coverage
## (most useful when displaying multiple series, see below)
plot(flow)
xblocks(flow)

## can also show gaps:
plot(flow, type = "s")
xblocks(time(flow), is.na(flow), col = "hotpink")

## Example of alternating colors, here showing calendar months
flowdates <- Sys.Date() + as.vector(time(flow))
flowz <- zoo(coredata(flow), flowdates)
plot(flowz)
xblocks(flowz, months, ## i.e. months(time(flowz)),
  col = c("lightgrey", "#e6e6e6"), border = "darkgrey")
lines(flowz)

## Example of multiple series.
## set up example data
z <- ts(cbind(A = 0:5, B = c(6:7, NA, NA, 10:11), C = c(NA, 13:17)))

## show data coverage only (highlighting gaps)
plot(z, panel = function(x, ...)
  xblocks(x, col = "darkgrey"))

## draw gaps in red
plot(z, type = "s", panel = function(x, ...) {
  xblocks(time(x), is.na(x), col = "red")
  lines(x, ...); points(x)
})

## Example of overlaying blocks from a different series.
## Are US presidential approval ratings linked to sunspot activity?
## Set block height to plot blocks along the bottom.
plot(presidents)
xblocks(sunspot.year > 50, height = 3)
}
\keyword{ dplot }
