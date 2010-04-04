\name{na.approx}
\alias{na.approx}
\alias{na.approx.zoo}
\alias{na.approx.zooreg}
\alias{na.approx.ts}
\alias{na.approx.default}
\alias{na.spline}
\alias{na.spline.zoo}
\alias{na.spline.zooreg}
\alias{na.spline.ts}
\alias{na.spline.default}
\title{Replace NA by Interpolation}
\description{
Generic functions for replacing each \code{NA} with interpolated
values.
}
\usage{
na.approx(object, \dots) 
\method{na.approx}{zoo}(object, x = index(object), xout = x, \dots, na.rm = TRUE, \dots)
\method{na.approx}{zooreg}(object, \dots) 
\method{na.approx}{ts}(object, \dots)
\method{na.approx}{default}(object, x = index(object), xout = x, \dots, na.rm = TRUE, maxgap = Inf, along) 

na.spline(object, \dots) 
\method{na.spline}{zoo}(object, x = index(object), xout = x, \dots, na.rm = TRUE, along)
\method{na.spline}{zooreg}(object, \dots) 
\method{na.spline}{ts}(object, \dots)
\method{na.spline}{default}(object, x = index(object), xout = x, \dots, na.rm = TRUE, maxgap = Inf, along) 

}
\arguments{
  \item{object}{object in which \code{NA}s are to be replaced}
  \item{x, xout}{Variables to be used for interpolation as in \code{approx}. Coerced to numeric if they have different classes.  \code{xout} defaults to \code{x} and \code{x} defaults to \code{index(object)}. If either are functions then they are applied to \code{index(object)}.}
  \item{na.rm}{logical. Should leading \code{NA}s be removed?}
  \item{maxgap}{maximum number of consecutive \code{NA}s to
    fill. Any longer gaps will be left unchanged. Note that all methods listed
    above can accept \code{maxgap} as it is ultimately passed to the 
    \code{default} method.}
  \item{along}{deprecated.}
  \item{\dots}{further arguments passed to methods.}
}

\details{
  Missing values (\code{NA}s) are replaced by linear interpolation via
  \code{\link{approx}} or cubic spline interpolation via \code{\link{spline}},
  respectively.  

  By default the index associated with \code{object} is used
  for interpolation. Note, that if this calls \code{index.default}
  this gives an equidistant spacing \code{1:NROW(object)}. If \code{object}
  is a matrix or data.frame, the interpolation is done separately for
  each column.
}

\value{
An object in which each \code{NA} in the input object is replaced
by interpolating the non-\code{NA} values before and after it.
Leading or trailing \code{NA}s are omitted (if \code{na.rm = TRUE})
or not replaced (if \code{na.rm = FALSE}). 
}

\seealso{\code{\link{zoo}}, \code{\link{approx}}, \code{\link{na.contiguous}}, \code{\link{na.locf}}, \code{\link{na.omit}}, \code{\link{na.trim}}, \code{\link{spline}}, \code{\link[stinepack]{stinterp}}} 

\examples{

z <- zoo(c(2,NA,1,4,5,2), c(1,3,4,6,7,8))

## use underlying time scale for interpolation
na.approx(z) 
## use equidistant spacing
na.approx(z, 1:6)

# with and without na.rm = FALSE
zz <- c(NA,9,3,NA,3,2)
na.approx(zz, na.rm = FALSE)
na.approx(zz)

z <- zoo(c(11, NA, 13, NA, 15, NA), Sys.Date() + 1:6)
na.approx(z)
na.approx(z, na.rm = FALSE)
na.approx(z, xout = Sys.Date() + 1:6)
na.approx(z, xout = Sys.Date() + 2:5)
na.approx(z, xout = Sys.Date() + 7)
na.approx(z, xout = Sys.Date() + 7, na.rm = FALSE)
na.approx(z, xout = Sys.Date() + 7, rule = 2)

zm <- zoo(cbind(a = coredata(z), b = coredata(z)), Sys.Date() + 1:3)
na.approx(zm, na.rm = FALSE)
na.approx(zm, xout = Sys.Date() + 1:6)
na.approx(zm, xout = Sys.Date() + 2:5)
na.approx(zm, xout = Sys.Date() + 7)
na.approx(zm, xout = Sys.Date() + 7, na.rm = FALSE)
na.approx(zm, xout = Sys.Date() + 7, rule = 2)

zm2 <- zoo(cbind(coredata(z), coredata(z)), Sys.Date() + 1:3)
na.approx(zm2, na.rm = FALSE)
na.approx(zm2, xout = Sys.Date() + 1:6)
na.approx(zm2, xout = Sys.Date() + 2:5)
na.approx(zm2, xout = Sys.Date() + 7)
na.approx(zm2, xout = Sys.Date() + 7, na.rm = FALSE)
na.approx(zm2, xout = Sys.Date() + 7, rule = 2)

z <- zoo(c(11, NA, 13, NA, 15, NA), Sys.Date() + 1:6)
na.spline(z)
na.spline(z, na.rm = FALSE)
na.spline(z, xout = Sys.Date() + 1:6)
na.spline(z, xout = Sys.Date() + 2:5)
na.spline(z, xout = Sys.Date() + 7)
na.spline(z, xout = Sys.Date() + 7, na.rm = FALSE)

zm <- zoo(cbind(a = coredata(z), b = coredata(z)), Sys.Date() + 1:3)
na.spline(zm, na.rm = FALSE)
na.spline(zm, xout = Sys.Date() + 1:6)
na.spline(zm, xout = Sys.Date() + 2:5)
na.spline(zm, xout = Sys.Date() + 7)
na.spline(zm, xout = Sys.Date() + 7, na.rm = FALSE)

zm2 <- zoo(cbind(coredata(z), coredata(z)), Sys.Date() + 1:3)
na.spline(zm2, na.rm = FALSE)
na.spline(zm2, xout = Sys.Date() + 1:6)
na.spline(zm2, xout = Sys.Date() + 2:5)
na.spline(zm2, xout = Sys.Date() + 7)
na.spline(zm2, xout = Sys.Date() + 7, na.rm = FALSE)

## using na.approx for disaggregation

zy <- zoo(1:3,  2000:2001)

# yearly to monthly series

zmo <- na.approx(zy, xout = as.yearmon(2000+0:13/12)); zmo

# monthly to daily series

sq <- seq(as.Date(start(zmo)), as.Date(end(zmo), frac = 1), by = "day")
zd <- na.approx(zmo, x = as.Date, xout = sq); head(z3)

# weekly to daily series
zww <- zoo(1:3, as.Date("2001-01-01") + seq(0, length = 3, by = 7)); zww
zdd <- na.approx(zw, xout = seq(start(zw), end(zw), by = "day")); zdd

# using na.approx to create regularly spaced series
# z has points at 10, 20 and 40 minutes while output also has a point at 30
\dontrun{
library(chron)
tt <- as.chron("2000-01-01 10:00:00") + c(1, 2, 4) * as.numeric(times("00:10:00"))
z <- zoo(1:3, tt)
tseq <- seq(start(z), end(z), by = times("00:10:00"))
na.approx(z, xout = tseq)
}
}

\keyword{ts}
