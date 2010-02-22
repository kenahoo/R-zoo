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
\method{na.approx}{zoo}(object, along = index(object), na.rm = TRUE, \dots) 
\method{na.approx}{zooreg}(object, along = index(object), na.rm = TRUE, \dots) 
\method{na.approx}{ts}(object, \dots)
\method{na.approx}{default}(object, along = index(object), na.rm = TRUE,
          maxgap = Inf, x = time(object), \dots) 

na.spline(object, \dots) 
\method{na.spline}{zoo}(object, along = index(object), na.rm = TRUE, \dots) 
\method{na.spline}{zooreg}(object, along = index(object), na.rm = TRUE, \dots) 
\method{na.spline}{ts}(object, \dots)
\method{na.spline}{default}(object, along = index(object), na.rm = TRUE,
          maxgap = Inf, x = time(object), \dots) 
}
\arguments{
  \item{object}{object in which \code{NA}s are to be replaced}
  \item{along}{variable to use for interpolation. Has to be numeric, is
    otherwise coerced to numeric.}
  \item{na.rm}{logical. Should leading \code{NA}s be removed?}
  \item{maxgap}{maximum number of consecutive \code{NA}s to
    fill. Any longer gaps will be left unchanged. Note that all methods listed
    above can accept \code{maxgap} as it is ultimately passed to the 
    \code{default} method.}
  \item{x}{The time vector for the object. For objects with a \code{time} method
     the default of \code{time(object)} would normally be used.}
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
na.approx(z, Sys.Date() + 1:6)
na.approx(z, Sys.Date() + 2:5)
na.approx(z, Sys.Date() + 7)
na.approx(z, Sys.Date() + 7, na.rm = FALSE)
na.approx(z, Sys.Date() + 7, rule = 2)

zm <- zoo(cbind(a = coredata(z), b = coredata(z)), Sys.Date() + 1:3)
na.approx(zm, na.rm = FALSE)
na.approx(zm, Sys.Date() + 1:6)
na.approx(zm, Sys.Date() + 2:5)
na.approx(zm, Sys.Date() + 7)
na.approx(zm, Sys.Date() + 7, na.rm = FALSE)
na.approx(zm, Sys.Date() + 7, rule = 2)

zm2 <- zoo(cbind(coredata(z), coredata(z)), Sys.Date() + 1:3)
na.approx(zm2, na.rm = FALSE)
na.approx(zm2, Sys.Date() + 1:6)
na.approx(zm2, Sys.Date() + 2:5)
na.approx(zm2, Sys.Date() + 7)
na.approx(zm2, Sys.Date() + 7, na.rm = FALSE)
na.approx(zm2, Sys.Date() + 7, rule = 2)

z <- zoo(c(11, NA, 13, NA, 15, NA), Sys.Date() + 1:6)
na.spline(z)
na.spline(z, na.rm = FALSE)
na.spline(z, Sys.Date() + 1:6)
na.spline(z, Sys.Date() + 2:5)
na.spline(z, Sys.Date() + 7)
na.spline(z, Sys.Date() + 7, na.rm = FALSE)
na.spline(z, Sys.Date() + 7, rule = 2)

zm <- zoo(cbind(a = coredata(z), b = coredata(z)), Sys.Date() + 1:3)
na.spline(zm, na.rm = FALSE)
na.spline(zm, Sys.Date() + 1:6)
na.spline(zm, Sys.Date() + 2:5)
na.spline(zm, Sys.Date() + 7)
na.spline(zm, Sys.Date() + 7, na.rm = FALSE)
na.spline(zm, Sys.Date() + 7, rule = 2)

zm2 <- zoo(cbind(coredata(z), coredata(z)), Sys.Date() + 1:3)
na.spline(zm2, na.rm = FALSE)
na.spline(zm2, Sys.Date() + 1:6)
na.spline(zm2, Sys.Date() + 2:5)
na.spline(zm2, Sys.Date() + 7)
na.spline(zm2, Sys.Date() + 7, na.rm = FALSE)
na.spline(zm2, Sys.Date() + 7, rule = 2)

}
\keyword{ts}
