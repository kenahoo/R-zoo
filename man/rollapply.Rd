\name{rollapply}
\alias{rollapply}
\alias{rollapply.default}
\alias{rollapply.zoo}
\alias{rollapply.ts}
\title{Apply Rolling Functions}
\description{
  A generic function for applying a function to rolling margins of an array.
}
\usage{

rollapply(data, width, FUN, ..., by = 1, 
  by.column = TRUE, na.pad, align = c("center", "left", "right"), 
  which, rule = c("drop", "NA", "extend", "partial"))
}
\arguments{
  \item{data}{the data to be used (representing a series of observations).}
  \item{width}{number of points per group.}
  \item{FUN}{the function to be applied.
    In the case of functions like \code{+}, \code{\%*\%}, etc., the
    function name must be quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{by}{calculate FUN at every by-th time point rather than every point.}
  \item{by.column}{logical. If \code{TRUE}, \code{FUN} is applied to each column separately.}
  \item{na.pad}{logical. If \code{TRUE} 
    then additional elements or rows of \code{NA}s are added so that 
    result has same number of elements or rows as \code{data}. This argument is
    deprecated. Use \code{rule} instead.}
  \item{align}{character specifying whether the index of the result
    should be left- or right-aligned or centered (default) compared
    to the rolling window of observations.}
  \item{which}{Sequence of relative indexes. Using \code{which} is a more
    flexible alternative to using \code{width} and \code{align}.  For
    example, \code{which=seq(-1, 1)} is the same as \code{width=3} with
	\code{align="center"} and as another 
    example, \code{which=seq(-2, 0)} is the same as \code{width=3} with
	\code{align="right"}.}
  \item{rule}{This argument may be \code{"drop"}, 
	\code{"NA"}, \code{"extend"} or \code{"partial"}.  The
    numbers 0, 1, 2 and 3, respectively, may be used instead (for consistency
    with the \code{rule} argument in \code{approx}.  \code{"drop"} causes
    endpoints to be dropped, \code{"NA"} causes additional \code{NA} rows or
    elements to be appended, \code{"extend"} causes the first and last computed
    points to be extended (this option is not  yet implemented) and \code{"partial"}
    then at the ends partials sets less than \code{width} are used.}
}
\details{
  Groups time points in successive sets of \code{width} time points and 
  applies \code{FUN} to the corresponding values.   If \code{FUN} is
  \code{mean}, \code{max} or \code{median} and \code{by.column} is 
  \code{TRUE} and there are no extra arguments
  then special purpose code is used to enhance performance.
  Also in the case of \code{mean} such special purpose code is only 
  invoked if the
  \code{data} argument has no \code{NA} values.
  See \code{\link{rollmean}}, \code{\link{rollmax}} and \code{\link{rollmedian}}
  for more details.
  
  Currently, there are methods for \code{"zoo"} and \code{"ts"} series
  and \code{"default"} method for ordinary vectors and matrices.
  
  In much older versions of the zoo package, 
  this function was called \code{rapply}. It was renamed
  because from \R 2.4.0 on, base \R provides a different function \code{rapply}
  for recursive (and not rolling) application of functions.
}
\value{
  A object of the same class as \code{data} with the results of the rolling function.
}

\seealso{\code{\link{rollmean}}}

\examples{
## rolling mean
z <- zoo(11:15, as.Date(31:35))
rollapply(z, 2, mean)

## non-overlapping means
z2 <- zoo(rnorm(6))
rollapply(z2, 3, mean, by = 3)      # means of nonoverlapping groups of 3
aggregate(z2, c(3,3,3,6,6,6), mean) # same

## optimized vs. customized versions
rollapply(z2, 3, mean)   # uses rollmean which is optimized for mean
rollmean(z2, 3)          # same
rollapply(z2, 3, (mean)) # does not use rollmean

## use of which - same as last one
rollapply(z2, FUN = mean, which = seq(-1, 1))

## rolling regression:
## set up multivariate zoo series with
## number of UK driver deaths and lags 1 and 12
seat <- as.zoo(log(UKDriverDeaths))
time(seat) <- as.yearmon(time(seat))
seat <- merge(y = seat, y1 = lag(seat, k = -1),
  y12 = lag(seat, k = -12), all = FALSE)

## run a rolling regression with a 3-year time window
## (similar to a SARIMA(1,0,0)(1,0,0)_12 fitted by OLS)
fm <- rollapply(seat, width = 36,
  FUN = function(z) coef(lm(y ~ y1 + y12, data = as.data.frame(z))),
  by.column = FALSE, align = "right")

## plot the changes in coefficients
plot(fm)
## showing the shifts after the oil crisis in Oct 1973
## and after the seatbelt legislation change in Jan 1983

## different values of rule argument
z <- zoo(c(NA, NA, 2, 3, 4, 5, NA))
rollapply(z, 3, sum, na.rm = TRUE)
rollapply(z, 3, sum, na.rm = TRUE, rule = "drop")
rollapply(z, 3, sum, na.rm = TRUE, rule = "NA")
## rollapply(z, 3, sum, na.rm = TRUE, rule = "extend") # not yet implemented
rollapply(z, 3, sum, na.rm = TRUE, rule = "partial")


}
\keyword{iteration}
\keyword{array}
\keyword{ts}
