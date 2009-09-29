\name{as.multitime}
\alias{multitime}
\alias{as.multitime}
\alias{as.multitime.default}
\alias{coredata.multitime}
\alias{index.multitime}
\alias{as.character.multitime}
\alias{MATCH.multitime}
\alias{ORDER.multitime}
\alias{xtfrm.multitime}
\alias{Ops.multitime}
\alias{"[.multitime"}
\alias{c.multitime}
\alias{as.numeric.multitime}
\alias{as.Date.multitime}
\alias{as.yearmon.multitime}
\alias{as.yearqtr.multitime}
\alias{make.unique.multitime}
\title{ A time object with multiple time scales. }
\description{
  Multitime objects have a primary time scale and a secondary time scale.
  The latter can be used to distinguish between duplicates in the primary
  time scale or provide an alternate representation such as specific dates
  that correspond to year/months.
}
\usage{
multitime(x, ...)

as.multitime(x, ...)
\method{as.multitime}{default}(x, index, ...)
}
\arguments{
  \item{x}{ Primary time scale. }
  \item{index}{ Optional secondary time scale. }
  \item{\dots}{ Other arguments passed to individual methods. }
}
\details{
Creates an object of class \code{multitime} which has a primary and
possibly secondary time scale.  Methods provided are: \code{as.multitime}, 
\code{as.multitime.default}, \code{coredata.multitime}, \code{index.multitime}, 
\code{as.character.multitime}, \code{MATCH.multitime}, \code{ORDER.multitime}, 
\code{xtfrm.multitime}, \code{Ops.multitime}, \code{[.multitime} 
\code{c.multitime}, \code{as.numeric.multitime}, 
\code{as.Date.multitime}, \code{as.yearmon.multitime}, 
\code{as.yearqtr.multitime} and \code{make.unique.multitime}.
The primary and secondary time scales when considered in conjunction with
each other must be unique if they are to be used as the times of a zoo
object yet individually need not be.

\code{as.Date}, \code{as.yearmon} and \code{as.yearqtr} methods search
through the time scales and return the first one found of the indicated
class or if none found convert the first time scale to the indicated class.
}
\value{
  An object of class \code{"multitime"}.
}
\examples{
# use secondary time scale to uniquify times
z <- zoo(1:3, as.multitime(c(1, 1, 2), c(1, 2, 1)))
z

# an object with both yearmon and Date scales
ym <- as.yearmon(2001:2003)
z2 <- zoo(1:3, as.multitime(ym, as.Date(ym, frac = 1)))
z2
as.yearmon(time(z2))
as.Date(time(z2))

}
\keyword{ts}
