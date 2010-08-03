\name{listindex}
\alias{listindex}

\alias{c.listindex}
\alias{[.listindex}
\alias{length.listindex}
\alias{ORDER.listindex}
\alias{MATCH.listindex}

\alias{as.numeric.listindex}
\alias{as.vector.listindex}
\alias{as.character.listindex}

\alias{print.listindex}
\alias{xtfrm.listindex}

\title{An Index Class for Testing the Generality of zoo}
\description{
  \code{"listindex"} is a deliberately awkward class, set up only to test
  \pkg{zoo}, especially in conversion to C. It is not intended for users.
}
\usage{
listindex(object, \dots)
}
\arguments{
  \item{object}{any object that could be used as an index for a \code{"zoo"} object.}
  \item{\dots}{currently not used.}
}
\details{
  The \code{"listindex"} class does nothing else than wrapping its \code{object}
  into a list with a single element called \code{index}. This assures that
  none of the usual methods required by \pkg{zoo} work sensibly out of the box.
  
  Then, the required methods \code{c()}, \code{[}, \code{length}, \code{ORDER},
  and \code{MATCH} are set up. These simply work by emulating a dispatch on
  \code{listindex$index}.
  
  Additionally, methods for \code{as.numeric}, \code{as.vector}, and \code{as.character}
  are required as well. These are sort of expected by \pkg{zoo} in some places.
  
  Finally, adding \code{print} and \code{xtfrm} methods helps in other places as well.
}

\value{
  Returns \code{structure(list(index = object), class = "listindex")}, i.e., a list
  of class \code{"listindex"} with a single element \code{index} containing the
  unchanged input \code{object}.
}

\seealso{\code{\link{zoo}}}

\examples{
## two listindex objects wrapping Date vectors
x <- listindex(as.Date(0) + 0:2)
y <- listindex(as.Date(0) + 3:5)

## define zoo objects based on them
z1 <- zoo(rnorm(4), c(x, y[1]))
z2 <- zoo(rnorm(4), c(x[3], y))

## check some simple operations
z1 + z2
lag(z1, -1)
merge(z1, z2)
c(z1[1:2], z2[4])
}
\keyword{ts}
