\name{lagts}
\alias{lagts}
\alias{lagts.default}
\alias{lagts.ts}
\alias{lagts.zoo}
\alias{leadts}

\title{Lags and Leads for Time Series Objects}
\description{
  \code{lagts} and \code{leadts} are new generic functions that do
  intuitive computations of lags and leads. The main purpose is to overcome
  the confusion about the sign of the lag order in the base \code{lag}
  function that is employed differently by different classes.
}

\usage{
lagts(x, \dots)
leadts(x, \dots)

\method{lagts}{default}(x, k = 1, na.pad = TRUE, \dots)
\method{lagts}{ts}(x, k = 1, na.pad = TRUE, \dots)
\method{lagts}{zoo}(x, k = 1, na.pad = TRUE, \dots)
}

\arguments{
  \item{x}{an object. The default method works for vectors and matrices.}
  \item{k}{For \code{lag} the number of lags (in units of observations).
    Note the sign of \code{k} behaves as employed in most textbooks and \emph{not}
    as in \code{\link[stats]{lag}}.}
  \item{na.pad}{logical. If \code{TRUE} it adds any times that would not otherwise have been in
    the result with a value of \code{NA}.  If \code{FALSE} those times are dropped.}
  \item{\dots}{currently not used.}
}

\details{
\code{lagts} by default computes real lags for positive \code{k} while
\code{\link[stats]{lag}} would in fact compute leads. The latter lead to a lot
of confusion among users of \code{lag}, especially because some time series
classes followed the base behaviour while others decided for the intuitive
(but inconsistent) behaviour. \code{lagts} aims to overcome this confusions

This functionality is still under development.
}

\value{
The lags or leads of the original object.
}


\seealso{\code{\link[stats]{lag}}}

\examples{
x <- zoo(11:21)
lag(x, k = 1)
lag(x, k = -1)
lagts(x, k = 1)
lagts(x, k = -1) ## leadts not written yet
}
\keyword{ts}
