##***********************************************************************
## $Id$
##
## this file is part of the R library zoo.  zoo is free software: you
## can redistribute it and/or modify it under the terms of the GNU
## General Public License as published by the Free Software
## Foundation, either version 3 of the License, or (at your option)
## any later version.
##
## zoo is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
## or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
## License for more details.
##
## You should have received a copy of the GNU General Public License
## along with the nens libraray.  If not, see
## <http://www.gnu.org/licenses/>.
##

require(RUnit)

"test.[<-.zoo.full.replacement" <- function() {
  current <- zoo(matrix(1:24, 6))
  current[] <- 3
  target <- zoo(matrix(rep(3, 24), 6))
  checkIdentical(target, current)
}

test.zoo.empty <- function() {
  ## this is the model
  target <- structure(numeric(0), index = 1:6, class = "zoo")
  current <- zoo(order.by=1:6)
  checkIdentical(target, current)
}

test.zoo.data.frame.from.empty <- function() {
  ## follows the model
  target <- zoo(order.by=1:6)
  current <- zoo(data.frame(), order.by=1:6)
  checkIdentical(target, current)
}

test.zoo.matrix.from.empty <- function() {
  ## follows the model
  target <- zoo(order.by=1:6)
  current <- zoo(matrix(nrow=6, ncol=0), order.by=1:6)
  checkIdentical(target, current)
}

test.zoo.vector.1.nameless.col <- function() {
  ## this is the model
  target <- structure(1:6, .Dim = c(6L, 1L), index = 1:6, class = "zoo")
  current <- zoo(1:6, order.by=1:6)
  checkIdentical(target, current)
}

test.zoo.matrix.1.nameless.col <- function() {
  ## follows the model
  target <- zoo(1:6, order.by=1:6)
  current <- zoo(cbind(1:6), order.by=1:6)
  checkIdentical(target, current)
}

test.zoo.data.frame.1.nameless.col <- function() {
  ## I think it is not really possible to have a data.frame where
  ## columns have no name
}

test.zoo.vector.2.nameless.cols <- function() {
  ## you can't pass more than one vector to the zoo creator.
  ## current <- zoo(1:6, 6:1, order.by=1:6)
}

test.zoo.matrix.2.nameless.cols <- function() {
  ## this is the model
  target <- structure(c(1:6, 6:1), .Dim = c(6L, 2L), index = 1:6, class = "zoo")
  current <- zoo(cbind(1:6, 6:1), order.by=1:6)
  checkIdentical(target, current)
}

test.zoo.data.frame.2.nameless.cols <- function() {
  ## I think it is not really possible to have a data.frame where
  ## columns have no name
}

test.zoo.vector.1.named.col <- function() {
  ## zoo does not accept named vectors
  ## current <- zoo(a=1:6, order.by=1:6)
}

test.zoo.matrix.1.named.col <- function() {
  ## this is the model
  target <- structure(1:6, .Dim = c(6L, 1L), .Dimnames = list(NULL, "b"), index = 1:6, class = "zoo")
  current <- zoo(cbind(b=1:6), order.by=1:6)
  checkIdentical(target, current)
}

test.zoo.data.frame.1.named.col <- function() {
  ## this follows the model
  target <- zoo(cbind(b=1:6), order.by=1:6)
  current <- zoo(data.frame(b=1:6), order.by=1:6)
  checkIdentical(target, current)
}

test.zoo.vector.2.cols <- function() {
  ## zoo does not accept neither named nor multiple vectors
  ## current <- zoo(a=1:6, b=6:1, order.by=1:6)
}

test.zoo.matrix.2.cols <- function() {
  ## this is the model
  target <- structure(c(1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
                      .Dim = c(6L, 2L),
                      .Dimnames = list(NULL, c("a", "b")),
                      index = 1:6,
                      class = "zoo") 
  current <- zoo(cbind(a=rep(1,6), b=3), order.by=1:6)
  checkIdentical(target, current)
}

test.zoo.data.frame.2.cols <- function() {
  ## this follows the model
  target <- zoo(cbind(a=rep(1,6), b=3), order.by=1:6)
  current <- zoo(data.frame(a=1, b=3), order.by=1:6)
  checkIdentical(target, current)
}

test.transform.zoo <- function() {
	target <- structure(logical(0), .Dim = c(0L, 3L), .Dimnames = list(NULL, 
    c("a", "b", "c")), index = structure(numeric(0), class = "Date"), 
	class = "zoo")
	z <- zoo(cbind(a = numeric(0), b = numeric(0)), as.Date(numeric(0)))
	current <- transform(z, c = a + 1)
	checkIdentical(target, current)

	DF <- data.frame(a = 1:3, b = 4:6)
	zz <- zoo(DF, as.Date(1:3))
	target <- zoo(transform(DF, c = 10 * a), time(zz))
	current <- transform(zz, c = 10 * a)
	checkIdentical(target, current)
}

