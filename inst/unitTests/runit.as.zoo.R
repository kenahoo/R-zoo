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

test.as.zoo.matrix <- function() {
  ## testing generic function and automatic choice
  x <- cbind(a = 1:2, b = 3:4)
  current <- as.zoo(x)
  target <- structure(1:4, .Dim = c(2L, 2L), .Dimnames = list(NULL, c("a", "b")), index = 1:2, class = "zoo")
  checkEquals(target, current)
}

test.as.zoo.data.frame <- function() {
  ## testing generic function and automatic choice
  x <- data.frame(a = 1:2, b = 3:4)
  current <- as.zoo(x)
  target <- structure(1:4, .Dim = c(2L, 2L), .Dimnames = list(NULL, c("a", "b")), index = 1:2, class = "zoo")
  checkEquals(target, current)
}

test.as.zoo.ts <- function() {
  # removed erroneous test
}

test.as.zoo.factor <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zoo.fts <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zoo.irts <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zoo.its <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zoo.mcmc <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zoo.timeSeries <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zoo.xts <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zooreg.xts <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zoo.zoo <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.vector.zoo <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.matrix.zoo <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.data.frame.zoo <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.list.zoo <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.list.ts <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zooreg <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zooreg.default <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.ts.zooreg <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.ts.zoo <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zoo.zooreg <- function() {
  DEACTIVATED("placeholder test function")
}

test.as.zooreg.zoo <- function() {
  DEACTIVATED("placeholder test function")
}
