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

test.rollapply.default <- function() {
  current <- rollapply(c(1,2,3,4), 2, sum)
  target <- c(3, 5, 7)
  checkEquals(target, current)
}

test.rollapply.zoo.from.data.frame <- function() {
  Z <- zoo(data.frame(a=1, b=1:3, c=c(4,2,3)), order.by=7:9)
  current <- rollapply(Z, 2, sum)
  target <- zoo(data.frame(a=2, b=c(3,5), c=c(6,5)), order.by=7:8)
  checkEquals(target, current)
}

test.rollapply.zoo.from.cbind <- function() {
  Z <- zoo(cbind(a=1, b=1:3, c=c(4,2,3)), order.by=7:9)
  current <- rollapply(Z, 2, sum)
  target <- zoo(cbind(a=2, b=c(3,5), c=c(6,5)), order.by=7:8)
  checkEquals(target, current)
}

