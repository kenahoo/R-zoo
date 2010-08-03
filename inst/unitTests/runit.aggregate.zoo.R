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

test.aggregate.zoo.factor.ignore.NA <- function() {
  F <- factor(c(rep("a",2), rep(NA,2), rep("b", 2)))
  target <- structure(c(1.5, 5.5), index = structure(c(1L, 2L), .Label = c("a", "b"), class = "factor"), class = "zoo")
  current <- aggregate(zoo(1:6,1:6), F, mean)
  checkEquals(target, current)
}
