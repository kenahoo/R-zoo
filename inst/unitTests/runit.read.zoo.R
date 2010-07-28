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

test.read.zoo.index.column.numeric <- function() {
  DF <- data.frame(Time=1:3, Value=11:13)
  current <- read.zoo(DF, index.column=1)
  target <- zoo(11:13, order.by=1:3)
  checkEquals(target, current)
}

test.read.zoo.index.column.character <- function() {
  DF <- data.frame(Time=1:3, Value=11:13)
  current <- read.zoo(DF, index.column="Time")
  target <- zoo(11:13, order.by=1:3)
  checkEquals(target, current)
}

test.read.zoo.index.column.character.multicolumn <- function() {
  DF <- data.frame(Time=1:3, Value=11:13, Second=9:7)
  current <- read.zoo(DF, index.column="Time")
  target <- zoo(data.frame(11:13, 9:7), order.by=1:3)
  checkEqualsNumeric(target, current)
}

test.read.zoo.multicolumn.keeps.names <- function() {
  DF <- data.frame(Time=1:3, Value=11:13, Second=9:7)
  current <- read.zoo(DF, index.column=1)
  checkEquals(c("Value", "Second"), colnames(current))
}

