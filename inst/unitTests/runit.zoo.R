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
  checkEquals(target, current)
}

