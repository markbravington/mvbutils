\name{extract.named}
\alias{extract.named}
\title{Create variables from corresponding named list elements}
\description{This is a convenience function for creating named variables from lists. It's particularly useful for "unpacking" the results of calls to \code{.C}.
}
\usage{extract.named( l, to=parent.frame())
}
\arguments{
\item{ l}{a list, with some named elements (no named elements is OK but pointless)}
\item{ to}{environment}
}
\value{nothing directly, but will create variables
}
\examples{
ff <- function(...) \{ extract.named( list(...)); print( ls()); bbb \}
# note bbb is not "declared"
ff( bbb=6, ccc=9) # prints [1] "bbb" "ccc", returns 6
}
\author{Mark Bravington}
\keyword{programming
}
