\name{strip.missing}
\alias{strip.missing}
\title{Exclude "missing" objects}
\description{To be called inside a function, with a character vector of object names in that function's frame. \code{strip.missing} will return all names except those corresponding to formal arguments which were not set in the original call and which lack defaults. The output can safely be passed to \code{get}.
}
\usage{strip.missing( obs)
}
\arguments{\item{ obs}{character vector of object names, often from \code{ls(all=TRUE)}}
}
\details{Formal arguments that were not passed explicitly, but which \bold{do} have defaults, will \bold{not} be treated as missing; instead, they will be set equal to their evaluated defaults. This could cause problems if the defaults aren't meant to be evaluated.
}
\examples{
funco <- function( first, second, third) \{
  a <- 9
  return( do.call("returnList", lapply( strip.missing( ls()), as.name)))
\}
funco( 1) # list( a=9, first=1)
funco( second=2) # list( a=9, second=2)
funco( ,,3) # list( a=9, third=3)
funco2 <- function( first=999) \{
  a <- 9
  return( do.call("returnList", lapply( strip.missing( ls()), as.name)))
\}
funco2() # list( a=9, first=999) even tho' "first" was not set
}
\seealso{\code{\link{returnList}}
}
\author{Mark Bravington}
\keyword{programming
}
