\name{local.return}
\alias{local.return}
\title{Macro-like functions}
\description{In an \code{\link{mlocal}} function, \code{local.return} should be used whenever \code{return} is called, wrapped inside the \code{return} call around the return arguments.
}
\usage{
local.return(...) # Don't use it like this!
# Correct usage: return( local.return( ...))
}
\arguments{\item{ ...}{named and unnamed list, handled the same way as \code{return} before R{} 1.8, or as \code{\link{returnList}}}
}
\examples{
ffin <- function( nlocal=sys.parent()) mlocal( return( local.return( a)))
ffout <- function( a) ffin()
ffout( 3) # 3
# whereas:
ffin <- function( nlocal=sys.parent()) mlocal( return( a))
try(
  ffout( 3) # error:; "return" alone doesn't work
)
}
\seealso{\code{\link{mlocal}}
}
\author{Mark Bravington}
\keyword{programming
}
