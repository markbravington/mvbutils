\name{localfuncs}
\alias{localfuncs}
\title{"Declare" child functions, allowing much tidier code}
\description{Only call this within a function, say \code{f}. The named functions are copied into the environment of \code{f}, with their environments set to the environment of \code{f}. This means that when you call one of the named functions later in \code{f}, it will be able to see all the variables in \code{f}, just as if you had defined the function inside \code{f}. Using \code{localfuncs} avoids you having to clutter \code{f} with definitions of child functions. It differs from \code{\link{mlocal}} in that the local functions won't be changing objects directly in \code{f} unless they use \code{<<-} -- they will instead have normal R{} lexical scoping.
}
\usage{localfuncs(funcs)
}
\arguments{\item{ funcs}{character vector of function names}
}
\seealso{\code{\link{mlocal}} for a different approach
}
\examples{
inner <- function( x) \{
  y <<- y+x
  0
\}
outer <- function( z) \{
  # Multiply z by 2!
  y <- z
  localfuncs( 'inner')
  inner( z)
  return( y)
\}
outer( 4) # 8
}
\keyword{misc}
