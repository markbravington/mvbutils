\name{NEG}
\alias{NEG}
\title{Generate a negated version of your function. Useful for 'nlminb' etc.}
\description{You pass it a function \code{f(.)}; it returns a function whose result will be \code{-f(.)}. The arguments, return attributes, and environment are identical to those of \code{f}.
}
\usage{NEG(f)
}
\arguments{\item{ f}{Normally, a function that returns a scalar; rarely, a NULL.}
}
\value{A function that returns \code{-f}. However, if \code{is.null(f)}, the result is also NULL; this is useful e.g. for gradient arg to \code{nlminb}.
}
\examples{
NEG( sqrt)( 4) # -2
# should put in more complex one here...
e <- new.env()
e$const <- 3
funco <- function( x) -sum( ( x-const)^2L)
environment( funco) <- e
nlminb( c( 0, 0), NEG( funco)) # c( 3, 3)
dfunco <- NULL
nlminb( c( 0, 0), NEG( funco), gradient=NEG( dfunco)) # c( 3, 3)
}
\keyword{misc}
