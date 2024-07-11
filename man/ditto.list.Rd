\name{ditto.list}
\alias{ditto.list}
\title{Shorthand filler-inner for lists}
\description{Suppose you want to set up a list where several consecutive elements take the same value, but you don't want to repeatedly type that value: then use \code{dittolist} to set empty (missing) elements to the previous non-empty element. Wrap in \code{unlist()} to create a vector instead of a list.
}
\usage{
ditto.list(...)
# EG:
# ditto.list( a=1, b=, c='hello') # a: 1; b: 1, c: 'hello'
}
\arguments{\item{ ...}{anything, named or unnamed; missing elements OK}
}
\value{List
}
\examples{
unlist( ditto.list( a=1, b=, c='hello')) # a: 1; b: 1, c: 'hello'
}
\keyword{misc}
