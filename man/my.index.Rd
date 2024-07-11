\name{my.index}
\alias{my.index}
\alias{my.index.assign}
\alias{my.index.exists}
\title{Arbitrary-level retrieval from and modification of recursive objects}
\description{As of R{} 2.12, you probably don't need these at all. But, in case you do: \code{my.index} and \code{my.index.assign} are designed to replace \code{[[} and \code{[[<-} \emph{within} a function, to allow arbitrary-depth access into any recursive object. In order to avoid conflicts with system usage and/or slowdowns, it is wise to do this only inside a function definition where they are needed. A zero-length index returns the entire object, which I think is more sensible than the default behaviour (chuck a tanty). \code{my.index.exists} tests whether the indexed element actually exists. Note that these functions were written in 2001; since then, base-R has extended the default behaviour of \code{[[} etc for recursive objects, so that \code{my.index( thing, 1, 3, 5)} can sometimes be achieved just by to \code{thing[[c(1,3,5)]]} with the system version of \code{[[}. However, at least as of R{} 2.10.1, the system versions still have limited "recursability".
}
\usage{
# Use them like this, inside a function definition:
# assign( "[[", my.index); var[[i]]
# assign( "[[<-", my.index.assign); var[[i]] <- value
my.index( var, ...) # not normally called by name
my.index.assign( var, ..., value) # not normally called by name
my.index.exists( i, var)
}
\arguments{
\item{ var}{a recursive object of any mode (not just \code{list}, but e.g. \code{call} too)}
\item{ value}{anything}
\item{ ...}{one or more numeric index vectors, to be concatenated}
\item{ i}{numeric index vector}
}
\details{
Although R{} allows arbitrary-level access to lists, this does not (yet) extend to \code{call} objects or certain other language objects-- hence these functions. They are written entirely in R{}, and are probably very slow as a result. Notwithstanding EXAMPLES below, it is \bold{unwise} to replace system \code{[[} and \code{[[<-} with these replacements at a global level, i.e. outside the body of a function-- these replacements do not dispatch based on object class, for example.

Note that \code{my.index} and \code{my.index.assign} distort strict R{} syntax, by concatenating their \code{...} arguments before lookup. Strictly speaking, R{} says that \code{x[[2,1]]} should extract one element from a matrix list; however, this doesn't really seem useful because the same result can always be achieved by \code{x[2,1][[1]]}. With \code{my.index}, \code{x[[2,1]]} is the same as \code{x[[c(2,1)]]}. The convenience of automatic concatentation seemed slightly preferable (at least when I wrote these, in 2001).

\code{my.index.exists} checks whether \code{var} is "deep enough" for \code{var[[i]]} to work. Unlike the others, it does not automatically concatenate indices.

At present, there is no facility to use a mixture of character and numeric indexes, which you can in S+ via "list subscripting of lists".
}
\examples{
local(\{
  assign( "[[", my.index)
  assign( "[[<-", my.index.assign)
  ff <- function() \{ a <- b + c \}
  body( ff)[[2,3,2]] # as.name( "b")
  my.index.exists( c(2,3,2), body( ff)) # TRUE
  my.index.exists( c(2,3,2,1), body( ff)) # FALSE
  body( ff)[[2,3,2]] <- quote( ifelse( a>1,2,3))
  ff # function () \{ a <- ifelse(a > 1, 2, 3) + c \}
  my.index.exists( c(2,3,2,1), body( ff)) # now TRUE
\})
}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities
}
