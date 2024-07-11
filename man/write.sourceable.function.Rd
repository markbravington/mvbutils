\name{write.sourceable.function}
\alias{write.sourceable.function}
\title{Sourceable code for functions (and more) with flat-format documentation}
\description{Works like \code{write} for functions without flat documentation (i.e. without a "doc" attribute). If a "doc" attribute exists, the file is written in a form allowing it to be edited and then read back in with "source.mvb"; the "doc" attribute is given as free-form text following the function definition. If applied to a non-function with a "source" attribute, just the source attribute is printed; the idea is that this could be read back by \code{source} (or \code{\link{source.mvb}}), probably in the course of \code{\link{FF}} after \code{\link{fixr}}, to regenerate the non-function object.
}
\usage{
write.sourceable.function( x, con, append=FALSE, print.name=FALSE,
    doc.special=TRUE, xn=NULL)
}
\arguments{
\item{ x}{function or other object, or the name thereof, that is to be written. If \code{x} is not a function, then it must have an attribute "source".}
\item{ con}{a connection or filename}
\item{ append}{if "con" is not already open, should it be appended to rather than overwritten?}
\item{ print.name}{should output start with \code{"NAME" <-} (where NAME is deduced from \code{x})?}
\item{ doc.special}{TRUE if \code{doc} attribute is to be printed as flat doc-- assumes readback via \code{source.mvb}}
\item{ xn}{(string) can set this to be the name of the function if \code{print.name} is TRUE}
}
\details{
If \code{x} is unquoted and \code{print.name=TRUE}, the name is obtained from \code{deparse( substitute( x))}. If \code{x} is a character string, the name is \code{x} itself and the function printed is \code{get(x)}.

The real criterion for an attribute to be output in \code{\link{flatdoc}}-style, is not whether the attribute is called \code{doc}, but rather whether it is a character-mode object of class \code{docattr}. You can use this to force \code{\link{flatdoc}}-style output of several \code{doc}-like attributes.

The default EOF line for an attribute is <<end of doc>>, but this will be adjusted if it appears in the attribute itself.
}
\examples{
\dontrun{
write.sourceable.function( write.sourceable.function, "wsf.r")
# To dump all functions and their documentation in a workspace into a single sourceable file:
cat( "", file="allfuns.r")
sapply( find.funs(), write.sourceable.function, file="allfuns.r", append=TRUE, print.name=TRUE)
# A non-function
scrunge <- c( 1:7, 11)
attr( scrunge, "source") <- c( "# Another way:", "c( 1:4, c( 5:7, 11))")
scrunge # [1] 1 2 3 4 5 6 7 11
write.sourceable.function( scrunge, stdout()) # source
fixr( scrunge) # source
}
}
\seealso{\code{\link{source.mvb}}, \code{\link{readLines.mvb}}, \code{\link{flatdoc}}, the file "demostuff/original.dochelp.rrr", the demo in "flatdoc.demo.r"
}
\keyword{internal
}
