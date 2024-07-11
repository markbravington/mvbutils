\name{find.documented}
\alias{find.documented}
\alias{find.docholder}
\title{Support for flat-format documentation}
\description{\code{find.documented} locates functions that have flat-format documentation; the functions and their documentation can be separate, and are looked for in all the environments in \code{\link{pos}}, so that functions documented in one environment but existing in another will be found. \code{find.docholder} says where the documentation for one or more functions is actually stored. Both \code{find.documented} and \code{find.docholder} check two types of object for documentation: (i) functions with "doc" attributes, and (ii) character-mode objects whose name ends in ".doc"
}
\usage{
find.documented( pos=1, doctype=c( "Rd", "casual", "own", "any"),
  only.real.objects=TRUE, exclude.internal=FALSE)
find.docholder( what, pos=find( what[1]))
}
\arguments{
\item{ pos}{search path position(s), numeric or character. In \code{find.documented}, any length. In \code{find.docholder}, only \code{pos[1]} will be used; it defaults to where the first element of \code{what} is found.}
\item{ doctype}{Defaults to "Rd". If supplied, it is partially matched against the choices in \bold{Usage}. "Rd" functions are named in the alias list at the start of (i) any \code{doc} attribute of a function, and (ii) any text object whose name ends with ".doc", that exist in \code{pos} (see \code{\link{doc2Rd}}). "casual" functions have their own \code{doc} attribute and will be found by the replacement of \code{help}; note that the \code{doc} attribute can be just a reference to another documented function, of mode "list" as described in \code{\link{dochelp}}. "own" functions (a subset of "casual") have their own character-mode \code{doc} attribute, and are suitable for \code{doc2Rd}. "any" combines \code{casual} and \code{Rd}.}
\item{ only.real.objects}{If TRUE, only return names of things that exist somewhere in the \code{pos} environments. FALSE means that other things such as the name of helpfiles might be returned, too.}
\item{ exclude.internal}{If TRUE, check the \code{doc} attributes to see if they have "KEYWORDS<whitespace>internal", and if so, omit that function. Normally you probably wouldn't want that yourself; but it is used in \code{make.NAMESPACE} to decide about exportees.}
\item{ what}{names of objects whose documentation you're trying to find.}
}
\value{
\item{ find.documented}{Character vector of function names.}
\item{ find.docholder}{list whose names are \code{what}; element \code{i} is a character vector showing which objects hold documentation for \code{what[i]}. Normally you'd expect either 0 or 1 entries in the character vector; more than 1 would imply duplication.}
}
\note{
\code{doctype="Rd"} looks for the alias names, i.e. the first word of all lines occurring before the first blank line. This may include non-existent objects, but these are checked for and removed.

Start informal documentation (i.e. not intended for \code{\link{doc2Rd}}) with a blank line to avoid confusion.
}
\seealso{\code{\link{flatdoc}}, \code{\link{doc2Rd}}, \code{\link{dochelp}}
}
\author{Mark Bravington}
\keyword{documentation}
\keyword{programming}
\keyword{utilities
}
