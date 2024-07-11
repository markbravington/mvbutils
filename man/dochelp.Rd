\name{dochelp}
\alias{dochelp}
\title{Documentation (informal help)}
\description{\code{dochelp(topic)} will be invoked by the replacement \code{\link{help}} if conventional \code{\link{help}} fails to find documentation for topic \code{topic}. If \code{topic} is an object with a \code{doc} attribute (or failing that if \code{<<topic>>} or \code{<<topic>>.doc} is a character vector), then the attribute (or the character object) will be formatted and displayed by the pager or browser. \code{dochelp} is not usually called directly.
}
\usage{
# Not usually called directly
# If it is, then normal usage is: dochelp( topic)
dochelp( topic, doc, help_type=c( "text", "html"))
# Set options( mvb_help_type="text") if the browser gives you grief
}
\arguments{
\item{ topic}{(character) name of the object to look for help on, or name of "...doc" character object-- e.g. either \code{thing} or \code{thing.doc} if the character object is \code{thing.doc}.}
\item{ doc}{(character or list)-- normally not set, but deduced by default from \code{topic}; see \bold{Details}.}
\item{ help_type}{as per \code{help}. Defaults to \code{getOption( "mvb_help_type")} in normal usage, which in turn defaults to \code{getOption( "help_type")} as for standard \code{help}. Only \code{"text"} and \code{"html"} are supported by \code{dochelp}; anything else maps to \code{"text"}, which invokes R{}'s internal pager.}
}
\details{
\code{dochelp} will only be called if the original \code{\link{help}} call was a simple \code{help( topic=X, ...)} form, with X not a call and with no \code{try.all.packages} or \code{type} or \code{lib.loc} arguments (the other \code{\link{help}} options are OK).

The \code{doc} argument defaults to the \code{doc} attribute of \code{get("topic")}. The only reason to supply a non-default argument would be to use \code{dochelp} as a pager; this might have some value, since \code{dochelp} does reformat character vectors to fit nicely in the system pager window, one paragraph per element, using \code{\link{strwrap}}. Elements starting with a "\%" symbol are not displayed.

To work with \code{dochelp}, a \code{doc} attribute should be either:

\itemize{
\item a character vector, of length >=1. New elements get line breaks in the pager. Or:
\item a length-one list, containing the name of another object with a \code{doc} attribute. \code{dochelp} will then use the \code{doc} attribute of that object instead. This referencing can be iterated.
}

If the documentation is very informal, start it with a blank line to prevent \code{find.documented( ..., doctype="Rd")} from finding it.

With \code{help_type="text"}, the doco will be re-formatted to fit the pager; each paragraph should be a single element in the character vector. Elements starting with a \% will be dropped (but may still be useful for \code{\link{doc2Rd}}).

With \code{help_type="html"}, the doco will be passed thru \code{\link{doc2Rd}} and then turned into HTML. \code{\link{doc2Rd}} is pretty forgiving and has a fair crack at converting even very informal documentation, but does have its limits. If there is an error in the \code{\link{doc2Rd}} conversion then \code{help_type} will be reset to \code{"text"}.

\code{\link{flatdoc}} offers an easy way to incorporate plain-text (flat-format) documentation-- formal or informal-- in the same text file as a function definition, allowing easy maintenance. The closer you get to the displayed appearance of formal R-style help, the nicer the results will look in a browser (assuming \code{help_type="html"}), but the main thing is to just write \emph{some} documentation-- the perfect is the enemy of the good in this case!
}
\seealso{\code{\link{flatdoc}}, \code{\link{doc2Rd}}, \code{\link{find.documented}}, \code{strwrap}
}
\examples{
#
myfun <- structure( function() 1,
  doc="Here is some informal documentation for myfun\\n")
dochelp( "myfun")
help( "myfun") # calls dochelp
}
\author{Mark Bravington}
\keyword{documentation
}
