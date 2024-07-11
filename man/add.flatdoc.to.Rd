\name{add.flatdoc.to}
\alias{add.flatdoc.to}
\title{Skeletal flat-format documentation}
\description{You very likely don't need to read this--- \code{add.flatdoc.to} is usually called automatically for you, by \code{fixr( ..., new.doc=TRUE)}. It adds skeletal flat-format documentation to a function, suitable for conversion to Rd-format using \code{\link{doc2Rd}}. The result should pass RCMD CHECK (but won't be much use until you actually edit the documentation).
}
\usage{
# See *Examples* for practical usage
add.flatdoc.to(x, char.x = NULL, pkg=NULL, env=NULL, convert.to.source=FALSE)
}
\arguments{
\item{ x}{unquoted function name, sought in \code{parent.frame()} unless \code{pkg} is set}
\item{ char.x}{[string] function name}
\item{ pkg}{[string] name of maintained package where \code{x} lives (optional)}
\item{ env}{[environment] where to get \code{x} from; defaults to caller unless \code{pkg} is set.}
\item{ convert.to.source}{[logical] if TRUE and \code{x} has a "srcref" attribute, the latter is converted to a \code{source} attribute. This ensures that \code{fixr} will write the function correctly to the temporary file used by your text editor. Default is FALSE for historical reasons.}
}
\value{A function with attribute \code{doc} containing the flat-format documentation.
}
\details{You don't \bold{have} to write Rd-compatible documentation from the outset. You can write documentation that's as free-form as you wish, and there's no need to use \code{add.flatdoc.to} to do it-- you can write the doco directly in your text editor provided that you can \code{source} the resultant melange OK (see \code{\link{fixr}}). I find \code{add.flatdoc.to} useful, though, because I can never remember the headings or mild layout conventions of \code{\link{doc2Rd}} and Rd-format itself.
}
\seealso{\code{\link{flatdoc}}, \code{\link{fixr}}, \code{\link{doc2Rd}}
}
\examples{
\dontrun{
myfun <- function( ...) ...
myfun <- mvbutils:::add.flatdoc.to( myfun)
# 'fixr( myfun)' will now allow editing of code & doco together
# Or, in a maintained package:
# ..mypack$myfun <<- add.flatdoc.to( myfun, pkg='mypack')
}
}
\keyword{internal
}
