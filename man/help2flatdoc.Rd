\name{help2flatdoc}
\alias{help2flatdoc}
\title{Convert help files to flatdoc format.}
\description{Converts a vanilla R{} help file (as shown in the internal pager) to plain-text format. The output conventions are those in \code{\link{doc2Rd}}, so the output can be turned into Rd-format by running it through \code{\link{doc2Rd}}. This function is useful if you have existing Rd-format documentation and want to try out the \code{\link{flatdoc}} system of integrated code and documentation. Revised Nov 2017: now pretty good, but not perfect; see \bold{Details}.
}
\usage{ help2flatdoc( fun.name, pkg=NULL, text=NULL, aliases=NULL)
}
\arguments{
\item{ fun.name}{function name (a character string)}
\item{ pkg}{name of package}
\item{ text}{plain-text help}
\item{ aliases}{normally leave this empty--- see \bold{Details}.}
The real argument is \code{text}; if missing, this is deduced from the help for \code{fun.name} (need not be a function) in the installed package \pkg{pkg}.
}
\details{
The package containing \code{fun.name} must be loaded first. If you write documentation using \code{\link{flatdoc}}, prepare the package with \code{\link{pre.install}}, build it with RCMD BUILD or INSTALL, and run \code{help2flatdoc} on the result, you should largely recover your original flat-format documentation. Some exceptions:

\itemize{
\item Nesting in lists is ignored.
\item Numbered lists won't convert back correctly (Nov 2017), but the problem there is in \code{\link{doc2Rd}}.
\item Link-triggering phrases (i.e. that will be picked up by \code{\link{doc2Rd}}, such as "see <blah>") aren't explicitly created-- probably, links could be automated better via an argument to \code{\link{doc2Rd}}.
}

Aliases (i.e. if this doco can be found by \code{\link{help}} under several different names) are deduced from function calls in the \bold{Usage} section, in addition to anything supplied specifically in the \code{alias} argument. The latter is really just meant for internal use by \code{\link{unpackage}}.
}
\examples{
cd.doc <- help2flatdoc( "cd", "mvbutils")
print( cd.doc)
cd.Rd <- doc2Rd( cd.doc)
}
\seealso{\code{\link{doc2Rd}}
}
\keyword{programming
}
