\name{make.arguments.section}
\alias{make.arguments.section}
\alias{make.usage.section}
\title{Construct sections of documentation}
\description{Don't bother reading about these unless you are sure you need to! These are really intended for expediting documentation of large numbers of "internal" functions in a proto-package, and are called by \code{make.internal.doc}. \code{make.usage.section} and \code{make.arguments.section} form prototype USAGE and ARGUMENTS section for the specified functions. These are ready for pasting into flat-format documentation (and subsequent editing).
}
\usage{
make.usage.section( funs=, file=stdout(), env=.GlobalEnv)
make.arguments.section( funs=, file=stdout(), env=.GlobalEnv)
}
\arguments{
\item{ funs}{character vector of function names, defaulting to \code{find.funs() \%except\% find.documented( doctype="Rd")}}
\item{ file}{where to put the output (\code{"clipboard"} is useful). NULL means don't print.}
\item{ env}{where to look for the functions}
}
\value{Character vector containing the doc section (in plain text, not Rd format).
}
\details{
The default \code{funs} argument will find all functions not mentioned in flat-format ready-for-doc2Rd documentation. This is useful for documenting a group of "internal" functions.

\code{make.usage.section} simply puts the name of each function before its deparsed and concatenated argument list, one function per line.

\code{make.arguments.section} puts one argument per line, then a colon, then the name of the function in parentheses. The idea is that something about the argument should be added manually in a text editor.
}
\examples{
if( FALSE)\{
  # Can't run this directly, coz internal
  # so not exported
  ns <- asNamespace( 'mvbutils')
  make.usage.section( c( "make.usage.section", "find.funs"),
    env=ns)
  make.arguments.section( c( "make.usage.section", "find.funs"),
    env=ns)
\}
}
\seealso{\code{\link{flatdoc}}, \code{\link{pre.install}}
}
\author{Mark Bravington}
\keyword{internal
}
