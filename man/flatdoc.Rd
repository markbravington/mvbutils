\name{flatdoc}
\alias{flatdoc}
\title{Flat-format documentation}
\description{The \code{flatdoc} convention lets you edit plain-text documentation in the same file as your function's source code. \code{flatdoc} is hardly ever called explicitly, but you will see it in text files produced by \code{\link{fixr}}; you can also add it to such files yourself. \code{mvbutils} extends \code{\link{help}} so that \code{?myfunc} will display this type of documentation for \code{myfunc}, even if \code{myfunc} isn't in a package. There are no restrictions on the format of informal-help documentation, so \code{flatdoc} is useful for adding quick simple help just for you or for colleagues. If your function is to be part of a maintained package (see \code{\link{mvbutils.packaging.tools}}), then the documentation should follow a slightly more formal structure; use \code{fixr( myfun, new.doc=T)} to set up the appropriate template.
}
\usage{
# ALWAYS use it like this:
# structure( function( ...) \{body\},
# doc=flatdoc( EOF="<<end of doc>>"))
# plaintext doco goes here...
# NEVER use it like this:
flatdoc( EOF="<<end of doc>>")
}
\arguments{
\item{ EOF}{character string showing when plain text ends, as in \code{readlines.mvb}}
\item{ body}{replace with your function code}
\item{ ...}{replace with your function arg list}
}
\value{Character vector of class \code{docattr}, as read from the \code{current.source()} (qv) connection. The print method for \code{docattr} objects just displays the string "# FLAT-FORMAT DOCUMENTATION", to avoid screen clutter.
}
\section{Internal details}{
This section can be safely ignored by almost all users.

On some text editors, you can modify syntax highlighting so that the "start of comment block" marker is set to the string "doc=flatdoc(".

It's possible to use \code{flatdoc} to read in more than one free-format text attribute. The \code{EOF} argument can be used to distinguish one block of free text from the next. These attributes can be accessed from your function via \code{attr( sys.function(), "<<attr.name>>")}, and this trick is occasionally useful to avoid having to include multi-line text blocks in your function code; it's syntactically clearer, and avoids having to escape quotes, etc. \code{mvbutils:::docskel} shows one example.

\code{\link{fixr}} uses \code{\link{write.sourceable.function}} to create text files that use the \code{flatdoc} convention. Its counterpart \code{\link{FF}} reads these files back in after they're edited. The reading-in is not done with \code{source} but rather with \code{\link{source.mvb}}, which understands \code{flatdoc}. The call to \code{doc=flatdoc} causes the rest of the file to be read in as plain text, and assigned to the \code{doc} attribute of the function. Documentation can optionally be terminated before the end of the file with the following line:

\preformatted{
  <<end of doc>>
}

or whatever string is given as the argument to \code{flatdoc}; this line will cause \code{\link{source.mvb}} to revert to normal statement processing mode for the rest of the file. Note that vanilla \code{source} will not respect \code{flatdoc}; you do need to use \code{\link{source.mvb}}.

\code{flatdoc} should never be called from the command line; it should only appear in text files designed for \code{\link{source.mvb}}.

\emph{The rest of this section is probably obsolete, though things should still work.}

If you are writing informal documentation for a group of functions together, you only need to \code{flatdoc} one of them, say \code{myfun1}. Informal help will work if you modify the others to e.g.

\preformatted{
  myfun2 <- structure( function(...) { whatever}, doc=list("myfun1"))
}

If you are writing with \code{\link{doc2Rd}} in mind and a number of such functions are to be grouped together, e.g. a group of "internal" functions in preparation for formal package release, you may find \code{\link{make.usage.section}} and \code{\link{make.arguments.section}} helpful.
}
\seealso{
\code{\link{source.mvb}}, \code{\link{doc2Rd}}, \code{\link{dochelp}}, \code{\link{write.sourceable.function}}, \code{\link{make.usage.section}},

\code{\link{make.arguments.section}}, \code{\link{fixr}}, the demo in "flatdoc.demo.R"
}
\examples{
\dontrun{
## Put next lines up to "<<end of doc>>" into a text file <<your filename>>
## and remove the initial hashes
#structure( function( x) \{
#  x*x
#\}
#,doc=flatdoc("<<end of doc>>"))
#
#Here is some informal documentation for the "SQUARE" function
#<<end of doc>>
## Now try SQUARE <- source.mvb( <<your filename>>); ?SQUARE
## Example with multiple attributes
## Put the next lines up to "<<end of part 2>>"
## into a text file, and remove the single hashes
#myfun <- structure( function( attname) \{
#  attr( sys.function(), attname)
#\}
#,  att1=flatdoc( EOF="<<end of part 1>>")
#,  att2=flatdoc( EOF="<<end of part 2>>"))
#This goes into "att1"
#<<end of part 1>>
#and this goes into "att2"
#<<end of part 2>>
## Now "source.mvb" that file, to create "myfun"; then:
myfun( 'att1') # "This goes into \\\"att1\\\""
myfun( 'att2') # "and this goes into \\\"att2\\\""
}
}
\author{Mark Bravington}
\keyword{documentation}
\keyword{programming
}
