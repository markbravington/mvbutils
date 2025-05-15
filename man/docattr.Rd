\name{docattr}
\alias{docattr}
\alias{flatdoc}
\alias{tidyup_docattr}
\title{Flat-format documentation}
\description{
The \code{docattr} convention, and its obsolete ancestor \code{flatdoc}, lets you edit plain-text documentation in the same file as your function's source code. \code{docattr} and \code{flatdoc} are hardly ever called explicitly, but you will see them in text files produced by \code{\link{fixr}}; you can also add it to such files yourself. They are mostly used to write Rd-style help with almost no markup (\emph{much} cleaner than Roxygen!) that will be converted into Rd-format when building/exporting packages. However, \code{mvbutils} extends \code{\link{help}} so that \code{?myfunc} will display plain-text documentation for \code{myfunc}, even if \code{myfunc} isn't in a package. There are no restrictions on the format of informal-help documentation, so \code{docattr} is useful for adding quick simple help just for you or for colleagues. If your function is to be part of a maintained package (see \code{\link{mvbutils.packaging.tools}}), then the documentation should follow a slightly more formal structure; use \code{fixr( myfun, new.doc=T)} to set up the appropriate template.

A neat trick, for a function where you want "internal" documentation but not visible (yet), is to name the attribute "secret_doc" rather than "doc".

The difference between these two functions is that \code{docattr} (which requires R{} >= 4.1) has completely regular R{} syntax, taking advantage of "raw strings" (see \code{\link{Quotes}}). \code{flatdoc} had to use a rather devious trick which required that the file was subsequently read in by \code{\link{source.mvb}} rather than \code{source}. If you have a task package \pkg{wundapak} which uses \code{flatdoc}, then you can convert it with eg \code{tidyup_docattr(..wundapak)}.

\code{docattr} is a simple wrapper for \code{\link{string2charvec}}, to which it just adds the "docattr" class so that the documentation is not printed by default; you will just see "# FLAT-FORMAT DOCUMENTATION" appended to the function body.
}
\usage{
# ALWAYS use it like this:
# structure( function( ...) \{body\},
# doc=docattr( r"--\{
#... including newlines, etc\}--
# \}--"))
# almost NEVER like this
docattr( rawstr)
# Obsolete flatdoc() version
# ALWAYS use it like this:
# structure( function( ...) \{body\},
# doc=flatdoc( EOF="<<end of doc>>"))
# plaintext doco goes here...
# NEVER use it like this:
flatdoc( EOF="<<end of doc>>")
tidyup_docattr( e)
}
\arguments{
\item{ rawstr}{a single string, almost certainly a raw string containing the plain-text documentation.}
\item{ EOF}{character string showing when plain text ends, as in \code{readlines.mvb}}
\item{ body}{replace with your function code}
\item{ ...}{replace with your function arg list}
\item{ e}{an environment (usually a task package, starting with \code{..}--- but it could be \code{.GlobalEnv}), or the name of an environment.}
}
\value{\code{docattr} returns a character vector of class \code{docattr}. The print method for \code{docattr} objects just displays the string "# FLAT-FORMAT DOCUMENTATION", to avoid screen clutter.
}
\section{Internal details of flatdoc}{
This section can be safely ignored by almost all users, and as of \code{mvbutils} v2.11.0, it's obsolete anyway since \code{docattr} should now replace \code{flatdoc} throughout.

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
\code{\link{doc2Rd}}, \code{\link{dochelp}}, \code{\link{write_sourceable_function}}, \code{\link{source.mvb}},

\code{\link{make.usage.section}}, \code{\link{make.arguments.section}}, \code{\link{fixr}},

the demo in "flatdoc.demo.R"
}
\examples{
# This illustrate the general format for a function with attached plain-text documentation. It is the format produced by write_sourceable_function()
flubbo <- structure( function( x)\{
  ## A comment
  x+1
\}
,doc=mvbutils::docattr( r"-{
flubbo       not-yet-in-a-package
'flubbo' is a function! And here is some informal doco for it. Whoop-de-doo!
You can have multiple lines, lots of "double" and 'single' quotes, and there's no need to escape weird characters, so "\\\" is tickety-boo.
And you can use the power of raw strings to r"{have a short one}" inside your function. Just make sure your final closing "quote" matchs the number of dashes (0 or more) that follow the first r-double-quote, and exceeds the number in any r"{short quotelets}" inside the documentation. Usually there won't be any, so you don't need any dashes at all.
\}-"))
\dontrun{
## Put next lines up to "<<end of doc>>" into a text file <<your filename>>
## and remove the initial hashes
#structure( function( x) {
#  x*x
#}
#,doc=flatdoc("<<end of doc>>"))
#
#Here is some informal documentation for the "SQUARE" function
#<<end of doc>>
## Now try SQUARE <- source.mvb( <<your filename>>); ?SQUARE
## Example with multiple attributes
## Put the next lines up to "<<end of part 2>>"
## into a text file, and remove the single hashes
#myfun <- structure( function( attname) {
#  attr( sys.function(), attname)
#}
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
