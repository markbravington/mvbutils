\name{write_sourceable_function}
\alias{write_sourceable_function}
\alias{string2charvec}
\alias{simplest_name_generator}
\alias{cat_strings_rawly}
\title{Sourceable text for functions, including character attributes}
\description{
\code{write_sourceable_function} works like \code{write}, to produce a \code{source()}-friendly printout of a function. However, for the sake of clarity, any suitable character-vector attribute is printed as a multi-line raw string (see \code{\link{Quotes}}) wrapped in a call to \code{docattr} or to \code{string2charvec}, which will turn the string back into a character vector when the file is read back in by \code{source}. This hides a \emph{lot} of ugliness, including escaped special characters and superfluous quotes. Character objects that you want attached to the function (but not inside its code) actually looks like the real thing! (They can be accessed by the function code since they live inside \code{environment(sys.function())}.) My own main use is a \code{doc} attribute for free-text documentation (which later gets turned into "Rd" format by \code{\link{doc2Rd}} when I produce my packages, but that's a detail). However, I quite often keep other text snippets too, eg "templates".

Raw strings didn't use to exist in R{}, so before version 2.10 of \code{mvbutils}, the alternative version \code{\link{write.sourceable.function}} (note the dots) instead relied the contortions of \code{\link{flatdoc}} and \code{\link{source.mvb}} and \code{\link{readLines.mvb}} to trick R{} into accepting unmodified text. None of that should be necessary now.

\emph{Obsolete}: if \code{write_sourceable_function} is applied to a non-function with a "source" attribute, then just the source attribute is printed; the idea is that this could be read back by \code{source}, probably in the course of \code{\link{FF}} after \code{\link{fixr}}, to regenerate the non-function object. I don't think it's wise to rely on this....

\code{string2charvec}, \code{docattr}, and \code{simplest_name_generator} are helper functions that you're unlikely to use yourself. For the record, though:

\itemize{
\item \code{string2charvec} turns a string (length-1 non-empty character vector with no attributes) into a character vector, with a new element for every newline. The first element is discarded, because it's usually just a linebreak (perhaps preceded by accidental spaces etc) inserted to let the "real" raw string start on a fresh line. \code{string2charvec} is called by \code{\link{docattr}} which facilitates keeping plain-text documentation directly with the function, as an attribute.
\item \code{docattr} is very similar, but adds an S3 class "docattr". It simplifies the code produced by \code{write_sourceable_function} for presenting the plain-text documentation. I don't recommend using \code{docattr} for anything except an attribute called "doc" that contains, yes, documenbloodytation.
\item \code{simplest_name_generator} prints an R{} symbol (a "name") in a way that could appear on the LHS of \code{<symbol> <- 0}. If the name is simple, with no funny characters in it, then it's not quoted and is left alone. If it contains mildly strange characters that would cause the unquoted version to not parse, then it's quoted. If it contains characters that would break simple quotes (for example, quotes or backticks!) then it's wrapped in a bullet-proof raw string. "Only the paranoid survive"...
\item \code{cat_strings_rawly} outputs (using \code{cat}) a character vector as a single raw string wrapped in a call to \code{docattr} (if its argument has class "docattr") or otherwise \code{string2charvec}. Thus, \code{source} will break up the raw string back into a separate element for each newline. (\code{cat_strings_rawly} is probably a bad name for this function, since it actually takes a character vector as input, not a string...). It calls \code{cat} directly, so you already need to have directed output to wherever you want, eg via \code{sink}.
}
\subsection{Limitations}{
Some exotic language elements simply cannot be represented in sourceable text: for example, a "hard-coded" environment. A file will still be produced, but it won't work with \code{source}. There's no solution to such cases. For example:

\preformatted{
  f <- function( e=.GlobalEnv) environmentName( e)
  formals( f)$e <- new.env()
  tf <- tempfile()
  write_sourceable_function( f, tf)
  source( tf)
  # ... complains about e = <environment>
}
}
}
\usage{
write_sourceable_function( x, con, append=FALSE, print.name=FALSE, xn=NULL, ...)
string2charvec( x)
docattr( rawstr)
simplest_name_generator( x)
cat_strings_rawly( x)
}
\arguments{
\item{ x}{function or other object, or the name thereof, that is to be written by \code{write_sourceable_function}. If \code{x} is not a function, then it must have an attribute "source". For the helper functions, \code{x} is either a string itself (length-1 character vector), or for \code{cat_strings_rawly} a character vector.}
\item{ con}{a connection or filename}
\item{ append}{if "con" is not already open, should it be appended to rather than overwritten?}
\item{ print.name}{should output start with \code{NAME <-}, where NAME is deduced from \code{x}? Note that NAME will be processed by \code{simplest_name_generator} to make sure everything goes thru \code{source} nicely.}
\item{ xn}{(string) can set this to be the name of the function if \code{print.name} is TRUE}
\item{ ...}{ignored, but allows calls that use old \code{write.sourceable.function} arguments}
\item{ rawstr}{a string (length-1 character vector), presumably a "raw string" though R{} doesn't care.}
}
\details{
If \code{x} is unquoted and \code{print.name=TRUE}, the name is obtained from \code{deparse( substitute( x))}. If \code{x} is a character string, the name is \code{x} itself and the function printed is \code{get(x)}.

The criteria for deciding whether to raw-string-ify an attribute are:

\itemize{
\item it must be mode \code{character}
\item it must have length>1 (otherwise there's little point)
\item it must not have any attributes, except perhaps an S3 \code{class} (e.g. no \code{names}, no \code{dim})
\item it must not contain newline characters (since they would be confused with newlines inserted between elements).
}

Iff the attribute has S3 class "docattr", then \code{cat_strings_rawly} will wrap it in a call to \code{mvbutils::docattr} (which will mean it doesn't get full printed out at the console); otherwise, it will be wrapped in a call to \code{mvbutils::string2charvec}.
}
\examples{
# This is from the examples for 'flatdoc'. It's there to illustrate plain-text documentation, but you can see the call to 'docattr' in the middle.
flubbo <- structure( function( x)\{
  ## A comment
  x+1
\}
,doc=docattr( r"-{
flubbo       not-yet-in-a-package
'flubbo' is a function! And here is some informal doco for it. Whoop-de-doo!
You can have multiple lines and lots of "quotes" and even weird characters like "\\\".
And you can use the power of raw strings to r"{have a short one}" inside your function. Just make sure your final closing "quote" matchs the number of dashes (0 or more) that follow the first r-double-quote, and exceeds the number in any r"{short quotelets}" inside the documentation. Usually there won't be any, so you don't need any dashes at all.
\}-"))
\dontrun{
write_sourceable_function( write_sourceable_function, "wsf.r")
# To dump all functions and their documentation in a workspace into a single sourceable file:
cat( "", file="allfuns.r")
sapply( find.funs(), write_sourceable_function,
    file="allfuns.r", append=TRUE, print.name=TRUE)
# A non-function. Probably don't do this!
scrunge <- c( 1:7, 11)
attr( scrunge, "source") <- c( "# Another way:", "c( 1:4, c( 5:7, 11))")
scrunge # [1] 1 2 3 4 5 6 7 11
write_sourceable_function( scrunge, stdout()) # source
fixr( scrunge) # source
}
}
\keyword{misc}
