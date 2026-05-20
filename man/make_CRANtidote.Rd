\name{make_CRANtidote}
\alias{make_CRANtidote}
\title{Suppress stupid CRAN notes}
\description{
Sigh... CRAN will \emph{reject} packages with \emph{NOTE} "no visible binding for global variable" from R{} CMD check--- not WARNING, just NOTE. This is stupid because of false-positives whenever \code{\link{mlocal}}, \code{\link{cq}}, or indeed \code{evalq} or \code{local}, are called :/. Arguing with CRAN is about as productive as trying to teach a dead fish to climb trees. So, call \code{make_CRANtidote} instead, to get a bit of code that you can paste at the end of your package's \code{.onLoad} to suppress the non-problem. Your package does not need to import \code{mvbutils} to use that code.

It works by creating a dummy for each of those variables in the namespace environment, specifically a function that returns the string "CRANtidote" (since as-CRAN checks look not just for variables, but for functions). Sigh.

Unwanted side-effects are unlikely, but possible: here's two examples. First, when I initially tried this with \code{mvbutils} itself, it noticed that \code{.onAttach} was mentioned directly somewhere, and "helpfully" tried to add that--- causing failure on \code{attach()}, since R{} tried to run a non-function \code{.onAttach}. So \code{.onAttach} is now specifically excluded to avoid this, and will need tweaking manually in your code. There might be other special things only created/needed after \code{.onLoad} that need similar attention.

Second, if you \code{requireNamespace} a package (it's OK if that's in Suggests) but don't refer to its members by name (e.g. \code{otherpak::funcaroo}, which R{} says you're Supposed To but IMO that's silly as standard advice; it makes code harder to follow, and the code is all inside your own package, so \emph{you} will know whether there's any conflicting definition of \code{funcaroo}; if there is, do use double-colons obvs) , then its members will already have been created as fake functions by \code{make_CRANtidote} (and they need to ). So you gotta double-colon them, or something like that. Sigh.
}
\usage{make_CRANtidote( checkout)
}
\arguments{\item{ checkout}{Result of a call to \code{check.pkg}; AFAIK, it doesn't matter here whether the \code{CRAN} argument is set.}
}
\value{A character vector of class \code{cat}, which will display nicely. If the p
}
\examples{
\dontrun{
if( FALSE && is_very_annoying( CRAN))\{ # to avoid CRAN trying to run this despite dont-run :/
  test <- check.pkg( 'mypak')
  make_CRANtidote( test)
\}
}
}
\keyword{misc}
