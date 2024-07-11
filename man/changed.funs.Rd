\name{changed.funs}
\alias{changed.funs}
\title{Show functions and callees in environment 'egood' that have changed or disappeared in environment 'ebad'.}
\description{
Useful eg when you have been modifying a package, and have buggered stuff up, and want to partly go back to an earlier version... entirely hypothetical of course, things like that never ever happens to \emph{me}. Mere mortals might want to create a new environment \code{goodenv}, use \code{evalq(source(<<old.mypack.R.source.file>> local=T), goodenv)}, then \code{find.changes( goodenv, asNamespace("mypack"))}. If your package is lazy-loaded, you're stuffed; I avoid lazy-loading, except perhaps for final distribution, because it just makes it much harder to track problems. Not that \emph{I} ever have problems, of course.

Can be applied either to a specified set of functions, or by default to all the functions in \code{egood}. If the former, then all callees of the specified functions are also checked for changes, as are all their callees, and so on recursively.
}
\usage{changed.funs(egood, ebad, topfun, fw = NULL)
}
\arguments{
\item{ egood, ebad}{environments #1 & #2. Not symmetric; functions only in \code{ebad} won't be checked.}
\item{ topfun}{name of functions in \code{egood} to check; all callees will be checked too, recursively. Default is all functions in \code{egood}.}
\item{ fw}{if non-NULL, the result of a previous call to \code{foodweb(egood)}, but this will be called automatically if not.}
}
\value{Character vector with the names of changed/lost functions.
}
\keyword{misc}
