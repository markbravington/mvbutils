\name{screen_masked_imports}
\alias{screen_masked_imports}
\title{Avoid clashing package imports}
\description{
Suppose your package imports various other packages. Despite the pious advice about only selectively importing certain functions, it's \emph{perfectly fine} and \emph{very convenient} to just be able to \code{import(otherpack)} in your NAMESPACE (or the equivalent, in whatever system you use to auto-generate the NAMESPACE). One annoyance, though, is that several packages may export different things with the same name, which leads to disconcerting warnings about "replacing import..." when your package is loaded. This can be circumvented in NAMEPACE using this syntax:

\preformatted{
}

But you sure don't want to have to work out those NAMESPACE details yourself, so this function does it for you.

Mostly, this function will be called invisibly during \code{\link{make.NAMESPACE}} during \code{\link{pre.install}}, but you could try it yourself for more manual things.
}
\usage{screen_masked_imports( imports, myfuns)
}
\arguments{
\item{ imports}{names of packages that yours imports (or Depends on)}
\item{ myfuns}{functions in your package. You don't want to import other functions with the same name, because those imports will just be overridden.}
}
\value{List of character vectors, one element per \code{imports}, saying which if any functions exported by that importee should \emph{not} be imported via your NAMESPACE.
}
\keyword{misc}
