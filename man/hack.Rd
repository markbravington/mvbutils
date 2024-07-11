\name{hack}
\alias{hack}
\alias{assign.to.base}
\title{Modify standard R functions, including tweaking their default arguments}
\description{
You probably shouldn't use these... \code{hack} lets you easily change the argument defaults of a function. \code{assign.to.base} replaces a function in \code{base} or \code{utils} (or any other package and its namespace and S3 methods table) with a modified version, possibly produced by \code{hack}. Package \pkg{mvbutils} uses these two to change the default position for library attachment, etc; see the code of \code{mvbutils:::.onLoad}.

Note that, if you call \code{assign.to.base} during the \code{.onLoad} of your package, then it must be called \emph{directly} from the \code{.onLoad}, not via an intermediate function; otherwise, it won't correctly reset its argument in the import-environment of your namespace. To get round this, wrap it in an \code{\link{mlocal}}; see \code{mvbutils:::.onLoad} for an example.

\code{assign.to.base} is only meant for changing things in packages, e.g. not for things that merely sit in non-package environments high on the search path (where \code{<<-} should work). I don't know how it will behave if you try. It won't work for S4 methods, either.
}
\usage{
 hack( fun, ...)
 assign.to.base( x, what=,  where=-1, in.imports=, override.env = TRUE)
}
\arguments{
\item{ fun}{a function (not a character string)}
\item{ ...}{pairlist of arguments and new default values, e.g. arg1=1+2. Things on RHS of equal signs will \bold{not} be evaluated.}
\item{ x}{function name (a character string)}
\item{ what}{function to replace \code{x}, defaulting to \code{"replacement." \%&\% x}}
\item{ where}{where to find the replacement function, defaulting to usual search path}
\item{ in.imports}{usually TRUE, if this is being called from an \code{.onLoad} method in a namespace. Make sure any copies of the function to be changed that are in the "imports" namespace also get changed. See \bold{Description}.}
\item{ override.env}{should the replacement use its own environment, or (by default) the one that was originally there?}
}
\examples{
\dontrun{
hack( dir, all.files=getOption( "ls.all.files", TRUE)) # from my '.First'
assign.to.base( "dir", hack( dir, all.files=TRUE))
}
}
\keyword{programming
}
