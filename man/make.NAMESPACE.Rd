\name{make.NAMESPACE}
\alias{make.NAMESPACE}
\alias{write.NAMESPACE}
\title{Auto-create a NAMESPACE file}
\description{Called by \code{\link{pre.install}} for would-be packages that have a \code{.onLoad} function, and are therefore assumed to want a namespace. Produces defaults for the import, export, and S3Methods. You can modify this information prior to the NAMESPACE file being created, using the pre-install hook mechanism. The default for \code{import} is taken from the DESCRIPTION file, but the defaults for export and S3 methods are deduced from your functions, and are described below.
}
\usage{
# Don't call this directly-- pre.install will do it automatically for you
make.NAMESPACE( env=1, path=attr( env, "path"),
  description=read.dcf( file.path( path, "DESCRIPTION"))[1,], more.exports=character( 0),
  useDynLib=character())
}
\arguments{
\item{ env}{character or numeric position on search path}
\item{ path}{directory where proto-package lives}
\item{ description}{(character) elements for the DESCRIPTION file, e.g. \code{c( ..., Author="R.A. Fisher", ...)}. By default, read from existing file.}
\item{ more.exports}{(character) things to export that normally wouldn't be.}
\item{ useDynLib}{character vector of DLLs, without path or extension. Elements with names will get a NAMESPACE entry of the form \code{<env_name>=useDynLib( <DLL>, .registration=TRUE)}, with the symbols being placed into a subenvironment withint the package namespace environment. Unnamed elements just get \code{useDynLib(<DLL>)} and the symbols go directly into the package namespace environment; that's how \code{Rcpp} currently operates (and I'm not keen on it!). If you \emph{don't} want native-symbol registration, then you should use a PIBH (pre-install build hook; see \code{\link{pre.install}}) and move the non-regstrees from \code{nsinfo$useDynLib} into \code{nsinfo$useDynLib_sans_rego}.}
}
\details{
There is (currently) no attempt to handle S4 methods.

The imported packages are those listed in the "Depends:" and "Imports:" field of the DESCRIPTION file. All exported functions in those packages will be imported (i.e. currently no "importFrom" provision), except if a function would be screened by a later import or by your package's own functions. The latter should avoid clash warnings when your package is loaded.

The exported functions are all those in \code{find.documented(doctype="any")} unless they appear to be S3 methods, plus any functions that have a non-NULL \code{export.me} attribute. The latter is a cheap way of arranging for a function to be exported, but without formal documentation (is that wise??). \code{\link{pre.install}} will incorporate any undocumented \code{export.me} functions in the "mypack-internal.Rd" file, so that RCMD CHECK will be happy.

The S3 methods are all the functions whose names start "<<generic>>." and whose first argument has the same name as in the appropriate \code{<<generic>>}. The generics that are checked are (i) the names of the character vector \code{.knownS3Generics} in package \pkg{base}; (ii) all functions that look like generics in any importees or dependees of your would-be package (i.e. functions in the namespace whose name is a prefix of a function in the S3 methods table of the namespace, and whose body contains a call to \code{UseMethod}); (iii) any plausible-looking generic in your would-be package (effectively the same criterion). Documented functions which look like methods but whose flat-doc documentation names them explicitly in the \bold{Usage} section (e.g. referring to \code{print.myclass(...)} rather than just \code{print(...)}, the latter being how you're supposed to document methods) are assumed not be methods.
}
\seealso{\code{\link{pre.install}}, \code{\link{flatdoc}}
}
\keyword{utilities}
\keyword{programming
}
