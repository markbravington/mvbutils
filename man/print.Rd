\name{print}
\alias{print}
\alias{print.cat}
\alias{print.compacto}
\alias{print.specialprint}
\alias{print.pagertemp}
\alias{print.function}
\alias{print.call}
\alias{print.name}
\alias{print.<-}
\alias{print.(}
\alias{print.\{}
\alias{print.if}
\alias{print.for}
\alias{print.while}
\alias{print.default}
\title{Print values}
\description{
See base-R documentation of \code{print} and \code{print.default}. Users should see no difference with the \code{mvbutils} versions; they need to be documented and exported in \code{mvbutils} for purely technical reasons. There are also three useful special-purpose print methods in \code{mvbutils}; see \bold{Value}.Some of the base-R documentation is reproduced below.

The motive for redeclaration is to have a seamless transition within the \code{\link{fixr}} editing system, from the nice simple "source"-attribute system used to store function source-code before R2.14, to the quite extraordinarily complicated "srcref" system used thereafter. \code{mvbutils} does so via an augmented version of base-R's print method for functions, without which your careful formatting and commenting would be lost. If a function has a "source" attribute but no "srcref" attribute (as would be the case for many functions created prior to R2.14), then the augmented \code{print.function} will use the "source" attribute. There is no difference from base-R in normal use.

See \bold{How to override an s3 method} if you really want to understand the technicalities.
}
\usage{
print(x, ...) # generic
\method{print}{default}(x, ...) # S3 method for default
\method{print}{function}(x, useSource=TRUE, ...) # S3 method for function
\method{print}{cat}(x, ...) # S3 method for cat
\method{print}{compacto}(x, ...,
    gap= attr( x, 'gap'),
    width= attr( x, 'width'),
    extra= attr( x, 'extra'))# S3 method for compacto
\method{print}{specialprint}(x, ...) # S3 method for specialprint
\method{print}{pagertemp}(x, ...) # S3 method for pagertemp
\method{print}{call}(x, ...) # S3 method for call
\method{print}{`<-`}(x, ...) # S3 method for "<-" (a special sort of call)
\method{print}{`(`}(x, ...) # S3 method for "(" (a special sort of call)
#print(x, ...) # S3 method for "\{" (a special sort of call)
\method{print}{`if`}(x, ...) # S3 method for "if" (a special sort of call)
\method{print}{`for`}(x, ...) # S3 method for "for" (a special sort of call)
\method{print}{`while`}(x, ...) # S3 method for "while" (a special sort of call)
\method{print}{name}(x, ...) # S3 method for name (symbol)
}
\arguments{
\item{ x}{thing to print.}
\item{ ...}{other arguments passed to \code{NextMethod} and/or ignored. There are many special arguments to base-R \code{print.default}, as described in its documentation. They are not named individually in the \code{mvbutils} version for technical reasons, but you can still use them.}
\item{ gap, width, extra}{see \code{\link{compacto}}}
\item{ useSource}{[print.function] logical, indicating whether to use source references or copies rather than deparsing language objects. The default is to use the original source if it is available. The \code{mvbutils} override will print a "source" attribute if one exists but no "srcref" attribute does, whereas base-R post-2.14 would just print a deparsed version of the function.}
}
\value{
Technically, an \code{invisible} version of the object is returned. But the point of \code{print} is to display the object. \code{print.function} displays source code, as per \bold{Description}. \code{print.default} and \code{print.call} need to exist in \code{mvbutils} only for technical reasons. The other two special methods are:
\code{print.cat} applies to character vectors of S3 class \code{cat}, which are printed each on a new line, without the [1] prefix or double-quotes or backslashes. It's ideal for displaying "plain text". Use \code{\link{as.cat}} to coerce a character vector so that it prints this way.
\code{print.compacto} shows \code{\link{compacto}} matrices with \emph{vertical} column names/labels, and optionally with no gaps between columns.
\code{print.specialprint} can be used to ensure an object (of class \code{specialprint}) displays in any particular way you want, without bothering to define a new S3 class and write a print method. Just give the object an attribute "print" of mode \code{expression}, which can refer to the main argument \code{x} and any other arguments. That expression will be run by \code{print.specialprint}-- see \bold{Examples}.
\code{print.pagertemp} is meant only for internal use by the informal-help viewer.
}
\section{How to override an s3 method}{
Suppose you maintain a package \pkg{mypack} in which you want to mildly redefine an existing S3 method, like \code{mvbutils} does with \code{print.function}. (Drastic redefinitions are likely to be a bad idea, but adding or tweaking functionality can occasionally make sense.) The aim is that other packages which import \code{mypack} should end up using your redefined method, and so should the user if they have explicitly called \code{library( mypack)}. But your redefined method should \emph{not} be visible to packages that don't import \code{mypack}, nor to the user if \code{mypack} has only been loaded implicitly (i.e. if \code{mypack} is imported by another package, so that \code{asNamespace(mypack)} is loaded but \code{package:mypack} doesn't appear on the search path). It's hard to find out how to do this. Here's what I have discovered:

\itemize{
\item For a \emph{new} S3 method (i.e. for a class that doesn't already have one), then you just need to mark it as an \code{S3method} in the \code{mypack} NAMESPACE file (which \code{mvbutils} packaging tools do for you automatically). You don't need to document the new method explicitly, and consequently there's no need to export it. The new method will still be found when the generic runs on an object of the appropriate class.
\item If you're modifying an existing method, you can't just declare it as \code{S3method} in the NAMESPACE file of \code{mypack}. If that's all you did, R{} would complain that it already has a registered method for that class--- fair enough. Therefore, you also have to redeclare and export the \emph{generic}, so that there's a "clean slate" for registering the method (specifically, in the S3 methods table for \code{mypack}, where the new generic lives). The new generic will probably be identical to the existing generic, very likely just a call to \code{UseMethod}. Because it's exported, it needs to be documented; you can either just refer to base-R documentation (but you still need all the formal stuff for Arguments etc, otherwise RCMD CHECK complains), or you can duplicate the base-R doco with a note. \code{\link{help2flatdoc}} is useful here, assuming you're wisely using \code{mvbutils} to build & maintain your package.
\item If you redeclare the generic, you also need to make sure that your \emph{method} is \emph{exported} as well as S3-registered in the NAMESPACE file of \code{mypack}. This is somehow connected with the obscure scoping behaviour of \code{UseMethod} and I don't really understand it, but the result is: if you don't export your method, then it's not found by the new generic (even though it exists in \code{asNamespace(mypack)}, which is the environment of the new generic, and even though your method is also S3-registered in that same environment). Because you export the method, you also need to document it.
\item Unfortunately, the new generic won't know about the methods already registered for the old generic. So, for most generics (exceptions listed later), you will also have to define a \code{generic.default} method in \code{mypack}--- and you need to export and therefore document it too, as per the previous point. This \code{generic.default} just needs to invoke the original generic, so that the already-registered S3 methods are searched. However, this can lead to infinite loops if you're not careful. See \code{mvbutils:::print.default} for how to do it. If you were redefining a generic that was originally (or most recently) defined somewhere other than \code{baseenv()}, then you'd need to replace the latter with \code{asNamespace(<<original.defining.package>>)}.
\item Because your new \code{generic.default} might invoke any of the pre-existing (or subsequently-registered) methods of the \emph{original} generic, you should just make its argument list \code{x,...}. In other words, don't name individual arguments even if they are named in the original \code{generic.default} (eg for \code{print.default}).
\item Objects of mode \code{name}, \code{call}, and \code{"("} or \code{"{"} or \code{"<-"} (special types of \code{call}) cause trouble in \code{generic.default} (at least using the approach in the previous point, as in \code{mvbutils:::print.default}). Unless they have a specific method, the object will be automatically evaluated. So if your generic is ever likely to be invoked on a \code{call} object, you'll need a special \code{generic.call} method, as in \code{mvbutils:::print.call}; the same goes for those other objects.
\item A few generics--- \code{\link{rbind}} and \code{cbind}, for example--- use their own internal dispatch mechanism and don't have e.g. an \code{rbind.default}. Of course, there is a default behaviour, but it's not defined by an R-level function; see \code{?InternalGenerics}. For these generics, the previous point wouldn't work as a way of looking for existing methods. Fortunately, at least for \code{\link{rbind}}, things seem to "just work" if your redefined generic simply runs the code of the base generic (but don't call the latter directly, or you risk infinite loops--- just run its body code). Then, if \emph{your} generic is run, the search order is (1) methods registered for \emph{your} generic in \code{asNamespace("mypack")}, whether defined in \code{mypack} itself or subsequently registered by another package that uses \code{mypack}, (2) methods defined/registered for the base generic (ie in the original generic's namespace), (3) the original "implicit default method". But if the \emph{original} generic is run (e.g. from another package that doesn't import \code{mypack}), then step (1) is skipped. This is good; if another package \pkg{pack2} imports \code{mypack} and registers an S3 method, the S3 registration will go into the \code{mypack} S3 lookup table, but if \code{pack2} \emph{doesn't} import \code{mypack} then the S3 registration will go into the base S3 lookup table (or the lookup table for whichever package the generic was originally defined in, eg package \pkg{stats}).
}
}
\examples{
\dontrun{
# Special methods shown below; basic behaviour of 'print', 'print.default',
# and 'print.function' is as for base-R
#cat
ugly.bugly <- c( 'A rose by any other name', 'would annoy taxonomists')
ugly.bugly
#[1] "A rose by any other name"                 "would annoy taxonomists"
as.cat( ugly.bugly) # calls print.cat--- no clutter
#A rose by any other name
#would annoy taxonomists
# nullprint:
biggo <- 1:1000
biggo
# [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
# [2] 19  20  21  22  23  24  25  26  27  28 etc...
oldClass( biggo) <- 'nullprint'
biggo # calls print.nullprint
# nuthin'
# specialprint:
x <- matrix( exp( seq( -20, 19, by=2)), 4, 5)
attr( x, 'print') <- expression( \{
    x[] <- sprintf( '\%12.2f', x);
    class( x) <- 'noquote';
    attr( x, 'print') <- NULL;
    print( x)
  \})
class( x) <- 'specialprint'
x # calls print.specialprint; consistently formatted for once
#     [,1]         [,2]         [,3]         [,4]         [,5]
#[1,]         0.00         0.00         0.02        54.60    162754.79
#[2,]         0.00         0.00         0.14       403.43   1202604.28
#[3,]         0.00         0.00         1.00      2980.96   8886110.52
#[4,]         0.00         0.00         7.39     22026.47  65659969.14
}
}
\keyword{misc}
