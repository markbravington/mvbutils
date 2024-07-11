\name{foodweb}
\alias{foodweb}
\alias{callers.of}
\alias{callees.of}
\alias{plot.foodweb}
\title{Shows which functions call what}
\description{\code{foodweb} is applied to a group of functions (e.g. all those in a workspace); it produces a graphical display showing the hierarchy of which functions call which other ones. This is handy, for instance, when you have a great morass of functions in a workspace, and want to figure out which ones are meant to be called directly. \code{callers.of(funs)} and \code{callees.of(funs)} show which functions directly call, or are called directly by, \code{funs}.
}
\usage{
foodweb( funs, where=1, charlim=80, prune=character(0), rprune,
    ancestors=TRUE, descendents=TRUE, plotting =TRUE, plotmath=FALSE,
    generics=c( "c","print","plot", "["), lwd=0.5, xblank=0.18,
    border="transparent", boxcolor="white", textcolor="black",
    color.lines=TRUE, highlight="red", calc_xpos=plotting, ...)
\method{plot}{foodweb}(x, textcolor, boxcolor, xblank, border, textargs = list(),
    use.centres = TRUE, color.lines = TRUE, poly.args = list(),
    expand.xbox = 1.05, expand.ybox = expand.xbox * 1.2, plotmath = FALSE,
    cex=par( "cex"), ...) # S3 method for foodweb
callers.of( funs, fw, recursive=FALSE)
callees.of( funs, fw, recursive=FALSE)
}
\arguments{
\item{ funs}{character vector OR (in \code{foodweb} only) the result of a previous \code{foodweb} call}
\item{ where}{position(s) on search path, or an environment, or a list of environments}
\item{ charlim}{controls maximum number of characters per horizontal line of plot}
\item{ prune}{character vector. If omitted, all \code{funs} will be shown; otherwise, only ancestors and descendants of functions in \code{prune} will be shown. Augments \code{funs} if required.}
\item{ rprune}{regexpr version of \code{prune}; \code{prune <- funs \%matching\% rprune}. Does NOT augment \code{funs}. Overrides \code{prune} if set.}
\item{ ancestors}{show ancestors of \code{prune} functions?}
\item{ descendents}{show descendents of \code{prune} functions?}
\item{ plotting}{graphical display?}
\item{ plotmath}{leave alone}
\item{ generics}{calls TO functions in \code{generics} won't be shown}
\item{ lwd}{see \code{\link{par}}}
\item{ xblank}{leave alone}
\item{ border}{border around name of each object (\code{TRUE/FALSE})}
\item{ boxcolor}{background colour of each object's text box}
\item{ textcolor}{of each object}
\item{ color.lines}{will linking lines be coloured according to the level they originate at?}
\item{ highlight}{seemingly not used}
\item{ cex}{text size (see "cex" in \code{?par})}
\item{ calc_xpos}{whether to calculate reasonable on-screen positions. Defaults to TRUE if plotting, and FALSE otherwise (to save a bit of time). If you aren't plotting immediately but might plot the results \emph{later}, you should set this to TRUE.}
\item{ ...}{passed to \code{plot.foodweb} and thence to \code{par}}
\item{ textargs}{not currently used}
\item{ use.centres}{where to start/end linking lines. \code{TRUE} is more accurate but less tidy with big webs.}
\item{ expand.xbox}{how much horizontally bigger to make boxes relative to text?}
\item{ expand.ybox}{how much vertically bigger to ditto?}
\item{ poly.args}{other args to \code{rect} when boxes are drawn}
\item{ fw}{an object of class \code{foodweb}, or the \code{funmat} element thereof (see \bold{Value})}
\item{ x}{a foodweb (as an argument to \code{plot.foodweb})}
\item{ recursive}{(\code{callees.of} and \code{callers.of} only) whether to include callee/rs of callee/rs of... (Thanks to William Proffitt for this suggestion.)}
}
\details{
The main value is in the graphical display. At the top ("level 0"), functions which don't call any others, and aren't called by any others, are shown without any linking lines. Functions which do call others, but aren't called themselves, appear on the next layer ("level 1"), with lines linking them to functions at other levels. Functions called only by level 1 functions appear next, at level 2, and so on. Functions which call each other will always appear on the same level, linked by a bent double arrow above them. The colour of a linking line shows what level of the hierarchy it came from.

\code{foodweb} makes some effort to arrange the functions on the display to keep the number of crossing lines low, but this is a hard problem! Judicious use of \code{prune} will help keep the display manageable. Perhaps counterintuitively, any functions NOT linked to those in \code{prune} (which all will be, by default) will be pruned from the display.

\code{foodweb} tries to catch names of functions that are stored as text, and it will pick up e.g. \code{glm} in \code{do.call( "glm", glm.args)}. There are limits to this, of course (?methods?).

The argument list may be somewhat daunting, but the only ones normally used are \code{funs}, \code{where}, and \code{prune}. Also, to get a readable display, you may need to reduce \code{cex} and/or \code{charlim}. A number of the less-obvious arguments are set by other functions which rely on \code{plot.foodweb} to do their display work. Several may disappear in future versions.

If the display from \code{foodweb} is unclear, try \code{foodweb( .Last.value, cex=<<something below 1>>, charlim=<<something probably less than 100>>)}. This works because \code{foodweb} will also accept a \code{foodweb}-class object as its argument. You can also assign the result of \code{foodweb} to a variable, which is useful if you expect to do a lot of tinkering with the display, or to inspect the who-calls-whom matrix by hand.

\code{callers.of} and \code{callees.of} process the output of \code{foodweb}, looking for immediate dependencies only. The second argument will call \code{foodweb} by default, so it may be more efficient to call \code{foodweb} first and assign the result to a variable. NB you can set \code{recursive=TRUE} for the obvious result.
\subsection{Bug in rgui windows graphics}{When plotting the foodweb, there's a display bug in Rgui for windows which somehow causes the fontsize to shrink in each successive calls! Somehow \code{par("ps")} keeps on shrinking. Indeed, on my own machines, calling \code{par(ps=par("ps"))$ps} will show a decreasing value each time... Working around this was very tricky; variants of saving/restoring \code{par} \emph{inside} \code{plot.foodweb} do not work. As of package \pkg{mvbutils} version 2.8.142, there's an attempted fix directly in \code{foodweb}, but conceivably the fixe will somehow cause problems for other people using default graphics windows in Rgui. Let me know if that's you... (in which case I'll add an \code{option()} to not apply the fix).
}
}
\value{
\code{foodweb} returns an object of (S3) class \code{foodweb}. This has three components:
\item{ funmat}{a matrix of 0s and 1s showing what (row) calls what (column). The \code{dimnames} are the function names.}
\item{ x}{shows the x-axis location of the centre of each function's name in the display, in \code{par("usr")} units}
\item{ level}{shows the y-axis location of the centre of each function's name in the display, in \code{par("usr")} units. For small numbers of functions, this will be an integer; for larger numbers, there will some adjustment around the nearest integer}
Apart from graphical annotation, the main useful thing is \code{funmat}, which can be used to work out the "pecking order" and e.g. which functions directly call a given function. \code{callers.of} and \code{callees.of} return a character vector of function names.
}
\examples{
foodweb( ) # functions in .GlobalEnv
# I have had to trim this set of examples because CRAN thinks it's too slow...
# ... though it's only 5sec on my humble laptop. So...
\dontrun{
foodweb( where="package:mvbutils", cex=0.4, charlim=60) # yikes!
foodweb( c( find.funs("package:mvbutils"), "paste"))
# functions in .GlobalEnv, and "paste"
foodweb( find.funs("package:mvbutils"), prune="paste")
# only those parts of the tree connected to "paste";
# NB that funs <- unique( c( funs, prune)) inside "foodweb"
foodweb( where="package:mvbutils", rprune="aste")
# doesn't include "paste" as it's not in "mvbutils", and rprune doesn't augment funs
foodweb( where=asNamespace( "mvbutils")) # secret stuff
fw <- foodweb( where="package:mvbutils")
}
fw <- foodweb( where=asNamespace( "mvbutils")) # also plots
fw$funmat # a big matrix
callers.of( "mlocal", fw)
callees.of( "find.funs", fw)
# ie only descs of functions whose name contains 'name'
foodweb( where=asNamespace( 'mvbutils'), rprune="name", ancestors=FALSE, descendents=TRUE)
}
\keyword{misc}
