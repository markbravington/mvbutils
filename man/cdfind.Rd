\name{cdfind}
\alias{cdfind}
\alias{cdregexpr}
\alias{cdtree}
\alias{cd.change.all.paths}
\alias{cd.write.mvb.tasks}
\alias{cditerate}
\alias{plot.cdtree}
\title{Hierarchy-crawling functions for cd-organized workspaces}
\description{
These functions work through part or all of a workspace (task) hierarchy set up via \code{\link{cd}}. \code{cdfind} searches for objects through the (attached and unattached) task hierarchy. \code{cdtree} displays the hierarchy structure. \code{cd.change.all.paths} is useful for moving or migrating all or part of the hierarchy to new disk directories. \code{cd.write.mvb.tasks} sets up sourceable text representations of the hierarchy, as a safeguard. \code{cditerate} is the engine that crawls through the hierarchy, underpinning the others; you can write your own functions to be called by \code{cditerate}.

If a task folder or its ".RData" file doesn't exist, a warning is given and (obviously) it's not iterated over. If that file does exist but there's a problem while loading it (e.g. a reference to the namespace of a package that can't be loaded-- search for \code{partial.namespaces} in \code{\link{mvbutils.packaging.tools}}) then the iteration is still attempted, because something might be loaded. Neither case should cause an error.
}
\usage{
cdfind( pattern, from = ., from.text, show.task.name=FALSE)
cdregexpr( regexp, from = ., from.text, ..., show.task.name=FALSE)
cdtree( from = ., from.text = substitute(from), charlim = 90)
cd.change.all.paths( from.text = "0", old.path, new.path)
cd.write.mvb.tasks( from = ., from.text = substitute(from))
cditerate( from.text, what.to.do, so.far = vector("NULL", 0), ..., show.task.name=FALSE)
\method{plot}{cdtree}( x, ...) # S3 method for cdtree; normally plot( cdtree(<<args>>))
}
\arguments{
\item{ pattern}{regexpr to be checked against object names.}
\item{ regexp}{regexpr to be checked against function source code.}
\item{ from}{unquoted path specifier (see \code{\link{cd}}); make this 0 to operate on the entire hierarchy.}
\item{ from.text}{use this in place of \code{from} if you want to use a character string instead}
\item{ show.task.name}{(boolean) as-it-happens display of which task is being looked at}
\item{ charlim}{maximum characters per line allowed in graphical display of \code{cdtree}; reduce if unreadable, or change par( \code{cex})}
\item{ old.path}{regexpr showing portion of directory names to be replaced}
\item{ new.path}{replacement portion of directory names}
\item{ what.to.do}{function to be called on each task (see \bold{Details})}
\item{ so.far}{starting value for accumulated list of function results}
\item{ ...}{further fixed arguments to be passed to \code{what.to.do} (for \code{cditerate}), or \code{grep} (for \code{cdregexpr}), or \code{\link{foodweb}} (for \code{plot.cdtree})}
\item{ x}{result of a call to \code{cdtree}, for plotting}
}
\value{
\code{cdfind} returns a list with one element for each object that is found somewhere; each such element is a character vector showing the tasks where the object was found.
\code{cdregexpr} returns a list with one element for each task where a function whose source matches the regexpr is found; the names of each list element names the functions within that task (an ugly way to return results, for sure).
\code{cdtree} returns an object of class \code{cdtree}, which is normally printed with indentations to show the hierarchy. You can also \code{plot(cdtree(...))} to see a graphical display.
\code{cd.change.all.paths} and \code{cd.write.mvb.tasks} do not return anything useful.
}
\details{
All these functions start by default from the task that is currently top of the search list, and only look further down the hiearchy (i.e. to unattached descendents). To make them work through the whole hierarchy, supply \code{0} as the \code{from} argument. \code{cdtree} has a \code{plot} method, useful for complicated task hierarchies.

If you want to automatically crawl through the task hierarchy to do something else, you can write a wrapper function which calls \code{cditerate}, and an inner function to be passed as the \code{what.to.do} argument to \code{cditerate}. The wrapper function will typically be very short; see the code of \code{cdfind} for an example.

The inner function (typically called \code{cdsomething.guts}) must have arguments \code{found}, \code{task.dir}, \code{task.name}, and \code{env}, and may have any other arguments, which will be set according as the \code{...} argument of \code{cditerate}. \code{found} accumulates the results of previous calls to \code{what.to.do}. Your inner function can augment \code{found}, and should return the (possibly augmented) \code{found}. As for the other parameters: \code{task.dir} is obvious; \code{task.name} is a character(1) giving the full path specifier, e.g. \code{"ROOT/mytask"}; and \code{env} holds the environment into which the task has been (temporarily) loaded. \code{env} allows you to examine the task; for instance, you can check objects in the task by calling \code{ls(env=env)} inside your \code{what.to.do} function. See the code of \code{cdfind.guts} for an example.
}
\author{Mark Bravington}
\examples{
cdfind( ".First", 0) # probably returns list( .First="ROOT")
}
\seealso{\code{\link{cd}}
}
\keyword{utilities
}
