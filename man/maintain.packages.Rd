\name{maintain.packages}
\alias{maintain.packages}
\alias{unmaintain.package}
\title{Set up task package for live editing}
\description{
See \code{\link{mvbutils.packaging.tools}} before reading or experimenting!

Set up task package(s) for editing and/or live-editing. Usually called in \code{.First} or \code{.First.task}. You need to be \code{\link{cd}}ed into the parent task of your task-package. \code{maintain.packages} must be called \emph{before} loading the package via \code{library} or \code{require}. The converse, \code{unmaintain.package}, is rarely needed; it's really only meant for when \code{\link{unpackage}} doesn't work properly, and you want a "clean slate" task package.
}
\usage{
# E.g. in your .First, after library( mvbutils), or in...
# ... a '.First.task' above yr task-package
maintain.packages(..., character.only = FALSE, autopatch=FALSE)
unmaintain.package( pkg, character.only = FALSE)
}
\arguments{
\item{ ...}{names of packages, unquoted unless \code{character.only} is TRUE. Package names must correspond to subtasks of the current task.}
\item{ character.only}{see above}
\item{ pkg}{name of package, unquoted unless \code{character.only} is TRUE.}
\item{ autopatch}{whether to \code{patch.install} out-of-date installed packages (default FALSE, but TRUE is common).}
}
\details{\code{maintain.packages( mypack)} loads a copy of your task-package "mypack" (as stored in its ".RData" file) into a environment \code{..mypack} (an "in-memory-task-package"), which itself lives in the "mvb.session.info" environment on the search path. You don't normally need to know this, because normally you'd modify/create/delete objects in the package via \code{\link{fixr}} or \code{fixr(..., pkg="mypack")} or \code{rm.pkg( ..., pkg="mypack")}. But to move objects between the package and other tasks, you do need to refer to the in-memory task package, e.g. via \code{move( ..., from=..Splendid, to=subtask/of/current)}. In most cases, you will be prompted afterwards for whether to save the task package on disk, but you can always do yourself via \code{Save.pos( ..Splendid)}. Note that only these updates and saves only update the \emph{task package} and the \emph{loaded package}. To update the \emph{source package} using the task package, call \code{\link{pre.install}}; to update the \emph{installed package} on disk as well as the source package, call \code{\link{patch.install}}.
\subsection{Creating new things}{It's always safe to create new objects of any type in \code{.GlobalEnv}, then use \code{move(newthing,.,..mypack)}. For a new \emph{function}, you can shortcut this two-step process and create it directly in the in-memory maintained package, via \code{fixr(..mypack$newfun)}; \code{\link{fixr}} will take care of synchronization with the loaded package. This also ought to work for text objects created via \code{\link{fixtext}}. Otherwise, use the two-step route, unless you have a good reason to do the following...
}
\subsection{Directly modifying the maintained package}{
Rarely, you may have a really good reason to directly modify the contents of \code{..mypack}, e.g. via

\preformatted{
  ..mypack$newfun <<- function( x) whatever
}

You can do it, but there are two problems to be aware of. The first is that changes won't be directly propagated to the loaded package, possibly not even after \code{\link{patch.install}} (though they will be honoured when you \code{library()} the package again). That is definitely the case for general data objects, and I'm not sure about functions; however, successful propagation after \code{\link{patch.install}} may happen for a special objects such as \code{mypack.DESCRIPTION} and documentation objects. Hence my general advice is to use \code{\link{fixr}} or \code{\link{move}}.

The second, minor, problem is that you will probably forget to use \code{<<-} and will use \code{<-} instead, so that a local copy of \code{..mypack} will be created in the current task. This is no big deal, and you can just \code{rm} the local copy; the local copy and the master copy in "mvb.session.info" both point to the same thing, and modifying one implies modifying the other, so that deleting the local copy won't lose your changes. \code{\link{Save}} detects accidental local copies of task packages, and omits them from the disk image, so there shouldn't be any problems next time you start R{} even if you completely forget about local/master copies.
}
\subsection{Autopatch}{If \code{autopatch==TRUE}, then \code{maintain.packages} will check whether the corresponding \emph{installed} packages are older than the ".RData" files of the task packages. If they are, it will do a full \code{\link{patch.install}}; if not, it will still call \code{\link{patch.install}} but only to reverse-update any bundled DLLs (see \code{\link{pre.install}}), not to re-install the R-source. I find \code{autopatch} useful with packages containing C code, where a crash in the C code can cause R{} to die before the most recent R-code changes have been "committed" with \code{\link{patch.install}}. When you next start R{}, a call to \code{maintain.packages} with \code{autopatch=TRUE} will "commit" the changes \emph{before} the package is loaded, because you have to call \code{maintain.packages} before \code{library}; this seems to be more reliable than running \code{\link{patch.install}} manually after \code{library} after a restart.
}
}
\section{Maintained packages as tasks}{
If you use \code{mvbutils} to pre-build your package, then your package must exist as a task in the \code{\link{cd}} hierarchy. Older versions of \code{mvbutils} allowed you to \code{\link{cd}} to a maintained package, but this is now forbidden because of the scope for confusion. Thanks to \code{maintain.packages}, there is no compelling need to have the package/task at the top of the search path; \code{\link{fixr}}, \code{\link{move}}, etc work just fine without. If you really do want to \code{\link{cd}} to a maintained package, you must call \code{unmaintain.package} first.

One piece of cleanup that I recommend, is to move any subtasks of "mypack" one level up in the task hierarchy, and to remove the \code{tasks} object from "Splendid" itself, e.g. via something like:

\preformatted{
  cd( task.above.splendid)
  tasks <- c( tasks, combined.file.paths( tasks[ "Splendid"], ..Splendid$tasks))
  # ... combined.file.paths is an imaginary function. Watch out if you've used relative paths!
  rm.pkg( tasks, pkg="Splendid")
}
}
\seealso{\code{\link{mvbutils.packaging.tools}}, \code{\link{fixr}}, \code{\link{pre.install}}, \code{\link{patch.installed}}, \code{\link{unpackage}}
}
\examples{
\dontrun{
# In your .First:
library( mvbutils)
maintain.packages( myfirstpack, mysecondpack, mythirdpack)
# or...
live.edit.list <- c( 'myfirstpack', 'mysecondpack', 'mythirdpack')
maintain.packages( live.edit.list, character.only=TRUE)
library( myfirstpack) # etc
}
}
\keyword{misc}
