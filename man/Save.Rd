\name{Save}
\alias{Save}
\alias{Save.pos}
\title{Save R objects}
\description{These function resemble \code{save} and \code{save.image}, with two main differences. First, any functions which have been \code{mtrace}d (see package \pkg{debug}) will be temporarily untraced during saving (the \pkg{debug} package need not be loaded). Second, \code{Save} and \code{Save.pos} know how to deal with lazy-loaded objects set up via \code{\link{mlazy}}. \code{Save()} is like \code{save.image()}, and also tries to call \code{savehistory} (see \bold{Details}). \code{Save.pos(i)} saves all objects from the \code{i}th position on the search list in the corresponding ".RData" file (or "all.rda" file for image-loading packages, or "*.rdb/*.rdx" for lazyloading packages). There is less flexibility in the arguments than for the system equivalents. If you use the \code{\link{cd}} system in \code{mvbutils}, you will rarely need to call \code{Save.pos} directly; \code{\link{cd}}, \code{\link{move}} and \code{\link{FF}} will do it for you.
}
\usage{
Save()
Save.pos( pos, path, ascii=FALSE)
}
\arguments{
\item{  pos}{string or numeric position on search path, or environment (e.g. \code{..mypack} if "mypack" is a maintained-package).}
\item{  path}{directory or file to save into (see \bold{Details}).}
\item{  ascii}{file type, as per \code{\link{save}}}
}
\details{
There is a safety provision in \code{Save} and \code{Save.pos}, which is normally invisible to the user, but can be helpful if there is a failure during the save process (for example, if the system shuts down unexpectedly). The workspace image is first saved under a name such as "n.RData" (the name will be adapted to avoid clashes if necessary). Then, if and only if the new image file has a different checksum to the old ".RData" file, the old file will be deleted and the new one will be renamed ".RData"; otherwise, the new file will be deleted. This also means that the ".RData" file will not be updated at all if there have been no changes, which may save time when synchronizing file systems or backing up.

Two categories of objects will not be saved by \code{Save} or \code{Save.pos}. The first category is anything named in \code{options( dont.save)}; by default, this is ".packageName", ".SavedPlots", "last.warning", and ".Traceback", and you might want to add ".Last.value". The second category is anything which looks like a maintained package, i.e. an environment whose name starts with ".." and which has attributes "name", "path", and "task.tree". A warning will be given if such objects are found. [From bitter experience, this is to prevent accidents on re-loading after careless mistakes such as \code{..mypack$newfun <- something}; what you \emph{meant}, of course, is \code{..mypack$newfun <<- something}. Note that the accident will not cause any bad effects during the current R{} session, because environments are not duplicated; anything you do to the "copy" will also affect the "real" \code{..mypack}. However, a mismatch will occur if the environment is accidentally saved and re-loaded; hence the check in \code{Save}.]

\code{path} is normally inferred from the \code{path} attribute of the \code{\link{pos}} workspace. If no such attribute can be found (e.g. if the attached workspace was a list object), you will be prompted. If \code{path} is a directory, the file will be called ".RData" if that file already exists, or "R/all.rda" if that exists, or "R/*.rbd" for lazy loads if that exists; and if none of these exist already, then the file will be called ".RData" after all. If you specify \code{path}, it must be a complete directory path or file path (i.e. it will not be interpreted relative to a \code{path} attribute).
\subsection{Compression}{\code{mvbutils} uses the default compression options of \code{\link{save}}, unless you set \code{options()} "mvbutils.compress" and/or "mvbutils.compression_level" to appropriate values as per \code{?save}. The same applies to \code{\link{mlazy}} objects. Setting \code{options(mvbutils.compression_level=1)} can sometimes save quite a bit of time, at the cost of using more disk space. Set these options to NULL to return to the defaults.
}
\subsection{History files}{
\code{Save} calls \code{savehistory()}. With package \pkg{mvbutils} from about version 2.5.6 on, \code{savehistory} and \code{loadhistory} will by default use the same file throughout each and every R{} session. That means everything works nicely for most users, and you really don't need to read the rest of this section unless you are unhappy with the default behaviour.

If you are unhappy, there are two things you might be unhappy about. First, \code{savehistory} and \code{loadhistory} are by default modified to always use the \emph{current} value of the R_HISTFILE environment variable at the time they are called, whereas default R{} behaviour is to use the value when the session started, or ".Rhistory" in the current directory if none was set. I can't imagine why the default would be preferable, but if you do want to revert to it, then try to follow the instructions in \code{?mvbutils}, and email me if you get stuck. Second, the default for R_HISTFILE itself is set by \code{mvbutils} to be the file ".Rhistory" in the \code{.First.top.search} directory-- normally the one you start R{} in. You can change that default by specifying R_HISTFILE yourself before loading \code{mvbutils}, in one of the many ways described by the R{} documentation on \code{?Startup} and \code{?Sys.getenv}.
}
}
\seealso{\code{save}, \code{save.image}, \code{ mtrace} in package \pkg{debug}, \code{\link{mlazy}}
}
\examples{
\dontrun{
Save() #
Save.pos( "package:mvbutils") # binary image of exported functions
Save.pos( 3, path="temp.Rdata") # path appended to attr( search()[3], "path")
}
}
\author{Mark Bravington}
\keyword{debugging}
\keyword{file
}
