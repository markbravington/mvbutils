\name{mlazy}
\alias{mlazy}
\alias{mtidy}
\alias{demlazy}
\alias{mcachees}
\alias{attach.mlazy}
\title{Cacheing objects for lazy-load access}
\description{
\code{mlazy} and friends are designed for handling collections of biggish objects, where only a few of the objects are accessed during any period, and especially where the individual objects might change and the collection might grow or shrink. As with "lazy loading" of packages, and the \code{gdata/ASOR} packages, the idea is to avoid the time & memory overhead associated with loading in numerous huge R{} binary objects when not all will be needed. Unlike lazy loading and \code{gdata}, \code{mlazy} caches each mlazyed object in a separate file, so it also avoids the overhead that would be associated with changing/adding/deleting objects if all objects lived in the same big file. When a workspace is \code{\link{Save}}d, the code updates only those individual object files that need updating.

Apart from possibly \code{environment} objects (see subsection), \code{mlazy} does not require any special structure for object collections; in particular, the data doesn't have to go into a package. \code{mlazy} is particularly useful for users of \code{\link{cd}} because each \code{\link{cd}} to/from a task causes a read/write of the binary image file (usually ".RData"), which can be very large if \code{mlazy} is not used. Read DETAILS next. Feedback is welcome.
\subsection{Environments}{Sometimes nowadays I use an R{} \code{environment} instead of a \code{list} to store stuff, usually to take advantage of inheritance. They are a bit different to other R{} objects, and if you don't understand them properly, then be careful! The salient point here is that an \code{environment} is really a pointer, and unlike other R{} objects, two R{} \code{environment} "objects" can actually point to exactly the same "shared memory". Now, \code{mlazy} works fine with single copy of an \code{environment} (when it's saved into the cache folder, R{} will automatically include any necessary parent environments, etc) \emph{but} if you have two objects that point to the same "real" environment, and you \code{mlazy} just one of them or both of them, then I don't know what's going to happen when you reload them; do you now end up with two separate unlinked copies, or what? So... like I said, be very careful with \code{mlazy} and \code{environment} objects. (It would be handy if there was a tool to check for other references to a given environment, which must be buried inside R{}'s internal structures since reference-counting is certainly used there. But...)
}
}
\usage{
mlazy( ..., what, envir=.GlobalEnv, save.now=TRUE)
  # cache some objects
mtidy( ..., what, envir=.GlobalEnv)
  # (cache and) purge the cache to disk, freeing memory
demlazy( ..., what, envir=.GlobalEnv)
  # makes 'what' into normal uncached objects
mcachees( envir=.GlobalEnv)
  # shows which objects in  envir are cached
attach.mlazy( dir, pos=2, name=)
  # load mcached workspace into new search environment,
  # or create empty s.e. for cacheing
}
\arguments{
\item{ ...}{unquoted object names, overridden by \code{what} if supplied}
\item{ what}{character vector of object names, all from the same environment. For \code{mtidy} and \code{demlazy}, defaults to all currently-cached objects in \code{envir}}
\item{ envir}{environment or position on the search path, defaulting to the environment where \code{what} or \code{objs} live.}
\item{ save.now}{see DETAILS}
\item{ dir}{name of directory, relative to \code{\link{task.home}}.}
\item{ pos}{numeric position of environment on search path, 2 or more}
\item{ name}{name to give environment, defaulting to something like "data:current.task:dir".}
}
\value{These functions are used only for their side-effects, except for \code{cachees} which returns a character vector of object names.
}
\section{More details}{
All this is geared to working with saved images (i.e. ".RData" or "all.rda" files) rather than creating all objects anew each session via \code{source}. If you use the latter approach, \code{mlazy} will probably be of little value.

The easiest way to set up cacheing is just to create your objects as normal, then call

\code{mlazy( <<objname1>>, <<objname2>>, <<etc>>)}

\code{Save()}

This will not seem to do much immediately-- your object can be read and changed as normal, and is still taking up memory. The memory and time savings will come in your next R{} session in this workspace.

You should never see any differences (except in time & memory usage) between working with cached (AKA mlazyed) and normal uncached objects.[One minor exception is that cacheing a function may stuff up the automatic backup system, or at any rate the "backstop" version of it which runs when you \code{\link{cd}}. This is deliberate, for speeding up \code{\link{cd}}. But why would you cache a \emph{function} anyway?]

\code{mlazy} itself doesn't save the workspace image (the ".RData" or "all.rda" file), which is where the references live; that's why you need to call \code{\link{Save}} periodically. \code{save.image} and \code{save} will \bold{not} work properly, and nor will \code{load}-- see NOTE below. \code{\link{Save}} doesn't store cached objects directly in the ".RData" file, but instead stores the uncached objects as normal in \code{.RData} together with a special object called something like \code{.mcache00} (guaranteed not to conflict with one of your own objects). When the \code{.RData} file is subsequently reloaded by \code{\link{cd}}, the presence of the \code{.mcache00} object triggers the creation of "stub" objects that will load the real cached objects from disk when and only when each one is required; the \code{.mcache00} object is then deleted. Cached objects are loaded & stored in a subdirectory "mlazy" from individual files called "obj*.rda", where "*" is a number.

\code{mlazy} and \code{\link{Save}} do not immediately free any memory, to avoid any unnecessary re-loading from disk if you access the objects again during the current session. To force a "memory purge" \emph{during} an R{} session, you need to call \code{mtidy}. \code{mtidy} purges its arguments from the cache, replacing them by \code{promise}s just as when loading the workspace; when a reference is next accessed, its cached version will be re-loaded from disk. \code{mtidy} can be useful if you are looping over objects, and want to keep memory growth limited-- you can \code{mtidy} each object as the last statement in the loop. By default, \code{mtidy} purges the cache of all objects that have previously been cached. \code{mtidy} also caches any formerly uncached arguments, so one call to \code{mtidy} can be used instead of \code{mlazy( ...); mtidy( ...)}.

\code{\link{move}} understands cached objects, and will shuffle the files accordingly.

\code{demlazy} will \bold{delete} the corresponding "obj*.rda" file(s), so that only an in-memory copy will then exist; don't forget to \code{\link{Save}} soon after.
\subsection{Warning}{The system function \code{load} does not understand cacheing. If you merely \code{load} an image file saved using \code{\link{Save}}, cached objects will not be there, but there will be an extra object called something like \code{.mcache00}. Hence, if you have cached objects in your ROOT task, they will not be visible when you start R{} until you load the \code{mvbutils} library-- another fine reason to do that in your \code{.First}. The \code{.First.lib} function in \code{mvbutils} calls \code{setup.mcache( .GlobalEnv)} to automatically prepare any references in the ROOT task.
}
\subsection{Cacheing in other search environments}{
It is possible to cache in search environments other the current top one (AKA the current workspace, AKA \code{.GlobalEnv}). This could be useful if, for example, you have a large number of simulated datasets that you might need to access, but you don't want them cluttering up \code{.GlobalEnv}. If you weren't worried about cacheing, you'd probably do this by calling \code{attach( "<<filename>>")}. The cacheing equivalent is \code{attach.mlazy( "cachedir")}. The argument is the name of a directory where the cached objects will be (or already are) stored; the directory will be created if necessary. If there is a ".RData" file in the directory, \code{attach.mlazy} will load it and set up any references properly; the ".RData" file will presumably contain mostly references to cached data objects, but can contain normal uncached objects too.

Once you have set up a cacheable search environment via \code{attach.mlazy} (typically in search position 2), you can cache objects into it using \code{mlazy} with the \code{envir} argument set (typically to 2). If the objects are originally somewhere else, they will be transferred to \code{envir} before cacheing. Whenever you want to save the cached objects, call \code{Save.pos(2)}.

You will probably also want to modify or create the \code{.First.task} (see \code{\link{cd}}) of the current task so that it calls \code{attach.mlazy("<<cache directory name>>")}. Also, you should create a \code{.Last.task} (see \code{\link{cd}}) containing \code{detach(2)}, otherwise \code{cd(..)} and \code{cd(0/...)} won't work.
}
\subsection{Options}{
By default, \code{mlazy} now saves & loads into a auto-created subdirectory called "mlazy". In the earliest releases, though, it saved "obj*.rda" files into the same directory as ".RData". It will now \bold{move} any "obj*.rda" files that it finds alongside ".RData" into the "mlazy" subdirectory. You can (possibly) override this by setting \code{options( mlazy.subdir=FALSE)}, but the default is likely more reliable.

By default, there is no way to figure out what object is contained in a "obj*.rda" without forcibly loading that file or inspecting the \code{.mcache00} object in the "parent" \code{.RData} file-- not that you should ever need to know. However, if you set \code{options( mlazy.index=TRUE)} (\bold{recommended}), then a file "obj.ind" will be maintained in the "mlazy" directory, showing (object name - value) pairs in plain text (tab-separated). For directories with very large numbers of objects, there may be some speed penalty. If you want to create an index file for an existing "mlazy" directory that lacks one, \code{\link{cd}} to the task and call \code{mvbutils:::mupdate.mcache.index.if.opt(mlazy.index=TRUE)}.

See \code{\link{Save}} for how to set compression options, and \code{save} for what you can set them to; \code{options(mvbutils.compression_level=1)} may save some time, at the expense of disk space.
}
\subsection{Troubleshooting}{
In the unlikely event of needing to manually load a cached image file, use \code{\link{load.refdb}}-- \code{\link{cd}} and \code{attach.mlazy} do this automatically.

In the unlikely event of lost/corrupted data, you can manually reload individual "obj*.rda" files using \code{load}-- each "obj*.rda" file contains one object stored with its correct name. Before doing that, call \code{demlazy( what=mcachees())} to avoid subsequent trouble. Once you have reloaded the objects, you can call \code{mlazy} again.

See \bold{Options} for the easy way to check what object is stored in a particular "obj*.rda" file. If that feature is turned off on your system, the failsafe way is to load the file into a new environment, e.g. \code{e <- new.env(); load( "obj99.rda", e); ls( e)}.

To see how memory changes when you call \code{mlazy} and \code{mtidy}, call \code{gc()}.

To check object sizes \emph{without} actually loading the cached objects, use \code{\link{lsize}}. Many functions that iterate over all objects in the environment, such as \code{eapply}, will cause \code{mlazy} objects to be loaded.

Housekeeping of "obj**.rda" files happens during \code{\link{Save}}; any obsolete files (i.e. corresponding to objects that have been \code{remove}d) are deleted.
}
\subsection{Inner workings}{
What happens: each workspace acquires a \code{mcache} attribute, which is a named numeric vector. The absolute values of the entries correspond to files-- 53 corresponds to a file "obj53.rda", etc., and the names to objects. When an object \code{myobj} is \code{mlazy}ed, the \code{mcache} is augmented by a new element named "myobj" with a new file number, and that file is saved to disk. Also, "myobj" is replaced with an active binding (see \code{\link{makeActiveBinding}}). The active binding is a function which retrieves or sets the object's data within the function's environment. If the function is called in change-value mode, then it also makes negative the file number in \code{mcache}. Hence it's possible to tell whether an object has been changed since last being saved.

When an object is first \code{mlazy}ed, the object data is placed directly into the active binding function's environment so that the function can find/modify the data. When an object is \code{mtidy}ed, or when a cached image is loaded from disk, the thing placed into the A.B.fun's environment is not the data itself, but instead a \code{promise} saying, in effect, "fetch me from disk when you need me". The promise gets forced when the object is accessed for reading or writing. This is how "lazy loading" of packages works, and also the \pkg{gdata} package. However, for \code{mlazy} there is the additional requirement of being able to determine whether an object has been modified; for efficiency, only modified objects should be written to disk when there is a \code{\link{Save}}.

There is presumably some speed penalty from using a cache, but experience to date suggests that the penalty is small. Cached objects are saved in compressed format, which seems to take a little longer than an uncompressed save, but loading seems pretty quick compared to uncompressed files.
}
}
\seealso{\code{\link{lsize}}, \code{gc}, package \pkg{gdata}, package \pkg{ASOR}
}
\author{Mark Bravington}
\examples{
\dontrun{
biggo <- matrix( runif( 1e6), 1000, 1000)
gc() # lots of memory
mlazy( biggo)
gc() # still lots of memory
mtidy( biggo)
gc() # better
biggo[1,1]
gc() # worse; it's been reloaded
}
}
\keyword{programming}
\keyword{data
}
