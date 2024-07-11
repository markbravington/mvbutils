\name{mvbutils.packaging.tools}
\alias{mvbutils.packaging.tools}
\title{How to create & maintain packages with mvbutils}
\description{
This document covers:

\itemize{
\item using \code{mvbutils} to create a new package from scratch;
\item using \code{mvbutils} to maintain a package you've created (e.g. edit it while using it);
\item converting an existing package into \code{mvbutils}-compatible format;
\item how to customize the package-creation process.
}

For clarity, the simplest usage is presented first in each case. For how to do things differently, first look further down this document, then in the documentation for \code{\link{pre.install}} and perhaps \code{\link{doc2Rd}}.

You need to understand \code{\link{cd}} and \code{\link{fixr}} before trying any of this.
}
\section{Setting up a package from scratch}{
First, the simplest case: suppose you have some pure R{} code and maybe data that you'd like to make into a package called "Splendid". The bare-minimum steps you need are:-

\itemize{
\item Make sure all the code & data lives in a single task called "Splendid".
\item \code{\link{cd}} to the task above "Splendid"
\item \code{maintain.packages( Splendid)}
\item \code{pre.install( Splendid)}. This will create a "source package" in a subdirectory of Splendid's task directory. The subdirectory will be called "Splendid".
\item Make sure you have all the R{} build tools installed and on your path-- see "R-exts" for details (and NB that if you need to install Latex, then google MikTex & choose a \emph{minimal} install).
\item \code{install.pkg( Splendid)} to do what you'd expect. On Windows, you can alternatively first do \code{build.pkg.binary( Splendid)}, then use R{}'s menus to "Packages/Install from local zip files".
\item \code{library(Splendid)}; your package will be loaded for use, and is also ready for live-editing.
}

Your package will probably just about work now, but the result won't yet be perfect. The additional steps you'll likely need are these:

\itemize{
\item Sort out the \bold{Description file or object}[(]see below[)]
\item Provide \bold{Documentation and metadata}[(]see below[)]
\item Sort out any C/Fortran source code, pre-compiled code, demos, and other additional files (see \code{\link{pre.install}})
\item Move any subtasks of Splendid to one level up the task hierarchy (see \code{\link{maintain.packages}})
}

Once you have set up "Splendid" so that \code{\link{maintain.packages}} works, you won't need to \code{\link{cd}} directly into "Splendid" again--- which is good, because you're not allowed to.
\subsection{Glossary}{
\emph{Task package} is a folder with at least an ".RData" file, linked into the \code{\link{cd}} hierarchy. It contains master copies of the objects in your package, plus perhaps a few other objects required to build the package (e.g. stand-alone items of documentation).

\emph{In-memory task package} is an environment in the current R{} session that contains an image of the task package. Objects in it are never used directly, only as templates for editing. It is loaded by \code{\link{maintain.packages}}, and \code{\link{Save.pos}} uses it to update the task package (usually automatic).

\emph{Source package} is a folder containing, yes, an R-style source package. It is created initially by \code{\link{pre.install}}, and subsequently by \code{\link{patch.install}} or \code{\link{pre.install}}.

\emph{Installed package} is a folder containing, yes, an R-style installed package. It is always created from the source package, initially by \code{\link{install.pkg}} and subsequently by \code{\link{patch.install}} or \code{\link{install.pkg}}.

\emph{Loaded package} is the in-memory version of an installed package, loaded by \code{library}.

\emph{Tarball package} is a zipped-up version of a source package, for distro on non-Windows-Mac platforms or submission to CRAN and subsequent installation via "R CMD INSTALL". Usually it will not contain DLLs of any low-level code, just the source low-level code. It is created by \code{\link{build.pkg}}.

\emph{Binary package} is a special zipped-up version for distro to Windows or Macs that includes actual DLLs, for installation via e.g. the "Packages/Install from local ZIP" menu. It is created by \code{\link{build.pkg.binary}}.

\emph{Built package} is a tarball package or binary package.
}
}
\section{Converting an existing package}{
Suppose you have already have a \emph{source} package "hardway", and would like to try maintaining it via \code{mvbutils}. You'll need to create a task package, then create a new version of the source package, then re-install it. The first step is to call \code{unpackage( hardway)} to creat the task package "hardway" in a subdirectory of the current task. Plain-text documentation will be attached to functions, or stored as ".doc" text objects. All functions and documentation must thereafter be edited using \code{\link{fixr}}. The full sequence is something like:

\preformatted{
  # Create task package in subdirectory of current:
  unpackage( "path/to/existing/source/package/hardway")
  #
  # Load image into memory:
  maintain.packages( hardway)
  #
  # Make new version of source package:
  pre.install( hardway, ...) # use dir= to control where new source pkg goes
  #
  install.pkg( hardway) # or build.pkg.binary( hardway) followed by "install from local zip file" menu
  #
  library( hardway) # off yer go
}

If you get problems after \code{\link{maintain.packages}}, you might need \code{unmaintain.package( hardway)} to clear out the in-memory copy of the new task package.
}
\section{Documentation and metadata}{
Documentation for functions can be stored as plain text just after a function's source code, as described in \code{\link{flatdoc}}. Just about anything will do-- you don't absolutely have to follow the conventional structure of R{} help if you are really in a hurry. However, the easiest way to add kosheR but skeletal documentation to your function \code{brilliant}, is \code{fixr( brilliant, new.doc=TRUE)}; again, see \code{\link{flatdoc}} and \code{\link{doc2Rd}} if you want to understand what's going on. The format is almost exactly as displayed in plain-text help, i.e. from \code{help(..., help_type="text")}. My recommendation is to just start writing something that looks reasonable, and see if it works. To quickly test the ultimate appearance, you can use e.g. \code{docotest(..Splendid$brilliant)}. More generally, run \code{patch.install(Splendid)} which, as explained in \bold{Maintaining a package} below, updates everything for your package including the help system, so you can then just do \code{?brilliant}. If you run into problems with writing documentation for your functions, then refer to \code{\link{doc2Rd}} for further details of format, such as how to document several functions in the same file.

You can also provide three other types of documentation, for: (i) general use of your package (please do! it helps the user a lot; packages where the doco PDF consists only of an alphabetical list of functions/objects are a pain); (ii) more specific aspects of usage that are not tied to individual functions, such as this file; and (iii) datasets. These types of documentation should be stored in the package as text objects whose name ends in ".doc"; examples of the three types could be "Splendid.package.doc", "glitzograms.with.Splendid.doc", and "earlobes.doc" if you have a dataset \code{earlobes}. See \code{\link{doc2Rd}} for format details.

You must document every function and dataset that the user will see, but you don't need to document any others. The foregoing applies iff your package has a \bold{Namespace}, which it must for R{} 2.14 up.
\subsection{Description file or object}{
When you first create a package from a task via \code{\link{pre.install}}, there probably won't be any DESCRIPTION information, so \code{mvbutils} will create a default "DESCRIPTION" file in your task folder, which it then copies to the source package. However, the default won't really be what you really want, as you'll realize if you type \code{library( help=Splendid)}. You can either manually edit the default "DESCRIPTION" file, or you can use \code{fixtext(Splendid.DESCRIPTION, pkg="Splendid")} to create a text object in your task package, which you then populate with the contents of the default "DESCRIPTION" file, and then edit. If a \code{Splendid.DESCRIPTION} object exists, \code{mvbutils} will use it in preference to a file; I find this tidier, because more of the package metadata lives in a single place, viz. inside the task package.

Apart from the obvious changes needed to the default "DESCRIPTION" file or text object, the most important fields to add are "Imports:" (or "Depends:" for packages that are pre-R2.14 and that also don't have a namespace), to say what other packages are needed by "Splendid". The DESCRIPTION file/text should rarely need to be updated, since the "autoversion" feature (see \code{\link{pre.install}} doco) can be used to take care of version numbering. The most common reason to change the DESCRIPTION is probably to add/remove packages in "Imports"; at present, this pretty much requires you to unload & reload the package, but I may try to expedite this in future versions.
}
\subsection{Vignettes}{
Vignettes can be slow to build, and a teeny bit awkward (because they are a post-hoc addition to R{} adopted only with some reluctance by R-core, is my guess), so they are not handled automatically by \code{\link{pre.install}} or friends. Instead, you have to build them yourself (after the package is loaded) by calling \code{\link{vignette.pkg}}, after which \code{\link{patch.install}} and \code{\link{build.pkg}} etc should work fine. What it does, is put the "built" vignette files (as distinct from your ".Rmd" etc source) into a folder "inst/doc".

There's no automatic generation of an index for vignettes, except for their filenames. To provide more information, use \code{\link{fixtext}} to create a text object in your task package called e.g. \code{mypack.VIGNETTES}, with lines as follows:

\preformatted{
  my.first.vignette: Behold leviathan, mate
  my.second.vignette: What a good idea, to write a vignette
}

The next bit is hopefully obsolete as of 2023, because \code{\link{vignette.pkg}} should get the job done. But here it is anyway...

As a \emph{very} experimental feature, you can also include R{} code for a homebrewed vignette, via a file with the same name but extension ".R" also in "inst/doc". Users can access it as normal for vignette code, via \code{edit( vignette( "my.first.vignette", package="mypack"))} or via doing something to \code{system.file( file.path( "doc", "my.first.vignette.R"), package="mypack")}.

You can put full-on Sweave-style vignettes into a "vignettes" folder, and they should be set up correctly in the source package. Currently, though, they are \bold{not} re-installed by \code{\link{patch.install}}; you need to use \code{\link{build.pkg}} and \code{\link{install.pkg}} (partly defeating the point of these package-building utilities).
\subsection{Very technical details about homebrewed vignettes}{"Rnw stubs" are created for all homebrewed vignettes so that the help system finds them. A rudimentary index will be created for vignettes not mentioned in \code{<<mypack>>.VIGNETTES}. If you create your own "inst/doc/index.html" file, this takes precedence over mvbutil's versions, so that \code{<<mypack>>.VIGNETTES} is not used.
}
}
\subsection{Namespace}{Usually this is automatic. \code{\link{pre.install}} etc automatically creates a "NAMESPACE" file for your package, ensuring inter alia that all documented objects are user-visible. To load DLLs, add a \code{.onLoad} function that contains the body code of \code{\link{generic.dll.loader}} in package \pkg{mvbutils} (thus avoiding dependence on \code{mvbutils}). For more complicated fiddling, see \bold{Customizing package creation}.
\subsection{Packages without namespaces pre r 2 14}{Namespaces only became compulsory with R{} 2.14. If you're setting up your package in an earlier version of R{}, \code{mvbutils} will \emph{not} create a namespace unless it finds a \code{.onLoad} function. To trigger namespacing, just create a \code{.onLoad} with this definition: \code{function( libname, pkgname) {}}.
}
}
}
\section{Maintaining a package}{
Once you have successfully gotten your "Splendid" package installed and loaded the first time, you should rarely need to call \code{\link{install.pkg}} or \code{\link{build.pkg}} etc again, except when you are about to distribute to others. In your own work, after calling \code{\link{maintain.packages}} and \code{library} in an R{} session, you can modify, add and delete functions, datasets, and documentation in your package via the standard functions \code{\link{fixr}}, \code{\link{move}}, and \code{\link{rm.pkg}} (or directly), and these changes will mostly be immediately manifested in the loaded package within your R{} session-- this is "live editing". The changes are made first to the in-memory task package, which will be called e.g. \code{..Splendid}, and then propagated to the loaded package. Don't try to manipulate the loaded package's namespace directly. See \code{\link{maintain.packages}} for details.

To update the installed package (on disk), call \code{patch.install( Splendid)}; this also calls \code{\link{pre.install}} to update the source package, updates the help system in the current session, and does a few other synchronizations. You need to call \code{\link{patch.install}} before quitting R{} to ensure that the changes are manifest in the loaded package the next time you start R{}; otherwise they will only exist in the in-memory task package, and won't be callable.
\subsection{Troubleshooting}{
In rare cases, you may find that \code{maintain.packages( Splendid)} fails. If that happens, there won't be a \code{..Splendid} environment, which means you can't fix whatever caused the load failure. The load failure is (invariably in my experience) caused by a hidden attempt to load a namespaced package, which is failing for yet another reason, usually something in its \code{.onLoad}; that package might or might not be "Splendid" itself. If you can work out what other package is trying to load itself-- say \code{badpack}-- you can temporarily get round the problem by making use of the character vector \code{partial.namespaces}, which lives in the "mvb.session.info" search environment, as follows:

\preformatted{
  partial.namespaces <<- c( partial.namespaces, "badpack")
}

That will prevent execution of \code{badpack:::.onLoad}. Consequently \code{badpack} won't be properly loaded, but at least the task package will be loaded into \code{..Splendid}, so that you can make a start on the problem. If you can't work out which package is causing the trouble, try

\preformatted{
  partial.namespaces <<- "EVERY PACKAGE"
}

After that, no namespaced package will load properly, so remember to clear \code{partial.namespaces <<- NULL} before resuming normal service.

Occasionally (usually during \code{\link{patch.install}}), you might see R{} errors like "cannot allocate vector of size 4.8Gb". I think this happens when some internal cache gets out-of-synch. It doesn't seem to cause much damage to the installed package, but once it's happened in an R{} session, it tends to happen again. I usually quit & restart R{}.

You might also find \code{\link{find.lurking.envs}} useful, via \code{eapply( ..Splendid, find.lurking.envs)}; this will show any functions (or other things) in \code{..Splendid} that have accidentally acquired a non-standard environment such as a namespace, which can trigger a "hidden" package load attempt. The environment for all functions in \code{..Splendid} \emph{should} probably be \code{.GlobalEnv}; the environments in the \emph{loaded} package will be different, of course.

It's rare to need to manually inspect either the source package or the installed package. But if you do, then \code{\link{spkg}} helps for the former, e.g. \code{dir( spkg( mypack))}; and \code{system.file} helps for the latter, e.g. \code{system.file( package="mypack")}, or \code{system.file( file.path( "help", "AnIndex"), package="mypack")}.
}
}
\section{Distributing and checking}{
\code{\link{build.pkg}} calls R{} CMD BUILD to create a "tarball" of the package (a ".tar.gz" file), which is the appropriate format for distribution to Unix folk and submission to CRAN. \code{\link{build.pkg.binary}} creates a binary package (a ".zip" file), suitable for Windows or Macs. \code{\link{check.pkg}} runs R{} CMD CHECK (but see next paragraph for a quicker alternative), which is required by CRAN and sometimes useful at other times. These \code{.pkg} functions are pretty simple wrappers to the R{} CMD tools with similar names. However, for those with imperfect memories and limited time, there are enough arcane and mutable nuances with the "raw" R{} CMD commands (including the risk of inadvertently deleting existing installations) to make the wrappers in \code{mvbutils} useful.

Various functions in the \pkg{tools} package can be used to quickly check specific aspects of an \emph{installed} package, without needing a full-on, and slow, R{} CMD CHECK. In particular, I sometimes use

\preformatted{
  codoc( spkg( mypack)) # also spkg( "mypack"), spkg( ..mypack)
  undoc( spkg( ..mypack))
}

Nothing is printed unless a problem is found, so a blank result is good news! It's also possible to run other tools such as \code{checkTnF} and \code{checkFF} similarly.

By default, \code{mvbutils} adds code to the source package to circumvent the CRAN checks for "no visible function/binding", which I consider to be a waste of time; for example, unless circumvented they generate 338 false positives for package \pkg{mvbutils}. If for some reason you actually want these checks, see "Overriding defaults" in \code{\link{pre.install}}.
}
\section{Folders and different r versions}{
Life can get complicated when there are several versions of R{} around, particularly when they require different package formats at source or build or install time (eg R{} 2.10, 2.12, R{} 3.0). \code{\link{install.pkg}} etc do their best to simplify this for you. You won't normally need to know the details unless you are trying to maintain several versions of your package for different versions of R{} for distribution to other people who use those different R{} versions. But if you do need to know the details, then the default folder structure is as follows. If the task package lives in folder "mypack", then the source package is created by \code{\link{pre.install}} in "mypack/mypack", and the built package(s) will go into folders such as "mypack/R2.15" depending on what R{} version is running.

Note that your \emph{task package} can only ever have one version; if different behaviour is required for different R{} versions, then you need to code this up your functions, or via some trickery in \code{.onLoad}.
\subsection{Built packages}{
Building comes first: the tarballed/zipped packages from \code{\link{build.pkg}} and \code{\link{build.pkg.binary}} are placed in a folder parallel to the source package, with a name of the form "Rx.y". \code{mvbutils} tries to be sensible about what "x.y" should be. It will never be newer than the \emph{running} R{} version. It will never be older than the most recent major R{} version that required mandatory package rebuilds (eg R{} 3.0 and R{} 2.12). If one or more folders already exist that satisfy those properties, the highest-numbered one will be used. If not, a new folder will be created with the current R{} major version (eg R{} 2.15.3 will trigger a folder "R2.15"). You can create your own "Rx.y" folder, for instance if the current version of your package requires an R{} feature only found in R{} version "x.y". Also, \code{mvbutils} knows which R{} versions change the format of built packages, and will create a new folder for such a version if required.

The default behaviour is therefore that \code{build.pkg.<binary>} will keep building into the same folder. For example, if at some point a "mypack/R2.12" folder was created, then that's where all builds will be sent regardless of the running R{} version, until you either manually create an "mypack/Rx.y" folder that's closer to the running R{} version, or the latter hits 3.0 which automatically triggers the creation of a new "mypack/R3.0" folder. Thanks to the "autoversion" feature of \code{\link{pre.install}}, the version number of the build will change whenever \code{<pre/patch>.install} is used. (Note that old built packages are not removed until/unless you explicitly call \code{\link{cull.old.builds}}, although it's "good housekeeping" to do the latter occasionally.) By manually creating new "Rx.y" folder when necessary, you can ensure that there \emph{won't} be any updates to built packages for R{} older than "x.y", which gives a kind of "checkpoint" feature; your built packages for older versions of R{} (ie for distribution to users of those older R{} versions) won't be accidentally zapped by \code{\link{cull.old.builds}} housekeeping, and you can be sure that old code running under old versions of R{} will still work.

What this does \emph{not} let you do easily, is use your current R{} version to create updated versions of your package for R-versions that pre-date the most up-to-date "Rx.y" folder. For example, if you are running R3.0, there is guaranteed to be an "R3.0" folder, so calling \code{build.pkg<.binary>} won't build new packages in an "R2.15" folder. Again, usually this doesn't matter, because new "Rx.y" folders are only rarely created automatically, so builds will tend to stay in the same folder and the newest version will be accessible to all. But sometimes it is a hassle... Nevertheless, I have managed to maintain parallel versions of my packages across the R2.15-R3.0 change, by (sequentially) running two R{} versions and calling \code{build.pkg<.binary>} from each. (Note that \code{build.pkg<.binary>} can only build in the format of running R{} version-- you can't "cross-build" for different built formats from the same R{} session.)
}
\subsection{Source packages}{
R{} occasionally demands a change in \emph{source} package format, as opposed to \emph{built} package format (as with R{} 3.0). (IIRC one example is R{} 2.10, with the change in helpfile format.) Then you face the problem of how to keep several source packages. This can be controlled by \code{options("mvbutils.sourcepkgdir.postfix")}, which is appended to the name of the folder where your source package will be created and used for building or installing. The default is the empty string \code{""}, so that the default source package folder for "mypack" is "mypack/mypack". To allow for multiple source package versions, you could put something like this in your \code{.First} or ".Rprofile":

\preformatted{
  if( getRversion() >= numeric_version( '4.0')) {
    # New source package format
    options( mvbutils.sourcepkgdir.postfix='[R4]')
  }
}

Everything \emph{should} then work automatically; all source-package operations will refer to "mypack/mypack[R4]" if you are running version 4 or above, or to "mypack/mypack" if you are running an earlier R{} version, and you should never really need to know the source package foldername yourself (\code{\link{build.pkg}} etc do it all for you). This depends on you setting the option yourself, and \emph{has not been tested} yet. Eventually I may hardwire the feature automatically into \code{mvbutils} (or is it better for each source package to go into an appropriate built-package folder? but that sounds a bit like version hell).
}
}
\section{Customizing package creation}{You can customize many aspects of the \pkg{mvbutils} package-creation process, by adding a function \code{pre.install.hook.Splendid} to your package. See \code{\link{pre.install}} for further details.
}
\keyword{misc}
