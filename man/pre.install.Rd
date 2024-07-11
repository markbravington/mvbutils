\name{pre.install}
\alias{pre.install}
\alias{patch.installed}
\alias{patch.install}
\alias{pre.install.hook...}
\alias{spkg}
\title{Update a source and/or installed package from a task package}
\description{
See \code{\link{mvbutils.packaging.tools}} before reading or experimenting!

\code{pre.install} creates a "source package" from a "task package", ready for first-time installation using \code{\link{install.pkg}}. You must have called \code{maintain.packages( mypack)} at some point in your R{} session before \code{pre.install( mypack)} etc.

\code{patch.install} is normally sufficient for subsequent maintenance of an already-installed package (ie you rarely need call \code{\link{install.pkg}} again). Again, \code{\link{maintain.packages}} must have been called earlier. It's also expected that the package has been loaded via \code{library()} before \code{patch.install} is called, but this may not be required. \code{patch.install} first calls \code{pre.install} and then modifies the installed package accordingly on-the-fly, so there is no need to re-load or re-build or re-install. \code{patch.install} also updates the help system with immediate effect, i.e. during the current R{} session. You don't need to call \code{patch.install} after every little maintenance change to your package during an R{} session; it's usually only necessary when (i) you want updated help, or (ii) you want to make the changes "permanent" (eg so they'll work in your next R{} session). However, it's not a problem to call \code{patch.install} quite often. \code{patch.installed} is a synonym for \code{patch.install}.

It's possible to tweak the source-package-creation process, and this is what 'pre.install.hook..." is for; see \bold{Details} and section on \bold{Overriding defaults} below.

\code{spkg} is a rarely-needed utility that returns the folder of source package created by \code{pre.install}.

Vignettes have to built "manually" (it's easy!), using \code{\link{vignette.pkg}}.
}
\usage{
 # 95\% of the time you just need:
 # pre.install( pkg)
 # patch.install( pkg)
 # Your own hook: pre.install.hook.<<mypack>>( default.list, <<myspecialargs>>, ...)
 pre.install(
     pkg,
     character.only=FALSE,
     force.all.docs=FALSE,
     rewrap.forcibly=FALSE,
     dir.above.source="+",
     autoversion=getOption("mvb.autoversion", TRUE),
     click.version=TRUE,
     R.target.version=getRversion(),
     Roxygen=NULL,
     timeout_Roxygen= getOption( 'mvb.timeout_Roxgyen', 0), # seconds
     vignette.build= TRUE,
     silent=FALSE,
     ...)
 patch.installed(
     pkg,
     character.only= FALSE,
     force.all.docs= FALSE,
     rewrap.forcibly= FALSE,
     help.patch= TRUE,
     DLLs.only= FALSE,
     update.installed.cache= getOption("mvb.update.installed.cache", TRUE),
     pre.inst= !DLLs.only,
     dir.above.source= "+",
     R.target.version= getRversion(),
     autoversion= getOption("mvb.autoversion", TRUE),
     click.version= TRUE,
     vignette.build= FALSE,
     compress.lazyload= getOption( 'mvb.compress.lazyload', TRUE),
     silent= FALSE)
 patch.install(...) # actually, args are exactly as for 'patch.installed'
 spkg( pkg)
}
\arguments{
\item{ pkg}{package name. Either quoted or unquoted is OK; unquoted will be treated as quoted unless \code{character.only=TRUE}. Here and in most other places in \code{mvbutils}, you can also specify an actual in-memory-task-package object such as \code{..mypack}.}
\item{ character.only}{Default FALSE, which allows unquoted package names. You can set it to TRUE, or just set e.g. \code{char="my@funny@name"}, which will trump any use of \code{pkg}.}
\item{ force.all.docs}{normally just create help files for objects whose documentation has changed (which will always be generated, regardless of \code{force.all.docs}). If TRUE, then recreate help for all documented objects. Can also be a character vector of specific docfile names (usually function names, but can be the names of the Rd file, without path or the Rd extension), in which case those Rd files will be regenerated.}
\item{ rewrap.forcibly}{iff the package contains low-level code (C etc) and this is TRUE, this will re-invoke the PIBH (Pre-Install Build Hook) to recreate "housekeeping" code that e.g. creates R{} wrappers to call the low-level code from (avoiding direct use of \code{.Call} etc). Otherwise, the "housekeeping" code will only be recreated if the low-level source code has changed. It will also \emph{purge} any pre-existing files in the "R" subfolder of of your \emph{task}--- be warned! That feature is so that if you change from using eg the \pkg{Rcpp} package to the \pkg{RcppTidy} package, you won't have multiple versions of C-loaders. See other places in doco...}
\item{ help.patch}{if TRUE, patch the help of the installed package}
\item{ DLLs.only}{just synchronize the DLLs and don't bother with other steps (see \bold{Compiled code})}
\item{ default.list}{list of various things-- see under "Overriding..." below}
\item{ ...}{arguments to pass to your \code{pre.install.hook.XXX} function, usually if you want to be able to build different "flavours" of a package (e.g. a trial version vs. a production version, or versions with and without enormous datasets included). In \code{patch.install}, \code{...} is just shorthand for the arg list of \code{patch.installed}.}
\item{ update.installed.cache}{If TRUE, then clear the installed-package cache, so that things like \code{installed.packages} work OK. The only reason to set to FALSE could be speed, if you have lots of packages; feedback appreciated. Default is TRUE unless you have set \code{options( mvb.update.installed.cache=FALSE)}.}
\item{ pre.inst}{?run \code{pre.install} first? Default is TRUE unless \code{DLLs.only=TRUE}; leave it unless you know better.}
\item{ autoversion}{if TRUE, use the \code{<mypack>.VERSION} counter to update source-package DESCRIPTION. This is generally much better than manually tweaking the task package's \code{<mypack>.DESCRIPTION} object (or task package's DESCRIPTION file, in the hyper-manual case). Only versions with at least 3 levels will be updated:so 1.0.0 will go to 1.0.1, 1.0.0.0 will go to 1.0.0.1, but 1.0 will stay the same. Default is TRUE unless you have set \code{options( mvb.autoversion=FALSE)}. To force a \code{major} revsion, modify \code{..<mypack>$<mypack>.VERSION} yourself (unless you are using a manual version in \bold{Description}, which is discouraged). However, if you have manually changed the DESCRIPTION object or file's version to something beyond the source/installed version, then the larger number will take precedence.}
\item{ click.version}{if TRUE, try to automatically increment the version number in the source (and installed, if \code{patch.install}) packages. Normally a good idea, \emph{except} if you are updating several incompatible libraries for different R{} versions--- then you will need to run \code{pre.install/patch.install} in each R{} version but for the same underyling package, and you don't want the versions to get out of synch!}
\item{ vignette.build}{if TRUE, call \code{tools::buildVignettes} to generate HTMLs and/or PDFs from any RMD vignettes (only), so that the (?binary?) package will install properly. At present, you are still responsible for generating the RMD files in the first place; and the installed package is \emph{not} updated by \code{patch.install} even if \code{vignette.build=TRUE}. Note that the default value is different for \code{pre.install} vs \code{patch.install}, because the former is meant to prepare a package for distro, whereas the latter is mainly updating the locally-installed version, for which vignette-updates won't work (would need to rebuild installed index, etc). If \code{vignette.build=FALSE}, any hand-pre-built vignettes \emph{may} still get installed correctly. This is all messy stuff, subject to change!}
\item{ dir.above.source}{folder within which the source package will go, with a \code{+} at the start being shorthand for the task package folder (the default). Hence \code{pre.install( pkg=mypack, dir="+/holder")} will lead to creation of "holder/mypack" below the task folder of \code{mypack}. Set this manually if you have to maintain different versions of the package for different R{} versions, or different flavours of the package for other reasons, or if your source package must live in a "subversion tree" (whatever that is).}
\item{ R.target.version}{Not needed 99\% of the time; use only if you want to create source package for a different version of R{}. Supercedes the \code{Rd.version} argument of \code{pre.install} pre-'mvbutils' 2.5.57, used to control the documentation format. Set \code{R.target.version} to something less than "2.10" for ye olde "Rd version 1" format.}
\item{ compress.lazyload}{Installed packages feature "lazy-load" databases for documentation and for the R{} functions themselves (whether you like it or not), and \code{patch.installed} updates them. By default, R{} will compress these databases as it builds them, which can be remarkably slow. This option is an experimental feature to make uncompressed versions (by tweaking the \code{compress} argument to \code{tools:::makeLazyLoadDB}). You can try setting \code{options(mvbutils.compress.lazyload=FALSE)} for a speedup, but it's not really tested yet...}
\item{ Roxygen}{?should the Rd files be run thru \code{Rd2Roxygen} and added into the source-script "<mypack>.R"? NULL (default) means that the decision is based on whether the DESCRIPTION file contains a "RoxygenNote" field.}
\item{ timeout_Roxygen}{In case \code{Rd2roxygen} just does not work properly, you can stop it hanging the machine by setting this timeout (in which, pre-installation will continue, but without any Roxygen in the source--- ohdearhowsadnevermindeh). Use \code{options( mvb.timeout_Roxygen=<something>)} to do it for your whole R{} session (which is also the only way to apply it during \code{patch.install}).}
\item{ silent}{whether to show messages about starting/finishing documentation-prep and lazyification.}
}
\details{
As per the Glossary section of \code{\link{mvbutils.packaging.tools}}: the "task package" is the directory containing the ".RData" file with the guts of your package, which should be linked into the \code{\link{cd}} task hierarchy. The "source package" is usually the directory "<<pkg>>" below the task package, which will be created if needs be.

The default behaviour of \code{pre.install} is as follows-- to change it, see \bold{Overriding defaults}. A basic source package is created in a sourcedirectory "<<pkg>>" of the current task. The package will have at least a DESCRIPTION file, a NAMESPACE file, a single R{} source file with name "<<pkg>>.R" in the "R" sourcedirectory, possibly a "sysdata.rda" file in the same place to contain non-functions, and a set of Rd files in the "man" sourcedirectory. Rd files will be auto-created from \code{\link{flatdoc}} style documentation, although precedence will be given to any pre-existing Rd files found in an "Rd" sourcedirectory of your task, which get copied directly into the package. Any "inst", "demo", "vignettes", "tests", "src", "exec", and "data" subedirectories will be copied to the source package, recursively (i.e. including any of \emph{their} sourcedirectories). There is no compilation of source code, since only a source package is being created; see also \bold{Compiled code} below.

Most objects in the task package will go into the source package, but there are usually a few you wouldn't want there: objects that are concerned only with how to create the package in the first place, and ephemeral system clutter such as \code{.Random.seed}. The default exceptions are: functions \code{pre.install.hook.<<pkg>>}, \code{.First.task}, and \code{.Last.task}; data \code{<<pkg>>.file.exclude.regexes}, \code{<<pkg>>.DESCRIPTION}, \code{<<pkg>>.VERSION}, \code{<<pkg>>.UNSTABLE}, \code{forced!exports}, \code{.required}, \code{.Depends}, \code{tasks}, \code{.Traceback}, \code{.packageName}, \code{last.warning}, \code{.Last.value}, \code{.Random.seed}, \code{.SavedPlots}; and any character vector whose name ends with ".doc".

All pre-existing files in the "man", "src", "tests", "exec", "demo", "inst", and "R" sourcedirectories of the source-package directory will be removed (unless you have some \code{\link{mlazy}} objects; see below). If--- but this is deprecated--- a file ".Rbuildignore" file is present in the task package, it's copied to the package directory, but I've never gotten this feature to work. If not and there's an object \code{<pkg>.Rbuildignore} (the preferred way; it should be a character vector), that's used (and is automatically augmented to exclude some task-package housekeeping files). To exclude files that would otherwise be copied, i.e. those in "inst/demo/src/data" folders, create a character vector of regexes called \code{<<pkg>>.file.exclude.regexes}; any file matching any of these won't be copied.

If there is a "changes.txt" file in the task package, it will be copied to the "inst" sourcedirectory of the package, as will any files in the task's own "inst" sourcedirectory. A DESCRIPTION file will be created, preferably from a \code{<<pkg>>.DESCRIPTION} \emph{object} in the task package; see \code{\link{mvbutils.packaging.tools}} for more. Any "Makefile.*" in the task package will be copied, as will any in the "src" sourcedirectory (not sure why both places are allowed). No other files or sourcedirectories in the package directory will be created or removed, but some essential files will be modified.

Any other character-vectors in the task package with names \code{mypack.x}, where "mypack" is your packagename and "x" is one of (NEWS, CHANGES, LICENCE, LICENSE, INSTALL, configure, cleanup, ChangeLog, README, Rbuildignore) or "README.y" where "y" is whatever, will be written into the source package as the corresponding file (e.g. a NEWS file will be created).

If a NAMESPACE file is present in the task (usually no need), then it is copied directly to the package. If not, then \code{pre.install} will generate a NAMESPACE file by calling \code{\link{make.NAMESPACE}}, which makes reasonable guesses about what to import, export, and S3methodize. What is & isn't an S3 method is generally deduced OK (see \code{\link{make.NAMESPACE}} for gruesome details), but you can override the defaults via the pre-install hook. FWIW, since adding the package-creation features to \code{mvbutils}, I have never bothered explicitly writing a NAMESPACE file for any of my packages. By default, only \emph{documented} functions are exported (i.e. visible to the user or other packages); the rest are only available to other functions in your package.

The R{} source file will contain functions only. Any \code{doc} and \code{export.me} attributes are dropped, but other attributes are kept; in particular, source code is kept in the \code{source} attribute.

If any of the Rd files starts with a period, e.g. ".dotty.name", it will be renamed to e.g. "01.dotty.name.Rd" to avoid some problems with RCMD. This should never matter, but just so you know...

To speed up conversion of documentation, a list of raw & converted documentation is stored in the file "doc2Rd.info.rda" in the task package, and conversion is only done for objects whose raw documentation has changed, unless \code{force.all.docs} is TRUE.

\code{pre.install} creates a file "funs.rda" in the package's "R" sourcedirectory, which is subsequently used by \code{patch.installed}. The function \code{\link{build.pkg}} (or R{} CMD BUILD) and friends will omit this file (currently with a complaint, which I intend to fix eventually, but which does not cause trouble).
\subsection{Compiled code}{
\code{pre.install} tries to produce automatic R-side/C-side wrappers of C(++) code written for package \pkg{Rcpp}. The system used is extensible by the user (a pretty advanced user!) to other flavours of R/C code, via \code{\link{Clink_packages}} (qv). Current extensions are \code{RcppTidy} and \code{ADT}. Version-tracking on the automatically-generated wrapper files (one in "./src" and one in "./R") doesn't seem to be working that well yet, and you may well need to call \code{rewrap_forcibly} to make it happen. Watch out for old versions of those files left lying around by accident, since they can cause havoc.

In the case of package \pkg{RcppTidy}, what happens is this (actually via \code{Clink_packages()$RcppTidy}, which itself is a function that \code{mvbutils} is notified of by package \pkg{RcppTidy} itself when the latter is loaded):

\itemize{
\item it calls \code{compileAttributes} to generate an "RcppExports.cpp" file in the source package (in folder "src"), if a change from previous one is detected. The file is edited to retain the md5sum of the sources, which is used in subsequent runs to check for changes.
\item it \emph{modifies} the "RcppExports.R" file so that the R-side auto headers are all placed into an environment \code{DLL} in the namespace.
}

This is to avoid polluting your namespace at point-of-load with possible aliases for C code, and to allow you to document and/or export "Rcpp functions" in the same way you would your other functions. It is less automated but arguably more controlled. To export (for the R-level user) a "Rcpp function", you need to explicitly write a wrapper, eg

\preformatted{
  rapid_thing <- function( a, b, c) DLL$rapid_thing( a, b, c)
}

and add documentation for \code{rapid_thing}. There is also provision for different compilation systems, like \code{RcppTidy} and an ADT-oriented one...

\code{patch.install} does not compile source code; currently, you need to do that yourself, though I might add support for that if I can work a sufficiently general mechanism. If you use R{} to do your compilation, then \code{\link{install.pkg}} should work after \code{pre.install}, though you may need \code{detach("package:mypack", unload=T)} first and that will disrupt your R{} session. Alternatively, you may be able to use R{} CMD SHLIB to create the DLL directly, which you can then copy into the "libs" sourcedirectory of the installed package, without needing to re-install. I haven't tried this, but colleagues have reported success.

If, like me, you pre-compile your own DLLs manually (not allowed on CRAN, but fine for distribution to other users on the same OS), then you can put the DLLs into a folder "inst/libs" of the task (see next for Windows); they will end up as usual in the "libs" folder of the installed package, even though R{} itself hasn't compiled them. On Windows, put the DLLs one level deeper in "inst/libs/<<arch>>" instead, where "<<arch>>" is found from \code{.Platform$r_arch}; for 32-bit Windows, it's currently "i386". All references in this section to "libs", whether in the task or source or installed package, should be taken as meaning "libs/<<arch>>". You pretty much also need to create the alternate "x64" folder, too, even if it's empty; otherwise, the \code{mvbutils} installation tools will fail ( >= R3.3 or so).

To load your package's DLLs, call \code{library.dynam} in the \code{.onLoad} function, for example like this:

\preformatted{
  .onLoad <- function( libname, pkgname){
    library.dynam( 'my_first_dll', package=pkgname)
    library.dynam( 'my_other_dll', package=pkgname)  # fine to have several DLLs
  }
}

To automatically load all DLLs, you can copy the body of \code{mvbutils:::generic.dll.loader} into your own \code{.onLoad}, or just include a call to \code{generic.dll.loader(libname,pkgname)} if you don't mind having dependence on \code{mvbutils}.

After the package has been installed for the first time, I change my compiler settings so that the DLL is created directly in the installed package's "libs" folder; this means I can use the compiler's debugger while R{} is running. To accommodate this, \code{patch.install} behaves as follows:

\itemize{
\item any new DLLs in the task package are copied to the installed package;
\item any DLLs in the installed package but not in the task package are deleted;
\item for any DLLs in both task & installed, both copies are synchronized to the \emph{newer} version;
\item the source package always matches the task package
}

You can call \code{patch.install( mypack, DLLs.only=TRUE)} if you only want the DLL-synching step.

(Before version 2.5.57, \code{mvbutils} allowed more latitude in where you could put your home-brewed DLLs, but it just made life more confusing. The only place that now works is as above.)
}
\subsection{Data objects}{
Data objects are handled a bit differently to the recommendations in "R extensions" and elsewhere-- but the end result for the package user is the same, or better. The changes have been made to speed up package maintenance, and to improve useability. Specifically:

\itemize{
\item Undocumented data objects live only in the package's namespace, i.e. visible only to your functions.
\item Documented data objects appear both in the visible part of the package (i.e. in the search path), and in the namespace. [The R{} standard is that these should not be visible in the namespace, but this doesn't seem sensible to me.]
\item The easiest way to export a data object, is to "document" it by putting its name into an alias line of the doc attribute of an existing function. (Alias lines are single-word lines directly after the first line of the doc attr.)
\item To document a data object \code{xxx} in its own right, include a flat-format text object \code{xxx.doc} in your task package; see \code{\link{doc2Rd}}. \code{xxx.doc} itself won't appear in the packaged object, but will result in documentation for \code{xxx} \emph{and} any other data objects that are given as alias lines.
\item Big data objects can be set up for transparent individual lazy-loading (see below) to save time & memory, but lazy-loading is otherwise off by default for individual data objects.
\item There is no need for the user ever to call \code{\link{data}} to access a dataset in the package, and in fact it won't work.
}

Note that the \code{data(...)} function has been pretty much obsolete since the advent of lazy-loading in R{} 2.0; see R-news #4/2.

In terms of package structure, as opposed to operation, there is no "data" sourcedirectory. Data lives either in the "sysdata.rdb/rdx" files in the "R" sourcedirectory (but can still be user-visible, which is not normally the case for objects in those files), or in the "mlazy" sourcedirectory for those objects with individual lazy-loading.
\subsection{Big data objects}{Lazy-loading objects cached with \code{\link{mlazy}} are handled specially, to speed up \code{pre.install}. Such objects get their cache-files copied to "inst/mlazy", and the \code{.onLoad} is prepended with code that will load them on demand. By default, they are exported if and only if documented, and are not locked. The following objects are not packaged by default, even if \code{\link{mlazy}}ed: \code{.Random.seed}, \code{.Traceback}, \code{last.warning}, and \code{.Saved.plots}. These are \code{\link{mlazy}}ed automatically if \code{options( mvb.quick.cd)} is \code{TRUE}-- see \code{\link{cd}}.
}
}
\subsection{Tinytests}{Any "scriptlets" (charvecs whose name ends ".r" or ".R') whose first line contains the word "tinytest", are assumed to be for package \pkg{tinytest}. They will be written into eponymous files in the "inst/tinytest" folder, where they will be accessible to \code{tinytest::test_package(<mypack>)}. If you already have manual tinytest script-files in "inst/tinytest", they will be copied into the sourcedirectory tree too (and will overwrite any scriptlets with the same names). Package \pkg{tinytest} also requires a magic file "tinytest.R" to exist in the folder "<mypack>/tests", and that will be created in the sourcedirectory if it does not exist in your task directory. Remember to add "tinytest" to "Suggests" field in \bold{Description}.
}
\subsection{Documentation and exporting}{
\subsection{Package documentation}{Just because you have a package \pkg{Splendid}, it doesn't follow that a user will be able to figure out how to use it from the alphabetical list of functions in \code{library( help=Splendid)}; even if you've written vignettes, it may not be obvious which to use. The recommended way to provide a package overview is via "package documentation", which the user accesses via \code{package?Splendid}. You can write this in a text object called e.g. "Splendid.package.doc", which will be passed through \code{\link{doc2Rd}} with an extra "docType\{package\}" field added. The first line should start e.g. "Splendid-package" and the corresponding ".Rd" file will be put first into the index. Speaking as a frequently bewildered would-be user of others' packages-- and one who readily gives up if the "help" is impenetrable-- I urge you to make use of this feature!
}
\subsection{Vignettes}{See \code{\link{mvbutils.packaging.tools}}.
}
\subsection{Bare minimum for export}{
Only documented functions and data are exported from your package (unless you resort to the subterfuge described in the subsection after this). Documented things are those found by \code{find.documented( doc="any")}. The simplest way to document something is just to add its name as an "alias line" to the existing documentation of another function, before the first empty line. For example, if you're already using \code{\link{flatdoc}} to document \code{my.beautiful.function}, you can technically "document" and thus export other functions like so:

\preformatted{
  structure( function( blahblahblah)...
  ,doc=flatdoc())
  my.beautiful.function    package:splendid
  other.exported.function.1
  other.exported.function.2
}

The package will build & install OK even if you don't provide USAGE and ARGUMENTS sections for the other functions. Of course, R{} CMD CHECK wouldn't like it (and may have a point on this occasion). If you just are after "legal" (for R{} CMD CHECK) albeit unhelpful documentation for some of your functions that you can't face writing proper doco for yet, see \code{\link{make.usage.section}} and \code{make.argument.section}.
}
\subsection{Exporting undocumented things and vice versa}{
A bit naughty (RCMD CHECK complains), but quite doable. Note that "things" can be data objects, not just functions. Simply write a pre-install hook (see \bold{Overriding defaults}) that includes something like this:

\preformatted{
  pre.install.hook.mypack <- function( hooklist) {
    hooklist$nsinfo$exports <- c( hooklist$nsinfo$exports, "my.undocumented.thing")
  return( hooklist)
  }
}

You can follow a similar approach if you want to document something but \emph{not} to export it (so that it can only be accessed by \code{Splendid:::unexported.thing}. This probably isn't naughty.
}
}
\subsection{Overriding defaults}{
Source package folder can be controlled via \code{options("mvbutils.sourcepkgdir.postfix")}, as per "Folders and different R{} versions" in \code{\link{mvbutils.packaging.tools}}. You'd only need to do this if you have multiple R{} versions installed that require different source-package formats (something that does not often change).

If a function \code{pre.install.hook.<<pkgname>>} exists in the task "<<pkgname>>", it will be called during \code{pre.install}. It will be passed one list-mode argument, containing default values for various installation things that can be adjusted; and it should return a list with the same names. It will also be passed any \code{...} arguments to \code{pre.install}, which can be used e.g. to set "production mode" vs "informal mode" of the end product. For example, you might call \code{preinstall(mypack,modo="production")} and then write a function \code{pre.install.hook.mypack( hooklist, modo)} that includes or excludes certain files depending on the value of \code{modo}. The hook can do two things: sort out any file issues not adequately handled by \code{pre.install}, and/or change the following elements in the list that is passed in. The return value should be the possibly-modified list. Hook list elements are:

\describe{
\item{copies}{files to copy directly}
\item{dll.paths}{DLLs to copy directly}
\item{extra.filecontents}{named list; each element is the contents of a text file, the corresponding name being the path of the file to create eg \code{"inst/src/utils.pas"}--- a nonstandard name}
\item{extra.docs}{names of character-mode objects that constitute flat-format documentation}
\item{description}{named elements of DESCRIPTION file}
\item{task.path}{path of task (ready-to-install package will be created as a sourcedirectory in this)}
\item{has.namespace}{should a namespace be used?}
\item{use.existing.NAMESPACE}{ignore default and just copy the existing NAMESPACE file?}
\item{nsinfo}{default namespace information, to be written iff \code{has.namespace==TRUE} and \code{use.existing.NAMESPACE==FALSE}}
\item{exclude.funs}{any functions \bold{not} to include}
\item{exclude.data}{non-functions to exclude from \code{system.rda}}
\item{dont.check.visibility}{either TRUE (default default), FALSE, or a specified character vector, to say which objects are \emph{not} to be checked for "globality" by RCMD CHECK (using the \code{globalVariables} mechanism). Leave alone if you don't understand this. You can change the "default default" via \code{options( mvb_dont_check_visibility=FALSE)}.}
}

There are two reasons for using a hook rather than directly setting parameters in \code{pre.install}. The first is that \code{pre.install} will calculate sensible but non-obvious default values for most things, and it is easier to change the defaults than to set them up from scratch in the call. The second is that once you have written a hook, you can forget about it-- you don't have to remember special argument values each time you call \code{pre.install} for that task.
\subsection{Debugging a pre install hook}{To understand what's in the list and how to write a pre-install hook, the easiest way is probably to write a dummy one and then \code{mtrace} it before calling \code{pre.install(mypack)}. However, it's all a bit clunky at present (July 2011). Because the hook only exists in the "..mypack" shadow environment, \code{mtrace} won't find it automatically, so you'll need \code{mtrace( pre.install.hook.mypack, from=..mypack)}. That's fine, but if you then modify the source of your hook function, you'll get an error following the "Reapplying trace..." message. So you need to do \code{mtrace.off} \emph{before} saving your edited hook-function source, and then \code{mtrace} the hook again before calling \code{pre.install(mypack)}. To be fixed, if I can work out how...
}
}
\subsection{Different versions of r}{
R{} seems to be rather fond of changing the structural requirements of source & installed packages. \code{mvbutils} tries to shield you from those arcane and ephemeral details-- usually, your task package will not need changing, and \code{pre.install} will automatically generate source & installed packages in whatever format R{} currently requires. However, sometimes you do at least need to be able to build different "instances" of your package for different versions of R{}. The \code{sourcedir} and maybe the \code{R.target.version} arguments of \code{pre.install} may help with this.

But if you need to build instances of your package for a different version of R{}, then you may need this argument (and \code{dir.above.source}). I try to keep \code{mvbutils} up-to-date with R{}'s fairly frequent revisions to package structure rules, with the aim that you (or I) can easily produce a source/binary-source package for a version of R{} later than the one you're using right now, merely by setting \code{R.target.version}. However, be warned that this may not always be enough; there might at some point be changes in R{} that will require you to be running the appropriate R{} version (and an appropriate version of \code{mvbutils}) just to recreate/rebuild your package in an appropriate form.

The nuances of \code{R.target.version} change with the changing tides of R{} versions, but the whole point of \code{pre.install} etc is that you shouldn't really need to know about those details; \code{mvbutils} tries to look after them for you. For example, though: as of 10/2011, the "detailed behaviour" is to enforce namespaces if \code{R.target.version} >= 2.14, regardless of whether your package has a \code{.onLoad} or not.
\subsection{Packages without namespaces pre r2 14}{You used to be allowed to build packages without namespaces-- not to be encouraged for general distribution IMO, but occasionally a useful shortcut for your own stuff nevertheless (mainly because everything is "exported", documented or not). For R{} <= 2.14, \code{mvbutils} will decide for itself whether your package is meant to be namespaced, based on whether any of the following apply: there is a NAMESPACE file in the task package; there is a \code{.onLoad} function in the task; there is an "Imports" directive in the DESCRIPTION file.
}
}
}
\seealso{\code{\link{mvbutils.packaging.tools}}, \code{\link{cd}}, \code{\link{doc2Rd}}, \code{\link{maintain.packages}}
}
\examples{
\dontrun{
# Workflow for simple case:
cd( task.above.mypack)
maintain.packages( mypack)
# First-time setup, or after major R version changes:
pre.install( mypack)
install.pkg( mypack)
library( mypack)
# ... do stuff
# Subsequent maintenance:
maintain.packages( mypack) # only once per session, usually at the start
library( mypack) # maybe optional
# ...do various things involving changes to mypack, then...
patch.install( mypack) # keep disk image up-to-date
# Prepare copies for distribution
build.pkg( mypack) # for Linux or CRAN
build.pkg.binary( mypack) # for Windows or Macs
check.pkg( mypack) # if you like that sort of thing
}
}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities
}
