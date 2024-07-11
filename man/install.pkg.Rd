\name{install.pkg}
\alias{install.pkg}
\alias{build.pkg}
\alias{build.pkg.binary}
\alias{check.pkg}
\alias{cull.old.builds}
\alias{set.rcmd.vars}
\title{Package building, distributing, and checking}
\description{These are convenient wrappers for R{}'s package creation and installation tools. They are designed to be used on packages created from tasks via \code{mvbutils} package, specifically \code{\link{pre.install}} (though they can be used for "home-made" packages). The \code{mvbutils} approach deliberately makes re-installation a rare event, and one call to \code{install.pkg} might suffice for the entire life of a simple package. After that very first installation, you'd probably only need to call \code{install.pkg} if (when...) new versions of R{} entail re-installation of packages, and \code{build.pkg/build.pkg.binary/check.pkg} when you want to give your package to others, either directly or via CRAN etc.
\subsection{Folders}{
Source packages and built packages go into various folders, depending on various things. Normally you shouldn't have to mess around with the folder structure, but you will still need to \emph{know} where built packages are put so that you can send them to other people. Specifically, these \code{...pkg...} functions work in the highest-versioned "Rx.y" folder that is not newer than the \emph{running} R{} version. If no such folder exists, then 'build.pkg/build.pkg.binary" will create one from the running R{} version; you can also create such a folder manually, as a kind of "checkpoint", when you want to make your package dependent on a specific R{} version. See "Folders and R{} versions" in \code{\link{mvbutils.packaging.tools}} for full details.

There are also two minor housekeeping functions: \code{cull.old.builds} to tidy up detritus, and \code{set.rcmd.vars} which does absolutely nothing (yet). \code{cull.old.builds} looks through \emph{all} "Rx.y" folders (where built packages live) and deletes the least-recent ".tar.gz" and ".zip" files in each (regardless of which built package versions are in the other "Rx.y" folders).
}
}
\usage{
  # Usually: build.pkg( mypack) etc
  install.pkg( pkg, character.only=FALSE, lib=.libPaths()[1], flags=character(0),
      multiarch=NA, preclean=TRUE)
  build.pkg( pkg, character.only=FALSE, flags=character(0), cull.old.builds=TRUE)
  build.pkg.binary( pkg, character.only=FALSE, flags=character(0),
      cull.old.builds=TRUE, multiarch=NA, preclean=TRUE)
  check.pkg( pkg, character.only=FALSE, build.flags=character(0),
      check.flags=character( 0), CRAN=FALSE)
  cull.old.builds( pkg, character.only=FALSE)
  set.rcmd.vars( ...) # NYI; ...
  # ... if you need to set env vars eg PATH for R CMD to work,  then...
  # ... you have to do so yourself; see *Details*
}
\arguments{
See the examples
\item{ pkg}{usually an unquoted package name, but interpretation can be changed by non-default \code{character.only}. You can also get away with eg \code{..mypack}, ie a direct reference to the maintained package. A folder name can also be used, for a non-mvbutils-maintained package. Just as if it was "maintained", the folder should contain a subfolder with the (same) package name and the real package contents (eg "c:/r/mypack/mypack/DESCRIPTION" should exist), and any built things will go into eg "c:/r/mypack/R3.2"}
\item{ character.only}{default FALSE. If TRUE, treat \code{pkg} as a normal object, which should therefore be a string containing the package's name. If \code{character.only} is itself a string, it will override \code{pkg} and be treated as the name of the package.}
\item{ lib}{(\code{install.pkg} only) where to install to; default is the same place R{} would install to, i.e. \code{.libPaths()[1]}.}
\item{ flags}{character vector, by default empty. Any entries should be function-specific flags, such as "--md5" for \code{build.pkg}. It will be passed through \code{paste( flags, collapse=" ")}, so you can supply flags individually (eg \code{flags=c( "--md5", "--compact.vignettes")}) or jointly (eg \code{flags="--md5 --compact.vignettes"}).}
\item{ build.flags, check.flags}{(\code{check.pkg} only) as per \code{flags} but for the two separate parts of \code{check.pkg} (see \bold{Details}). \code{check.flags} is overridden if \code{CRAN==TRUE}'.}
\item{ preclean}{adds flag "--preclean" if TRUE (the default); this is probably a good idea since one build-failure can otherwise cause R{} to keep failing to build.}
\item{ multiarch}{Adds flag "-no-multiarch" if FALSE. Defaults to TRUE unless "Biarch:FALSE" is found in the DESCRIPTION. Default used to be FALSE when I was unable to get 64bit versions to build. Now I mostly can (after working round BINPREF64 bug in R3.3.something by futzing around in etc/arch/Makeconf based on random internet blogs).}
\item{ cull.old.builds}{self-explanatory}
\item{ CRAN}{(\code{check.pkg} only) if TRUE, set the \code{--as-cran} flag to "RCMD check" and unset all other check flags (except library locations, which are set automatically by all these functions). Note that this will cause R{} to check various internet databases, and so can be slow.}
\item{ ...}{name-value pairs of system environment variables (not used for now)}
}
\details{
Before doing any of this, you need to have used \code{\link{pre.install}} to create a source package. (Or \code{\link{patch.install}}, if you've done all this before and just want to re-install/build/check for some reason.)

The only environment variable currently made known to R{} CMD is R_LIBS-- let me know if others would be useful.

\code{install.pkg} calls "R CMD INSTALL" to install from a source package.

\code{build.pkg} calls "R CMD build" to wrap up the source package into a "tarball", as required by CRAN and also for distribution to non-Windows-and-Mac platforms.

\code{build.pkg.binary} (Windows & Mac only) calls "R CMD INSTALL --build" to generate a binary package. A temporary installation directory is used, so your existing installation is \emph{not} overwritten or deleted if there's a problem; R{} CMD INSTALL --build has a nasty habit of doing just that unless you're careful, which \code{build.pkg.binary} is.

\code{check.pkg} calls "R CMD check" after first calling \code{build.pkg} (more efficiently, I should perhaps try to work out whether there's an up-to-date tarball already). It doesn't delete the tarball afterwards. It \emph{may} also be possible for you to do some checks directly from R{} via functions in the \pkg{utils} package, which is potentially a lot quicker. However, NB the possibility of interference with your current R{} session. For example, at one stage \code{codoc} (which is the only check that I personally find very useful) tried to unload & load the package, which was very bad; but I think that may no longer be the case.

You \emph{may} have to set some environment variables (eg PATH, and perhaps R_LIBS) for the underlying R{} CMD calls to work. Currently you have to do this manually--- your \code{.First} or \code{.Rprofile} would be a good place. If you really object to changing these for the whole R{} session, let me know; I've left a placeholder for a function \code{set.rcmd.vars} that could store a list of environment variables to be set temporarily for the duration of the R{} CMD calls only, but I haven't implemented it (and won't unless there's demand).

Perhaps it would be desirable to let some flags be set automatically, eg via something in the \code{pre.install.hook} for a package. I'll add this if requested.
}
\value{
Ideally, the "status code" of the corresponding RCMD operation: 0 for success or some other integer if not. It will have several attributes attached, most usefully "output" which duplicates what's printed while the functions are running. (Turn off "buffered output" in RGui to see it as it's happening.) This requires the existence of the "tee" shell redirection facility, which is built-in to Linux and presumably Macs, but not to Windows. You can get one version from Coreutils in GnuWin32; make sure this is on your PATH, but probably \emph{after} the Rtools folders required by the R{} build process, to avoid conflicts between the other Coreutils versions and those in Rtools (I don't know what I'm talking about here, obviously; I'm just describing what I've done, which seems to work). If "tee" eventually moves to Rtools, then this won't be necessary.
If no "tee" is available, then:
\item{ - progress of RCMD will be shown "live" in a separate shell window}{}
\item{ - the status code is returned as NA, but still has the attributes including "output". You could, I suppose, "parse" the output somehow to check for failure.}{}
The point of all this "tee" business is that there's no reliable way in R{} itself to both show progress on-screen within R{} (which is useful, because these procedures can be slow) and to return the screen output as a character vector (which is useful so you can subsequently, pore through the error messages, or bask in a miasma of smugness).
}
\examples{
\dontrun{
# First time package installation
# Must be cd()ed to task above 'mvbutils'
maintain.packages( mvbutils)
pre.install( mvbutils)
install.pkg( mvbutils)
# Subsequent maintenance is all done by:
patch.install( mvbutils)
# For distro to
build.pkg( mvbutils)
# or on Windows (?and Macs?)
build.pkg.binary( mvbutils)
# If you enjoy R CMD CHECK:
check.pkg( mvbutils)
# Also legal:
build.pkg( ..mvbutils)
# To do it under programmatic control
for( ipack in all.my.package.names) \{
  build.pkg( char=ipack)
\}
}
}
\keyword{misc}
