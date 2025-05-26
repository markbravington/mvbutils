\name{vignette.pkg}
\alias{vignette.pkg}
\title{Build vignette(s) for mvbutils-style package}
\description{
Vignette-building is insanely complicated (though this might be hidden from you) and can be slow. So it's not handled directly by \code{\link{pre.install}}, \code{\link{patch.install}}, and friends. Doing \code{\link{build.pkg}} and \code{\link{install.pkg}} will work normally, but if you want to \emph{change} a vignette in an installed package without complete re-installation, then you have to manually (re)build vignette(s) and indices. \code{vignette.pkg} should do that for you.

The \emph{bare-bones} usage (which is not the recommended way--- see next para) is to copy all files (and folders) from the task's "vignettes" folder into the source package's "vignettes" folder (after zapping the latter). If \code{build=TRUE} (the default) it will then build the vignettes in the \emph{installed} package (that's just how R{} does it, for whatever reason).

Also (recommended!), there can be an intermediate level of vignette, where all the calculations/plots are already done and saved, and the precompiled vignette is just ready to be turned into HTML and/or PDF, something which should be fairly quick for whatever machine ends up doing it (eg an R-universe or CRAN server). If you give your original vignette files the extension ".Rmd.orig", then an R{} script "precomp.R" will be created by \code{\link{pre.install}} in the task package vignette. It is a very simple script that mainly just shows the \code{knitr} command to use. \code{vignette.pkg(...,precompile=TRUE)} will then run that script to precompile all the vignettes (which can be slow, of course) in the task package "vignettes" folder, producing ".Rmd" files that are precompiled, along with figure files etc in subfolders. If you are precompiling, you can use \code{decache=TRUE} to clear the cache automatically; see \bold{Arguments}.

Precompilation happens only if \code{precompile=TRUE}. Copying the "vignettes" folder always happens, unless \code{precompile=TRUE} and precompilation fails, in which case the function aborts. After copying, building happens unless \code{build=FALSE}. Index reconstruction happens only if some building has taken place.
}
\usage{
vignette.pkg( pkg, pattern= "[.]Rmd$",
  character.only= FALSE, precompile= FALSE,
  build= TRUE, decache= FALSE,
  ...)
}
\arguments{
\item{ pkg}{Name of package; see \code{\link{pre.install}} for options}
\item{ pattern}{Regex to select vignette files (only if \code{build} is TRUE). Default matches anything ending ".Rmd". You can specify a complete filename to only do that one vignette.}
\item{ character.only}{for automated use; see \code{\link{pre.install}}}
\item{ precompile}{?run the "precomp.R" script in the source package vignettes folder?}
\item{ build}{?should the vignettes be rebuilt?}
\item{ decache}{if \code{TRUE} and if \code{precompile} is \code{TRUE}, then the envar "VIGDECACHE_<PKG>" will be set to "yes" when \code{knit} is called (note that the package name will be capitalized). You can add some code to your vignette to clear the cache iff that envar exists, as in}
\item{ \%\%#}{}
\item{ knitr::opts_chunk$set( cache.rebuild=nzchar( Sys.getenv( VIGDECACHE_<PKG>))}{}
\item{ ...}{passed to \code{tools::buildVignette} (qv)}
}
\value{A character vector of all files that were built. If there are errors during the build process, you should see on-screen messages.
}
\seealso{\code{\link{pre.install}}
}
\examples{
\dontrun{
vignette.pkg( kinference)
}
}
\keyword{misc}
