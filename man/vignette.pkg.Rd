\name{vignette.pkg}
\alias{vignette.pkg}
\title{Build vignette(s) for mvbutils-style package}
\description{
Vignette-building is insanely complicated (though this might be hidden from you) and can be slow. So it's not handled directly by \code{\link{pre.install}}, \code{\link{patch.install}}, and friends. Doing \code{\link{build.pkg}} and \code{\link{install.pkg}} will work normally, but if you want to \emph{change} a vignette in an installed package without complete re-installation, then you have to manually (re)build vignette(s) and indices. \code{vignette.pkg} should do that for you.

The \emph{bare-bones} usage (which is not the recommended way--- see next para) is to copy all files (and folders) from the task's "vignettes" folder into the source package's "vignettes" folder (after zapping the latter). If \code{build=TRUE} (the default) it will then build the vignettes in the \emph{installed} package (that's just how R{} does it, for whatever reason).

Also (recommended!), there can be an intermediate level of vignette, where all the calculations/plots are already done and saved, and the precompiled vignette is just ready to be turned into HTML and/or PDF, something which should be fairly quick for whatever machine ends up doing it (eg an R-universe or CRAN server). If you give your original vignette files the extension ".Rmd.orig", then an R{} script "precomp.R" will be created by \code{\link{pre.install}} in the task package vignette. It is a very simple script that mainly just shows the \code{knitr} command to use. \code{vignette.pkg(...,precompile=TRUE)} will then run that script to precompile all the vignettes (which can be slow, of course) in the task package "vignettes" folder, producing ".Rmd" files that are precompiled, along with figure files etc in subfolders. If you are precompiling, you can use \code{decache=TRUE} to clear the cache automatically; see \bold{Arguments}.

Precompilation happens only if \code{precompile=TRUE}. Copying the "vignettes" folder always happens, unless \code{precompile=TRUE} and precompilation fails, in which case the function aborts. After copying, building happens unless \code{build=FALSE}. Index reconstruction happens only if some building has taken place.
\subsection{To do }{
I like PDFs. It would be great to produce two versions, one in PDF so you can peruse it while not glued to your goddamn screen. But R{} has no mechanism to formally xtuple rendered versions of a vignette (you get N=1), and suggestions to the contrary seem to elicit the usual CRANiac bleating. TBF the PDF produced by putting the following in place of the "rmarkdown::html_vignette" line doesn't look very pretty anyway, but no doubt it could be tweaked with lashing of options:

 pdf_document:

\describe{
\item{latex_engine}{pdflatex}
\item{pandoc_args}{[ --listings ]}
}

The closest approach is something like this, suggested by Peter Harrison in 2018, to make a special-prupose \code{knit} function that produces all outputs.

\describe{
\item{title}{"multiple outputs"}
}

 output:

\describe{
\item{pdf_document}{<blah options>}
}

 rmarkdown::html_vignette: <blah options>

\describe{
\item{knit}{(function(inputFile, encoding) \{}
}

 rmarkdown::render(inputFile, encoding = encoding,

 output_dir = "output", output_format = "all") \})

which does at some stage produce both files, but it looks like R{} tries to delete anything exectp the PDF. Genius. Also, there's a dot-log file that creeps into the source package somehow.

One manualesque version (on Windows) might be to print the HTML to a PDF using a browser from CLI. This from

\preformatted{
  https://superuser.com/questions/1537277/how-can-i-print-html-file-to-pdf-from-command-line-in-windows-10-without-admin
  "C:\\Program Files\\Microsoft\\Edge\\Application\\msedge.exe" --headless --print-to-pdf="c:\\outdir\\out.pdf" "c:\\indir\\in.html"
}
}
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
\item{ decache}{if \code{TRUE} and if \code{precompile} is \code{TRUE}, then the envar "VIGDECACHE_<PKG>" will be set to "yes" when \code{knit} is called (note that the package name will be capitalized). You can add some code to your vignette to clear the cache iff that envar exists, as in \bold{Examples}.}
\item{ ...}{passed to \code{tools::buildVignette} (qv)}
}
\value{A character vector of all files that were built. If there are errors during the build process, you should see on-screen messages.
}
\seealso{\code{\link{pre.install}}
}
\examples{
\dontrun{
vignette.pkg( kinference)
# Code you can maybe add at the start of your vignette, to optionally clear the cache as per 'decache' arg
knitr::opts_chunk$set( cache.rebuild=nzchar( Sys.getenv( 'VIGDECACHE_<PKG>')))
}
}
\keyword{misc}
