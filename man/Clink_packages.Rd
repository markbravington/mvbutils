\name{Clink_packages}
\alias{Clink_packages}
\alias{src_changed}
\alias{PIBH}
\alias{dummy_PIBH}
\title{Pre-install-buildy hooks for compiled code}
\description{
\code{Clink_packages} registers or returns pre-install-buildy hook(s) in task-packages for different types of source code, eg for \code{Rcpp} or \code{RcppTidy} or TMB or ADT. You should never need to call \code{Clink_packages} yourself; it's meant for use by helper-mini-packages that tell \code{mvbutils} what to do about a specific type(s) of source code. Authors of "proper" packages containing real low-level source code don't really need to know about any of this--- though you could have a look at TIDY.STUBS.AND.SYMBOLS below, and at \code{RcppTidy} if you are normally using \code{Rcpp}.

Packages built with \code{mvbutils::pre.install}--- and any package wanting to debug low-level code via eg package \pkg{vscode}--- need to have a \code{.onLoad} that starts by calling \code{run_Cloaders_<pkgname>()}. That function does any work connected with setting up native-symbols and R{} stubs; package \pkg{Rcpp} on its own will normally cause such stuff to happen \emph{before} \code{.onLoad}, but that will be gently subverted if you use \code{mvbutils::pre.install}, which instead makes the function \code{run_Cloader_<pkgname>} for you automatically. By having that function, it becomes possible to change the low-level code (eg during debugging), including changing function arguments and so on, without re-installing the entire package. See something else for more details.

The list of potential helper-mini-packages for your whole package (normally just one, but you never know...) is determined by Description->Imports. Source code is expected to be directly in "<mypack>/src" (the only choice if plain old \code{Rcpp} is being used), and/or in the "N>=0" different subfolders of that. Each of these "N+1" folders will potentially generate a single DLL with the name of the folder. Each folder is scanned by all registered helper-minis, to see if that helper is wanted; processing of the folder stops after the first helper-mini that finds something to do. The helper-mini might add extra files to the folder (eg a wrapper that exports R-callable stubs, like "RcppExports.cpp"), and will probably add a "Cloader" written in R{}, to "<mypack>/R" (like "RcppExports.R")--- though that will be obscured in the "source package" seen later by INSTALL. If "N>0", or if any of the folders demand it, then an overall "Makefile" will be produced (required for multiple DLLs). Subfolders starting with a period are skipped (e.g. "src/.vscode").

The code that does the scanning for one specific type of low-level code (eg for \code{Rcpp}-type code) is a "Pre-Install-Buildy Hook" (PIBH), which are called during \code{\link{pre.install}}. Each PIBH should have five named arguments (see below). The PIBH will be invoked for folder "<mypack>/src", and for each subfolder thereof--- this lets you generate several DLLs (one from each subfolder) of the same type--- useful eg for ADT or TMB. A PIBH should check whether it's suitable for that folder, and if so (re)generate any necessary files; but it should also check whether regeneration is necessary (see .REGENERATION.CHECKS). Then the PIBH should return either NULL if there's nothing to do (e.g. it didn't find any suitable source files), or a list/dataframe with these elements:

\describe{
\item{Cloader}{path to R{} file(s) that set up native symbols and stubs, etc--- eg "R/RcppExports.R". Usually just one, but there could be several if multiple DLLs of the same type are required.}
\item{DLL}{pretty self-explanatory; omit the path and the extension}
\item{subenv}{see .TIDY.STUBS.AND.SYMBOLS below}
\item{makelines}{what to put in the Makefile, if there is one (see below). If several commands are required, paste them together separated by "'\\n'". At the moment this is left blank for \code{Rcpp} which is "needy" about being the only DLL in town, and/but also for \code{RcppTidy} where it shouldn't be (to allow multiple DLLs).}
\item{needs_makefile}{anything except TRUE means that a Makefile will \emph{definitely} be generated in the \code{src} folder. Otherwise, no Makefile is generated unless more than one Clinker is active (basically because multiple DLLs will then be required). Omit it if you don't need it (same effect as FALSE).}
\item{postcopy_hook_expr}{if set, an expression to be run inside the body of \code{\link{pre.install}} \emph{after} the source package has been set up "fully". The PIBH itself will be called \emph{before} the source package exists, but via this hook it can arrange to do much of the work post hoc; that approach is needed for \code{Rcpp} in particular, because \code{Rcpp::compileAttributes} demands a full source package. If you can put stuff into the PIBH rather than into this hook, it's probably better to do so. However, if you must, then \code{mvbutils:::Clinks_Rcpp} shows how to add a hook, and \code{mvbutils:::Clinks_Rcpp_postcopy} shows an example of what might be in a hook.}
}

The arguments to a PIBH must be, in order:

\describe{
\item{pkg, DLL}{self-explanatory names (no paths or extensions)}
\item{lldir, Rdir}{paths to folders containing the low-level "source" code (which will be either "<mypack>/src", or a subfolder thereof) and the R{} code (which will be "<mypack>/R").}
\item{src_changed}{this will contain the \code{src_changed} function, which your PIBH can then use without referring to package \pkg{mvbutils} at all.}
}
\subsection{Tidy stubs and symbols}{
A PIBH like \code{RcppTidy:::RcppTidy_pre_install} can arrange to put all the native-symbols and corresponding R{} stubs for each DLL into a separate sub-environment of your package's namespace. You then access the R{} stub for your low-level function \code{myCfun} in your DLL \code{Cbits} by calling \code{DLL_Cbits$myCfun(...)}. The effect is like \code{DLL_Cbits=useDynLib(Cbits,.registration=TRUE)} in your NAMESPACE (see "Writing R{} Exensions")---. except NB that, at least for \code{Rcpp} and \code{RcppTidy}, you should always call the R{} stub, rather than trying to access the native-symbol directly via \code{.Call}. To achieve this effect, your PIBH should return \code{subenv=TRUE}. There are a couple of minor differences from what's described in 'Writing R{} Extensions".

\itemize{
\item \code{DLL_Cbits} will actually be an environment, rather than an S3-classed list.
\item All the native-symbols from \code{Cbits} will be moved into \code{DLL_Cbits}, rather than cluttering up the main namespace.
\item Info on the DLL itself is available via \code{attr(DLL_Cbits,"DLLInfo")}, rather than as \code{DLL_Cbits} itself--- not that you should need it.
\item \code{DLL_Cbits} gets a finalizer that will unload the DLL when/if the package is unloaded--- this avoids you having to write a \code{.onLoad} for the package.
}

Note that if you don't like the name \code{DLL_Cbits} for the environment--- eg because it's too cumbersome--- then you can rename it yourself in \code{.onLoad}, eg like so:

\preformatted{
  mypack:::.onLoad <- function (libname, pkgname) { # include Rbrace to get around doc2Rd bug !}
   #### mypack onload ####
    run_Cloaders_mypack() # must come first
    evalq({
      DLL <- DLL_Cbits
      rm( DLL_Cbits)
    }, envir=asNamespace( 'mypack'))
  # ...
}
}
\subsection{Registering a pre install buildy hook}{
The point is that there's no way to know whether your package will load before or after \code{mvbutils}, so a little subterfuge is required. If \code{mvbutils} is already loaded, you can just call \code{Clink_packages} directly; if not, it's necessary to set a hook to be run when-and-if \code{mvbutils} does load (it may never be needed, eg during production use of a "proper" package that just uses your helper-mini-package). So your helper-mini-package will need a \code{.onLoad} containing something very like this:

\preformatted{
    # Tell mvbutils::pre.install about this package
    # Try to minimize lookups at time-of-future-use...
    xfun <- eval( substitute( function(...) mvbutils::Clink_packages( pkgname, RcppTidy_pre_install)))
    if( 'mvbutils' \%in\% loadedNamespaces()) { # already there
      xfun()
    } else {
      setHook(packageEvent("mvbutils", "onLoad"), xfun)
    }
}

Note that: (i) \code{pkgname} will normally be the first argument of the \code{.onLoad}, and will be the name of your helper-mini-package; and (ii) \code{RcppTidy_pre_install} should the name of \emph{your} PIBH; and (iii) your helper-mini-package should list \code{mvbutils} in "Description->Suggests", but probably not in "Description->Imports" because the latter will force \code{mvbutils} to be loaded even if the end-point "proper" pacakge doesn't need it.
}
\subsection{Regeneration checks}{There's no point in regenerating headers if the main source files haven't changed. \code{mvbutils::src_changed} is a utility function that your PIBH can call, to check whether source files have changed.
}
\subsection{Makefile}{
If there is only one type of source code in your package, then no Makefile will be produced unless the PIBH sets \code{needs_makefile}; normally that's not necessary. It might be required for eg Pascal sources--- and might be a bloody sight easier to do than figuring out Makevars from the spectacularly opaque doco in "Writing R{} Extensions"... not that anything about "make" is \emph{easy} AFAICS.

"Writing R{} Extensions" discourages the use of a Makefile, but I think there's no way round it in the case of multiple targets (see eg \url{https://github.com/kaskr/adcomp/issues/43}).
}
}
\usage{
Clink_packages(...)
src_changed( source_files, Cloader)
dummy_PIBH( pkg, DLL, lldir, Rdir, src_changed) # not "real"
# ... but its existence lets you see what the args should be
# Code of 'dummy_PIBH' is actually real, but from a different place
}
\arguments{
\item{ ...}{(Clink_packages) either missing, or a single character string naming a helper-mini-package such as \code{"Rcpp"} or \code{"RcppTidy"}, or a named function for such a package. See \bold{Value} and EXAMPLES.}
\item{ source_files}{(src_changed) the ones to check to see if re-pre-build is necessary}
\item{ Cloader}{(src_changed) the current version of the R{} file that produces "stubs" for source routines, etc. \code{pre.install} will add a checksum-manifest as the first line.}
\item{ pkg}{(PIBH) name of package}
\item{ DLL}{(PIBH) DLL...}
\item{ lldir}{(PIBH) folder where the source files live; normally "<mypack>/src", but could be a subfolder of that.}
\item{ Rdir}{(PIBH) folder where the Cloader (written in R) should go, eg "<myprotopack>/R"}
\item{ src_changed}{(PIBH) You can call this function to check for changes in source-files. It will be set to \code{mvbutils::src_changed} when your PIBH is called; this means that your PIBH does not have to refer to package \pkg{mvbutils} at all, and so packages depending/importing your helper also do not need to import/depend on package \pkg{mvbutils}.}
}
\value{
\code{Clink_packages()} returns all registered helper packages, as a list. \code{Clink_packages( "RcppTidy")} returns the PIBH for package "RcppTidy". \code{Clink_packages( RcppTidy=<<some function>>, ADThelper=<<some function>>)} registers the PIBH for those packages. PIBHs for registration are checked to see that they have just two arguments, \code{Cdir} and \code{Rdir}.
\code{src_changed} returns "" if nothing has changed, or a new first line for the manifest file if rebuilding is needed. In which case, your PIBH should do the rebuilding, and prepend the new first line to the manifest. [Currently the code half-attempts to \bold{edit} the manifest... not right.]
PIBH should return a list with these elements:
\item{ Cloader}{pathname of R{} script that will do any post-useDynLib setup for the DLL, eg creating R{} stubs to call the low-level routines. Should be in "<mypack>/R".}
\item{ makelines}{if a Makefile does end up being used, what instruction should compile this DLL? Single string, with "\\n" for any newlines.}
\item{ needs_makefile}{normally FALSE for C(etc) code; set to TRUE if this particular bit of source-code \emph{demands} a Makefile (eg if it's in Pascal); see .MAKEFILE}
\item{ subenv}{Name of environment within the package namespace, within which native-symbols should be created. Can be "", in which case the symbols are created directly in the namespace (like \code{Rcpp} does mid-2019).}
\item{ extra_copies}{new files that will need copying from the task package to the source package--- eg "<mypack>/src/RcppExports.cpp". Don't include \code{Cloader}.}
}
\examples{
# Setup in a helper-mini-package
\dontrun{
# In ADT:::._onLoad, where ADT:::ADT_PIBH is a PIBH; see main text
  Clink_packages( ADT=ADT_PIBH)
# Inside ADT:::ADT_PIBH <- function( dir, Rdir) \{...\}
  redo <- mvbutils::src_changed( Rcpp_files, this_Cloader)
  if( nzchar( redo)) \{ recompile() \}
}
}
\keyword{misc}
