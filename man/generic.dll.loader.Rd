\name{generic.dll.loader}
\alias{generic.dll.loader}
\alias{create.wrappers.for.dll}
\alias{ldyn.tester}
\alias{ldyn.unload}
\title{Convenient automated loading of DLLs}
\description{
\code{generic.dll.loader} is to be called from the \code{.onLoad} of a package. It calls \code{library.dynam} on all the DLLs it can find in the "libs" folder (so you don't need to specify their names), or in the appropriate sub-architecture folder below "libs". It also creates "R aliasses" in your namespace for all the \emph{registered} low-level routines in each DLL (i.e. those returned by \code{getDLLRegisteredRoutines}, qv), so that the routines can be called efficiently later on from your code--- see \bold{Details}.

If you just want to use \code{mvbutils} to help build/maintain your package, and don't need your package to import/depend on other functions in \code{mvbutils}, then it's fine to just copy the code from \code{generic.dll.loader} etc and put it directly into your own \code{.onLoad}.

\code{ldyn.tester}, \code{create.wrappers.for.dll}, and \code{ldyn.unload} are to help you develop a DLL that has fully-registered routines, without immediately having to create an R{} package for it. \code{ldyn.tester} loads a DLL and returns its registration info. The DLL must be in a folder \code{.../libs/<subarch>} where \code{<subarch>} is \code{.Platform$r_arch} iff that is non-empty; this is because \code{ldyn.tester} merely tricks \code{library.dynam} into finding a spurious "package", and that's the folder structure that \code{library.dynam} needs to see. \code{create.wrappers.for.dll} does the alias-creation mentioned above for \code{generic.dll.loader}. \code{ldyn.unload} unloads the DLL.
}
\usage{
# Only call this inside your .onLoad!
generic.dll.loader(libname, pkgname, ignore_error=FALSE, dlls=NULL)
# Only call these if you are informally developing a DLL outside a package
ldyn.tester(chname)
create.wrappers.for.dll( this.dll.info, ns=new.env( parent=parent.frame(2)))
ldyn.unload( l1)
}
\arguments{
\item{ libname, pkgname}{as per \code{.onLoad}}
\item{ ignore_error}{?continue to load other DLLs if one fails?}
\item{ dlls}{default (NULL) means "load all the DLLs you can find". Otherwise, it should be a character vector specifying the DLLs by name, without folder--- no extension is necessary.}
\item{ chname}{(for \code{ldyn.tester}) Path to the DLL (extension not required)}
\item{ this.dll.info}{(for \code{create.wrappers.for.dll}) A \code{DLLInfo} object, as returned by \code{.dynLibs()[[N]]} or \code{library.dynam(...)}}
\item{ ns}{(for \code{create.wrappers.for.dll}) If you're calling \code{create.wrappers.for.dll} manually, then this defaults to the calling environment, probably \code{.GlobalEnv}. For "internal use", \code{ns} is meant to be a namespace, but you shouldn't be using it like that!}
\item{ l1}{(for \code{ldyn.unload}) Result of previous call to \code{ldyn.tester}}
}
\value{
\code{generic.dll.loader} returns NULL (but see \bold{Details}).
\code{ldyn.tester} returns a class "DLLInfo" object if successful. \code{ldyn.unload} should return NULL if successful, and crash otherwise.
\code{create.wrappers.for.dll} returns the environment containing the aliasses.
Be careful with accidentally saving and loading the results of \code{ldyn.tester} and \code{create.wrappers.for.dll}; they won't be valid in a new R{} session. You might be better off creating them in the \code{mvb.session.info} environment on the search path; they will still be found, but won't persist in a different R{} session. See \bold{Examples}.
}
\details{
R-callable aliasses for your low-level routines will be called e.g. \code{C_myrout1}, \code{Call_myrout2}, \code{F_myrout3}, or \code{Ext_myrout4}, depending on type. Those for routines in "myfirstdll" will be stored in the environment \code{LL_myfirstdll} ("Low Level") in your package's namespace, which itself inherits from the namespace. In your own R{} code elsewhere in your package, you can then have something like

\preformatted{
  .C( LL_myfirstdll$C_myrout1, <<arguments>>) # NB no need for PACKAGE argument
}

Getting fancy, you can alternatively set the environment of your calling function to \code{LL_myfirstdll} (which inherits from the namespace, so all your other functions are still visible). In that case, you can just write

\preformatted{
  .C( C_myrout1, <<arguments>>)
}
}
\seealso{\code{\link{set.finalizer}} for a safe way to ensure cleanup after low-level routines.
}
\examples{
\dontrun{
mypack:::.onLoad <- function( libname, pkgname) generic.dll.loader( libname, pkgname)
#... or just copy the code into your .onLoad
# For casual testing of a DLL that's not yet in a package
dl <- ldyn.tester( 'path/to/my/dll/libs/i386/mydll.dll')
getDLLRegisteredRoutines( l1)
LL_mydll <- create.wrappers.for.dll( dl)
.C( LL_mydll$C_rout1, as.integer( 0)) # ... whatever!
ldyn.unload( dl)
# Safer because not permanent:
assign( 'dl', ldyn.tester( 'path/to/my/dll/libs/i386/mydll.dll'), pos='mvb.session.info')
assign( 'LL_mydll', create.wrappers.for.dll( dl), pos='mvb.session.info')
.C( LL.mydll$C_rout1, as.integer( 0)) # ... whatever!
}
}
\keyword{misc}
