\name{mvbutils-package}
\alias{mvbutils}
\alias{mvbutils-package}
\docType{package}
\title{How to use the mvbutils package}
\description{
Package \pkg{mvbutils} is a collection of utilities, which eventually (?late 2025?) will be split into two parts: all-purpose routines that are usable in other packages and/or interactively, and an R-life-management part only for interactive use.

For now, \code{mvbutils} offers the following main features:

\itemize{
\item Miscellaneous goodies: local/nested functions (\code{\link{mlocal}}), display of what-calls-what (\code{\link{foodweb}}), multiple replacement (\code{\link{multirep}}), nicely-formatted latex tables (\code{xtable.mvb}), autoprinting of statements and outputs within a function (\code{\link{visify}}), universal-ish date convertor (\code{\link{autodate}}), parallel-processing numerical derivative (\code{\link{numvbderiv_parallel}}), matching multiple columns in dataframes/matrices (\code{\link{multimatch}}), nicely converting between arrays and dataframes (\code{\link{A2D}} and \code{\link{D2A}}), numerous lower-level lower-level utility functions and operators (\code{\link{mvbutils.utils}}, \code{\link{mvbutils.operators}}, \code{\link{extract.named}}, \code{\link{mdeparse}}, \code{\link{mcut}}, \code{\link{search.for.regexpr}}, \code{\link{strip.missing}}, \code{\link{FOR}}). These functions mostly aren't spectacular in themselves, but many are used in other more spectacular packages.
}

The interactive-only part has:

\itemize{
\item Hierarchical organization of projects (AKA tasks) and sub-tasks, allowing switching within a single R{} session, searching and moving objects through the hierarchy, objects in ancestor tasks always visible from child (sub)tasks, etc. See \code{\link{cd}}.
\item Improved function, text, and script editing facilities, interfacing with whichever text editor you prefer. The R{} command line is not frozen while editing, and you can have multiple edit windows open. Scriptlets can be edited as expressions, for subsequent calls to \code{eval}. Function documentation can be stored as plain text after the function definition, and will be found by \code{\link{help}} even if the function isn't part of a package. There is also a complete automatic text-format backup system for functions & text. See \code{\link{fixr}}.
\item Automated package construction, including production of Rd-format from plain text documentation. Packages can be edited & updated while loaded, without needing to quit/rebuild/reinstall. See \code{\link{mvbutils.packaging.tools}}.
\item "Lazy loading" for individual objects, allowing fast and transparent access to collections of biggish objects where only a few objects are used at a time. See \code{\link{mlazy}}.
}

The all-purpose part is fairly CRAN-friendly but the interactive-only part has various features that need to be hidden from prying eyes (which I have duly done). I long ago lost the energy to keep fighting pointless battles with a moving target, so I don't bother putting \code{mvbutils} on CRAN nowadays. After I split it, I might put the all-purpose part back on CRAN (still called \code{mvbutils}), since it is meant to be imported into other packages etc. The other part will become \code{mvbutifuls}, available somewhere else.
}
\section{Interactive use for r life management}{
As of 2024, I may be the only person still using these features! There used to be more users, before the Rstudio publicity machine juggernauted its way thru. Mine's still pretty good though: better, I reckon. Anyway! To get the full features of the \pkg{mvbutils} package-- in particular, the project organization-- you need to start R{} in the same directory every time (your "ROOT task"), and then switch to whichever project from inside R{}; see \code{\link{cd}}. Various \code{options} always need to be set to make \code{\link{fixr}} and the \pkg{debug} package work the way you want, so one advantage of the start-in-the-same directory-approach is that you can keep all your project-independent \code{options()}, library loads, etc., in a single \code{.First} function or ".Rprofile" file, to be called automatically when you start R{}. However, many features (including support for the \pkg{debug} package) will work even if you don't follow this suggestion.

The remaining sections of this document cover details that most users don't know about; there's no need to read them when you are just starting out with \code{mvbutils}.
}
\section{Housekeeping info}{
On loading, the \pkg{mvbutils} package creates a new environment in the search path, called \code{mvb.session.info}, which stores some housekeeping information. \code{mvb.session.info} is never written to disk, and disappears when the R{} session finishes. [For Splus users: \code{mvb.session.info} is similar to frame 0.] You should never change anything in \code{mvb.session.info} by hand, but it is sometimes useful to look at some of the variables there:

\itemize{
\item \code{.First.top.search} is the directory R{} started in (your ROOT task).
\item \code{.Path} shows the currently-attached part of the task hierarchy.
\item \code{base.xxx} is the original copy of an overwritten system function, e.g. \code{library}
\item \code{fix.list} keeps track of objects being edited via \code{\link{fixr}}
\item \code{session.start.time} is the value of \code{Sys.time()} when \code{mvbutils} was loaded
\item \code{source.list} is used by \code{\link{source.mvb}} to allow nesting of sources
\item \code{r.window.handle} is used by the \pkg{handy} package (Windows only)
\item \code{partial.namespaces} is used to alleviate difficulties with unloadable data files-- see \code{\link{mvbutils.packaging.tools}}
\item things whose name starts with ".." are environments used in live-editing packages
\item \code{maintained.packages} is a list of the latter
}
}
\section{Redefined functions}{
On loading, package \pkg{mvbutils} redefines a few system functions: \code{lockEnvironment}, \code{importIntoEnv} \code{loadNamespace}, \code{\link{print.function}}, \code{\link{help}}, \code{\link{rbind.data.frame}} and, by default, \code{library}, \code{savehistory}, \code{loadhistory}, and \code{save.image}. (The original version of routine \code{xxx} can always be obtained via \code{base.xxx} if you really need it.) The modifications, which are undone when you unload \code{mvbutils}, should have [almost] no side-effects. Briefly:

\itemize{
\item \code{library} is modified so that its default \code{\link{pos}} argument is just under the ROOT workspace (the one that was on top when \code{mvbutils} was loaded), which is needed by \code{\link{cd}}. This means that packages no longer get attached by default always in position 2.
\item \code{lockEnvironment} and \code{importIntoEnv} are modified to allow live-editing of your own maintained packages-- no change to default behaviour.
\item \code{loadNamespace} has the default value of its "partial" argument altered, to let you bypass \code{.onLoad} for selected faulty packages-- see \code{\link{mvbutils.packaging.tools}} and look for \code{partial.namespaces}. This allows the loading of certain ".RData" files which otherwise crash from hidden attempts to load a namespace. It lets you get round some truly horrendous problems arising from faults with 3rd-party packages, as well as problems when you stuff up your own packages.
\item \code{\link{rbind.data.frame}} does not ignore zero-row arguments (so it takes account of their factor levels, for example).
\item \code{\link{rbind.data.frame}}: dimensioned elements (i.e. matrices & arrays within data.frames) no longer have any extra attributes removed. Hence, for example, you can (if you are also using my \code{nicetime} package) \code{\link{rbind}} two data frames that both have POSIXct-matrix elements without turning them into raw seconds and losing timezones.
\item \code{\link{help}} and \code{?} are modified so that, if \code{utils:::help} can't find help for a function (but not a method, dataset, or package), it will look instead for a \code{doc} attribute of the function to display in a pager/browser using \code{\link{dochelp}}. Character objects with a ".doc" extension will also be found and displayed. This lets you write and distribute "informal help".
\item \code{loadhistory} and \code{savehistory} are modified so that they use the \emph{current} "R_HISTFILE" environment variable if it set. This can be set dynamically during an R{} session using \code{Sys.setenv}. Standard R{} behaviour is to respect "R_HISTFILE" iff it is set \emph{before} the R{} session starts, but not to track it during a session. If "R_HISTFILE" is not set, then \code{\link{cd}} will on first use set "R_HISTFILE" to "<<ROOT task>>/.RHistory", so that same the history file will be used throughout each and every session.
\item \code{save.image} is modified to call \code{\link{Save}} instead; this will behave exactly the same for workspaces not using \code{mvbutils} task-hierarchy feature or the \pkg{debug} package, but otherwise will prevent problems with \code{mtrace}d functions and \code{\link{mlazy}}ed objects.
\item \code{\link{print.function}} is modified to let you go on seamlessly using functions written prior to R{} 2.14 in conjunction with the \code{srcref} system imposed by R{} 2.14; see \code{\link{fixr}}.
}

Some of these redefinitions are optional and can be turned off if you really want: \code{loadhistory}, \code{savehistory}, \code{save.image}, \code{library}, \code{lockEnvironment}, \code{importIntoEnv}, and \code{loadNamespace}. To turn them off, set \code{options(mvbutils.replacements=FALSE)} \emph{before} loading \code{mvbutils}. However, I really don't recommend doing so; it will prevent \code{\link{cd}} etc, \code{\link{fixr}}, and the package-maintenance tools from working properly, and if you use \code{debug} you will probably cause yourself trouble when you forgetfully \code{save.image} an \code{mtrace}d function. You can also set the "mvbutils.replacements" option to a character vector comprising some or all of the above names, so that only those happen; if so, you're on your own. The other replacements are unavoidable (but should not be apparent for packages that don't import \code{mvbutils}).

After \code{mvbutils} has loaded, you can undo the modification of a function \code{xxx} by calling \code{assign.to.base( "xxx", base.xxx)}. Exceptions are \code{\link{help}}, \code{?}, \code{\link{print.function}}, \code{\link{rbind.data.frame}} which are intrinsic to \code{mvbutils. Unloading }mvbutils' will undo all the modifications.
\subsection{Nicer posixt behaviour}{\code{POSIXct} etc have some nasty behaviour, and \code{mvbutils} used to include some functions that ameliorated things. I've moved them into a separate package \pkg{nicetime}, available on request.
}
}
\section{Ess and mvbutils}{
For ESS users: I'm not an Emacs user and so haven't tried ESS with the \pkg{mvbutils} package myself, but a read-through of the ESS documentation (as of ~2005) suggests that a couple of ESS variables may need changing to get the two working optimally. Please check the ESS documentation for further details on these points. I will update this helpfile when/if I receive more feedback on what works (though there hasn't been ESS feedback in ~8 years...).

\itemize{
\item \code{\link{cd}} changes the search list, so you may need to alter "ess-change-sp-regex" in ESS.
\item \code{\link{cd}} also changes the prompt, so you may need to alter "inferior-ess-prompt". Prompts have the form WORD1/WORD2/.../WORDn> where WORDx is a letter followed by zero or more letters, underscores, periods, or digits.
\item \code{\link{move}} can add/remove objects in workspaces other than the top one, so if ESS relies on stored internal summaries of "what's where", these may need updating.
}
}
\section{Display bugs}{If you have a buggy Linux display where \code{readline()} always returns the cursor to the start of the line, overwriting any prompt, then try \code{options( cd.extra.CR=TRUE)}.
}
\author{Mark Bravington}
\seealso{\code{\link{cd}}, \code{\link{fixr}}, \code{\link{mlazy}}, \code{\link{flatdoc}}, \code{\link{dochelp}}, \code{\link{maintain.packages}}, \code{\link{source.mvb}}, \code{\link{mlocal}}, \code{\link{do.in.envir}}, \code{\link{foodweb}}, \code{\link{mvbutils.operators}}, \code{\link{mvbutils.utils}}, \code{\link{mvbutils.packaging.tools}}, package \pkg{debug}
}
\keyword{misc}
