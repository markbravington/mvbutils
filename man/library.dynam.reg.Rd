\name{library.dynam.reg}
\alias{library.dynam.reg}
\title{Auto-registration and loading of dynamic library}
\description{
A bit like \code{useDynLib} but for direct use in your own package's \code{.onLoad}, this loads a DLL and creates objects that allow the DLL routines to be called directly. If your package "Splendid" calls \code{library.dynam.reg} in its \code{.onLoad()} to load a DLL "speedoo" which contains routines "whoosh" and "zoom", then an environment "C_speedoo" will be created in the \code{asNamespace("Splendid")}, and the environment will contain objects \code{whoosh} and \code{zoom}. R-code routines in "Splendid" can then call e.g.

\preformatted{
  .C( C_speedo$whoosh, ....)
}

You can only call \code{library.dynam.reg} inside \code{.onLoad}, because after that the namespace will be sealed so you can't poke more objects into it.
\subsection{Note}{Currently, \emph{all} routines go into \code{C_speedoo}, regardless of how they are meant to be called (\code{.C}, \code{.Call}, \code{.Fortran}, or \code{.External}). It's up to you to call them the right way. I might change this to create separate \code{Call_speedoo} etc.
}
\subsection{Note2}{As of R3.1.1 at least, it's possible that "recent" changes to the \code{useDynLib} directive in a package namespace might obviate the need for this function. In particular, \code{useDynLib} can now create an environment/list that refers directly to DLL, containing references to individual routines (which will be slightly slowed because they need to be looked up each time). Also, \code{useDynLib} can automatically register its routines. What's not obvious is whether it can yet do both these things together--- which is what \code{library.dynam.reg} is aimed at.
}
}
\usage{
# Only inside a '.onLoad', where you will already know "package" and "lib.loc"
library.dynam.reg(chname, package, lib.loc, ...)
}
\arguments{
\item{ chname}{DLL name, a string--- \emph{without} any path}
\item{ package, lib.loc}{strings as for \code{library.dynam}}
\item{ ...}{other args to \code{library.dynam}}
}
\keyword{misc}
