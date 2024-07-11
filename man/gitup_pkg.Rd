\name{gitup_pkg}
\alias{gitup_pkg}
\title{Update local git repo}
\description{
Update local git repo of your package (e.g. \code{splendid}), from a source package. Well, all it does is delete old files, and overcopy any whose MD5 sum has changed; you still have to do all the git bollocks yourself (add/commit/push, in my book). Maybe you should do a "git pull" first before all this, so that you can have the fun of reconciling changes before the extreme fun of "cannot pull; changes..." messages and the inevitable descent into "git push force".

IMO everything is simpler if your R{} source files are stored individually (function-by-function) \emph{because then you can easily see what changed}, but the vast and unenlightened hordes disagree with me and plonk it all in one single mega-file, complete with Roxygen "documentation" (don't get me started...). Sigh.

I hate Git, BTW--- in case that's not already obvious. This is really for my own use, in conjunction with \code{\link{unpackage}}, for a way to reconcile my own devel process with Git.
}
\usage{
gitup_pkg(
  pkg,
  gitparent,
  character.only = FALSE,
  excludo='funs.rda')
}
\arguments{
\item{ pkg}{name of yr task package, as per \code{install_pkg} etc (there are various options)}
\item{ gitparent}{folder where yr local git copy lives, or possibly one level higher}
\item{ character.only}{if TRUE, interpret \code{pkg} like a normal R{} variable, not like in \code{library}}
\item{ excludo}{files to not copy}
}
\keyword{misc}
