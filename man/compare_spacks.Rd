\name{compare_spacks}
\alias{compare_spacks}
\alias{compare_spack_code}
\title{Compare source packages eg for checking git}
\description{
Suppose you have a maintained task-package, and you've made a source package from it. And that there's a version on github, which you want to update. So you pull it, into your local github spot, then check for any changes with this function. If there aren't any, then you don't need to mess around with \code{\link{unpackage}}; you could carry on maintaining your task-package as usual, then scrunge it into your github spot, then push.

\code{compare_spack_code} actually looks for functions in "mypack.R" file that differ between the versions. It tries to look at attributes of the functions, too (usually there won't be any). If you ask for one specific function only, it will try to use the \pkg{diffr} package to display a nice diff of the two versions.

Probably I should describe what to do if you \emph{do} find a difference... haven't needed to yet!
}
\usage{
compare_spacks(pkg, gitplace = "d:/github/flub",
    d1, d2, character.only = FALSE)
compare_spack_code(pkg, gitplace = "d:/github/flub",
    d1, d2, character.only = FALSE, showdiff=NULL)
}
\arguments{
\item{ pkg}{as per \code{build.pkg} etc; eg \code{mypack} or \code{..mypack}}
\item{ gitplace}{your local github spot}
\item{ d1, d2}{Or you can specify the folders directly with these (need to set both)}
\item{ character.only}{as per \code{build.pkg} etc, eg \code{char="mypack"} (or more likely \code{char=thispack} when \code{thispack} is the index of a for-loop)}
\item{ showdiff}{optional, name of one function to show differences for.}
}
\value{A list with character-vector components \code{in1}, \code{in2}, and \code{diffs} (unless \code{showdiff} is set). Any file (or any function, for \code{compare_spack_code}) which are \emph{not} different won't be mentioned. If \code{showdiff} is set, nothing is returned, but you should see the results in your browser.
}
\keyword{misc}
