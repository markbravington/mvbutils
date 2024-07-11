\name{mvboption}
\alias{mvboption}
\title{Private options for mvbutils package and beyond}
\description{Set/get values in the environment \code{mvbutils::mvboptions}. Mostly for \code{mvbutils} itself, but anyone can use it at their own risk! Partly intended to ultimately obviate the dicey \code{mvb.session.info} environment on the search path...
}
\usage{
mvboption(...) # eg mvboption( use_something=TRUE) to set,
# ... or mvboption( 'what_am_i') to get
}
\arguments{\item{ ...}{Either a named pairlist (eg \code{mvboption( a=1, b=2)}) to set, or a character vector to get}
}
\value{Any previous value(s) of the options, if setting; this might mean an empty list. When getting, it's a list if more than one thing is being gotten, or the value itself if just one. More obvious than it sounds. See \bold{Examples}.
}
\examples{
mvboption( something=1)  # empty list
mvboption( a=2, b=3)     # empty list
mvboption( 'b')          # [1] 3
mvboption( cq( a, b))    # list with two elements
}
\keyword{misc}
