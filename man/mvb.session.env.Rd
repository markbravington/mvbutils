\name{mvb.session.env}
\alias{mvb.session.env}
\alias{mvb_session_env}
\title{Session info environment}
\description{
Package \pkg{mvbutils} needs a place to stash useful session-level stuff, such as \code{fix.list} (see \code{\link{fixr}}). Since like foreeeever (2001), this has been via a special environment called \code{mvb.session.info} which is attached to the search path. However, that's not how yer sposed to do it apparently, so for better security the direct use of \code{mvb.session.info} is deprecated in favour of calling the \emph{function} \code{mvb_session_env()} or its dotty synonym. Like the base-R functions \code{globalenv()} and \code{baseenv()}, an \emph{environment} is returned.

Future versions of \code{mvbutils} package will move the session-info environment into "private" storage within the \code{mvbutils} namespace, so that it can only easily be accessed via \code{mvb_session_env()} or \code{mvb.session.env()}.
}
\usage{
mvb_session_env()
mvb.session.env()
}
\value{Environment where session-level info used by the \pkg{mvbutils} package (and perhaps by other packages, such as \code{debug}) is stashed.
}
\keyword{misc}
