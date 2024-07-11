\name{fast.read.fwf}
\alias{fast.read.fwf}
\title{Read in fixed-width files quickly}
\description{Experimental replacement for \code{read.fwf} that runs much faster. Included in \code{mvbutils} only to reduce dependencies amongst my other packages.
}
\usage{
fast.read.fwf(file, width,
    col.names = if (!is.null(colClasses))
        names( colClasses) else "V" \%&\%  1:ncol(fields),
    colClasses = character(0), na.strings = character(0L),  tz = "", ...)
}
\arguments{
\item{ file}{character}
\item{ width}{vector of column widths. Negative numbers mean "skip this many columns". Use an NA as the final element if there are likely to be extra characters at the end of each row after the last one that you're interested in.}
\item{ col.names}{names for the columns that are NOT skipped}
\item{ colClasses}{can be used to control type conversion; see \code{\link{read.table}}. It is an optional vector whose names must be part of \code{col.names}. There is one extension of the \code{read.table} rules:a \code{colClass} string starting \code{POSIXct.} will trigger automatic conversion to \code{POSIXct}, using the rest of the string as the format specifier. See also \code{tz}.}
\item{ na.strings}{are there any strings (other than NA) which should convert to NAs?}
\item{ tz}{used in auto-conversion to \code{POSIXct} when \code{colClass} is set}
\item{ ...}{ignored; it's here so that this function can be called just like \code{read.fwf}}
}
\value{
A \code{data.frame}, as per \code{read.fwf} and \code{read.table}.
misc
}
\keyword{misc}
