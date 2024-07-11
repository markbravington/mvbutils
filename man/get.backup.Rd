\name{get.backup}
\alias{get.backup}
\alias{create.backups}
\alias{read.bkind}
\title{Text backups of function source code}
\description{\code{get.backup} retrieves backups of a function or character object. \code{create.backups} creates backup files for all hitherto-unbacked-up functions in a search environment. For \code{get.backup} to work, all backups must have been created using the \code{\link{fixr}} system (or \code{create.backups}). \code{read.bkind} shows the names of objects with backups, and gives their associated filenames.
}
\usage{
get.backup( name, where=1, rev=TRUE, zap.name=TRUE, unlength=TRUE)
create.backups( pos=1)
read.bkind( where=1)
}
\arguments{
\item{ name}{function name (character)}
\item{ where, pos}{position in search path (character or numeric), or e.g. \code{..mypack} for maintained package \pkg{mypack}.}
\item{ rev}{if TRUE, most recent backup comes first in the return value}
\item{ zap.name}{if TRUE, the tag \code{"funname" <- } at the start of each backup is removed}
\item{ unlength}{if TRUE, the first line of each backup is removed iff it consists only of a number equal to 1+length( object). This matches the (current) format of character object backups.}
}
\value{
\item{ get.backup}{Either NULL with a warning, if no backups are found, or a list containing the backups, each as a character vector.}
\item{ create.backups}{NULL}
\item{ read.bkind}{a list with components \code{files} and \code{object.names}; these are character vector with elements in 1-1 correspondence. Some of the objects named may not currently exist in \code{where}.}
}
\details{
\code{\link{fixr}} and \code{\link{FF}} are able to maintain text-file backups of source code, in a directory ".Backup.mvb" below the task directory. The directory will contain a file called "index", plus files BU1, BU2, etc. "index" shows the correspondence between function names and BUx files. Each BUx file contains multiple copies of the source code, with the oldest first. Even if a function is removed (or \code{\link{move}}d) from the workspace, its BUx file and "index" entry are not deleted.

The number of backups kept is controlled by \code{options(backup.fix)}, a numeric vector of length 2. The first element is how many backups to keep from the current R{} session. The second is how many previous R{} sessions to keep the final version of the source code from. Older versions get discarded. I use \code{c(5,2)}. If you want to use the backup facility, you'll need to set this option in your \code{.First}. If the option is not set, no backups happen. If set, then every call to \code{\link{Save}} or \code{\link{Save.pos}} will create backups for all previously-unbackupped functions, by automatically calling \code{create.backups}. \code{create.backups} can also be called manually, to create the backup directory, index, and backup files for all functions in the currently-top task.

\code{get.backup} returns all available backup versions as \bold{character vectors}, by default with the most recent first. To turn one of these character vectors into a function, a \code{source} step is needed; see \bold{Examples}.

\code{read.bkind} shows which file to look for particular backups in. These files are text-format, so you can look at one in a text editor and manually extract the parts you want. You can also use \code{read.bkind} to set up a restoration-of-everything, as shown in \bold{Examples}. I deliberately haven't included a function for mass restoration in \code{mvbutils}, because it's too dangerous and individual needs vary.

Currently there is no automatic way to determine the type of a backed-up object. All backups are stored as text, so text objects look very similar to functions. However, the first line of a text object is just a number equal to the length of the text object; the first line of a function object starts with "function(" or "structure( function(". The examples show one way to distinguish automatically.

The function \code{\link{fix.order}} uses the access dates of backup files to list your functions sorted by date order.

\code{\link{move}} will also move backup files and update INDEX files appropriately.
}
\examples{
\dontrun{
## Need some backups first
# Restore a function:
g1 <- get.backup( "myfun", "package:myfun")[[1]] # returns most recent backup only
# To turn this into an actual function (with source attribute as per your formatting):
myfun <- source.mvb( textConnection( g1)) # would be nice to have an self-closing t.c.
cat( get.backup( "myfun", "package:myfun", zap=FALSE)[[1]][1])
# shows "myfun" <- function...
# Restore a character vector:
mycharvec <- as.cat( get.backup( 'mycharvec', ..mypackage)[[1]]) # ready to roll
# Restore most recent backup of everything... brave!
# Will include functions & charvecs that have subsequently been deleted
bks <- read.bkind() # in current task
for( i in bks$object.names) \{
  cat( "Restoring ", i, "...")
  gb <- get.backup( i, unlength=FALSE)[[1]] # unlength F so we can check type
  # Is it a charvec?
  if( grepl( '^ *[0-9]+ *$', gb[1])) # could check length too
    gb <- as.cat( gb[-1]) # remove line showing length and...
    # ...set class to "cat" for nice printing, as per 'as.cat'
  else \{
    # Nope, so it's a function and needs to be sourced
    tc <- textConnection( gb)
    gbfun <- try( source.mvb( gb)) # will set source attribute, documentation etc.
    close( tc)
    if( gbfun \%is.a\% "try-error") \{
      gbfun <- stop( function( ...) stop( ii \%&\% " failed to parse"), list( ii=i))
      attr( gbfun, 'source') <- gb # still assign source attribute
    \}
    gb <- gbfun
  \}
  assign( i, gb)
  cat( '\\n')
\}
}
}
\seealso{\code{\link{fixr}}, \code{\link{cd}}, \code{\link{move}}
}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities}
\keyword{documentation
}
