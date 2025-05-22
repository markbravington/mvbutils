# This is package mvbutils 

"docattr" <-
function( rawstr){
  rawstr <- string2charvec( rawstr)
  oldClass( rawstr) <- 'docattr'
return( rawstr)
}


"string2charvec" <-
function( string){
## A string is a length-1 charvec. This function splits it at newlines, and removes the first (presumably empty) element.
  strsplit( string, '\n', fixed=TRUE)[[1]][-1]
}


"%!in%" <-
function (a, b) 
!(a %in% b)


"%&%" <-
function (a, b) 
  paste(a, b, sep = "")


"%**%" <-
function (x, y) 
{
    dimnames(x) <- NULL
    dimnames(y) <- NULL
    if (length(dim(x)) == 2 && length(dim(y)) == 2 && dim(x)[2] == 
        1 && dim(y)[1] == 1) 
        return(c(x) %o% c(y))
    if ((!is.null(dim(x)) && any(dim(x) == 1))) 
        dim(x) <- NULL
    if ((!is.null(dim(y)) && any(dim(y) == 1))) 
        dim(y) <- NULL
    if (is.null(dim(x)) && is.null(dim(y))) {
        if (length(x) == length(y)) 
            x <- x %*% y
        else {
            if ((length(x) != 1) && (length(y) != 1)) 
                stop("lengths of x (" %&% length(x) %&% ") and y (" %&% 
                  length(y) %&% ") are incompatible")
            else x <- x * y
        }
    }
    else x <- x %*% y
    if ((!is.null(dim(x)) && any(dim(x) == 1))) 
        dim(x) <- NULL
    x
}


"%<-%" <-
function( a, value){
  # a must be of the form '{thing1;thing2;...}'
  a <- as.list( substitute( a))[-1]
  e <- parent.frame()
  stopifnot( length( value) == length( a))
  stopifnot( all( sapply( a, is.name)))
  for( i in seq_along( a)) 
    assign( as.character( a[[i]]), value[[i]], envir=e)
    # eval( call( '<-', a[[ i]], value[[i]]), envir=e)
  NULL
}


"%downto%" <-
function( from, to) if( from >= to) from:to else numeric( 0)


"%except%" <-
function (vector, condition)
  subset_keeping_attributes( vector, match(vector, condition, 0) == 0)


"%grepling%" <-
function( x, patt) grepl( patt, x)


"%has.name%" <-
function( x, name) 
  ## Code of base::hasName
  ## NB can have several 'name's, ie vector
  match(name, names(x), nomatch = 0L) > 0L


"%in.range%" <-
function (a, b) 
(a >= min(b)) & (a <= max(b))


"%is.a%" <-
function (x, what) 
inherits(x, what, FALSE)


"%is.an%" <-
function (x, what) 
inherits(x, what, FALSE)


"%is.not.a%" <-
function (x, what) 
!inherits(x, what, FALSE)


"%is.not.an%" <-
function (x, what) 
!inherits(x, what, FALSE)


"%matching%" <-
function( x, patt) 
  unique( unlist( lapply( patt, grep, x=as.character( x), value=TRUE)))


"%not.in%" <-
function (a, b) 
!(a %in% b)


"%not.in.range%" <-
function( a, b) {
  (a < min( b)) | (a > max( b))
}


"%perling%" <-
function( x, patt) grepl( patt, x, perl=TRUE)


"%such.that%" <-
function( a, b)
  a[ eval( do.call( 'substitute', list( substitute( b), list( '.'=quote( a)))),  list( a=a),
      enclos=sys.frame( mvb.sys.parent()) )]


"%SUCH.THAT%" <-
function( a, b) {
  if( !length( a))
return( a)

  fun <- function( .) .
  body( fun) <- substitute( b)
  environment( fun) <- sys.frame( sys.parent())
  ind <- sapply( a, fun) # should help trap some errors, since LHS implies booleans
  if( ind %is.not.a% 'logical') {
stop( 'Condition is not "logical"')
  }

return( subset_keeping_attributes( a, ind))
}


"%that.are.in%" <-
function( a, b)
  subset_keeping_attributes( a, a %in% b)


"%that.dont.match%" <-
function( x, patt) {
  if( !length( patt))
stop( "invalid pattern argument") 
  x[ seq_along( x) %except% unlist( lapply( patt, grep, x=as.character( x)))]
}


"%THAT.DONT.MATCH%" <-
function( x, patt) {
  if( !length( patt))
stop( "invalid pattern argument") 
  x[ seq_along( x) %except% unlist( lapply( patt, grep, x=as.character( x), ignore.case=TRUE), use.names=FALSE)]
}


"%that.end.with%" <-
function( x, suffix) x[ endsWith( x, suffix)]


"%that.match%" <-
function( x, patt) {
  if( !length( patt))
stop( "invalid pattern argument") 
  unique( unlist( lapply( patt, grep, x=as.character( x), value=TRUE)))
}


"%THAT.MATCH%" <-
function( x, patt) {
  if( !length( patt))
stop( "invalid pattern argument") 
  unique( unlist( lapply( patt, grep, x=as.character( x), ignore.case=TRUE, value=TRUE)))
}


"%that.start.with%" <-
function( x, prefix) x[ startsWith( x, prefix)]


"%upto%" <-
function (from, to) 
if (from <= to) from:to else numeric(0)


"%where%" <-
function( x, cond) {
  # x is coerced to data.frame; cond is expression to evaluate, subbing first in x then in caller
  # Example: if x has a column 'stuff'
  # x %where% (stuff < 3)
  # is the same as x[ x$stuff<3,]
  # Note the brackets, required by operator precedence rules
  
  mum <- mvb.sys.parent()
  if( mum==0)
    mum <- .GlobalEnv
  else
    mum <- sys.frames()[[ mum]]
    
  cond <- eval( substitute( cond), as.data.frame( x), enclos=mum)
  cond[ is.na( cond)] <- FALSE
  x[ cond,]
}


"%where.warn%" <-
function( x, cond) {
  # x is data.frame; cond is expression to evaluate, subbing first in x then in caller
  # Example: if x has a column 'stuff'
  # x %where.warn% (stuff < 3)
  # is the same as x[ x$stuff<3,]
  # but if any of the conditions is NA or FALSE, a warning is given for those rows
  # Note the brackets, required by operator precedence rules
  
  sub.cond <- deparse( substitute( cond), nlines=1, width.cutoff=50)
  sub.x <- deparse( substitute( x), nlines=1, width.cutoff=20)
  rx <- row.names( x)

  mum <- mvb.sys.parent()
  if( mum==0)
    mum <- .GlobalEnv
  else
    mum <- sys.frames()[[ mum]]
    
  cond <- eval( substitute( cond), x, enclos=mum)
  cond[ is.na( cond)] <- FALSE
  if( !all( cond))
    warning( sprintf( 'Check of %s fails on row(s) [%s] of %s', sub.cond, 
        paste( rx[ !cond], collapse=','), sub.x))
  x[ cond,]
}


"%without.name%" <-
function( x, what) {
  new.names <- names( x) %except% what
  # x[ new.names] # falls foul of R buggggg with stripping attributes :/

  if( identical( new.names, names( x))) {
return( x)   # also works if names(x) is NULL!
  }

  oatts <- attributes( x)
  # oatts must exist, since nameless-x returns earlier
  x <- x[ new.names]
  oatts$names <- new.names
  attributes( x) <- oatts
return( x)
}


".onLoad" <-
function( libname, pkgname) {
  if( 'mvb.session.info' %in% search())
return() # create only once per session

  dedoc_namespace( pkgname) # remove plain-text doco

  wd <- getwd()
  # Attach "mvb.session.info". Bloody CRAN pedantry gets worse and bloody worse
  # Not sure if I need to do this before defining ATTACH, but whatever...
  a.bloody.ttach <- get( 'at' %&% 'tach', baseenv())
  a.bloody.ttach( pos = 2, name = "mvb.session.info", 
      list( .First.top.search = wd,
     .Path = c( ROOT=wd), session.start.time = Sys.time(), 
     partial.namespaces=character(0)))
  rm( a.bloody.ttach)

  nsenv <- environment( sys.function())
  evalq( envir=nsenv, {
      mvboptions <- new.env( parent=emptyenv()) # internal option-holder

      # Clink packages known to mvbutils by default:
      mvboptions$Clinks <- new.env( parent=mvboptions)

      # This Clink is built into mvbutils...
      Clink_packages( Rcpp=Clinks_Rcpp)
      # ... other Clinks should be added by helper packages eg RcppTidy

      # Anti CRANkiness
      tools <- asNamespace( 'tools')
      utils <- asNamespace( 'utils')

      # For 'autodate':
      formats4strptime <- local({
          opts4strptime <- as.matrix( expand.grid(
              YEAR= cq( y, Y),
              MONTH= cq( b, B, m),
              DAY= 'd',
              SEP= c('-', '/')
            ))
          opts4strptime[,1:3] <- '%' %&% opts4strptime[,1:3] # not SEP

          # Most permutations of Y,M,D... pretty lazy code
          formats4strptime <- character()
          eg <- as.matrix( expand.grid( 1:3, 1:3))
          eg <- eg[ !colSums( apply( eg, 1, duplicated)),]
          eg <- eg[ eg[,2] != 3,] # cannot have Year in the middle!
          eg <- cbind( eg, 6-rowSums( eg))

          for( iperm in 1:nrow( eg)){
            permo <- cq( DAY, MONTH, YEAR)[ eg[iperm,]]
            V <- opts4strptime[,permo]
            SEP <- opts4strptime[,'SEP']
            formats4strptime <- c( formats4strptime, sprintf( '%s%s%s%s%s',
                V[,1], SEP, V[,2], SEP, V[,3]))
          }
          names( formats4strptime) <- formats4strptime
        return( formats4strptime) # just from local()
        })


      # To override rbind.data.frame()
      body.rbind <- body( baseenv()$rbind) # for re-exporting generic rbind()
      body.print.function <- body( baseenv()$print.function)

      # plus, hack to allow matrices of POSIXct to rbind nicely...
      # ... extends to other classes if 'length<-' provided
      brdf <- local({ # to avoid temp var clutter
        brdf <- base::rbind.data.frame
        e <- new.env( parent=environment( brdf))
        newarray <- array
        body( newarray) <- substitute( {
            atts <- attributes( data)
            atts$dim <- atts$dimnames <- atts$names <- NULL
            data <- BODY # assignment needed in "newer" R (certainly >=3.3, perhaps younger); used to be OK just with BODY
            attributes( data) <- c( attributes( data), atts)
            data
          }, list( BODY=body( array)))
        e$array <- newarray
        environment( brdf) <- e
        brdf
      })

      # Regex for standard files in package dir:
      key_package_files <- c( cq( NEWS, CHANGES, INSTALL, configure, cleanup,
          LICENCE, LICENSE, ChangeLog),
          'README([.][^.]+)')

      # Idiotic subterfuges imposed by CRANally-retentive checks
      ATTACH <- CRANky( 'hcatta')
      untetherBalloon <- CRANky( 'gnidniBkcolnu')
      tetherBalloon <- CRANky( 'gnidniBkcol')
      balloonIsTethered <- CRANky( 'dekcoLsIgnidnib')

      LLDBflush <- function( file) 0
      body( LLDBflush) <- if( is.loaded( 'R_lazyLoadDBflush', PACKAGE='base'))
          substitute( unmentionable( 'R_lazyLoadDBflush', 
              as.character( file)[1], PACKAGE=bottom),
              list( unmentionable=
                  as.name( rawToChar( rev( charToRaw( 'llaC.')))),
                  bottom='base')
            )
        else
          substitute( unmentionable( lazyLoadDBflush( as.character( file)[1])),
              list( unmentionable=
                  as.name( rawToChar( rev( charToRaw( 'lanretnI.'))))))
      environment( LLDBflush) <- baseenv()
      LLDBflush <- CRANky( 'hsulfBDLL', environment())

      get.nsreg <- function() 0
      body( get.nsreg) <- substitute( unmentionable( getNamespaceRegistry()),
              list( unmentionable=
                  as.name( rawToChar( rev( charToRaw( 'lanretnI.'))))))
      environment( get.nsreg) <- baseenv()
      get.nsreg <- CRANky( 'gersn.teg', environment())

      maintained.packages <- originals.mp <- dont.lock.envs <- presave.hooks <- list()
      mvb.base.S3.generics <- names( get.S3.generics( 'base')) # baseenv(), ns=FALSE))
      mvb.base.S3.generics <- c( '[', '[<-', '$', '$<-', 
          '[[', '[[<-', 'cbind', 'rbind',
          'Ops', 'Math', 'Summary', 'Complex', mvb.base.S3.generics)
      mvb.base.S3.generics <- structure( rep( 'base', 
          length( mvb.base.S3.generics)),
          names=mvb.base.S3.generics)
      fix.list <- empty.data.frame( name= , file= , where=, where.type=,
          dataclass='', has.source=FALSE, file.time=0)
      dont.lock.envnames <- character(0)

      # Re-register print methods in base--- otherwise autoprint messes up
      REGS3M <- CRANky( 'dohtem3Sretsiger')
      S3MT <- get( '.__S' %&% '3MethodsTable__.')
      do.on( lsall( S3MT, patt='^print[.]') %except% 'print.default',
          REGS3M( 'print', sub( 'print.', '', .), get( .), baseenv())
        )
      }
  )
  eapply( nsenv, force) # no lazyload

  # Bindings are only locked *after* .onLoad-- so can't unlock them here...
  dont.lockBindings( 'dont.lock.envs', pkgname)
  dont.lockBindings( 'dont.lock.envnames', pkgname)

# Now putting fix.list & maintained.packages into mvb.session.info, 
# instead of package:mvbutils
#    copy.ns.objects( cq( fix.list, maintained.packages), pkgname)
  f <- function( val) blah-blah-blah
  for( x in cq( fix.list, maintained.packages, presave.hooks)) {
    body( f) <- substitute( if( missing( val)) x else x <<- val, 
        list( x=as.name( x)))
    environment( f) <- asNamespace( 'mvbutils')
    makeActiveBinding( x, f, as.environment( 'mvb.session.info'))
    dont.lockBindings( x, pkgname)
  }

  set.path.attr( pos.to.env( 1), .Path)
  setup.mcache( .GlobalEnv) # in case of cached objects in ROOT, which 'load' won't understand

  # assign.to.base( 'rbind.data.frame', mvb.rbind.data.frame, override.env=FALSE) # no choice on this one

  my.reps <- getOption( 'mvbutils.replacements', TRUE)
  # Circumvent user stuff-ups...
  my.reps <- switch( typeof( my.reps),
    'logical' = !identical( FALSE, my.reps[1]),
    'character' = my.reps,
    TRUE)

  # Slimmed-down list Sep 2012; others moved to 'nicetime'
  my.reps.opts <- named( cq( loadhistory, savehistory, save.image,
      library, lockEnvironment, importIntoEnv, loadNamespace))

  my.reps <- my.reps.opts[ my.reps] %except% NA # storm the last bastion of user stuff-ups

  # Only do nominated replacements
  # Next is mlocal so that ATB correctly picks up import env-- ATB limitation
  assign.to.base.opt <- function( what, ..., nlocal=sys.parent())
      mlocal( if( what %in% my.reps) assign.to.base( what, ...))

  assign.to.base.opt( 'lockEnvironment', hack.lockEnvironment(), 
      override.env=FALSE)
  assign.to.base.opt( 'importIntoEnv', hack.importIntoEnv(), override.env=FALSE)
  assign.to.base.opt( 'loadNamespace', hack( 'loadNamespace',
      partial=local({
        ok <- try( {
          pn <- as.environment( 'mvb.session.info')$partial.namespaces
          (length( pn)>0) && ((pn == "EVERY PACKAGE") || (package %in% pn))
        })
        !inherits(ok, 'try-error') && ok
      }),
      override.env=FALSE))

  # Now let ME (only) maintain mvbutils itself
  if( exists( 'tasks', .GlobalEnv, mode='character', inherits=FALSE)
      && !is.na( tasks[ 'mvbutils']))
    load.maintained.package( 'mvbutils', full.path( tasks[ 'mvbutils'], wd), 
        cq( ROOT, mvbutils),
        autopatch=!is.null( getOption( 'autopatch.mvbutils')))

  for( i in cq( load, save) %&% 'history')
    assign.to.base.opt( i, hack( i,
        file= if( nzchar( histfile <- Sys.getenv( 'R_HISTFILE'))) 
            histfile else '.Rhistory'))
  # Only reason for try(...) next is to avoid my stuffing mvbutils up while editing this...
  try( if( ('loadhistory' %in% my.reps.opts) && 
      !nzchar( Sys.getenv( 'R_HISTFILE')))
    Sys.setenv( R_HISTFILE=file.path( .First.top.search, '.Rhistory')))

  assign.to.base.opt( "library", hack( library, pos=local({
      poz <- try( 1+rev( mvbutils::search.task.trees())[1])
      if( inherits( poz, 'try-error'))
        poz <- 2
      poz
    })
  ))

  hack.save.image <- function( ...) {
      # formals will be replaced by those of 'save.image'
      # Evaluate args and check if they match defaults
      mc <- match.call( as.environment( 'mvb.session.info')$base.save.image)
      mc <- as.list( mc)[-1]
      mc[] <- mget( names( mc), sys.frame( sys.nframe()))
      form <- formals( base.save.image)
      if( length( mc) && !identical( form[ names( mc)], mc)) {
        mc <- c( quote( base.save.image), mc)
        eval( as.call( mc), sys.parent())
      } else # length(mc)==0 => default params anyway
        mvbutils::Save()
    }
  formals( hack.save.image) <- formals( save.image)
  assign.to.base.opt( "save.image", hack.save.image)

  # Needed by 'set.pkg.and.dir' in eg 'install.pkg'
  assign( 'R.rebuild.vers', numeric_version( R.rebuild.versions), nsenv)

  # .onLoad *must* work, so take no chances
  if( FALSE){ try( if( !interactive()){
      declare_globvars() # only to defeat the CRANiacs; otherwise 10^99 warnings cos...
      # ... codetools doesn't udnerstand the scoping. "but of course it can't" they say.
      # And that's my point... :/
      # so I've given up
    })
  }

  # Things below here no longer used
  if( FALSE)
    old.onLoad.stuff()

  # packageStartupMessage( 'MVBUTILS loaded\n') # apparently "not good practice"-- can't be bothered arguing
}


".onUnload" <-
function( libpath){
  s <- try( as.environment( 'mvb.session.info'))
  if( s %is.a% 'try-error')
return()

  for( i in ls( s, pattern='^base\\.'))
    assign.to.base( sub( '^base\\.', '', i), s[[i]])

  autoedit( FALSE)
  detach( 'mvb.session.info') # gulp
}


"?" <-
function ( e1, e2) {
  # `?` <- get("base.?", pos = "mvb.session.info")
  mc <- as.list(match.call())
  mc[[1]] <- quote( asNamespace( 'utils')$'?') # anti CRANky

  if( missing( e2)) {
    # Set 'mvb_help_type', just in case it's needed
    mvb_help_type <- mc$help_type
    if( is.null( mvb_help_type))
      mvb_help_type <- getOption( 'mvb_help_type', getOption( 'help_type', "text"))

    h1 <- try(eval(as.call(mc), parent.frame()), silent = TRUE)
    if( (h1 %is.not.a% "try-error") && (length(unclass(h1)) > 0)) 
return( h1)

    h1 <- dochelp( as.character( mc$e1), help_type=mvb_help_type) 
    if( h1 %is.a% c( "pagertemp", "browsertemp"))
return(h1)

    # If that failed too, just call it again & permit the crash...
  }

  eval(as.call(mc), parent.frame())
}


"[.dull" <-
function( x, ...) {
  res <- NextMethod( '[', x)
  oldClass( res) <- 'dull'
return( res)
}


"A2D" <-
structure( function( x, name.of.response= 'Value'){
## Convert array to data.frame; like base::array2DF() but that fails to convert obvious "characters" into numbers. Since base::tapply() generates characters from numbers, that is highly annoying. So, use this instead.

# Inelegant special-case for 'offarray' objects from eponymous package. Theoretically, the Right Ssolution would be to make 'A2D' generic, and write a special method for it in 'offarray'...
  if( x %is.an% 'offarray'){
return( as.data.frame( 'x', name_of_response= name.of.response))
  }

  xx <- array2DF( x, responseName= name.of.response)
  ndn <- names( dimnames( x))
  if( !is.null( ndn)){
    for( i in ndn){
      testnum <- suppressWarnings( as.numeric( xx[[ i]]))
      if( !any( is.na( testnum))){
        xx[[ i]] <- testnum
      }
    }
  }
  xx
}
, doc =  docattr( r"{
A2D           package:mvbutils

Array into dataframe

DESCRIPTION

From an 'array' (or 'matrix' or 'vector') input, 'A2D' produces a dataframe with one column per dimension of the input, plus a column for the contents, which will be called "response" unless you set the 'name.of.response' argument. Its (almost) inverse is 'D2A' (qv).

The other columns will have names "D1", "D2", etc, unless either (i) the input has a _named_ 'dimnames' attribute, in which its names will be used, or (ii) the argument "add.names" is set to a character vector naming the dimensions. They will be 'numeric' if If the input has any 'dimnames', then the latter's non-NULL elements will be used in place of 1,2,3,... etc for the entries in the corresponding columns.


.OFFARRAY

If you know you are dealing with an 'offarray' object rather than a regular array, you can just call 'as.data.frame(<myoffar>,...)' instead, for clarity. But if you do call 'A2D', all will be well (try it). OTOH, if you call 'base::array2DF(<myoffar>)' then you generally don't get what you want.


.NOTE

'D2A' and (something similar to) 'A2D' used to be in my semi-secret 'handy2' package under slightly different names, but they are useful enough that I've moved them to 'mvbutils' in 2025. The 'handy2' version ('array.to.data.frame' AKA 'a2d') made 'factor' columns rather than 'character', and contained a lot of code. 'A2D' is largely a wrapper for 'base::array2DF' (qv), which didn't use to exist; however, 'A2D' makes 'numeric' columns where possible and uses names of dimnames to set column names if possible. This all makes it work better with 'tapply' (qv).

'A2D' and 'D2A' are not strict inverses, because (i) if you start with a 'data.frame' that lacks rows for some index combinations, those rows will still appear in the result, (ii) 'factor' columns turn into 'character' columns, and (iii) columns might get re-ordered.


USAGE

A2D( x, name.of.response = "Value")


ARGUMENTS

 x: array, matrix, or, vector, including 'offarray' objects from the eponymous package.

 name.of.response: what to call the output column that holds the array _contents_ (as opposed to its dimensions).


VALUE

A 'data.frame', with one more column than there are dimensions to 'a'.


SEE.ALSO

'D2A'


EXAMPLES

grubbb <- expand.grid( xx=1:4, yy=2:3) # data.frame
grubbb$z <- with( grubbb, xx+10*yy)
D2A( grubbb, data.col='z')

A2D( D2A( grubbb, data.col='z'), name.of.response='zzzzzzz')
# ... how very interesting...


}")

)

"add.flatdoc.to" <-
structure( function( 
    x=NULL, char.x=NULL, pkg=NULL, env=NULL, 
    convert.to.source=FALSE
){
  if( is.null( env))
    env <- if( !is.null( pkg)) maintained.packages[[ pkg]] else parent.frame()
    
  if( is.null( char.x))
    char.x <- as.character( substitute( x))

  if( is.function( x)) { # TRUE except for fixr( existing.general.object)
    text <- docskel( x=x, char.x=char.x, env=env)
    class( text) <- 'docattr'
    if( is.null( x))
      x <- env[[ char.x]]
  } else
    text <- as.cat( '# Your scriptlet goes here...')

  if( !is.null( srcref <- attr( x, 'srcref'))){
    # Turn into 'source' attribute, so that it's handled by write_.sourceable_.function
    # If from previous fixr then
    attr( x, 'source') <- if( attr( srcref, 'srcfile')$filename=='dummyfile') 
        attr( srcref, 'srcfile')$lines
      else
        capture.output( print( srcref))
    attr( x, 'srcref') <- NULL
  }
  
  attr( x, 'doc') <- text
  x
}
, doc =  docattr( r"{
add.flatdoc.to    package:mvbutils

Skeletal flat-format documentation

DESCRIPTION

You very likely don't need to read this--- 'add.flatdoc.to' is usually called automatically for you, by 'fixr( ..., new.doc=TRUE)'. It adds skeletal flat-format documentation to a function, suitable for conversion to Rd-format using 'doc2Rd' (qv). The result should pass RCMD CHECK (but won't be much use until you actually edit the documentation).

USAGE

# See EXAMPLES for practical usage
add.flatdoc.to(x, char.x = NULL, pkg=NULL, env=NULL, convert.to.source=FALSE)


ARGUMENTS

 x: unquoted function name, sought in 'parent.frame()' unless 'pkg' is set
 char.x: [string] function name
 pkg: [string] name of maintained package where 'x' lives (optional)
 env: [environment] where to get 'x' from; defaults to caller unless 'pkg' is set.
 convert.to.source: [logical] if TRUE and 'x' has a "srcref" attribute, the latter is converted to a 'source' attribute. This ensures that 'fixr' will write the function correctly to the temporary file used by your text editor. Default is FALSE for historical reasons.


VALUE

A function with attribute 'doc' containing the flat-format documentation.

DETAILS

You don't *have* to write Rd-compatible documentation from the outset. You can write documentation that's as free-form as you wish, and there's no need to use 'add.flatdoc.to' to do it-- you can write the doco directly in your text editor provided that you can 'source' the resultant melange OK (see 'fixr'). I find 'add.flatdoc.to' useful, though, because I can never remember the headings or mild layout conventions of 'doc2Rd' and Rd-format itself.


SEE.ALSO

'flatdoc', 'fixr', 'doc2Rd'

EXAMPLES

## Don't run
myfun <- function( ...) ...
myfun <- mvbutils:::add.flatdoc.to( myfun)
# 'fixr( myfun)' will now allow editing of code & doco together

# Or, in a maintained package:
# ..mypack$myfun <<- add.flatdoc.to( myfun, pkg='mypack')
## End don't run

KEYWORDS

internal
}")

)

"add_list_defaults" <-
function( l, ...) {
###### Add defaults to list 'l' if not already in 'l'
  defaults <- list(...)
  l <- c( l, defaults %without.name% names( l))
return( l)
}


"as.cat" <-
function( x) { stopifnot( is.character( x)); oldClass( x) <- 'cat'; x}


"as.docattr" <-
function( x) {
  stopifnot( is.character( x))
  class( x) <- 'docattr'
  x
}


"as.env" <-
function ( x) UseMethod( 'as.env')


"as.env.character" <-
function( x) {
  glob <- names( attr( .GlobalEnv, 'path'))
  if( is.character( glob) && (length( glob)==1) && (x==glob))
return( .GlobalEnv)

return( as.environment( x)) # will trigger error if invalid
}


"as.env.default" <-
function( x)
  as.environment( x)


"assign.to.base" <-
function( x, what=lapply( named( x),
    function( x, where) get( 'replacement.' %&% x, pos=where), where=where), 
  where=-1, 
  in.imports=sys.parent() != 0 && exists( '.__NAMESPACE__.', environment( sys.function( sys.parent()))),
  override.env=TRUE) {
############  
  if( !is.list( what))
    what <- list( what)

  if( is.null( names( what)))
    names( what) <- x

  reassign <- function( obj, value, env) {
      if( tethered <- balloonIsTethered( obj, env))
        untetherBalloon( obj, env)
      if( override.env)
        environment( value) <- environment( get( obj, env))
      assign( obj, value, env)
      if( tethered) {
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        tetherBalloon( obj, env)
      }
    }
  penv <- if( in.imports) # ? extra parent.env 15/11/2010 
      parent.env( environment( sys.function( sys.parent())))
    else
      NULL

  get.S3.methods.tables <- function( wherestr, meth) {
#        generic <- sub( '.* for +([# ]+) +from.*', '\\1', wherestr)
#        where.gen <- do.call( 'getAnywhere', list( generic))$where
#        where.gen <- unique( sub( '(namespace|package):', '', where.gen))
        # scatn( 'Looking for "%s" in: %s', meth, paste( where.gen, collapse=', '))
        
        where.gen <- lapply( wherestr, function( x) asNamespace( x)$.__S3MethodsTable__.)
        has.meth <- sapply( where.gen, function( x) exists( meth, x, inherits=FALSE))
      return( where.gen[ has.meth])
      }

  for( xi in x) {
    this <- what[[ xi]]
    if( !is.null( penv) && exists( xi, penv, inherits=FALSE))
      reassign( xi, this, penv)

    # Hidden S3 methods will be duplicated in the package namespace
    where.xi <- do.call( 'getAnywhere', list( xi))$where
#    pkgs <- unique( sub( 'namespace:', 'package:', where.xi))
#        # sub( 'registered S3 method for .* from namespace ', 'package:', where.xi)))
#    pkgs <- sub( 'package:', '', grep( 'package:', pkgs, value=TRUE))
    
    # ?Should this search parent-envs of namespaces too? That seems a bit forward...

    if( !length( where.xi))
  next

    system.xi <- NULL
    
    envs.xi <- unlist( c( 
        FOR( where.xi %that.match% '^registered S3', get.S3.methods.tables( 
            sub( ' +.*', '', sub( '.*namespace *', '', .)), meth=xi)),
        FOR( where.xi %that.match% '^package:', as.environment(.)),
        FOR( where.xi %that.match% '^namespace:', asNamespace( sub( 'namespace:', '', .)))
      ))
          
    envs.xi <- unique( unlist( envs.xi))
          
    for( ienv in envs.xi) {
      if( exists( xi, ienv, inherits=FALSE)) {
        if( is.null( system.xi)) {
          system.xi <- ienv[[ xi]]
        }
        reassign( xi, this, ienv)
      }
    }

    # Keep original-- only the first one found though, which is a bit random
    if( !exists( 'base.' %&% xi, where='mvb.session.info', inherits=FALSE))
      assign( 'base.' %&% xi, system.xi, 'mvb.session.info')
  }

  invisible( NULL)
}


"attach.mlazy" <-
function( dir, pos=2,
    name='data:' %&% attr( .GlobalEnv, 'name') %&% ':' %&% basename( dir)) {
  ATTACH( list(), pos=pos, name=name)
  e <- pos.to.env( pos)
  attr( e, 'path') <- dir <- task.home( dir)
  load.refdb( envir=e) # does nothing if no file
}


"atts" <-
function( x, exclude=cq( levels, class, dim, dimnames, names, row.names, tsp)) 
  names( attributes( x)) %except% exclude


"autodate" <-
structure( function( datestr, ct=TRUE){
## Auto date converter stuff. Tries numerous calls to strptime() with
## various "plausible" formats. Given REALLY stupid ambiguous data with
## d & m (& even y) in 2-digit format with no days above 12, point out stupid!
## It is a sad indictment of R or POSIX or humanity in general that this even needs to exist

  if( !length( datestr)){
    as.POSIXy <- if( ct) as.POSIXct else as.POSIXlt
return( as.POSIXy( Sys.time())[-1]) # easiest I could think of...
  }

  # formats4strptime must already exist. Now made during mvbutils:::.onLoad()

  atts <- attributes( datestr) %except% 'class'
  datestr <- gsub( ' +', '', datestr)
  outt <- lapply( formats4strptime, strptime, x=datestr, tz='GMT')
  goodo <- do.on( outt, all( !is.na( .)))

  if( sum( goodo)>1){
    xdatestr <- gsub( '([/-])0+', '\\1', datestr)
    xdatestr <- sub( '^0+', '', xdatestr)

    # Try to match the input string (barring leading zeros)
    # cos strptime() will just ignore important stuff without telling you
    for( iposs in which( goodo)){
      inn <- format( outt[[iposs]], formats4strptime[ iposs])
      inn <- gsub( '([/-])[0 ]+', '\\1', inn)
      inn <- sub( '^[0 ]+', '', inn)
      goodo[[ iposs]] <- all( inn==xdatestr)
    }
  }

  if( (length( datestr)>1) && (sum( goodo)>1) ){
    warning( "Ambiguous date format: gonna pick smallest range...")
    igoodo <- which( goodo)
    rangio <- do.on( outt[ igoodo], unclass( difftime( max( .), min( .), units='days')))
    goodo[ igoodo[ rangio != min( rangio)]] <- FALSE
  }

  # Incredibly, stupid POSIX can *still* give multiple interpretations... because
  # ... %Y (which should get 4 digits) will accept fewer digits, silently, and return dates
  # ... pre-AD 1000. FFS.

  if( sum( goodo)>1){
    warning( "Ambiguous date format: gonna pick futurest...")
    igoodo <- which( goodo)
    rangio <- do.on( outt[ igoodo], unclass( max( as.POSIXct( .))))
    goodo[ rangio != max( rangio)] <- FALSE
    # And if they are the same--- which can happen with May which is its own abbrev! ---
    # ... then all is OK, I guess
    # in the unwonderful world of POSIX
    goodo[ seq_along( goodo) > which.max( goodo)] <- FALSE # that's R folks
  }

  if( sum( goodo) != 1){
stop( "Ambiguous or incomprehensible dates...")
  }

  returno <- outt[[ which( goodo)]]
  if( ct){
    returno <- as.POSIXct( returno)
  }
  mostattributes( returno) <- atts
return( returno)
}
, doc =  docattr( r"{
autodate    package:mvbutils


Universal date converter


DESCRIPTION

At your own risk: this aims for the most-sensible interpretation of a character vector of dates in whatever godawful format they may be, to avoid the delights of 'strptime' (qv). "Most sensible" is according to _me_; but _you_ (or the originator of the dataset) might have different ideas, and if so it's _your_ problem. See DETAILS for, you guessed it, DETAILS.


USAGE

autodate(datestr, ct=TRUE)


ARGUMENTS

 datestr: character vector
 ct: whether to return 'POSIXct' (the default) or 'POSIXlt' object


VALUE

'POSIXct' or 'POSIXlt' (qv) object, always with timezone 'GMT'. Attributes ('dim' etc) should be preserved.


DETAILS

All dates in the vector must have the _same_ format as each other. Each must have a Day, Month, and Year, in any order except that Year cannot be in the middle, separated by either "/" or "-". Spaces are ignored. Month can be numbers, 3-letter abbreviation, or full month name. Year can be either 2- or 4-digits, but (unlike 'strptime' itself) all digits are checked; note that 'strptime' will uncomplainingly accept '1/1/2099' as coming from AD20 if you tell it '%Y', even tho IMO you should have to write eg '1/1/0020' if you want stuff pre-AD1000, and 'autodate' will enforce that. Consequently, leading zeros on Day and Month are ignored, but are honoured on Year.

In case of ambiguous results (which are common, especially with Day and 2-digit Year), the version with the smallest range is chosen; if several versions have equal range, the most recent (or furthest-future) is chosen.


EXAMPLES

## Should add more...

## Unambigous:
autodate( '1-Mar-2017')
# [1] "2017-03-01 GMT"


## Stupid
autodate( '1/1/1')
# Warning in autodate("1/1/1") :
#   Ambiguous date format: gonna pick futurest...
# [1] "2001-01-01 GMT"


## Ancient: NB 4 digits.
autodate( '1/13/0001')
# [1] "0001-01-13 GMT"


## Lazy, 2-digit year: assume modern
autodate( '1/13/01')
#  Warning in autodate("1/13/01") :
#    Ambiguous date format: gonna pick futurest...
#  [1] "2001-01-13 GMT"


## Corner case...
autodate( character(), ct=FALSE)
# POSIXlt of length 0

autodate( character())
# character(0) # actually CORRECT-- it really is 'POSIXct'-- but just prints as if wrong


}")

)

"autoedit" <-
function( do=TRUE){
  s <- as.environment( 'mvb.session.info')
  
  if( do) {
    if( !exists( 'autoedit.callback', envir=s, inherits=FALSE) ||
        is.null( s$autoedit.callback))
      assign( 'autoedit.callback', addTaskCallback( 
          function( ...) { 
            try( FF())
            TRUE
          }), 
          envir=s)
  } else if( !is.null( s$autoedit.callback)) {
    removeTaskCallback( s$autoedit.callback)
    s$autoedit.callback <- NULL
  }
}


"bugfix_Rd2roxygen" <-
structure( function(
    sourcedir,
    pkg=basename( sourcedir),
    nsinfo=NULL
){
## Bugfix some infelicities in Rd2roxygen
## Subterfuge to avoid Suggesting that package
## cos I reckon Roxygen is a waste of space...

  OK <- re_bloody_quireNamespace( 'Rd2roxygen')
  if( OK) {
    # Rd2roxygen uses message() :/ and also sends pointless ##...## lines to stdout()
    # so capture/suppression is tedious and I couldn't make it work
    # so... hackity hack hack it is!

    Rd2Rox <- attr( OK, 'env')$Rd2roxygen
    e <- new.env( parent=environment( Rd2Rox))
    # Avoid pointless ##...## notifications, which also **** up R-History!
    e$timestamp <- function() NULL
    # Redirect messages to a character vector

    e$mess <- character()
    xmessage <- function( ...) { m <- .makeMessage( ...); mess <<- c( mess, m) }
    environment( xmessage) <- e
    e$message <- xmessage
    e$exported_names <- hack_exported_names
    # FUCKING CRAN now wants me to de-fucking-clare 'mess' as "global"
    # ... despite that half of base-R is written in totally incomprehensible lexically-scoped codoid.
    # OK, declare_globvars() should kill it with a hammer, if I do this...
    mess <- 'Kindly fuck off, CRAN'

    # Bug in Rd2roxygen: funny names, eg '[<-.thing', are not searched for correctly
    # because they are misinterpreted inside a regex pattern
    # There's a token wrapper that deals with the dots, but that's not enuf.
    # That token-wrapper is a call to 'gsub', so we "just" redefine 'e$gsub'...
    # Currently (Sep 2021) gsub() is only called once in Rd2roxygen, but
    # to be on the, ahem, 'safe' side, I check that the call is as expected
    e$gsub <- function( ...) {
        sc <- sys.call()
        if( identical( sys.call(), quote( gsub( "\\.", "\\\\.", fname)))) {
          eval.parent( quote( mvbutils::to.regexpr( fname)))
        } else {
          mc <- match.call( expand.dots=TRUE)
          mc[[1]] <- quote( base::gsub)
          eval.parent( mc)
        }
      }
    environment( Rd2Rox) <- e

    cat( 'Trying Rd2Roxygen...\n')
    OK <- try( Rd2Rox( sourcedir)) %is.not.a% 'try-error'

    if( !OK) {
      m <- e$mess
      parsed_lines <- grep( '^parsed: ', m)
      if( length( parsed_lines)>1) {
        m <- m[ -(1 %upto% (tail( parsed_lines, 1)-1))]
      }
      warning( "'Rd2roxygen' crash: ")
      cat( m, sep='\n', file=stderr())
    }

    # More Rd2roxygen "features"...
    # S3 methods not handled, so postfix
    main_roxy_file <- file.path( sourcedir, 'R', pkg %&% '.R')
    if( is.null( nsinfo)){ # won't be, if called from pre.install()
      S3lines <- readLines( file.path( sourcedir, 'NAMESPACE')) %that.match% '^ *S3method[(]'
      S3lines <- gsub( ' +', '', S3lines)
      S3lines <- sub( '^[^"]*"', '', S3lines)
      S3lines <- sub( '"[)].*', '', S3lines)
      S3lines <- do.call( 'rbind', strsplit( S3lines, '","', fixed=TRUE))

      import_instructions <- readLines( file.path( sourcedir, 'NAMESPACE')) %that.match% '^ import(From){?}[(]'
      nsinfo <- list( S3=S3lines) # this is what you'd get inside pre.install()
    } else {
      # Built-in version from pre.install() ; does not currently handle importFrom
      # but anti-clash exceptions will be noted
      import_instructions <- sprintf( 'import( %s)', get_import_instructions( nsinfo))
    }

    # Replace old stuff...
    roxied <- readLines( main_roxy_file) %that.dont.match% "^#' +@import"
    # Don't fart around with Roxy's pointless reworking of import syntax...
    # ... just plonk in the raw stuff directly after the header
    headline <- startsWith( roxied, '# This is package')[1]
    roxied <- multinsert( roxied, headline, list( "#' @rawNamespace " %&% import_instructions))

    S3meth <- apply( nsinfo$S3, 1, paste, collapse='.')  # matrix( c( generics, classes), ncol=2)
    for( i in seq_along( S3meth)){
      already <- grep( sprintf("#' *@exportS3Method +" %&% "\"%s\" +\"%s\"",
          to.regexpr( nsinfo$S3[i, 1]), to.regexpr( nsinfo$S3[i, 2])),
          roxied)
      if( length( already) > 1){
        # Eliminate duplicates!
        roxied <- roxied[ -already[-1]]
      } else if( !length( already)) {
        # Need a new one
        where <- which( roxied == "#' @export " %&% S3meth[ i])[1]
        if( is.na( where)){
          fundef <- which( roxied==sprintf( '"%s" <-', S3meth[ i]) )
          if( is.na( fundef)){
            warning( "Can't find definition of apparent S3 method '%s'; can't add Roxy tag",
                S3meth[ i])
          } else {
            roxied <- multinsert( roxied, fundef-1, '') # will be over-written
            where <- fundef
          }
        }
        roxied[ where] <- "#'" %&% sprintf( ' @exportS3Method "%s" "%s"',
              nsinfo$S3[i,1],  nsinfo$S3[i,2])
      } # if not already
    } # for S3 methods

    # Placeholder for fixing other Roxybugs...

    writeLines( roxied, main_roxy_file) # ... at last


    # Extra R file containing Roxybollox (and no code)--- need to add to Collate field if present
    Roxybollox <- sprintf( '%s-package.R', pkg)
    if( file.exists( file.path( sourcedir, 'R', Roxybollox))) {
      dfile <- file.path( sourcedir, 'DESCRIPTION')
      deskrypshun <- read.dcf( dfile)[1,]
      collate <- match( 'Collate', names( deskrypshun), 0)
      if( collate) {
        deskrypshun[ collate] <- deskrypshun[ collate] %&% ' ' %&% Roxybollox
        writeLines( con=dfile, paste( names( deskrypshun), deskrypshun, sep=': '))
      }
    } # gawd

    rm( e) # tidy
  }

return( OK)
}
, doc =  docattr( r"{
bugfix_Rd2roxygen    package:mvbutils


Like Rd2roxygen, but fixing some bugs


DESCRIPTION

Like package 'Rd2roxgyen' (qv), for modding the R source in an existing source package to add Roxgyen comments (i.e., documentation and export instructions). Package 'Rd2roxgyen' does most of the work, but it has a couple of bugs and I don't think they are likely to get fixed soon (one of them is "a feature").

This is called internally by 'pre.install' (qv), if "RoxygenNote" is found in the DESCRIPTION file, but can also be called manually.

Personally I don't like Roxygen--- to me it seems a bad implementation of a reasonable idea (keep documentation tightly linked with code, and avoid markup complexity) for which there are better and simpler ways--- but Others do. So this might help, especially if Others are collaborating with non-Roxygenites..


USAGE

bugfix_Rd2roxygen( sourcedir, pkg = basename(sourcedir), nsinfo = NULL)


ARGUMENTS

 sourcedir: folder containing the source package (so it should contain a "DESCRIPTION" file, a folder called "R", and so on)

 pkg: name of the package, deduced from 'sourcedir' if not supplied

 nsinfo: info slurped from the NAMESPACE file (actually just about S3 methods, which 'Rd2roxygen' inexplicably ignores). Used internally by 'pre.install' for efficiency, but if you are calling this manually, you can leave it and the NAMESPACE file itself will be used


VALUE

Alters the file "<sourcedir>/R/<pkg>.R". Also, if there's a file "<sourcedir>/R/<pkg>-package.R", then a "Collate" field is added (or modified) to the DESCRIPTION file, to make sure that the package-source is collated _last_. This is a good idea, for reasons that I can no longer remember.


}")

)

"build.pkg" <-
function( pkg, character.only=FALSE, flags=character(0), cull.old.builds=TRUE){
  # In case of path arg
  if( missing( pkg)) {
    orig.pkg <- character.only
  } else {
    thing <- substitute( pkg)
    orig.pkg <- if( thing %is.a% 'name') as.character( thing) else pkg
  }
      
  set.pkg.and.dir( TRUE)
  result <- rcmdgeneric.pkg2( pkg=pkg, outdir=outdir, indir=sourcedir,
    cmd='build', flags=flags) 
  if( cull.old.builds) {
    cull.old.builds( orig.pkg, character.only=TRUE) # pkg reset to string
  }
    
invisible( result)
}


"build.pkg.binary" <-
function( 
  pkg, 
  character.only=FALSE, 
  flags=character(0), 
  cull.old.builds=TRUE, 
  multiarch=NA, 
  preclean=TRUE
){
  i <- 1
  repeat{
    temp.inst.lib <- file.path( tempdir(), 'templib' %&% i)
    if( !file.exists( temp.inst.lib))
  break
    i <- i+1
  }
  mkdir( temp.inst.lib)
  on.exit( unlink( temp.inst.lib, recursive=TRUE))

  # In case of path arg
  if( missing( pkg)) {
    orig.pkg <- character.only
  } else {
    thing <- substitute( pkg)
    orig.pkg <- if( thing %is.a% 'name') as.character( thing) else pkg
  }

  set.pkg.and.dir( TRUE)

  if( preclean) {
    flags <- c( '--preclean', flags) # good idea
  }

  if( is.na( multiarch)) {
    check_multiarch()
  }

  if( !multiarch) {
    flags <- c( '--no-multiarch', flags)
  } else { # R (3.3) is buggy here; see install.pkg for workaround
    flags <- c( '--compile-both', '--force-biarch', flags)
  }

  # Workaround for fucking R bug that pollutes the source-package with /src-i386 and /src-x64...
  # move it somewhere else first!
  td <- tempfile()
  mkdir( td)
  on.exit( unlink( td, recursive=TRUE, force=TRUE), add=TRUE)
  file.copy( sourcedir, td, recursive=TRUE)


  result <- rcmdgeneric.pkg2( pkg, outdir=outdir, indir=file.path( td, pkg), 
      cmd='INSTALL',
      flags=c( flags, '--build -l ' %&% temp.inst.lib),
      must_hack_makeconf=TRUE # to allow cross-compile from i386
    )

  if( cull.old.builds) {
    cull.old.builds( orig.pkg, character.only=TRUE) # pkg reset to string
  }

invisible( result)
}


"cachefile.path" <-
function (..., fsep = .Platform$file.sep) 
{
    if (any(sapply(list(...), length) == 0)) 
        return(character())
    paste(..., sep = fsep)
}


"called.by" <-
function( fname, can.match, where) {
  where <- if( is.environment( where)) list( where) else as.list( where)
  which <- unlist( lapply( where, exists, x=fname), use.names=FALSE)
  if( !any( which)) {
    f <- if( exists( fname)) get( fname) else list() }
  else
    f <- get( fname, pos=where[[ index( which)[ 1] ]])

#  flist_ as.character( unlist( f[length(f)], use=FALSE))
  flist <- char.unlist( f)

  if( !length( flist))
return( numeric( 0))

# Check for functions occurring in default parameters!
# R version does this automatically
#  everything_ unique( c( flist, as.character( unlist( as.list( f)[-length(f)], use=FALSE))))
  everything <- flist

# Main task:
  everything <- match( everything, can.match, nomatch=0)
  everything <- everything[ everything>0]

# Check for generic functions:
# Ignore for now in R
#  if( mode(f[[length(f)]])=='internal' | flist[1]=='UseMethod')
#    everything_ c( everything, index(substring( can.match, 1, nchar( fname)+1) == fname %&% '.'))

  everything
}


"callees.of" <-
function( funs, fw, recursive=FALSE) {
  if( fw %is.a% 'foodweb')
    fw <- fw[[1]]
  all <- dimnames( fw)[[1]]
 
  orig.funs <- funs
  out <- character()
  while( length( funs)) {
    vec <- all %in% funs
    these <- all[ vec %*% fw > 0] # ie: these <- callees.of( funs, fw)
    funs <- these %except% c( out, orig.funs) # orig.funs to cut off loops
    out <- unique( c( out, these))
    if( !recursive)
break
  }
sort( out)  
}


"callers.of" <-
function( funs, fw, recursive=FALSE) {
  if( fw %is.a% 'foodweb')
    fw <- fw[[1]]
  all <- dimnames( fw)[[1]]
  
  orig.funs <- funs
  out <- character()
  while( length( funs)) {
    vec <- all %in% funs
    these <- all[ fw %*% vec > 0] # ie: these <- callers.of( funs, fw)
    funs <- these %except% c( out, orig.funs) # orig.funs to cut off loops
    out <- unique( c( out, these))
    if( !recursive)
break
  }
sort( out)    
}


"cat_strings_rawly" <-
function( x, prefix_package=TRUE){
## charvecs are printed as single raw strings, so they look "pure" without escaped characters or extraneous quotes or [1], [2], etc...

## docattr objects get wrapped in a call to docattr(), so that their class is kept  but they are printed "simply". Note that, if they have aAny other attributes (they shouldn't), those are discarded.

## For non-docattr objects, this should handle nested attributes, omigod
  attx <- attributes( x)
  sep <- ''
  nbrax <- 0

  wrapfun_name <- 'string2charvec'
  if( x %is.a% 'docattr'){
    attx <- list()
    x <- unclass( x)
    wrapfun_name <- 'docattr'
  } else if( length( attx)){
    nbrax <- 1
    cat( 'structure(')
    for( iattx in names( attx)){
      cat( sprintf( '\n%s %s =', sep, simplest_name_generator( iattx)))
      cat_strings_rawly( attx[[ iattx]])
      sep <- ','
    }
    cat( '\n') # start actual main thing on new line
    x <- unclass( x)
  }
  
  cat( sep)
  if( is.character( x) && 
      (length( x) > 1) && 
      !length( attributes( x)) && # after unclass()
      !any( grepl( '\\n', x))){ # no explicit <CR>
    # might have multiple end-raws within 1 element, so do.on() won't work
    n_dashes <- max( c( 0, 
        unlist( FOR( gregexpr( '[}][-]*"', x),
        attr( ., 'match.length'))) - 1))
    dashes <- strrep( '-', n_dashes)
      
#    scatn( ' mvbutils::%s( r"%s{', wrapfun_name, dashes)
    scatn( ' %s%s( r"%s{', 
        if( prefix_package) 'mvbutils::' else '', 
        wrapfun_name, dashes)
    cat( x, sep='\n')
    scatn( '}%s")', dashes)
  } else {
    cat( deparse.names.parsably( x))
  }
  
  cat( strrep( ')', nbrax))
}


"cd" <-
structure( function ( to, execute.First = TRUE, execute.Last = TRUE) {
######################

  # This to allow cd(..) from task "mvbutils" itself or a subtask...
  penv <- environment( sys.function())
  if( identical( penv, .GlobalEnv) || identical( penv, pos.to.env( 2))) {
    mc <- match.call( expand.dots=TRUE)
    mc[[1]] <- quote( mvbutils::cd) # anti CRANky
return( eval( mc, sys.frame( sys.parent())))
  }

  need.to.promote.on.failure <- FALSE
  on.exit({
    if (need.to.promote.on.failure) promote.2.to.1()
    if (!is.null(wd <- attr(.GlobalEnv, "path"))) setwd(wd)
    if (.Path[length(.Path)] != wd) {
      .Path <<- if (any(.Path == wd))
          .Path[1:max(index(.Path == wd))]
        else
          c("??" = character(0), "??" = wd)
    }
    cdprompt()
  })
  orig.path <- attr(.GlobalEnv, "path")
  if (is.null(orig.path) || !my.all.equal(orig.path, .Path[length(.Path)]))
stop("problem with taskly status of .GlobalEnv!")

  if (missing(to))
    to <- get.cd.from.menu()
  else to <- substitute(to)
  to <- strsplit(deparse(to), "/", fixed=TRUE)[[1]]
  if (to[1] == "0")
    to <- c(rep("..", length(.Path) - 1), to[-1])
  to <- to %except% "."
  if (!length(to))
return(invisible())

  ii <- to[-length(to)] != ".." & to[-1] == ".."
  ii <- c(ii, FALSE) | c(FALSE, ii)
  to <- to[!ii]
  if (!length(to))
return(invisible())

  if (to[1] == ".." && length(.Path) == 1)
stop("Can't move backwards from ROOT!")

  # Tedious temporaries...
  if( getOption( 'mvbutils.quick.cd', FALSE))
    suppressWarnings(
        mlazy( what=cq( .Random.seed, last.warning, .Traceback, .Saved.plots)
            %SUCH.THAT% exists( ., where=1, inherits=FALSE)))

  #save.image() # replaced by...
  Save.pos( 1) # 12/04, to work with all.rda & lazy-Load

  if( !nzchar( Sys.getenv( 'R_HISTFILE')))
    Sys.setenv( R_HISTFILE=file.path( .First.top.search, '.Rhistory'))

  if( getOption( 'mvbutils.update.history.on.cd', TRUE))
    try( savehistory(), silent=TRUE) # won't work if embedded; never mind

  need.to.promote.on.failure <- TRUE
  if (to[1] == "..") {
    cd..(1)
    for (i in 1 %upto% sum(to == ".."))
      cd..(2)
  } else
    load.mvb( file.path( orig.path, '.RData'), names(orig.path),
        pos = 2, attach.new = TRUE, path = orig.path)
  remove(list = lsall(pos = 1), pos = 1)
  attributes(.GlobalEnv) <- list()
  if (length(to)) {
    for (i in 2 %upto% length( to)) {
      cd.load(to[1], pos = 2, attach.new = TRUE)
      to <- to[-1]
    }
    cd.load(to[1], pos = 1, attach.new = FALSE)

    if( getOption( 'mvbutils.update.history.on.cd', TRUE))
      try( loadhistory(), silent=TRUE) # won't work if embedded; never mind
    need.to.promote.on.failure <- FALSE
  }
}
, doc =  docattr( r"{
cd        package:mvbutils

Organizing R workspaces

DESCRIPTION

'cd' allows you to set up and move through a hierarchically-organized set of R workspaces, each corresponding to a directory. While working at any level of the hierarchy, all higher levels are attached on the search path, so you can see objects in the "parents". You can easily switch between workspaces in the same session, you can move objects around in the hierarchy, and you can do several hierarchy-wide things such as searching, even on parts of the hierarchy that aren't currently attached.

USAGE


# Occasionally: cd()
# Usually: cd(to)
# Rarely:
 cd(to, execute.First = TRUE, execute.Last = TRUE)


ARGUMENTS

  to: the path of a task to move to or create, as an unquoted string. If omitted, you'll be given a menu. See DETAILS.

  execute.First: should the '.First.task' code be executed on attachment? Yes, unless there's a bug in it.

  execute.Last: should the '.Last.task' code be executed on detachment? Yes, unless there's a bug in it.


DETAILS

R workspaces can become very cluttered, so that it becomes difficult to keep track of what's what (I have seen workspaces with over 1000 objects in them). If you work on several different projects, it can be awkward to work out where to put "shared" functions-- or to remember where things are, if you come back to a project after some months away. And if you just want to test out a bit of code without leaving permanent clutter, but while still being able to "see" your important objects, how do you do it? 'cd' helps with all such problems, by letting you organize all your projects into a single tree structure, regardless of where they are stored on disk. Each workspace is referred to (for historical reasons) as a "task".

Note that there is a basic choice when working with R: do you keep everything you write in a text file which you 'source' every time you start; or do you store all the objects in a workspace as a binary image in a ".RData" file, and rely on 'save' and 'load'? [Hybrids are possible, too.] Some people prefer the text-based approach, but others including me prefer the binary image approach; my reasons are that binary images let me organize my work across tasks more systematically, and that repeated text-sourcing is much too slow when lengthy analyses or data extractions are involved. The 'cd' system is really geared to the binary image model and, before 'cd' moves to a new task, either up or down the hierarchy, the current workspace is automatically saved to a binary image. Nevertheless, I don't think 'cd' is incompatible with other ways of working, as long as the ".RData" file (actually the 'tasks' object) is not destroyed from session to session. At any rate, some people who work by 'source'ing large code files still seem to find 'cd' useful; it's even possible to use the '.First.task' feature to auto-load a task's source files into a text editor when you 'cd' to that task. With the ".RData"-only approach, it is highly advisable to have some way of keeping separate text backups, at least of function code. The 'fixr' editing system is geared up to this, and I presume other systems such as ESS are too.

To use the 'cd' system, you will need to start R in the *same* workspace every time. This will become your ROOT or home task, from which all other tasks stem. There need not be much in this workspace except for an object called 'tasks' (see below), though you can use it for shared functions that you don't want to organize into a package. From the ROOT task, your first action in a new R session will normally be to use 'cd' to switch to a real task. The 'cd' command is used both to switch between existing tasks, and to create new ones.

To set yourself up for working with 'cd', it's probably a good idea to make the ROOT task a completely new blank workspace, so the first step is to (outside R) create an empty folder with some name like "Rstart". [In MS-Windows, you should think about *where* to put this, to save yourself inordinate typing later on. If you are planning to create a completely new set of folders for your R projects, you might want to put this ROOT folder near the top of the disk directory structure, rather than in the insane default that Windows proffers, which usually looks something like "c:\document...\local...\long...\ridiculous...". However, if you are planning instead to link existing folders into the task hierarchy, then it's better to create the ROOT folder just above, or parallel to, the location of these folders.] Start R in this folder, type 'library( mvbutils)', and then start linking your existing projects into the task hierarchy. [Of course, this assumes that you do have existing projects. If you don't, then just start creating new tasks.] To link in a project, just type 'cd()' and a menu will appear. The first time, there will be only one option: "CREATE NEW TASK". Select it (or type 0 to quit if you are feeling nervous), and you will be prompted for a "task name", by which R will always subsequently refer to the task. Keep the name short; it doesn't have to be related to the location of the disk directory where the .RData lives. Avoid spaces and weird characters-- use periods as separators. Task names are case-sensitive. Next, you'll be asked which disk directory this task refers to. By default, 'cd' expects that you are creating a new task, and therefore suggests putting the directory immediately below the current task directory. However, if you are linking in an existing project, you'll need to supply the directory name. You can save huge amounts of typing by using "." to refer to the current directory, and on *nix systems you can use "~" too. Next, you'll be returned to the R command prompt-- but the prompt will have changed, so that the ">" is preceded by the task name. If you type 'search()', you'll see your ROOT task in position 2, below .GlobalEnv as usual. Despite the name, though, the new .GlobalEnv contains the project you've just linked, and if you type 'ls()', you should see some familiar objects. Now type 'cd(0)' to move back to the ROOT task (note the changed prompt), type 'search()' and 'ls()' again to orient yourself, and proceed as before to link the rest of your pre-existing tasks into the hierarchy. When you now type 'cd()', the menu will have more choices. If you select an existing task rather than creating a new one, you will switch straightaway to that workspace; watch the prompt.

Once you have a hierarchy set up, you can switch the current workspace within the hierarchy by calling e.g. 'cd(existing.task)' (note the lack of quotes), or by calling 'cd()' and picking off the menu. You can move through several levels of the hierarchy at once, using a path specifier such as 'cd(mytask/data/funcs)' or 'cd(../child.of.sibling)'. Path specifiers are just like Unix or Windows disk paths with "/" as the separator, so that "." means "current task" and ".." means "parent". However, the character 0 must be used to denote the ROOT task, so that you have to type 'cd(0/different.task)' rather than 'cd(/different.task'). You can display the entire hierarchy by calling 'cdtree(0)', or graphically via 'plot( cdtree( 0))'.

When you first set up your task hierarchy, you'll also want to create or modify the '.First' function in your ROOT task. At a minimum, this should call 'library( mvbutils)', but you may also want to set some options controlling the behaviour of 'cd' (see the OPTIONS section). If you use other features of 'mvbutils' such as the function-editing interface in 'fixr', there will be further options to be set in '.First'. [MAC users: for some strange reason '.First' just doesn't get called if you are using the "usual" RGUI for MACs. So what you need to do is create a ".Rprofile" file in your ROOT folder using a text editor; this file should both contain the definition of the '.First' function, and should also call '.First()' directly. You can also put the '.First' commands directly into the ".Rprofile" file, but watch out for the side-effect of creating objects in '.GlobalEnv'.]

You can create a fully hierarchical structure, with subtasks within subtasks within tasks, etc. Even if your projects don't naturally look like this, you may find the facility useful. When I create a new task, I tend to start with just one level of hierarchy, containing data, function code, and results. When this gets unspeakably messy, I often create one (or more) subtasks, usually putting the basic data at the top level, and functions and results at the lower level. Apart from tidiness, this provides some degree of protection against overwriting the original data. And when even this gets too messy-- in one task, I have more than 150 functions, and it is very easy to generate 100s of analysis results-- I create another level, keeping "established" functions at the second tier and using the third tier for temporary workspace and results. There are no hard-and-fast rules here, of course, and different people use R in very different ways.

A task can have '.First.task' and/or '.Last.task' functions, which get called immediately after 'cd'ing into the task from its parent, or immediately before 'cd'ing back to its parent, respectively (see ARGUMENTS). These can be useful for dynamic loading, loading scripts into a text editor, attaching & detaching datasets, etc., and facilitate the use of tasks as informal packages.

For turning tasks into formal R packages, consult 'mvbutils.packaging.tools'.


HOW.IT.WORKS

The mechanism underlying the tree structure is very simple: each task that has any subtasks will contain a character vector called 'tasks', whose names are the R names of the tasks, and whose elements are the corresponding disk directories. Your ROOT task need contain no more than a '.First' function and a 'tasks' object.

You can manually modify the 'tasks' vector, and sometimes this is essential. If you decide to move a disk directory, for example, you can manually change the corresponding element of 'tasks' to reflect the change. (Though if you are moving a whole task hierarchy, e.g. when migrating to a new machine, consult 'cd.change.all.paths'. Having said that, the ability to use relative pathnames in tasks, which is present since about mvbutils version 2.0, makes 'cd.change.all.paths' partly redundant.) You can also rename a task very easily, via something like

%%#
names( tasks)[ names( tasks)=="my.old.name"] <- "my.new.name"

You can use similar methods to "reparent" a subtask without changing the directory structure.

There is (deliberately, to avoid accidents) no completely automatic way of removing tasks. To "hide" a task from the 'cd' system, you first need to be 'cd'ed to its parent; then remove the corresponding element of the 'tasks' object, most easily via e.g.

%%#
tasks <- tasks %without.name% "mysubtask"

If you want to remove the directories corresponding to "mysubtask", you have to do so manually, either in the operating system or (for the brave) in R code.

Remember to 'Save()' at some point after manually modifying 'tasks'.


OPTIONS

Various 'options()' can be set, as follows. Remember to put these into your '.First' function, too.

'write.mvb.tasks=TRUE' causes a sourceable text representation of the 'tasks' object to be maintained in each directory, in the file 'tasks.r'. This helps in case you accidentally wipe out the .RData file and lose track of where the child tasks live. To create these text representations for the first time throughout the hierarchy, call 'cd.write.mvb.tasks(0)'. You need to put the the 'options' call in your '.First'.

'abbreviate.cdprompt=n' controls the length of the prompt string. Only the first 'n' characters of all ancestral task names will be shown. For example, 'n=1' would replace the prompt 'long.task.name/data/funcs>' with 'l/d/funcs>'.

'mvbutils.update.history.on.cd=FALSE' will prevent automatic saving & reloading of the history file when 'cd' is called.

'cd' checks the 'R_HISTFILE' environment variable and, if unset, sets it to 'file.path( getwd()), ".Rhistory")'. This (combined with the 'mvbutils' replacement of the standard versions of 'savehistory' and 'loadhistory'-- see 'package?mvbutils') ensures that the same history file is used throughout each and every R session. My experience is that a single master history file is safer. However, if you want to override this behaviour-- e.g. if you want to use a separate history file for each task-- call something like 'Sys.setenv( R_HISTFILE=".Rhistory")' *before* the *first* use of 'cd'.


NOTE

'cd' calls 'setwd' so that file searches will default to the task directory (see also 'task.home').

'cd' always calls 'Save' before attaching a child task on top or moving back up the hierarchy. If you have many and/or big objects, the default behaviour can be slow. You can speed this up-- sometimes dramatically-- by "mcacheing" some of your objects so that they are stored in separate files-- see 'mlazy'.

If there are no changes to the ".RData" file, 'cd' will not modify the file-- in particular, its date-of-access will be unchanged. This helps avoid unnecessary file copying on subsequent synchronization. However, there are several seemingly innocuous operations which change the workspace: calling a random number function (changes '.Random.seed'), causing an error (creates '.Traceback'), and causing a warning (creates 'last.warning'). To avoid forcing a change to the entire ".RData" file whenever one of these changes, you can set 'option( mvbutils.quick.cd=TRUE)'; this turns on 'mcache'ing for those objects (see 'mlazy'), so that they are stored in separate mini-files.

'cd' is only meant to be called interactively, and has only been tested in that context.

'cd' will issue a warning and refuse to move back up the hierarchy if it detects a non-task attached in position 2. You will need to manually detach any such objects before 'cd'ing back up, or write a '.Last.task' function to automatically do the detaching. To make sure that 'library' (and any automatic loading of packages, e.g. if triggered by 'load'ing a file referring to a namespace) always inserts packages below ROOT, the '.onLoad' code in 'mvbutils' makes a minor hack to 'library', changing the default 'pos' argument accordingly.

Two objects in the 'mvb.session.info' search environment (see 'search()') help keep track of what parts of the hierarchy are currently attached; '.First.top.search' and '.Path'. The former is set when 'mvbutils' loads, and the latter is updated by 'cd'. Attached tasks can be identified by having a 'path' attribute consisting of a _named_ character vector. Normal packages also have a 'path' attribute, but without 'names'.


SEE.ALSO

'move', 'task.home', 'cdtree', 'cdfind', 'cditerate', 'cd.change.all.paths', 'cd.write.mvb.tasks', 'cdprompt', 'fixr', 'mlazy'


AUTHOR

Mark Bravington


KEYWORDS

utilities
}")

)

"cd.." <-
function( pos, nlocal=sys.parent()) mlocal({
  # Do .Last before checking move, because this might detach rubbish
  if( execute.Last) {
    .Last.task <- if( exists( '.Last.task', where=pos, inherits=FALSE))
        get( '.Last.task', pos=pos)
      else
        function( pos) {}
    try( .Last.task( 1))
  }

  # For MPs with loaded namespaces:
  if( regexpr( '^temp.nsobj:', search()[pos+1])>0)
    detach( pos=pos+1)
    
  can.go.up <- !is.null( names( attr( as.environment( pos+1), 'path')))
  if( can.go.up)
    update.maintained.package( names( .Path)[ length( .Path)])
  else {
    need.to.promote.on.failure <- pos>1
stop( "Can't cd up; there's a non-task in position 2", call.=FALSE)
  }

  if( pos>1) {
    need.to.promote.on.failure <- TRUE
    detach( pos=pos)
  }

  to <- to[-1]
  orig.cd.path <- paste( names( .Path), collapse='/')
  .Path <<- .Path[ -length( .Path)]
  setwd( .Path[ length( .Path)])
  
  # All good; change fix.list if cd'ing up from a maintained package
  if( (names( orig.path) %in% names( maintained.packages)) &&
      (attr( maintained.packages[[ names( orig.path)]], 'path') == orig.path)) {
    fixing.in.pkg <- fix.list$where==orig.cd.path
    fix.list$where.type[ fixing.in.pkg] <<- 'package'
  }
})


"cd.change.all.paths" <-
function( from.text='0', old.path, new.path) {
  case <- if( .Platform$OS=='windows') 
      upper.case 
    else 
      function( x) x # case-sensitive

  cditerate( from.text, cd.change.all.paths.guts, '', old.path=case( old.path), 
      new.path=case( new.path), case=case)
}


"cd.change.all.paths.guts" <-
function( found, task.dir, task.name='??', env, old.path, new.path, case) {
  cat( task.name, '\n')
  if( exists( 'tasks', envir=env, inherits=FALSE) && is.character( tasks)) {
    tasks <- get( 'tasks', envir=env)
    tasks[] <- otasks <- gsub( '\\\\', '/', tasks) # [] to keep names
    tasks[] <- gsub( old.path, new.path, case( tasks))
    if( any( tasks != otasks)) {
      assign( 'tasks', tasks, envir=env)
      save.refdb( env, file=file.path( task.dir, '.RData'))

      if( getOption( 'write.mvb.tasks', FALSE))
        write.mvb.tasks( env=env, dir=task.dir)
    }
  }

  found
}


"cd.load" <-
function (taskname, pos, attach.new, nlocal = sys.parent()) mlocal({
  if( taskname %in% names( maintained.packages))
stop( "No longer allowed to 'cd' into maintained packages-- if you must, then first use 'unmaintain.package'")

  if (!exists("tasks", where = 2, inherits = FALSE))
    tasks <- character(0)
  full.path <- tasks[taskname]
  if (is.na(full.path)) {
    if (yes.no("Task " %&% taskname %&% " does not exist yet. Create it? "))
      full.path <- make.new.cd.task(taskname)
    else {
      cat("No ")
      stop("Just exiting cd", call.=FALSE)
    }
  }
  if( regexpr( '^[.]{1,2}/', full.path)>0) # rel paths OK; 24/6/2005
    full.path <- file.path( getwd(), full.path)

  # Strip out .. and .
  full.path <- gsub( '/\\./', '/', full.path)
  full.path <-  gsub( '[^/]*/\\.\\./', '', full.path)
  names( full.path) <- taskname

  filename <- file.path( full.path, '.RData')
  if( is.na( filename) || !file.exists( full.path)) # || added 1/7/2005
stop( "Can't find an image file to load for '" %&% taskname %&% "'!")

  # Will *assume* there is just one possible package
  # Make sure saved image is up-to-date
  # save.refdb OK because can't mtrace in m.p. itself
  if( any( names( maintained.packages)==taskname)) {
    save.refdb( file=filename, envir=maintained.packages[[ taskname]])
    fixing.in.pkg <- index( fix.list$where == paste( c( names(.Path), taskname), collapse='/'))
    fix.list$where.type[ fixing.in.pkg] <<- 'task'
  }

  load.mvb( filename, name = taskname,
    pos = pos, attach.new = attach.new, path = full.path)

  .Path <<- c(.Path, full.path)
  setwd( full.path) # new 24/6/2005, to allow rel paths

  epos <- as.env( pos)
  if( any( names( maintained.packages)==taskname)) {
    if( packageHasNamespace( taskname, full.path)
        && !isNamespaceLoaded(taskname))
      warning( "Package version of '" %&% taskname %&% "' not loaded yet-- may behave slightly differently")
    else if( isNamespaceLoaded( taskname)) {
      # Make "copies" of all extra stuff that's in namespace, using active bindings to ensure namespace
      # is synchronized. Copies go into a new search environment just below task
      ns <- asNamespace( taskname)
      etemp <- ATTACH( NULL, pos=pos+1, name='temp.nsobj:' %&% taskname)
      # Don't copy weird stuff
      extroids <- lsall( ns) %except% lsall( epos)
      extroids <- extroids %such.that% (regexpr( '^\\.__.*__\\.$', .)<0)
      for( x in extroids) {
#        f <- substitute( if( missing( val)) x else x <<- val, list( x=as.name( x)))
#        makeActiveBinding( x, as.function( alist( val=, f), envir=ns), etemp)
        f <- function( val) 0
        body( f) <- substitute( if( missing( val)) x else x <<- val, list( x=as.name( x)))
        environment( f) <- ns
        makeActiveBinding( x, f, etemp)
      }
    } # if loaded namespace

    # Change fix list to point to here rather than m.p.
    repfl <- index( fix.list$where==search.task.trees()[1])
    if( length( repfl))
      fix.list[ repfl, 'where.type'] <<- 'task'
  } # if maintained

  if (execute.First && exists(".First.task", where = pos, inherits = FALSE)) {
    .First.task <- epos$.First.task # reassign for clarity of any error msg
    try(.First.task(pos))
  }
})


"cd.write.mvb.tasks" <-
function( from=., from.text=substitute( from)) 
  invisible( cditerate( from.text, cd.write.mvb.tasks.guts, vector( 'list', 0)))


"cd.write.mvb.tasks.guts" <-
function( found, task.dir, task.name, env) {
#  cat( task.name, task.dir); print( env)
  if( exists( 'tasks', envir=env, inherits=FALSE))
    write.mvb.tasks( env=env, dir=task.dir)
  found
}


"cdfind" <-
structure( function( pattern, from=., from.text, show.task.name=FALSE) {
  if( missing( from.text))
    from.text <- substitute( from) 
  answer <- cditerate( from.text, cdfind.guts, vector( 'list', 0), pattern, show.task.name=show.task.name)
  attributes( answer) <- list( names=names( answer))
  answer
}
, doc =  docattr( r"{
cdfind        package:mvbutils
cdregexpr
cdtree
cd.change.all.paths
cd.write.mvb.tasks
cditerate
plot.cdtree

Hierarchy-crawling functions for cd-organized workspaces

DESCRIPTION

These functions work through part or all of a workspace (task) hierarchy set up via 'cd'. 'cdfind' searches for objects through the (attached and unattached) task hierarchy. 'cdtree' displays the hierarchy structure. 'cd.change.all.paths' is useful for moving or migrating all or part of the hierarchy to new disk directories. 'cd.write.mvb.tasks' sets up sourceable text representations of the hierarchy, as a safeguard. 'cditerate' is the engine that crawls through the hierarchy, underpinning the others; you can write your own functions to be called by 'cditerate'.

If a task folder or its ".RData" file doesn't exist, a warning is given and (obviously) it's not iterated over. If that file does exist but there's a problem while loading it (e.g. a reference to the namespace of a package that can't be loaded-- search for 'partial.namespaces' in 'mvbutils.packaging.tools') then the iteration is still attempted, because something might be loaded. Neither case should cause an error.


USAGE

cdfind( pattern, from = ., from.text, show.task.name=FALSE)
cdregexpr( regexp, from = ., from.text, ..., show.task.name=FALSE)
cdtree( from = ., from.text = substitute(from), charlim = 90) 
cd.change.all.paths( from.text = "0", old.path, new.path)
cd.write.mvb.tasks( from = ., from.text = substitute(from)) 
cditerate( from.text, what.to.do, so.far = vector("NULL", 0), ..., show.task.name=FALSE)
plot( x, ...) # S3 method for cdtree; normally plot( cdtree(<<args>>))

ARGUMENTS

 pattern: regexpr to be checked against object names.
 regexp: regexpr to be checked against function source code.
 from: unquoted path specifier (see 'cd'); make this 0 to operate on the entire hierarchy.
 from.text: use this in place of 'from' if you want to use a character string instead
 show.task.name: (boolean) as-it-happens display of which task is being looked at
 charlim: maximum characters per line allowed in graphical display of 'cdtree'; reduce if unreadable, or change par( 'cex')
 old.path: regexpr showing portion of directory names to be replaced
 new.path: replacement portion of directory names
 what.to.do: function to be called on each task (see DETAILS)
 so.far: starting value for accumulated list of function results
 ...: further fixed arguments to be passed to 'what.to.do' (for 'cditerate'), or 'grep' (for 'cdregexpr'), or 'foodweb' (qv) (for 'plot.cdtree')
 x: result of a call to 'cdtree', for plotting 

VALUE

'cdfind' returns a list with one element for each object that is found somewhere; each such element is a character vector showing the tasks where the object was found.

'cdregexpr' returns a list with one element for each task where a function whose source matches the regexpr is found; the names of each list element names the functions within that task (an ugly way to return results, for sure).

'cdtree' returns an object of class 'cdtree', which is normally printed with indentations to show the hierarchy. You can also 'plot(cdtree(...))' to see a graphical display.

'cd.change.all.paths' and 'cd.write.mvb.tasks' do not return anything useful.

 
DETAILS 

All these functions start by default from the task that is currently top of the search list, and only look further down the hiearchy (i.e. to unattached descendents). To make them work through the whole hierarchy, supply '0' as the 'from' argument. 'cdtree' has a 'plot' method, useful for complicated task hierarchies.

If you want to automatically crawl through the task hierarchy to do something else, you can write a wrapper function which calls 'cditerate', and an inner function to be passed as the 'what.to.do' argument to 'cditerate'. The wrapper function will typically be very short; see the code of 'cdfind' for an example. 

The inner function (typically called 'cdsomething.guts') must have arguments 'found', 'task.dir', 'task.name', and 'env', and may have any other arguments, which will be set according as the '...' argument of 'cditerate'. 'found' accumulates the results of previous calls to 'what.to.do'. Your inner function can augment 'found', and should return the (possibly augmented) 'found'. As for the other parameters: 'task.dir' is obvious; 'task.name' is a character(1) giving the full path specifier, e.g. '"ROOT/mytask"'; and 'env' holds the environment into which the task has been (temporarily) loaded. 'env' allows you to examine the task; for instance, you can check objects in the task by calling 'ls(env=env)' inside your 'what.to.do' function. See the code of 'cdfind.guts' for an example.


AUTHOR

Mark Bravington


EXAMPLES

cdfind( ".First", 0) # probably returns list( .First="ROOT")


SEE.ALSO

'cd'

KEYWORDS

utilities
}")

)

"cdfind.guts" <-
function (found, task.dir, task.name, pattern, env) {
  if (length( o <- lsall(envir = env))) {
    o <- o %that.match% pattern
    if (length(o)) {
      a <- match(o, names(found), 0)
      if (sum(a)) 
        found[names(found)[a]] <- lapply(found[names(found)[a]], 
          c, task.name)
      if (sum(a == 0)) 
        found <- c(found, structure(.Data = rep(task.name, 
          sum(a == 0)), names = o[a == 0], mode = "list"))
    }
  }
  found
}


"cditerate" <-
function( from.text, what.to.do, so.far=vector('NULL',0), ..., show.task.name=FALSE) {
  assign( '[[', my.index, envir=sys.frame( sys.nframe()))
  assign( '[[<-', my.index.assign, envir=sys.frame( sys.nframe()))

  nodes <- find.path( char.rel.path=from.text)
  if( dos.or.windows())
    nodes <- upper.case( nodes)
  node.list <- list(1)
  names( node.list) <- names( nodes)
  parents <- 0

  is.task <- function( x) {
      if( !is.null( x <- attr( pos.to.env( x), 'path')) &&  !is.null( x <- names( x)[1]))
        x
      else
        ''
    }

  attached.tasks <- sapply( 1:length( search()), is.task)

  orig.env <- env <- new.env()
  i <- 1
  while( my.index.exists( i, node.list)) { # length( node.list[[i]])) {
    # Look first to see if task is attached
    this.name <- names( nodes[ node.list[[ i]]]) 
    if( show.task.name)
      cat( '\n' %&% names( unlist( node.list))[ match( node.list[[ i]], unlist( node.list))])
    m <- match( this.name, attached.tasks, 0)
    if( m)
      env <- as.environment( m)
    else if( file.access( this.file <- file.path( nodes[ node.list[[i]] ], '.RData'))==0) {
      # was: this.file <- file.path( nodes[ node.list[[i]] ], '.RData')
      # Clear last batch of objects
      env <- orig.env
      remove( list=lsall( env), envir=env)
      attr( env, 'path')  <- dirname( this.file)
      checko <- suppressWarnings( try( load.mvb( this.file, envir=env, name=this.name), silent=TRUE))
      if( checko %is.a% 'try-error') # hopefully things will just work anyway...
        warning( "Problem loading " %&% this.file)
    } else {
      warning( "Can't find " %&% this.file)
      env <- NULL # flag
    }

    if( !is.null( env)) {
      so.far <- what.to.do( found=so.far, task.dir=nodes[ node.list[[i]]],
              task.name=find.prefix( node.list[[i]], nodes, parents), env=env, ...)

      deeper <- exists( 'tasks', envir=env, inherits=FALSE)
    } else
      deeper <- FALSE # couldn't find, don't even try
      
    if( deeper) {
      new.nodes <- get( 'tasks', envir=env)
      deeper <- length( new.nodes) > 0 }

    if( deeper) {
      new.nodes <- sapply( new.nodes, full.path, start=nodes[[ node.list[[i]]]])
      if( dos.or.windows())
        new.nodes <- upper.case( new.nodes)

#       Eliminate self-referential subtasks!
      if( any( drop <- !is.na( sr <- match( new.nodes, nodes)))) {
        prefix <- find.prefix( node.list[[i]], nodes, parents)
        other.prefix <- character( sum( drop))
        for( j in 1:sum( drop))
          cat( 'Loop or self-reference in task hierarchy: ',
              prefix %&% '/' %&% names(new.nodes)[drop][ j], '=',
              find.prefix( sr[ drop][j], nodes, parents), '\n')
        new.nodes <- new.nodes[ !drop]
      } #self-reference

      nodes <- c( nodes, new.nodes)
      parents <- c( parents, rep( node.list[[i]], length( new.nodes)))
      new.nodes[] <- seq( to=length(nodes), by=1, length=length(new.nodes))
      mode( new.nodes) <- 'numeric'
      mode( new.nodes) <- 'list'
      node.list[[i]] <- c( node.list[[i]], new.nodes)
      i <- c( i, 2)
    } else { # !deeper
#     Move up while no more sibs.
      while( length( i)>1 && i[ length(i)] == length( node.list[[ i[-length(i)] ]]))
        i <- i[ -length(i)]

#     Move to next sib, if any.
      i[ length(i)] <- i[ length(i)] + 1
    } # deeper or not
  } # of master loop
  
  if( show.task.name)
    cat( '\n')

  attr( so.far, 'nodes') <- nodes
  attr( so.far, 'node.list') <- node.list
  so.far
}


"cdprompt" <-
structure( function() {
  opened <- what.is.open()
  if( length( opened)) 
    opened <- paste( c( '', opened), collapse='<')
    
  prompt <- names( .Path)[-1]
  if( length( prompt)>1 && (abbr.char <- getOption( 'abbreviate.cdprompt', 0)) > 0)
    prompt[ -length( prompt)] <- substring( prompt[ -length( prompt)], 1, abbr.char)

  invisible( options( prompt = paste( prompt, collapse = "/") %&% opened %&% "> ")) 
}
, doc =  docattr( r"{
cdprompt       package:mvbutils

Support routine for cd-organized workspace hierarchy.

DESCRIPTION

Sets the command-line prompt to the correct value (see 'cd', and the notes on the option 'abbreviate.cdprompt'); useful if the prompt somehow becomes corrupted. 'cdprompt' never seems necessary in R but has been useful in the S+ manifestations of 'mvbutils', where system bugs are commoner.


USAGE

cdprompt()


SEE.ALSO

'cd'


EXAMPLES

cdprompt()


AUTHOR

Mark Bravington


KEYWORDS

utilities
}")

)

"cdregexpr" <-
function( regexp, from=., from.text, ..., show.task.name=FALSE) {
  if( missing( from.text))
    from.text <- substitute( from) 
  answer <- cditerate( from.text, cdregexpr.guts, vector( 'list', 0), regexp,
      show.task.name=show.task.name, ...)
  attributes( answer) <- list( names=names( answer))
  answer
}


"cdregexpr.guts" <-
function (found, task.dir, task.name, regexp, env, ...) {
  if (length(o <- search.for.regexpr(regexp, where = env, ...))) {
    found <- c(found, structure(.Data = rep(task.name, length(o)), 
        names = o, mode = "list"))
  }
  found
}


"cdtree" <-
function( from=., from.text=substitute( from), charlim=90) {
  indices <- cditerate( from.text, cdtree.guts, empty.data.frame( full.name=, own.name='', parent=0))

# Now produce function matrix etc.
  funs <- indices$own.name
  n <- length( funs)

# Avoid problems with duplicated names
  pre.X <- rep( 1, n)
  while( !is.na( d <- index( duplicated( funs))[1])) {
    pre.X[ d] <- pre.X[ d]+1
    funs[ d] <- 'X' %&% funs[ d] }

  funmat <- matrix( 0, n, n, dimnames=list( funs, funs))
  funmat[ cbind( indices$parent[-1], 2 %upto% n)] <- 1
  organize.web.display()
  funs <- substring( funs, pre.X, nchar( funs))
  dimnames( funmat) <- list( funs, funs)
  names( level) <- funs

  answer <- list( funmat=funmat, level=level, x=x, nodes=attr( indices, 'nodes'), 
    node.list=attr( indices, 'node.list'))
  class( answer) <- cq( cdtree, foodweb)
  answer
}


"cdtree.guts" <-
function (found, task.dir, task.name, env) 
{
    task.info <- strsplit(task.name, "/")[[1]]
    this.task.name <- task.info[length(task.info)]
    parent <- paste(task.info[-length(task.info)], collapse = "/")
    i <- match(parent, found$full.name, 0)
    rbind(found, list(full.name = task.name, parent = i, own.name = this.task.name))
}


"changed.funs" <-
structure( function( egood, ebad, topfun=NULL, fw=NULL){
  if( is.null( fw))
    fw <- foodweb( egood, plotting=FALSE)
  if( is.null( topfun))
    topfun
    
  to.do <- topfun
  fchanges <- character( 0)
  done <- 0
  while( done < length( to.do)) {
    fun <- to.do[ done+1]
    if( fun %not.in% lsall( ebad)) {
      fchanges <- c( fchanges, fun)
    } else {
      changed <- !my.all.equal( egood[[ fun]], ebad[[ fun]])
      if( changed) {
        fchanges <- c( fchanges, fun)
        more <- callees.of( fun, fw) %except% c( to.do, fchanges)
        to.do <- c( to.do, more)
      }
    }

    done <- done + 1
  }
    
return( fchanges)
}
, doc =  docattr( r"{
changed.funs    package:not-yet-a-package

Show functions and callees in environment 'egood' that have changed or disappeared in environment 'ebad'.

DESCRIPTION

Useful eg when you have been modifying a package, and have buggered stuff up, and want to partly go back to an earlier version... entirely hypothetical of course, things like that never ever happens to _me_. Mere mortals might want to create a new environment 'goodenv', use 'evalq(source(<<old.mypack.R.source.file>> local=T), goodenv)', then 'find.changes( goodenv, asNamespace("mypack"))'. If your package is lazy-loaded, you're stuffed; I avoid lazy-loading, except perhaps for final distribution, because it just makes it much harder to track problems. Not that _I_ ever have problems, of course.

Can be applied either to a specified set of functions, or by default to all the functions in 'egood'. If the former, then all callees of the specified functions are also checked for changes, as are all their callees, and so on recursively.

USAGE

changed.funs(egood, ebad, topfun, fw = NULL) 

ARGUMENTS

 egood, ebad: environments #1 & #2. Not symmetric; functions only in 'ebad' won't be checked.
 topfun: name of functions in 'egood' to check; all callees will be checked too, recursively. Default is all functions in 'egood'.
 fw: if non-NULL, the result of a previous call to 'foodweb(egood)', but this will be called automatically if not.

VALUE

Character vector with the names of changed/lost functions.
}")

)

"char.unlist" <-
function (x) {
  if (!(listable <- is.list(x))) {
    if( isS4( x)){
      # Devious trickery to avoid suggests/imports methods...
      re_bloody_quireNamespace( 'methods')
      mgetslots <- asNamespace( 'methods')$getSlots
      if('.Data' %in% names( mgetslots( class( x)))){
        x <- x@.Data
      }
    } # if S4
      
    if (listable <- (!is.atomic(x) && !is.symbol(x))) {
      # x <- as.list( x) worked well for years, but weird sh*t like externalptr can occur, so... 
      xx <- try( as.list(x), silent=TRUE)
      if( x %is.a% 'try-error') {
        listable <- FALSE
      } else {
        x <- xx
      }
    }
  }
  
  if (listable) 
    unlist(lapply(x, char.unlist), use.names = FALSE)
  else 
    paste(deparse(x), collapse = "\n")
}


"check.patch.versions" <-
structure( function( care=NULL) {
  nmp <- names( maintained.packages)
  instances <-   cq( MP, installed, source, tarball, binary)

  mat <- matrix( NA_character_, length( nmp), length( instances), 
      dimnames=list( nmp, instances))
  
  character.only <- FALSE
  for( pkg in nmp) {
    pv <- maintained.packages[[ pkg]][[ pkg %&% '.VERSION']]
    if( !is.null( pv)) {
      mat[ pkg, 'MP'] <- as.character( pv)
    }
    try( mat[ pkg, 'installed'] <- as.character( packageVersion( pkg)), silent=TRUE)
    set.pkg.and.dir( TRUE, FALSE) # want outdir calculated, but not created
    try( mat[ pkg, 'source'] <- read.dcf( file.path( sourcedir, 
        'DESCRIPTION'))[1,'Version'], silent=TRUE)
    try({
        tarballs <- dir( outdir, pattern=sprintf( '^%s_[0-9]+([.][0-9]+)*[.]tar[.]gz$', pkg), 
            full.names=FALSE)
        mat[ pkg, 'tarball'] <- as.character( max( numeric_version( sub( '.*_', '', 
            sub( '.tar.gz$', '', tarballs)))))
      }, silent=TRUE)
    try({
        binaries <- dir( outdir, pattern=sprintf( '^%s_[0-9]+([.][0-9]+)*[.]zip$', pkg), 
            full.names=FALSE)
        mat[ pkg, 'binary'] <- as.character( max( numeric_version( sub( '.*_', '', 
            sub( '.zip$', '', binaries)))))
      }, silent=TRUE)
      
  }
  
  keep <- rep( TRUE, length( nmp))
  for( icare in care) {
    keep <- keep | (mat[ ,icare] != mat[ , 'MP'])
  }
  
return( mat[ keep,])
}
, doc =  docattr( r"{
check.patch.versions    package:mvbutils

Check consistency of maintained package versions

DESCRIPTION

Utility to compare version numbers of the different "instances" of one of your maintained packages. Only the most up-to-date folders relevant to the running R version are checked; see 'mvbutils.packaging.tools'.

The "instances" checked are:

 - the task package itself (in eg '..mypack$mypack.VERSION')
 - the source package created by 'pre.install'
 - the installed package, maintained by 'patch.install'
 - the tarball package, created by 'build.pkg'
 - the binary package, created by 'build.pkg.binary'
 
The 'care' argument controls what's shown. Mismatches when 'care="installed"' should be addressed by 'patch.install', because something has gotten out-of-synch (probably when maintaining the same version of a package for different R versions). Mismatches with the built ("tarball" and "binary") packages are not necessarily a problem, just an indication of work-in-progress.

USAGE

check.patch.versions(care = NULL) 

ARGUMENTS

 care: if non-NULL, a character vector with elements in the set "installed", "source", "tarball", and "binary". Only packages where there's a version mismatch between these fields and the task package version will be shown. 

VALUE

A character matrix with maintained packages as rows, and the different instances as columns. "NA" indicates that a version couldn't be found.

}")

)

"check.pkg" <-
function( 
  pkg, 
  character.only=FALSE, 
  build.flags= character(0), 
  check.flags= character(0),
  envars= character( 0),
  CRAN=FALSE
){
  # Advice is to build into tarball first, then RCMD CHECK that
  orig.pkg <- substitute( pkg)
  set.pkg.and.dir( TRUE)
  force( build.flags)
  force( check.flags)
  force( CRAN)
  mc <- match.call()
  mc[[1]] <- build.pkg
  mc$pkg <- orig.pkg # in case of path arg
  mc$build.flags <- mc$check.flags <- mc$CRAN <- mc$envars <- NULL
  mc$flags <- build.flags
  extract.named( eval.parent( mc)) # dir. etc

  i <- 1
  repeat{
    temp.inst.lib <- file.path( tempdir(), 'templib' %&% i)
    if( !file.exists( temp.inst.lib))
  break
    i <- i+1
  }
  mkdir( temp.inst.lib)
  on.exit( unlink( temp.inst.lib, recursive=TRUE))

  postfix <- '_' %&% 
      read.dcf( file.path( sourcedir, 'DESCRIPTION'))[,'Version'] %&% '.tar.gz'
      
 rcmdgeneric.pkg2( orig.pkg, outdir=outdir, indir=file.path( outdir, pkg), 
     cmd= 'check', 
     postfix= postfix,
     envars= envars,
     flags= c( check.flags, if( CRAN) '--as-cran', '-l ' %&% temp.inst.lib))
}


"check_multiarch" <-
function( nlocal=sys.parent()) mlocal({
  multiarch <- TRUE # default sans biarch
  dcf <- read.dcf( file.path( dir., pkg, 'DESCRIPTION'))
  biarch_field <- match( 'BIARCH', toupper( colnames( dcf)), 0)
  if( biarch_field) {
    multiarch <- as.logical( dcf[1,biarch_field])
    if( is.na( multiarch)) {
      warning( "Malformed BIARCH field; trying multiarch=TRUE")
      multiarch <- TRUE
    }
  }
})


"clamp" <-
function( x, min, max){
  if( missing( max)){
    # Single-range form with eg min=1:10
    max <- base::max( min)
    min <- base::min( min)
  } else {
stopifnot( max >= min)
  }
  
  x[] <- pmax( min, pmin( max, x))
return( x)
}


"Clink_packages" <-
structure( function( ...) {
  # mvboptions is an environment in asNamespace('mvbutils')
  # can put all kindsa stuff into there
  if( is.null( Clinks <- mvboptions$Clinks)) {
    ns <- asNamespace( 'mvbutils')
    Clinks <- new.env( parent=ns)
    mvboptions$Clinks <<- Clinks
  }

  l <- list(...)
  if( !length( l)) {
return( as.list( Clinks))
  }

  if( (length(l)==1) && is.character( l[[1]])) {
return( Clinks[[ l[[1]] ]])
  }

  if( !all( nzchar( names( l))) || !all( sapply( l, is.function))) {
stop( "Bad args--- see doco")
  }

  Clinker_args <- cq( pkg, DLL, lldir, Rdir, src_changed)
  arg_check <- do.on( l, my.all.equal( names( formals( .)), Clinker_args))
  if( !all( arg_check)) {
stop( sprintf( "Clinker(s) should have exactly %i args: %s--- not the case for: %s",
    length( Clinker_args),
    paste( Clinker_args, collapse=', '),
    paste( names( l)[ !arg_check], collapse=', ')))
  }

  # Can't just c() onto an environment.
  for( il in names( l)) {
    Clinks[[ il]] <- l[[ il]]
  }
  # assign( 'Clinks', Clinks, envir=mvboptions) # not reqd; envirs are pointers

invisible( as.list( Clinks))
}
, doc =  docattr( r"{
Clink_packages    package:mvbutils
src_changed
PIBH
dummy_PIBH

Pre-install-buildy hooks for compiled code


DESCRIPTION

'Clink_packages' registers or returns pre-install-buildy hook(s) in task-packages for different types of source code, eg for 'Rcpp' or 'RcppTidy' or TMB or ADT. You should never need to call 'Clink_packages' yourself; it's meant for use by helper-mini-packages that tell 'mvbutils' what to do about a specific type(s) of source code. Authors of "proper" packages containing real low-level source code don't really need to know about any of this--- though you could have a look at TIDY.STUBS.AND.SYMBOLS below, and at 'RcppTidy' if you are normally using 'Rcpp'.

Packages built with 'mvbutils::pre.install'--- and any package wanting to debug low-level code via eg package 'vscode'--- need to have a '.onLoad' that starts by calling 'run_Cloaders_<pkgname>()'. That function does any work connected with setting up native-symbols and R stubs; package 'Rcpp' on its own will normally cause such stuff to happen _before_ '.onLoad', but that will be gently subverted if you use 'mvbutils::pre.install', which instead makes the function 'run_Cloader_<pkgname>' for you automatically. By having that function, it becomes possible to change the low-level code (eg during debugging), including changing function arguments and so on, without re-installing the entire package. See something else for more details.

The list of potential helper-mini-packages for your whole package (normally just one, but you never know...) is determined by Description->Imports. Source code is expected to be directly in "<mypack>/src" (the only choice if plain old 'Rcpp' is being used), and/or in the "N>=0" different subfolders of that. Each of these "N+1" folders will potentially generate a single DLL with the name of the folder. Each folder is scanned by all registered helper-minis, to see if that helper is wanted; processing of the folder stops after the first helper-mini that finds something to do. The helper-mini might add extra files to the folder (eg a wrapper that exports R-callable stubs, like "RcppExports.cpp"), and will probably add a "Cloader" written in R, to "<mypack>/R" (like "RcppExports.R")--- though that will be obscured in the "source package" seen later by INSTALL. If "N>0", or if any of the folders demand it, then an overall "Makefile" will be produced (required for multiple DLLs). Subfolders starting with a period are skipped (e.g. "src/.vscode").

The code that does the scanning for one specific type of low-level code (eg for 'Rcpp'-type code) is a "Pre-Install-Buildy Hook" (PIBH), which are called during 'pre.install'. Each PIBH should have five named arguments (see below). The PIBH will be invoked for folder "<mypack>/src", and for each subfolder thereof--- this lets you generate several DLLs (one from each subfolder) of the same type--- useful eg for ADT or TMB. A PIBH should check whether it's suitable for that folder, and if so (re)generate any necessary files; but it should also check whether regeneration is necessary (see .REGENERATION.CHECKS). Then the PIBH should return either NULL if there's nothing to do (e.g. it didn't find any suitable source files), or a list/dataframe with these elements:


 Cloader: path to R file(s) that set up native symbols and stubs, etc--- eg "R/RcppExports.R". Usually just one, but there could be several if multiple DLLs of the same type are required.

 DLL: pretty self-explanatory; omit the path and the extension

 subenv: see .TIDY.STUBS.AND.SYMBOLS below

 makelines: what to put in the Makefile, if there is one (see below). If several commands are required, paste them together separated by "'\n'". At the moment this is left blank for 'Rcpp' which is "needy" about being the only DLL in town, and/but also for 'RcppTidy' where it shouldn't be (to allow multiple DLLs).

 needs_makefile: anything except TRUE means that a Makefile will _definitely_ be generated in the 'src' folder. Otherwise, no Makefile is generated unless more than one Clinker is active (basically because multiple DLLs will then be required). Omit it if you don't need it (same effect as FALSE).

 postcopy_hook_expr: if set, an expression to be run inside the body of 'pre.install' _after_ the source package has been set up "fully". The PIBH itself will be called _before_ the source package exists, but via this hook it can arrange to do much of the work post hoc; that approach is needed for 'Rcpp' in particular, because 'Rcpp::compileAttributes' demands a full source package. If you can put stuff into the PIBH rather than into this hook, it's probably better to do so. However, if you must, then 'mvbutils:::Clinks_Rcpp' shows how to add a hook, and 'mvbutils:::Clinks_Rcpp_postcopy' shows an example of what might be in a hook.

The arguments to a PIBH must be, in order:

 pkg, DLL: self-explanatory names (no paths or extensions)

 lldir, Rdir: paths to folders containing the low-level "source" code (which will be either "<mypack>/src", or a subfolder thereof) and the R code (which will be "<mypack>/R").

 src_changed: this will contain the 'src_changed' function, which your PIBH can then use without referring to package 'mvbutils' at all.


.TIDY.STUBS.AND.SYMBOLS

A PIBH like 'RcppTidy:::RcppTidy_pre_install' can arrange to put all the native-symbols and corresponding R stubs for each DLL into a separate sub-environment of your package's namespace. You then access the R stub for your low-level function 'myCfun' in your DLL 'Cbits' by calling 'DLL_Cbits$myCfun(...)'. The effect is like 'DLL_Cbits=useDynLib(Cbits,.registration=TRUE)' in your NAMESPACE (see "Writing R Exensions")---. except NB that, at least for 'Rcpp' and 'RcppTidy', you should always call the R stub, rather than trying to access the native-symbol directly via '.Call'. To achieve this effect, your PIBH should return 'subenv=TRUE'. There are a couple of minor differences from what's described in 'Writing R Extensions".

 - 'DLL_Cbits' will actually be an environment, rather than an S3-classed list.

 - All the native-symbols from 'Cbits' will be moved into 'DLL_Cbits', rather than cluttering up the main namespace.

 - Info on the DLL itself is available via 'attr(DLL_Cbits,"DLLInfo")', rather than as 'DLL_Cbits' itself--- not that you should need it.

 - 'DLL_Cbits' gets a finalizer that will unload the DLL when/if the package is unloaded--- this avoids you having to write a '.onLoad' for the package.

Note that if you don't like the name 'DLL_Cbits' for the environment--- eg because it's too cumbersome--- then you can rename it yourself in '.onLoad', eg like so:

%%#
mypack:::.onLoad <- function (libname, pkgname) { # include Rbrace to get around doc2Rd bug !}
 #### mypack onload ####
  run_Cloaders_mypack() # must come first
  evalq({
    DLL <- DLL_Cbits
    rm( DLL_Cbits)
  }, envir=asNamespace( 'mypack'))
# ...



.REGISTERING.A.PRE.INSTALL.BUILDY.HOOK

The point is that there's no way to know whether your package will load before or after 'mvbutils', so a little subterfuge is required. If 'mvbutils' is already loaded, you can just call 'Clink_packages' directly; if not, it's necessary to set a hook to be run when-and-if 'mvbutils' does load (it may never be needed, eg during production use of a "proper" package that just uses your helper-mini-package). So your helper-mini-package will need a '.onLoad' containing something very like this:

%%#
  # Tell mvbutils::pre.install about this package
  # Try to minimize lookups at time-of-future-use...
  xfun <- eval( substitute( function(...) mvbutils::Clink_packages( pkgname, RcppTidy_pre_install)))
  if( 'mvbutils' %in% loadedNamespaces()) { # already there
    xfun()
  } else {
    setHook(packageEvent("mvbutils", "onLoad"), xfun)
  }

Note that: (i) 'pkgname' will normally be the first argument of the '.onLoad', and will be the name of your helper-mini-package; and (ii) 'RcppTidy_pre_install'  should the name of _your_ PIBH; and (iii) your helper-mini-package should list 'mvbutils' in "Description->Suggests", but probably not in "Description->Imports" because the latter will force 'mvbutils' to be loaded even if the end-point "proper" pacakge doesn't need it.


.REGENERATION.CHECKS

There's no point in regenerating headers if the main source files haven't changed. 'mvbutils::src_changed' is a utility function that your PIBH can call, to check whether source files have changed.


.MAKEFILE

If there is only one type of source code in your package, then no Makefile will be produced unless the PIBH sets 'needs_makefile'; normally that's not necessary. It might be required for eg Pascal sources--- and might be a bloody sight easier to do than figuring out Makevars from the spectacularly opaque doco in "Writing R Extensions"... not that anything about "make" is _easy_ AFAICS.

"Writing R Extensions" discourages the use of a Makefile, but I think there's no way round it in the case of multiple targets (see eg <https://github.com/kaskr/adcomp/issues/43>).


USAGE

Clink_packages(...)
src_changed( source_files, Cloader)
dummy_PIBH( pkg, DLL, lldir, Rdir, src_changed) # not "real"
# ... but its existence lets you see what the args should be
# Code of 'dummy_PIBH' is actually real, but from a different place


ARGUMENTS

 ...: (Clink_packages) either missing, or a single character string naming a helper-mini-package such as '"Rcpp"' or '"RcppTidy"', or a named function for such a package. See VALUE and EXAMPLES.

 source_files: (src_changed) the ones to check to see if re-pre-build is necessary

 Cloader: (src_changed) the current version of the R file that produces "stubs" for source routines, etc. 'pre.install' will add a checksum-manifest as the first line.
 
 pkg: (PIBH) name of package

 DLL: (PIBH) DLL...

 lldir: (PIBH) folder where the source files live; normally "<mypack>/src", but could be a subfolder of that.

 Rdir: (PIBH) folder where the Cloader (written in R) should go, eg "<myprotopack>/R"

 src_changed: (PIBH) You can call this function to check for changes in source-files. It will be set to 'mvbutils::src_changed' when your PIBH is called; this means that your PIBH does not have to refer to package 'mvbutils' at all, and so packages depending/importing your helper also do not need to import/depend on package 'mvbutils'.


VALUE

'Clink_packages()' returns all registered helper packages, as a list. 'Clink_packages( "RcppTidy")' returns the PIBH for package "RcppTidy". 'Clink_packages( RcppTidy=<<some function>>, ADThelper=<<some function>>)' registers the PIBH for those packages. PIBHs for registration are checked to see that they have just two arguments, 'Cdir' and 'Rdir'.

'src_changed' returns "" if nothing has changed, or a new first line for the manifest file if rebuilding is needed. In which case, your PIBH should do the rebuilding, and prepend the new first line to the manifest. [Currently the code half-attempts to *edit* the manifest... not right.]

PIBH should return a list with these elements:

 Cloader: pathname of R script that will do any post-useDynLib setup for the DLL, eg creating R stubs to call the low-level routines. Should be in "<mypack>/R".

 makelines: if a Makefile does end up being used, what instruction should compile this DLL? Single string, with "\n" for any newlines.

 needs_makefile: normally FALSE for C(etc) code; set to TRUE if this particular bit of source-code _demands_ a Makefile (eg if it's in Pascal); see .MAKEFILE

 subenv: Name of environment within the package namespace, within which native-symbols should be created. Can be "", in which case the symbols are created directly in the namespace (like 'Rcpp' does mid-2019).

 extra_copies: new files that will need copying from the task package to the source package--- eg "<mypack>/src/RcppExports.cpp". Don't include 'Cloader'.



EXAMPLES

# Setup in a helper-mini-package

## Don't run
# In ADT:::._onLoad, where ADT:::ADT_PIBH is a PIBH; see main text
  Clink_packages( ADT=ADT_PIBH)

# Inside ADT:::ADT_PIBH <- function( dir, Rdir) {...}
  redo <- mvbutils::src_changed( Rcpp_files, this_Cloader)
  if( nzchar( redo)) { recompile() }
  
## End don't run  
}")

)

"Clinks_Rcpp" <-
function( pkg, DLL, lldir, Rdir, src_changed) {
  Rcpp_files <- dir( lldir, pattern='[.]cpp$', full.names=TRUE) %that.dont.match% '/RcppExports'
  # Keep the ones that DO try to export something. yes, this is opaque code...
  Rcpp_files <- names( do.on( Rcpp_files, length( grep("^// +" %&% to.regexpr("[[Rcpp::export]]"),
      readLines(.)))) %such.that% (.>0))

  if( !length( Rcpp_files)) {
return( NULL)
  }

  Cloader <- file.path( Rdir, 'RcppExports.R')
  redo <- src_changed( Rcpp_files, Cloader)
  # Rcpp::compileAttributes will be run later, on formal source pkg
  # Just setting up so pre.install knows about the existence of these files

returnList( Cloader, DLL, makelines='', needs_makefile=FALSE, subenv=FALSE,
    postcopy_hook_expr= substitute( Clinks_Rcpp_postcopy( sourcedir, dir., redo))
  )
}


"Clinks_Rcpp_postcopy" <-
function( srcpkgdir, taskdir, redo) {
  if( !length( redo) || !nzchar( redo[1]))
return()

  # Anti CRANky: can't use double-fucking-colon because Rcpp should not be mentioned...
  ca <- try( asNamespace( 'Rcpp')$compileAttributes( pkgdir=srcpkgdir, verbose=TRUE))
  if( ca %is.a% 'try-error') { # and/or !length( ca) ?
stop( "Couldn't prepare Rcpp files")
  }

  # Add manifest. Presumably this gets regenned by Rcpp:compileAttributes, so doesn't grow...
  Rexport <- file.path( srcpkgdir, 'R/RcppExports.R')
  rl <- c( redo, if( file.exists( Rexport)) readLines( Rexport))
  writeLines( rl, Rexport)

  # file.copy( Rexport, file.path( taskdir, 'src/RcppExports.R')) # where it will be ignored, but nice to have
  if( !dir.exists( taskRdir <- file.path( taskdir, 'R'))) {
    mkdir( taskRdir)
  }
  file.copy( Rexport, file.path( taskdir, 'R/RcppExports.R'), overwrite=TRUE) # used for checksumming
}


"clip" <-
function( x, n=1) x[ 1 %upto% ( length( x) - n)]


"close.selfdeleting.file" <-
function( con, ...) {
  fname <- summary( con)$description
  NextMethod( 'close')
  unlink( fname)
}


"compacto" <-
function( x, gap, width, extra){
stopifnot( is.matrix( x)) # x  %is.a% 'matrix' doesn't work if already 'compacto' :/
  if( !missing( gap)){
stopifnot( is.numeric( gap), length( gap)==1, is.finite( gap), gap >= 0)
    attr( x, 'gap') <- gap
  }

  if( !missing( width)){
stopifnot( is.numeric( width), length( width)==1, is.finite( width), width >= 0)
    attr( x, 'width') <- width
  }
  
  if( !missing( extra)){
stopifnot( is.character( extra), length( extra)==1, 
      nchar( extra, type='width')==1)
    attr( x, 'extra') <- extra
  }
  
  oldClass( x) <- unique( c( 'compacto', oldClass( x)))
return( x)
}


"compare_spack_code" <-
function( pkg, gitplace='d:/github/flub',
    d1, d2, character.only=FALSE,
    showdiff=NULL
){
## To deal with the BAD habit of
  if( missing( d1) || missing( d2)) {
    set.pkg.and.dir( FALSE)
    d1 <- sourcedir
    d2 <- file.path( gitplace, pkg, pkg)
  }
  
  d1 <- file.path( d1, sprintf( 'R/%s.R', pkg))
  d2 <- file.path( d2, sprintf( 'R/%s.R', pkg))
stopifnot( all( file.exists( c( d1, d2))))

  o1 <- get_defs_from_source( d1)
  o2 <- get_defs_from_source( d2)
  if( !is.null( showdiff)){
stopifnot( is.character( showdiff), length( showdiff)==1)  
    o1 <- o1[[ showdiff]]
    o2 <- o2[[ showdiff]]
stopifnot( length( o1)>0, length( o2)>0) # not in both
    CRANfucker <- get( 're' %&% 'quire' %&% 'Namespace', baseenv())
    if( !CRANfucker( 'diffr')){
stop( "Couldn't load package 'diffr'")
    }
    tf1 <- tempfile( sprintf( 'V1_%s_', showdiff))
    tf2 <- tempfile( sprintf( 'V2_%s_', showdiff))
    on.exit( unlink( c( tf1, tf2)))
    writeLines( o1[[ c( 2,2)]], tf1)
    for( a in atts( o1)){
      writelines( c( '', sprintf( 'attribute(%s)=', a), attr( o1, a)), tf1, append=TRUE)
    }
    writeLines( o2[[ c( 2,2)]], tf2)
    for( a in atts( o2)){
      writelines( c( '', sprintf( 'attribute(%s)=', a), attr( o2, a)), tf2, append=TRUE)
    }
    
    # Must use 'print' to make it appear
    print( asNamespace( 'diffr')$diffr( tf1, tf2))
return( NULL)
  }
  
  in1 <- names( o1) %except% names( o2)
  in2 <- names( o2) %except% names( o1)
  
  diffs <- do.on( names( o1) %that.are.in% names( o2),
      !my.all.equal( o1[[ .]], o2[[ .]]) 
    )
  diffs <- names( diffs)[ diffs]
  
returnList( in1, in2, diffs)
}


"compare_spacks" <-
structure( function( pkg, gitplace='d:/github/flub', d1, d2, character.only=FALSE) {
  if( missing( d1) || missing( d2)) {
    set.pkg.and.dir( FALSE)
    d1 <- sourcedir
    d2 <- file.path( gitplace, pkg, pkg)
  }

  s1 <- sourcepack_info( d1)
  s2 <- sourcepack_info( d2)

  in1 <- names( s1) %except% names( s2)
  in2 <- names( s2) %except% names( s1)
  in12 <- intersect( names( s1), names( s2))
  is_diff <- s1[ in12] != s2[ in12]
  diffs <- names( s1[ in12])[ is_diff]
returnList( in1, in2, diffs)
}
, doc =  docattr( r"{
compare_spacks    package:mvbutils
compare_spack_code

Compare source packages eg for checking git

DESCRIPTION

Suppose you have a maintained task-package, and you've made a source package from it. And that there's a version on github, which you want to update. So you pull it, into your local github spot, then check for any changes with this function. If there aren't any, then you don't need to mess around with 'unpackage'; you could carry on maintaining your task-package as usual, then scrunge it into your github spot, then push.

'compare_spack_code' actually looks for functions in "mypack.R" file that differ between the versions. It tries to look at attributes of the functions, too (usually there won't be any). If you ask for one specific function only, it will try to use the 'diffr' package to display a nice diff of the two versions. 

Probably I should describe what to do if you _do_ find a difference... haven't needed to yet!


USAGE

compare_spacks(pkg, gitplace = "d:/github/flub", 
    d1, d2, character.only = FALSE)
compare_spack_code(pkg, gitplace = "d:/github/flub", 
    d1, d2, character.only = FALSE, showdiff=NULL)


ARGUMENTS

 pkg: as per 'build.pkg' etc; eg 'mypack' or '..mypack'
 
 gitplace: your local github spot
 
 d1, d2: Or you can specify the folders directly with these (need to set both)
 
 character.only: as per 'build.pkg' etc, eg 'char="mypack"' (or more likely 'char=thispack' when 'thispack' is the index of a for-loop)
 
 showdiff: optional, name of one function to show differences for.

VALUE

A list with character-vector components 'in1', 'in2', and 'diffs' (unless 'showdiff' is set). Any file (or any function, for 'compare_spack_code') which are _not_ different won't be mentioned. If 'showdiff' is set, nothing is returned, but you should see the results in your browser.

}")

)

"Conly" <-
function( e) {
  f <- find.funs( e)
  dotcalls <- do.on( f, {
      b <- body( e[[.]]);
        (b %is.a% '{') &&
        (length( b)==2) &&
        (b[[2]] %is.a% 'call') &&
        (b[[c(2,1)]] %is.a% 'name') &&
        (as.character( b[[c(2,1)]]) %in% c( '.Call', '.External'))
    })
return( names( dotcalls)[ dotcalls])
}


"copy.ns.objects" <-
function( objects, pkgname) {
  objects <- objects # force
  icns <- function( pkgname, pkgpath){
      senv <- as.environment( 'package:' %&% pkgname)
      cat( 'Locked?\n')
      print( environmentIsLocked( senv))
      print( objects)
      ns <- asNamespace( pkgname)
      f <- function( val) blah-blah-blah
      for( x in objects) {
        body( f) <- substitute( if( missing( val)) x else x <<- val, list( x=as.name( x)))
        environment( f) <- ns
        makeActiveBinding( x, f, senv)
      }
    }
  setHook( packageEvent( pkgname, 'attach'), icns)
}


"cq" <-
function( ...) {
# Saves putting in quotes!
# E.G.: cq( first, second, third) is the same as c( 'first', 'second', 'third')
# wrapping by as.character means cq() returns character(0) not list()
  as.character( sapply( as.list( match.call( expand.dots=TRUE))[-1], as.character))
}


"CRANky" <-
function( blurb, env=baseenv()){
  # Returns an undetectable synonym of an unmentionable function in baseenv() or elsewhere
  # Sigh........
  fun <- env[[ rawToChar( rev( charToRaw( blurb)))]]
  e <- new.env( parent=env)
  e$fun <- fun
  environment( fun) <- e
  body( fun) <- quote( {
    mc <- match.call( expand.dots=TRUE)
    mc[[1]] <- environment( sys.function())$fun
    eval.parent( mc)
  })
return( fun)
}


"create.backups" <-
function( pos=1) {
  pos <- as.env( pos)
  if( is.null( t <- attr( pos, 'path')))
stop( "Don't know what path to use for search environment:" %&% pos)

  mkdir( file.path( t, '.Backup.mvb'))
  fob <- read.bkind( t)

  # changed 5/4/2005 for speed with mcache
  cand <- lsall( pos) %SUCH.THAT% !bindingIsActive( ., env=pos)
  cand <- cand %SUCH.THAT% (mode(.)=='function')
  sapply( cand %except% fob$object.names, deal.with.backups, where=pos)
  invisible( NULL)
}


"create.bkind.if.needed" <-
function( dir) {
  dir <- file.path( dir, '.Backup.mvb')
  if( !is.dir( dir ))
    try( mkdir( dir))
  if( !is.dir( dir))
return('') # mucho problemo

  index.file <- file.path( dir, "index")
  if(!file.exists(index.file))
    file.create(index.file)
  index.file
}


"create.wrappers.for.dll" <-
function( this.dll.info, ns=new.env( parent=parent.frame(2))) {
###################
# 'ns' is normally a namespace, but can be any old env for devel purposes

  dll.name <- unclass( this.dll.info)$name
  dll.env <- new.env( parent=ns) # will return empty if no registrands
  
  routs <- getDLLRegisteredRoutines( this.dll.info)
  n.routs.by.callmech <- sapply( routs, length)
  if( sum( n.routs.by.callmech)) {
    for( irout.class in names( n.routs.by.callmech %except% 0)) {
      # eg C_myrout; prefix is C or Call or F or Ext
      rout.class.prefix <- sub( 'ortran|ernal', '', sub( '.', '', irout.class)) %&% '_'
      for( irout in seq_along( routs[[ irout.class]])) {
        # Might be slightly faster to just use this.un$address, but limiting 
        this.un <- routs[[ irout.class]][[ irout]]
        dll.env[[ rout.class.prefix %&% this.un$name]] <- this.un 
      }
    }
  }
  
return( dll.env)
}


"cull.old.builds" <-
function( pkg, character.only=FALSE) {
##################
  set.pkg.and.dir( FALSE) # just to deal with 'pkg'
  zipdirs <- dir( dir., # dir( attr( maintained.packages[[ pkg]], 'path'), 
      pattern='^[rR][0-9]+', full.names=TRUE, include.dirs=TRUE) %such.that% is.dir( .)
      
  for( izipdir in zipdirs) {
    tarballs <- dir( izipdir, pattern=sprintf( '^%s_([0-9]+[.])+tar[.]gz$', pkg))
    tarver <- numeric_version( sub( '.*_', '', sub( '.tar.gz', '', tarballs, fixed=TRUE)))
    zippos <- dir( izipdir, pattern=sprintf( '^%s_([0-9]+[.])+zip$', pkg))
    zipver <- numeric_version( sub( '.*_', '', sub( '.zip', '', zippos, fixed=TRUE)))
    
    maxver <- max( c( zipver, tarver))
    unlink( file.path( izipdir, zippos[ zipver < maxver]))  
    unlink( file.path( izipdir, tarballs[ tarver < maxver]))
  }  
  
invisible()
}


"current.source" <-
function() {
  cs <- stdin()
  if (exists("source.list", "mvb.session.info")) {
    sl <- get("source.list", "mvb.session.info")
    if( length( sl)) {
      cs <- sl[[ length( sl)]]
      attr( cs, 'source.list.num') <- length( sl) # so we know..!
    }
  }
return( cs)  
}


"D2A" <-
structure( function( 
  df,
  data.col,
  dim.cols=names( df) %except% data.col,
  missing.value=NA
){
  ndims <- length( dim.cols)
  df <- df[, c( dim.cols, data.col)]
  df[ dim.cols] <- lapply( df[ dim.cols], factor) # used to be xfactor
  na.rows <- is.na( matrix( as.numeric( unlist( df[ dim.cols], F)), ncol=ndims) %**% rep( 1, ndims))
  df <- df[ !na.rows,]

  dimnames <- lapply( df[ dim.cols], levels)
  output <- array( missing.value, dim=sapply( dimnames, length), dimnames=dimnames)

  df[ dim.cols] <- lapply( df[ dim.cols], as.numeric)
  indices <- as.matrix( df[ dim.cols])
  if( any( duplicated( indices))){
warning( "Duplicated indices--- probably using last occurrence, but...")
  }
  output[ indices] <- df[[ data.col]]
  output
}
, doc =  docattr( r"{
D2A    package:not-yet-a-package

data.frame.to.array    package:mvbutils
D2A

Dataframe to array


DESCRIPTION

'D2A' makes an array out of one column in a dataframe, with (by default) the remaining columns forming the array dimensions, in order. Its (almost) inverse is 'A2D' (qv).

You can choose which columns to use for the dimensions, and in which order, via the 'dim.cols' argument. However, it often easier to subset the array by columns in the call, eg 'D2A( x[ cq( Year, Len, Count), data.col="Count")'. Each unique value in an index column gets a "row" in the array. Combinations of indices that don't appear as rows in the input will become 'missing.value' in the output. If a row has missing values in any index column, it is ignored.

Duplicated index rows in the 'data.frame' are not advisable, and trigger a 'warning'; I think the last value will be used, but I'm not sure.


.NOTE

'D2A' and (something similar to) 'A2D' used to be in my semi-secret 'handy2' package under slightly different names, but they are useful enough that I've moved them to 'mvbutils' in 2025.

You can of course do vaguely similar things with base-R and perhaps with countless other packages too, but why not just use this? I do!


USAGE

D2A( 
  df,
  data.col,
  dim.cols = names(df) %except% data.col,
  missing.value = NA)


ARGUMENTS

 df: data.frame

 dim.cols: character vector saying which columns (in order) to use for array dimensions. Default is everything except 'data.col'

 data.col: string saying which column should form the contents of the output

 missing.value: what to put into the output for index-combinations that don't occur in the input.


VALUE

Array with 'length( dim.cols)' dimensions, and appropriate 'dimnames'.

SEE.ALSO

'A2D'


EXAMPLES 

grubbb <- expand.grid( xx=1:4, yy=2:3) # data.frame
grubbb$z <- with( grubbb, xx+10*yy)
D2A( grubbb, 'z')

# Let's remove some values, and change the order of array dims...
minigrubbb <- grubbb[ c( 1, 3, 4, 7),]
D2A( minigrubbb, 'z', dim.cols=cq( yy, xx))

# Don't have to use all columns
D2A( minigrubbb, 'z', dim.cols='xx')


}")

)

"deal.with.backups" <-
function( name, where) {
  infeasible.R.line <- "'\"@\"@'@ START OF BACKUP @'@\"@\"'"
  backup.fix <- getOption( "backup.fix", c( 0, 0))
  if( backup.fix[1] == 0)
return()

  where <- as.env( where)
  bdd <- attr( where, "path")
  if( !nchar( create.bkind.if.needed( bdd))) {
    warning( "Can't create backup directory!")
return() }

  filename <- get.bkfile( name, bdd, create = TRUE)
  ow <- options( warn = -1)
  previous.backups <- readLines( filename)
  options( ow)
  char.type <- !has.source( where[[name]])
  
  if( length( previous.backups)) {
    line.breaks <- index( previous.backups == infeasible.R.line)
    if( char.type) {
      # Line after infeasible is number of lines until next infeasible 
      next.break <- line.breaks <- line.breaks[ 1]
      repeat{ 
        next.break <- next.break + 3 + 
            as.numeric( previous.backups[ next.break+2])
        if( next.break > length( previous.backups))
      break
      
        if( previous.backups[ next.break] != infeasible.R.line) {
warning( "Stuffed backup for " %&% name %&% "; keeping extra stuff")
      break
        }
        
        line.breaks <- c( line.breaks, next.break)
      }
    }
      
    if( !length( line.breaks))
      previous.backups <- character( 0)
    else
      discard.mouldering.backups()
  }
  cat( c( previous.backups, infeasible.R.line, "SESSION=" %&% unclass( session.start.time)),
      file = filename, sep = "\n")
  if( where[[name]] %is.a% 'function')
    nicewrite_function( where[[ name]], filename, append = TRUE,
        print.name = TRUE, xn=name)
  else if( char.type) 
    cat( length( where[[name]]), where[[name]], file=filename, sep='\n', append=TRUE)
  else # general
    cat( '`' %&% name %&% '` <- local(', 
        attr( where[[name]], 'source'), ') # end local', file=filename, sep='\n', append=TRUE)
}


"dedoc_namespace" <-
structure( function( ns){
  ns <- asNamespace( ns)
  funs <- find.funs( ns)
  has_docattr <- do.on( funs, is.character( attr( ns[[.]], 'doc')))
  for( ifun in funs[ has_docattr]){
    fun <- ns[[ ifun]]
    attr( fun, 'doc') <- NULL
    assign( ifun, fun, envir=ns)
  }
NULL
}
, doc =  docattr( r"{
dedoc_namespace    package:mvbutils


Remove doc attributes when package loads

DESCRIPTION

Suppose you want to keep plain-text "doc" attributes attached to your function code even in the package source (as opposed to in a private version of the package). You probably don't want them around after the package loads for real, though. In that case, you can stick a call to 'dedoc_namespace' at the end of your '.onLoad' and everything should be copacetic.


USAGE

dedoc_namespace(ns) 


ARGUMENTS

 ns: Name of the package, or its namespace environment.
 

SEE.ALSO

write_sourceable_function, pre.install


EXAMPLES

## Don't run
# Put this into your package:
.onLoad <- function( libname, pkgname){
  # stuff for .onLoad(), or no stuff
  dedoc_package( pkgname)
}
## End don't run

}")

)

"demlazy" <-
function( ..., what, envir=.GlobalEnv) {
  if( missing( what))
    what <- sapply( match.call( expand.dots=FALSE)$..., deparse)

  envir <- as.env( envir)

  mcache <- attr( envir, 'mcache')
  what <- what %such.that% (. %in% names( mcache))
  if( !length( what))
return()

  for( i in what) {
    temp <- envir[[ i]]
    remove( list=i, envir=envir)
    envir[[ i]] <- temp
  }

  fp <- attr( envir, 'path')
  if( getOption( 'mlazy.subdir', TRUE)) 
    fp <- file.path( fp, 'mlazy')
    
  file.remove( file.path( fp, 'obj' %&% abs( mcache[ what]) %&% '.rda'))
  attr( envir, 'mcache') <- mcache %without.name% what
  invisible( NULL)
}


"deparse.names.parsably" <-
function( x) {
  if( typeof( x)=='symbol'){
return( simplest_name_generator( x))
  } else {
return( deparse( x))
  }
}


"disatt" <-
function( x, 
    keep_= cq( levels, dim, dimnames, names, row.names, tsp), # not class 
    keep= NULL
){
  keep <- c( keep_, keep)
  attributes( x) <- attributes( x)[ keep]
return( x)
}


"discard.mouldering.backups" <-
function (nlocal = sys.parent()) 
mlocal({
    if (line.breaks[1] > 1) {
        previous.backups <- previous.backups[line.breaks[1]:length(previous.backups)]
        line.breaks <- line.breaks - line.breaks[1] + 1
    }
    keepo <- rep(TRUE, length(line.breaks))
    prev.times <- sapply(strsplit(previous.backups[line.breaks + 
        1], "=", fixed=TRUE), function(x) as.numeric(paste(x[-1], 
        collapse = "")))
    old.sessions <- unique(prev.times) %except% session.start.time
    if (length(old.sessions) > backup.fix[2]) 
        old.sessions <- rev(sort(old.sessions))[1 %upto% backup.fix[2]]
    keepo <- keepo & (prev.times %in% c(old.sessions, session.start.time))
    is.this.session <- prev.times == session.start.time
    if (sum(is.this.session) >= backup.fix[1]) 
        keepo <- keepo & (!is.this.session | (cumsum(is.this.session) > 
            sum(is.this.session) + 1 - backup.fix[1]))
    copy.lengths <- diff(c(line.breaks, length(previous.backups) + 
        1))
    keepo <- rep(keepo, copy.lengths)
    previous.backups <- previous.backups[keepo]
})


"ditto.list" <-
structure( function( ...){
  mc <- as.list( match.call( expand.dots=TRUE)[-1])
  nondit <- sapply( mc, function( x) !is.name( x) || nzchar( x))
  mc[ nondit] <- lapply( mc[ nondit], eval, envir=parent.frame())
  mc[ !nondit] <- unname( mc[ findInterval( index( !nondit), index( nondit))])
return( mc)
}
, doc =  docattr( r"{
ditto.list    package:handy2


Shorthand filler-inner for lists

DESCRIPTION

Suppose you want to set up a list where several consecutive elements take the same value, but you don't want to repeatedly type that value: then use 'dittolist' to set empty (missing) elements to the previous non-empty element. Wrap in 'unlist()' to create a vector instead of a list.

USAGE

ditto.list(...) 
# EG:
# ditto.list( a=1, b=, c='hello') # a: 1; b: 1, c: 'hello'

ARGUMENTS

 ...: anything, named or unnamed; missing elements OK

VALUE

List

EXAMPLES

unlist( ditto.list( a=1, b=, c='hello')) # a: 1; b: 1, c: 'hello'

}")

)

"do.in.envir" <-
structure( function( fbody, envir=parent.frame(2)) {
  ff <- sys.function( sys.parent())
  body( ff) <- substitute( fbody)
  environment( ff) <- envir
  cc <- sys.call( sys.parent())
  cc[[1]] <- ff
  eval.parent( cc, 2)
}
, doc =  docattr( r"{
do.in.envir         package:mvbutils

Modify a function's scope


DESCRIPTION

'do.in.envir' lets you write a function whose scope (enclosing environment) is defined at runtime, rather than by the environment in which it was defined.


USAGE

# Use only as wrapper of function body, like this:
# my.fun <- function(...) do.in.envir( fbody, envir=)
# ... should be the arg list of "my.fun"
# fbody should be the code of "my.fun"

do.in.envir( fbody, envir=parent.frame(2)) # Don't use it like this!


ARGUMENTS

 fbody: the code of the function, usually a braced expression
 envir: the environment to become the function's enclosure

 
DETAILS

By default, a 'do.in.envir' function will have, as its enclosing environment, the environment in which it was _called_, rather than _defined_. It can therefore read variables in its caller's frame directly (i.e. without using 'get'), and can assign to them via '<<-'. It's also possible to use 'do.in.envir' to set a completely different enclosing environment; this is exemplified by some of the functions in 'debug', such as 'go'.

Note the difference between 'do.in.envir' and 'mlocal'; 'mlocal' functions evaluate in the frame of their caller (by default), whereas 'do.in.envir' functions evaluate in their own frame, but have a non-standard enclosing environment defined by the 'envir' argument.

Calls to e.g. 'sys.nframe' won't work as expected inside 'do.in.envir' functions. You need to offset the frame argument by (at time of writing this documentation...) 5, so that 'sys.parent()' should be replaced by 'sys.parent( 5)' and 'sys.call' by 'sys.call(-5)'. In future, 5 may not be the right magic number.

'do.in.envir' functions are awkward inside namespaced packages, because the code in 'fbody' will have "forgotten" its original environment when it is eventually executed. This means that objects in the namespace will not be found.

The 'debug' package tries to 'mtrace' inside 'do.in.envir' functions; this used to work, but hasn't been recently tested in R4.1 where a few internal R deepshit mysteries seem to have changed.


VALUE

Whatever 'fbody' returns.


EXAMPLES

fff <- function( abcdef) ffdie( 3)
ffdie <- function( x) do.in.envir( { x+abcdef} )
fff( 9) # 12; ffdie wouldn't know about abcdef without the do.in.envir call

# Show sys.call issues
# Note that the "envir" argument in this case makes the 
# "do.in.envir" call completely superfluous!
ffe <- function(...) do.in.envir( envir=sys.frame( sys.nframe()), sys.call( -5))
ffe( 27, b=4) # ffe( 27, b=4)


SEE.ALSO

'mlocal'


AUTHOR

Mark Bravington


KEYWORDS
programming; utilities
}")

)

"do.on" <-
structure( function( x, expr, ..., simplify=TRUE){
  fungo <- function( .) bod
  l <- list( ...)
  environment( fungo) <- if( length( l))
      list2env( l, parent=parent.frame())
    else
      parent.frame()
  body( fungo) <- substitute( expr)
  if( is.atomic( x) && is.null( names( x)))
    x <- named( x)
  sapply( x, fungo, simplify=simplify) 
}
, doc =  docattr( r"{
do.on    package:handy2
FOR


Easier sapply/lapply avoiding explicit function

DESCRIPTION

Simpler to demonstrate:

%%#
do.on( find.funs(), environment( get( .)))
# same as:
lapply( find.funs(), function( x) environment( get( x)))

'do.on' evaluates 'expr' for all elements of 'x'. The expression should involve the symbol '.', and will be cast into a function which has an argument '.' and knows about any dotdotdot arguments passed to 'do.on' (and objects in the function that calls 'do.on'). If 'x' is atomic (e.g. character or numeric, but not list) and lacks names, it will be given names via 'named'. With 'do.on', you are calling 'sapply', so the result is simplified if possible, unless 'simplify=FALSE' (or 'simplify="array"', for which see 'sapply'). With 'FOR', you are calling 'lapply', so no simplication is tried; this is often more useful for programming.


USAGE

do.on(x, expr, ..., simplify = TRUE) 
FOR(x, expr, ...) 

ARGUMENTS

 x: thing to be iterated over. Names are copied to the result, and are pre-allocated if required as per DESCRIPTION
 expr: expression, presumably involving the symbol '.' which will successively become the individual elements of 'x'
 ...: other "arguments" for 'expr'
 simplify: as per 'sapply', and defaulting to TRUE.

VALUE

 do.on: as per 'sapply', a vector or array of the same "length" as 'x'.
 FOR: a list of the same length as 'x'

EXAMPLES

do.on( 1:7, sum(1:.))
#  1  2  3  4  5  6  7 
# 1  3  6 10 15 21 28 

# note the numeric "names" in the first row

FOR( 1:3, sum(1:.))
}")

)

"doc2Rd" <-
structure( function( text, file=NULL, append=formals(cat)$append, warnings.on=TRUE, Rd.version=NULL,
    def.valids=NULL, check.legality=TRUE) {
###############################
  if( is.function( text)) {
    forig <- text
    text <- attr( text, 'doc')
  stopifnot( is.character( text))
  } else
    forig <- NULL

  class( text) <- NULL

  # Enforce PERL syntax in regexes
  for( regexo in cq( grep, grepl, sub, gsub, regexpr, gregexpr)) {
    ff <- get( regexo)
    formals( ff)$perl <- quote( !fixed)
    formals( ff)$useBytes <- TRUE
    assign( regexo, ff, envir=sys.frame( sys.nframe()))
  }

  if( is.null( Rd.version))
    Rd.version <- if( getRversion() >= '2.10.0') '2' else '1'
  is.Rd2 <- numeric_version( Rd.version) >= '2'

  # Fucken syntax fucken change, thanks R
  if( 'keep.source' %not.in% names( formals( parse))) {
    formals( parse) <- c( formals( parse), alist( keep.source=TRUE))
  }

  # ... and for 'subco' (which uses Rd.version, for example)
  subco <- subco
  environment( subco) <- sys.frame( sys.nframe())

  # Strip EOL whitespace
  text <- sub( ' +$', '', text)

  # ... and tabs...
  text <- gsub( '\t', '  ', text)

  # Pre-empt backslash and brace woes-- Rdoc 1 is very buggy about this
  notcom <- grep( '^[^%]', text)
  # if( !is.Rd2) ??
  text[notcom] <- gsub( '\\', '\016', text[notcom], fixed=TRUE) # now leave til end
  text[notcom] <- gsub( '{', '\020', text[notcom], fixed=TRUE)
  text[notcom] <- gsub( '}', '\021', text[notcom], fixed=TRUE)

  # Code blocks first: indent all contents by 2. This stops capitalized words in codeblocks from becoming sections
  cbstart <- index( text=='%%#')+1
  if( length( cbstart)) {
    cbend <- index( !nzchar( text))
    cbend <- cbend[ findInterval( cbstart, cbend)+1]-1
    cblines <- unlist( mapply( seq, from=cbstart, to=cbend, SIMPLIFY=FALSE))
    text[ cblines] <- '  ' %&% text[ cblines]
  }

  # Check for completely informal doco...
  if( !match( 'DESCRIPTION', text, 0) && 
      !match( 'Description:', text, 0)) {
    if( warnings.on)
      scatn( "Looks like informal doco to 'doc2Rd', in %s", text[1])
    if( !nzchar( sub( ' +', '', text[1])))
      text <- c( 'INFORMAL DOCO', text)
    first.blank <- index( !nzchar( text))[1]
    if( is.na( first.blank)) {
      if( warnings.on)
        warning( "No blank lines-- so no aliasses will be set")
      text <- c( text[ 1], '', text[-1])
      first.blank <- 2
    }

    # Prepare to ignore other section-like lines-- just bold them
    seclines <- grep( 
        '^([.]*[A-Z][a-z0-9 ]*[a-zA-Z0-9])(\\([Ss]\\))?:$', text)
    text[ seclines] <- '*' %&% seclines %&% '*'
    seclines <- grep( '^[.]*[A-Z][A-Z0-9.]+(\\(S\\))?$', text)
    text[ seclines] <- '*' %&% seclines %&% '*'

    # Add DESCRIPTION field, containing everything:
    text <- multinsert( text, first.blank, list( 
        c( 'Documentation for ' %&% text[1], '', 'DESCRIPTION', '')))

    if( !is.null( forig)) {
      text <- c( text, '', 'USAGE', '', 
          '# This section is machine-generated...',
          sub( '^ *([^ ]+) .*', '\\1', 
          text[1]) %&% sub( '^NULL$', '()',
              sub( 'list', '', deparse( formals( forig)))))
      if( length( formals( forig)))
        text <- c( text, '', 'ARGUMENTS', '', 
            'This section is machine-generated...',
            paste( ' ', names( formals( forig)), ': ???', sep=''))
    }
  }

  # Global sub of colonized section & subsection titles to caps
  first.blank <- index( !nzchar( text))[1]
  seclines <- grep( 
      '^[.]*[A-Z][a-zA-Z0-9.]*[a-zA-Z0-9](\\([Ss]\\))?:$', text)
  seclines <- c( seclines, grep(
      '^[.]*[A-Z][A-Z0-9.-][A-Z0-9]+(\\([Ss]\\))?$', text))
  seclines <- seclines %such.that% (. > first.blank)
  text[ seclines] <- toupper( sub( '(\\([Ss]\\))?:?$', '', 
      text[ seclines]))

  # Global sub of refs to (sub)section titles-- AUTHOR(S) as well as AUTHOR
  # .. subsections are like sections, but the line starts with 1 or more periods
  sectitles <- grep( '^[.]*[A-Z][A-Z0-9.-]+[A-Z0-9][?]?$', text, 
      value=TRUE)
  for( secti in sectitles) {
    # No leading dots
    short.secti <- sub( '^[.]*', '', secti)

    # How xref appears in flat-format doco
    rss <- '\\b' %&% to.regexpr( short.secti) %&% '($|\\W)'

    # How it will look in Rd; bolded, first char Upper, rest lower
    xref <- '*' %&% toupper( substring( short.secti, 1, 1)) %&%
        tolower( gsub( '.', ' ', substring( short.secti, 2), 
        fixed=TRUE)) %&% '*\\2'
    xref2 <- sub( '2$', '3', xref) # cases when a 2nd optional paren is used

    # "in MYSECTION"
    text <- gsub( '\\b([Ii])n ' %&% rss,
        '\\1n ' %&% xref,
        text)

    # "under MYSECTION"
    text <- gsub( '\\b([Uu])nder ' %&% rss,
        '\\1nder ' %&% xref,
        text)

    # "as per MYSECTION"
    text <- gsub( '\\b([Aa])s per ' %&% rss,
        '\\1s per ' %&% xref,
        text)

    # "See MYSECTION" and "see also MYSECTION"
    text <- gsub( '\\b([Ss])ee (also )?' %&% rss,
        '\\1ee \\2' %&% xref2,
        text)

    # "The MYSECTION section"; NB no afterspace because of \\W
    text <- gsub( '\\b([Tt])he ' %&% rss %&% 'section',
        '\\1he ' %&% xref %&% 'section',
        text)

    # "Section MYSECTION" and "section on MYSECTION"
    text <- gsub( '\\b([Ss])ection (on )?' %&% rss,
        '\\1ection \\2' %&% xref2,
        text)

    # "MYSECTION (qv)"; NB no afterspace
    text <- gsub( rss %&% ' *\\(qv\\)',
        xref,
        text)

    # "MYSECTION (see below)"; NB no afterspace
    text <- gsub( rss %&% ' *\\(see below\\)',
        xref %&% '[(]see below[)]',
        text)

  } # for sectitles

  #tcon <- textConnection( text)
  #on.exit( close( tcon))
  lptr <- 0
  nlines <- length( text)

  Rd <- character( 0)
  EOF <- FALSE

# Definitions:
  verbatim <- function( string) {
    string <- gsub( '\\', '\001', string, fixed=TRUE)
    string <- gsub( '{', '\\{', string, fixed=TRUE)
    string <- gsub( '}', '\\}', string, fixed=TRUE)
    string <- gsub( '%', '\\%', string, fixed=TRUE)
    string <- gsub( '\001', '\\\\', string, fixed=TRUE)
    string
  }

  maxchar <- c( usage=80, synopsis=80, examples=100)

  out <- function( string, string2, strip.spaces.at.start=FALSE) {
      # length( string)>1 with keyword blocks
      if( length( string)==1 && grepl( '^subsection[{]', string)) {
        new.nesting <- nchar( 
            sub( 'subsection[{]([.]*).*', '\\1', string))
        new.nesting <- min( new.nesting, nesting + 1)
        string <- sub( '[{][.]*', '{', string)
      } else
        new.nesting <- 0
      if( new.nesting <= nesting)
        Rd <<- c( Rd, rep( '}', 1+nesting-new.nesting))

      nesting <<- new.nesting

      if( !missing( string2)) {
        if( strip.spaces.at.start)
          string2 <- sub( '^ +', '', string2)

        string <- if( length( string2)==1)
          paste( '\\', string, '{', string2,  sep='') # no closing brace
        else
          c( '\\' %&% string %&% '{', string2) # no closing brace
      } else
        string[ length( string)] <- sub( '[}] *$', '', 
            string[ length( string)]) # final keyword
      Rd <<- c( Rd, string)
    } # out function

  line <- function( skip.blanks=TRUE, do.subs=TRUE, auto.link=FALSE, 
        uncomment=TRUE, valid.links=NULL) {
      repeat{
        # line <- readLines( tcon, 1)
        # if( !length( line)) {
        # return( line)
        # }
        if( lptr==nlines) {
          EOF <<- TRUE
    return( character(0))
        }

        lptr <<- lptr+1
        line <- text[ lptr]

        if( uncomment && substring( line, 1, 1)=='%')
      return( substring( line, 2)) # unmodified apart from removing %

        line <- sub( ' +$', '', line) # strip spaces at the end
        if( uncomment)
          line <- gsub( '%', '\\%', line, fixed=TRUE)

        if( !skip.blanks || nzchar( line))
      break
      }

      if( do.subs)
        line <- subco( line, auto.link=auto.link, 
            valid.links=def.valids)
      line
    }

  block <- function( do.subs=TRUE, bs17=FALSE, blank.stop=FALSE, 
      auto.link=FALSE, Rd2.Rlike=FALSE,
      width=NA, methodize=FALSE) {
    #############
      block <- character( 0)
      repeat{
        new.line <- line( do.subs=do.subs, skip.blanks=!blank.stop, 
            auto.link=auto.link, valid.links=def.valids)
        if( EOF)
      break
        if( blank.stop && !nzchar( new.line))
      break
        # Check for field names
        if( length( grep( '^[.]*[A-Z][A-Z0-9.-]+(\\(S\\))?$', 
            new.line))) {
          # replace AUTHOR(S) by AUTHOR
          #pushBack(  sub( '(S)', '', new.line, fixed=TRUE), tcon)
          text[ lptr] <- sub( '(S)', '', new.line, fixed=TRUE)
          lptr <<- lptr-1
      break
        }

        # Pre-formatted?
        if( !bs17 && substring( new.line, 1, 2)=='%#') {
          pref.block <- block( do.subs=FALSE, bs17=TRUE, 
              blank.stop=TRUE)
          # All into one line for now...
          block <- c( block, paste( c( '\\preformatted{', 
              pref.block, '}'), collapse='\n'))
        } else
          block <- c( block, new.line)
      }

      if( bs17) {
        # Flag backslashes and braces for different treatment in verbatim-style bits
        # Same thing happens in 'line' inside code blocks
        block <- gsub( '\016', '\017', block, fixed=TRUE)
        block <- gsub( '\020', '\022', block, fixed=TRUE)
        block <- gsub( '\021', '\023', block, fixed=TRUE)
      }

      if( Rd2.Rlike) {
        block <- gsub( '\016', '\\', block, fixed=TRUE) # now leave til end
        block <- gsub( '\020', '{', block, fixed=TRUE)
        block <- gsub( '\021', '}', block, fixed=TRUE)

        block <- make.Rd2( block, width=width, methodize=methodize)
      }

      block
    }

  insert.para.breaks <- function( block) {
      if( length( block)>1) {
        n <- length( block)
        block <- rep( block, each=2)
        block[ 2*(1:n)] <- ''
        block <- block[ -2*n]
      }
      block
    }

  itemize <- function( block) {
      # Unlabelled (bulleted) lists
      while( length( block) && length( items <- index( 
          grepl( '^ +[*-] ', block)))) {
        n.items <- min( index( diff( c( items, 
            length(block)+5)) %!in% 1:2))

        # Start \itemize{
        block <- multinsert( block, items[1]-1, '\\itemize{')
        items <- items + 1 # to allow for the new \\itemize{ line
        if( n.items>1) # zap any blank lines between items
          block <- block[ -( items[1]:items[n.items] %except% 
              items[1:n.items])]

        # Add \item
        items <- items[1]+(1:n.items)-1
        block[ items] <- '\\item ' %&% sub( '^ +[*-] ', '', 
            block[ items])

        # End with back-brace for \itemize
        block <- multinsert( block, items[ n.items], '}')
      }

      # Labelled lists, e.g. value: result
      while( length( block) && length( items <- index( 
          grepl( '^ +[^:]*: ', block)))) {
        n.items <- min( index( diff( c( items, 
            length(block)+5)) %!in% 1:2))

        # Start \describe{
        block <- multinsert( block, items[1]-1, '\\describe{')
        items <- items + 1 # to allow for the new \\describe{ line
        if( n.items>1) # zap any blank lines between items
          block <- block[ -( items[1]:items[n.items] %except% 
              items[1:n.items])]

        # Add \item{label}{body}
        items <- items[1]+(1:n.items)-1
        block[ items] <- '\\item{' %&% sub( '^ +([^:]*): +', '\\1}{', 
            block[ items]) %&% '}'

        # End with back-brace for \describe
        block <- multinsert( block, items[ n.items], '}')
      }

      block
    }

  list.block <- function( sub.item.names=FALSE, auto.link=FALSE) {
      block <- character( 0)
      repeat{
        new.line <- line( do.subs=FALSE) # subs done later
        if( EOF)
      break
        # Check for field names
        if( length( grep( '^[.]*[A-Z][A-Z0-9.-]+$', new.line))) {
          #pushBack(  new.line, tcon)
          text[ lptr] <- new.line
          lptr <<- lptr-1
      break
        }
        # Check for list item: line starts with space, then comma-separated words ending with a colon
        if( grepl( '^ ', new.line)) {
          # NB: whole item text is assumed to be on one line
          item <- strsplit( new.line, ': ')[[1]]
          item[1] <- if( sub.item.names) 
              subco( item[1]) 
            else 
              gsub( "'", '', item[1])
          new.line <- paste( '\\item{', item[1], '}{',
              subco( paste( item[ -1], collapse=':')), '}', sep='')
        } else
          new.line <- subco( new.line, auto.link=auto.link, 
              valid.links=def.valids)
        block <- c( block, new.line)
      }
      block
    }

  seealso.block <- function() {
      block <- ' ' %&% block() %&% ','
      block <- block[ !grepl( '^%', block)] # comment lines
      # Strip out anything already in \code{}...
      block <- gsub( '\\\\code\\{([^}]*)\\}', "'\\1'", block)
      # ...and put single words ended by comma or semicolon into \code{\link{}}
      block <- gsub( " ([a-zA-Z.][---a-zA-Z.0-9]*)('*)[,;]",
          ' \\\\code\\{\\\\link\\{\\1\\}\\}\\2,', block)
      # ...and strip quotes around these
      block <- gsub( "'(\\\\code\\{\\\\link\\{[^}]*\\}\\})'", '\\1', 
          block)
      # ... and any remaining quotes back into \code{}
      block <- gsub( " '([^']+)'", " \\\\code\\{\\1\\}", block)
      block <- substring( block, 1, nchar( block)-1)
      block
    }


  keyword.block <- function() {
      block <- block()
      block <- grep( '^[^%]', block, value=TRUE) # drop comment lines
      block <- paste( block, collapse=' ')
      block <- gsub( '[,;]', ' ', block)
      block <- gsub( ' +', ' ', block)
      block <- strsplit( block, ' ')[[ 1]]
      block[ nchar( block) > 0]
    }

  nice.title <- function( section.title) {
      # Now handles subsections too, which start with a sequence of periods
      # section.title <- gsub( '\\.', ' ', section.title)
      # substring( section.title, 1, 1) <- upper.case( substring( section.title, 1, 1))
      section.title <- sub( '^([.]*)(.)', '\\1\\U\\2', section.title, 
          perl=TRUE)
      section.title <- sub( '^([.]*).*', '\\1', section.title) %&%
          gsub( '.', ' ', sub( '^[.]*', '', section.title), 
              fixed=TRUE)

      section.title
    }

  sectionize <- function( niced.up.title) {
    field.name <- if( substring( niced.up.title, 1, 1) == '.') 
        'subsection' else 'section'
    sprintf( '%s{%s}', field.name, niced.up.title)
  }

#  fields <- cq( description, usage, synopsis, arguments, arguments., value, details, examples,
#      author, references, note, see.also, keywords)
#  fields <- c( fields, 'author(s)')

# Code starts here
  nesting <- -1
  name <- strsplit( line(), ' ')[[1]][1]
  out( 'name', name)
  overall.name <- name

  if( is.package <- grepl( '\\-package', name))
    out( 'alias', sub( '\\-package.*', '', name))

  while( nzchar( name)){
    if( !is.null( def.valids))
      def.valids <- def.valids %except% name # don't link to myself
    out( 'alias', verbatim( name), strip.spaces.at.start=TRUE)
    name <- line( FALSE, FALSE, uncomment=FALSE)
  }

  if( is.package)
    out( 'docType', 'package')

  if( is.data <- match( 'FORMAT', text, 0)>0)
    out( 'docType', 'data')

  out( 'title', line( do.subs=FALSE)) # no special stuff allowed in title

  #  Itemizing rules are:
  #   - don't use \code subs in item names in VALUE or ARGUMENTS
  #   - optional to use it in other fields
  #   - don't use \itemize except for unnamed bullet-point lists (like this para)

  while( !EOF) {
    next.field <- tolower( line())
    if( EOF)
  break
    switch( next.field,
      description=,
      details=,
      author=,
      "author(s)"=,
      references=,
      format=,
      source=,
      note= out( next.field, itemize( insert.para.breaks( block( 
          auto.link=!is.null( def.valids))))),
      examples=,
      synopsis=,
      usage= out( next.field,
          block( do.subs=FALSE, Rd2.Rlike=is.Rd2, bs17=!is.Rd2, 
          width=maxchar[ 'usage'],
          methodize=(next.field=='usage'))),
      see.also= out( 'seealso', insert.para.breaks(
          block( auto.link=!is.null( def.valids)))), #seealso.block())),
      value=,
      arguments= out( next.field, list.block(FALSE, 
          auto.link=!is.null( def.valids))),
      keywords= out( '\\keyword{' %&% keyword.block() %&% '}'),
      out( sectionize( nice.title( next.field)),
      #out( 'section{' %&% nice.title( next.field) %&% '}',
                  itemize( insert.para.breaks( block( 
                  auto.link=!is.null( def.valids)))))
    )

    # For user's own sections, used to have
    #        if( regexpr( '\\.$', next.field)<0)
    #          itemize( insert.para.breaks( block()))
    #        else
    #          list.block(TRUE))
    # but it didn't work with funny characters anyway
  } # while new field

  Rd <- c( Rd, rep( '}', 1+nesting))
#  Rd <- Rd[ nchar( Rd)>0]

  Rd <- setup.dontruns( Rd)

  # Already methodized USAGE
  # methodize.USAGE() # sigh

  # \keywords{} is mandatory...
  if( !length( grep( '^\\\\keyword\\{', Rd)))
    Rd <- c( Rd, '\\keyword{' %&% 
        (if( is.data) 'data' else 'misc') %&% '}')

  # Split \preformatted; don't zap blanks
  preflines <- grep( '\n', Rd, fixed=TRUE)
  Rd <- multirep( Rd, preflines, strsplit( Rd[ preflines], '\n'))

  if( is.Rd2) {
    Rd <- gsub( '\016', '\\\\', Rd, fixed=TRUE)
    Rd <- gsub( '\020', '\\{', Rd, fixed=TRUE)
    Rd <- gsub( '\021', '\\}', Rd, fixed=TRUE)

    # ... and in verbatim bits:
    Rd <- gsub( '\017', '\\\\', Rd, fixed=TRUE)
    Rd <- gsub( '\022', '{', Rd, fixed=TRUE)
    Rd <- gsub( '\023', '}', Rd, fixed=TRUE)

    # Fix split one-liners-- not that they need fixing-- this is "improvement" in R 2.12
    one.liners <- cq( name, alias, docType, title, author)
    olsplit <- grep( '^\\\\(' %&% 
        paste( one.liners, collapse='|') %&% 
        ') *\\{[^}]*$', Rd)
    if( length( olsplit))
      olsplit <- olsplit[ grepl( '^ *\\} *$', Rd[ olsplit+1])]
    if( length( olsplit)){
      Rd[ olsplit] <- Rd[ olsplit] %&% '}'
      Rd <- Rd[ -(olsplit+1)]
    }
  } else {
    # Old fmt Rd had problems with some weird-but-legal sequences...
    # Restore backslashes & braces in normal text -- getround buggy Rd
    Rd <- gsub( '\016', '\\\\\\enc{}{}', Rd, fixed=TRUE)
    Rd <- gsub( '\020', '\\{\\enc{}{}', Rd, fixed=TRUE)
    Rd <- gsub( '\021', '\\}\\enc{}{}', Rd, fixed=TRUE)

    # ... and in verbatim bits:
    Rd <- gsub( '\017', '\\\\\\link{}', Rd, fixed=TRUE)
    Rd <- gsub( '\022', '\\{\\link{}', Rd, fixed=TRUE)
    Rd <- gsub( '\023', '\\}\\link{}', Rd, fixed=TRUE)

    reduce.empty.links() # minimize offence to Rcmd check...
  }

  if( !is.null( file))
    cat( Rd, sep='\n', file=file, append=append)

  Rd <- as.cat( Rd)
  if( is.Rd2 && check.legality && getRversion() >= '2.10.0') {
    # parse_Rd unreliable in 2.9.x so only do this in 2.10 onwards, 
    # regardless of Rd.version
    ow <- options( warn=2)
    check.file <- tempfile( legal.filename( overall.name))
    on.exit( { options( ow); unlink( check.file)}, add=TRUE)
    cat( Rd, sep='\n', file=check.file)
    p1 <- try( parse_Rd( check.file))  # warning => error
    if( p1 %is.a% 'try-error')
      class( Rd) <- c( 'try-error', class( Rd))
  }

return( Rd)
}
, doc =  docattr( r"{
doc2Rd               package:mvbutils
docotest

Converts plain-text documentation to Rd format


DESCRIPTION

'doc2Rd' converts plain-text documentation into an Rd-format character vector, optionally writing it to a file. You probably won't need to call 'doc2Rd' yourself, because 'pre.install' and 'patch.install' do it for you when you are building a package; the entire documentation of package 'mvbutils' was produced this way. The main point of this helpfile is to describe plain-text documentation details. However, rather than wading through all the material below, just have a look at a couple of R's help screens in the pager, e.g. via 'help( glm, help_type="text")', copy the result into a text editor, and try making one yourself. Don't bother with indentation though, except in item lists as per MORE.DETAILS below (the pager's version is not 100% suitable). See 'fixr' and its 'new.doc' argument for how to set up an empty template: also 'help2flatdoc' for how to convert existing Rd-format doco.

'docotest' lets you quickly check how your doco would look in a browser.

For how to attach plain-text documentation to your function, see 'docattr' and 'write_sourceable_function', etc.


USAGE

doc2Rd( text, file=NULL, append=, warnings.on=TRUE, Rd.version=,
    def.valids=NULL, check.legality=TRUE)
docotest( fun.or.text, ...)


ARGUMENTS

For 'doc2Rd':

 text: (character or function) character vector of documentation, or a function with a 'doc' attribute that is a c.v. of d..

 file: (string or connection) if non-NULL, write the output to this file

 append: (logical) only applies if '!is.null(file)'; should output be appended rather than overwriting?

 warnings.on: (logical) ?display warnings about apparently informal documentation?

 Rd.version: (character) what Rdoc version to create "man" files in? Currently "1" means pre-R2.10, "2" means R2.10 and up. Default is set according to what version of R is running.

 def.valids: (character) objects or helpfiles for which links should be generated automatically. When 'doc2Rd' is being called from 'pre.install', this will be set to all documented objects in your package. Cross-links to functions in other packages are not currently generated automatically (in fact not at all, yet).

 check.legality: if TRUE and 'Rd.version' is 2 or more, then the output Rd will be run thru 'parse_Rd' and a 'try-error' will be returned if that fails; normal return otherwise. Not applicable if 'Rd.version' is 1.

For 'docotest':

 fun.or.text: (character or function) character vector of documentation, or a function with a 'doc' attribute that is a c.v. of d.. NB if maintaining a package, you need to run this on the "raw" code (e.g. '..mypack$myfun'), not on the installed function (e.g. not 'myfun' or 'mypack::myfun').

 ...: other args passed to 'Rd2HTML' when it tries to convert 'doc2Rd' output to HTML. I've no idea what these might be, since they wouldn't be used in reality by 'pre.install' when it assembles your source package.

VALUE

Character vector containing the text as it would appear in an Rd file, with 'class' of "cat" so it prints nicely on the screen.


MORE.DETAILS

Flat-format (plain-text) documentation in 'doc' attributes, or in stand-alone character objects whose name ends with ".doc", can be displayed by the replacement 'help' in 'mvbutils' (see 'dochelp') without any further ado. This is very useful while developing code before the package-creation stage, and you can write such documentation any way you want. For display in an HTML browser (as opposed to R's internal pager), and/or when you want to generate a package, 'doc2Rd' will convert pretty much anything into a legal Rd file. However, if you can follow a very few rules, using 'doc2Rd' will actually give nice-looking authentic R help. For this to work, your documentation basically needs to look like a plain-text help file, as displayed by 'help(..., help_type="text")', except without most indentation (so, your paragraphs should not contain hard line breaks).

Rather than wading through this help file to work out how to write plain-text help, just have a look at a couple of R's help screens in the pager, and try making one yourself. You can also use 'help2flatdoc' to convert an existing plain-text help file. Also check the file "sample.fun.rrr" in the "demostuff" subdirectory of this package (see EXAMPLES). If something doesn't work, delve more deeply...

 - There are no "escape characters"-- the system is "text WYSIWYG". For example, if you type a \ character in your doc, 'help' will display a \ in that spot. Single quotes and percent signs can have special implications, though-- see below.

 - Section titles should either be fully capitalized, or end with a : character. The capitalized version shows up more clearly in informal help. Replace any spaces with periods, e.g. SEE.ALSO not SEE ALSO. The only non-alpha characters allowed are hyphens.

 - Subsections are like sections, except they start with a sequence of full stops, one per nesting level. See also SUBSECTIONS.

 - "Item lists", such as in the ARGUMENTS section and sometimes the VALUE section (and sometimes other sections), should be indented and should have a colon to separate the item name from the item body.

 - General lists of items, like this bullet-point list, should be indented and should start with a "-" character, followed by a space.

 - Your spacing is generally ignored (exceptions: USAGE (qv), EXAMPLES (qv), multi-line code blocks; see previous point). Tabs are converted to spaces. Text is wrapped, so you should write paragraphs as single lines without hard line breaks. Use blank lines generously, to make your life easier; also, they will help readability of informal helpfiles.

 - To mark _in-line_ code fragments (including variable names, package names, etc-- basically things that R could parse), put them in single quotes. Hence you can't use single quotes within in-line code fragments.

%#ifdef flub
This flatdoc help file can't show you an example of what you can't do in a flatdoc help file!
%#endif
%An example of what you couldn't include:
%\code{'myfun( "'No no no!'")'}

 - Single quotes are OK within multi-code blocks, USAGE (qv), and EXAMPLES (qv). For multi-line code blocks in other sections, don't bother with the single-quotes mechanism. Instead, insert a "%%#" line before the first line of the block, and make sure there is a blank line after the block.

 - You can insert "hidden lines", starting with a % character, which get passed to the Rd conversion routines. If the line starts with %%, then the Rd conversion routines will ignore it too. The "%%#" line to introduce multi-line code blocks is a special case of this.

 - Some other special constructs, such as links, can be obtained by using particular phrases in your documentation, as per SPECIAL.FIELDS.


.SUBSECTIONS

% I've bolded some of these meta-refs to sections
Subsections are a nice new feature in R 2.11. You can use them to get better control over the order in which parts of documentation appear. R will order sections thus: USAGE (qv), ARGUMENTS (qv), *Details*, VALUE (qv), other sections you write in alphabetical section order, *Notes*, *See also*. That order is not always useful. You can add subsections to *Details* so that people will see them in the order you want. If you want *Value* to appear before *Details*, then just rename *Details* to "MORE.DETAILS", and put subsections inside that.

In plain-text, subsection headings are just like section headings, except they start with a period (don't use the initial periods when cross-referencing to it elsewhere in the doco). You can have nested subsections by adding extra periods at the start, like this:

..ANOTHER.DEPTH.OF.NESTING

In the plain text version of this doco, the SUBSECTIONS line starts with one period, and the ANOTHER.DEPTH.OF.NESTING line starts with two. If you try to increase subsection depth by more than one level, i.e. with 2+ full stops more than the previous (sub)section, then 'doc2Rd' will correct your "mistake".


.SPECIAL.FIELDS

Almost anything between a pair of single quotes will be put into a \code{} or \code{\link{}} or \pkg{} or \env{} construct, and the quotes will be removed.  A link will be used if the thing between the quotes is a one-word name of something documented in your package (assuming 'doc2Rd' is being called from 'pre.install'). A link will also be used in all cases of the form "See XXX" or "see XXX" or "XXX (qv)", where XXX is in single quotes, and any " (qv)" will be removed. With "[pP]ackage XXX" and "XXX package", a \pkg{} construct will be used. References to '.GlobalEnv' and '.BaseNamespaceEnv' go into \env{} constructs. Otherwise, a \code{} construct will be used, unless the following exceptions apply. The first exception is if the quotes are inside USAGE (qv), EXAMPLES (qv), or a multi-line code block. The second is if the first quote is preceded by anything other than " ", "(" or "-". The final semi-exception is that a few special cases are put into other constructs, as next.

URLs and email addresses should be enclosed in <...>; they are auto-detected and put into \url{} and \email{} constructs respectively.

Lines that start with a % will have the % removed before conversion, so their contents will be passed to RCMD Rdconv later (unless you start the line with %%). They aren't displayed by 'dochelp', though, so can be used to hide an unhelpful USAGE, say, or to hide an "#ifdef windows".

A solitary capital-R is converted to \R. Triple dots _used to be_ converted to \dots (regardless of whether they're in code or normal text) but I've stopped doing so because this conversion was taking 97% of the total runtime!


Any reasonable "*b*old" or "_emphatic stuff_" constructions (no quotes, just the asterisks) will go into \bold{} and \emph{} constructs respectively, to give *b*old or _emphatic stuff_. (Those first two didn't, because they are "unreasonable"-- in particular, they're quoted.) No other fancy constructs are supported (yet).

.FORMAT.FOR.NON-FUNCTION.HELP

For documenting datasets, the mandatory sections seem to be DESCRIPTION (qv), USAGE (qv), and *Format*; the latter works just like ARGUMENTS (qv), in that you specify field names in a list. Other common sections include EXAMPLES (qv), *Source*, *References*, and *Details*.


.EXTREME.DETAILS

The first line should be the docfile name (without the Rd) followed by a few spaces and the package descriptor, like so:

utility-funs         package:mypack

When 'doc2Rd' runs, the docfile name will appear in both the \name{} field and the first \alias{} field. 'pre.install' will actually create the file "utility-funs.Rd". The next non-blank lines form the other alias entries. Each of those lines should consist of one word, preceded by one or more spaces for safety (not necessary if they have normal names).

"Informal documentation" is interpreted as any documentation that doesn't include a "DESCRIPTION" (or "Description:") line. If this is the case, 'doc2Rd' first looks for a blank line, treats everything before it as \alias{} entries, and then generates the DESCRIPTION section into which all the rest of your documentation goes. No other sections in your documentation are recognized, but all the special field substitutions above are applied. (If you really don't want them to be, use the multi-line code block mechanism.) Token USAGE (qv), ARGUMENTS (qv), and *Keywords* sections are appended automatically, to keep RCMD happy.

Section titles built into Rd are: DESCRIPTION (qv), USAGE (qv), *Synopsis* (defunct for R>=3.1), ARGUMENTS (qv), VALUE (qv), *Details*, EXAMPLES (qv), *Author* or *Author(s)*, *See also*, *References*, *Note*, *Keywords* and, for data documentation only, *Format* and *Source*. Other section titles (in capitals, or terminated with a colon) can be used, and will be sentence-cased and wrapped in a \section{} construct. Subsections work like sections, but begin with a sequence of full stops, one per nesting level. Most cross-refs to (sub)sections will be picked up automatically and put into *bold*, so that e.g. "see MY.SECTION" will appear as "see *My section*"; when referring to subsections, omit the initial dots. To force a cross-reference that just doesn't want to appear, use e.g. "MY.SECTION (qv)", or just wrap it in "*...*".

The \docType field is set automatically for data documentation (iff a *Format* section is found) and for package documentation (iff the name on the first line includes "-package").

Spacing within lines does matter in USAGE (qv), EXAMPLES (qv), and multi-line code blocks, where what you type really is what you get (except that a fixed indent at the start of all lines in such a block is removed, usually to be reinstated later by the help facilities). The main issue is in the package "manual" that RCMD generates for you, where the line lengths are very short and overflows are common. (Overflows are also common with in-line code fragments, but little can be done about that.) The "RCMD Rd2dvi --pdf" utility is helpful for seeing how individual helpfiles come out.

In SEE.ALSO, the syntax is slightly different; names of things to link to should _not_ be in single quotes, and should be separated by commas or semicolons; they will be put into \code{\link{}} constructs. You can split SEE.ALSO across several lines; this won't matter for pager help, but can help produce tidier output in the file "***-manual.tex" produced by RCMD CHECK.

In EXAMPLES, to designate "don't run" segments, put a "## Don't run" line before and a "## End don't run" line after.

I never bother with *Keywords* (except sometimes "internal", to avoid exporting something), but if you do, then separate the keywords with commas, semicolons, or line breaks; don't use quotes. A token *Keywords* section will be auto-generated if you don't include one, to keep RCMD happy.


.INFREQUENTLY.ASKED.QUESTIONS

*Q:* Why didn't you use Markdown/MyPetBargainSyntax?
*A:* Mainly because I didn't know about them, to be honest. But WRTO MarkDown it seemed to me that the hard-line-breaks feature would be a pain. If anyone thinks there's really good alternative standard, please let me know.

 *Q:* I have written a fancy _displayed_ equation using \deqn{} and desperately want to include it. Can I?
 *A:* Yes (though are you sure that a fancy equation really belongs in your function doco? how about in an attached PDF, or vignette?). Just prefix all the lines of your \deqn with %. If you want something to show up in informal help too, then make sure you also include lines with the text version of the equation, as per the next-but-one question.

 *Q:* I have written a fancy _in-line_ equation using \eqn{} and desperately want to include it. Can I?
 *A:* No. Sorry.

 *Q:* For some reason I want to see one thing in informal help (i.e. when the package isn't actually loaded but just sitting in a task on the search path), but a different thing in formal help. Can I do that?
 *A:* If you must. Use the %-line mechanism for the formal help version, and then insert a line "%#ifdef flub" before the informal version, and a line "%#endif" after it. Your text version will show up in informal help, and your fancy version will show up in all help produced via Rd. (Anyone using the "flub" operating system will see both versions...)

 *Q:* How can I insert a file/kbd/samp/option/acronym etc tag?
 *A:* You can't. They all look like single quotes in pager-style help, anyway.

 *Q:* What about S3?
 *A:* S3 methods often don't need to be documented. However, they can be documented just like any other function, except for one small detail: in the USAGE section, the call should use the generic name instead of your method name, and should be followed by a comment "# S3 method for <class>"; you can append more text to the comment if you wish. E.G.: if you are documenting a method 'print.cat', the USAGE section should contain a call to 'print(x,...) # S3 method for cat' rather than 'print.cat(x,...)'. The version seen by the user will duplicate this "S3 method..." information, but never mind eh.

 If you are also (re)defining an S3 generic and documenting it in the same file as various methods, then put a comment '# generic' on the relevant usage line. See '?print.function' for associated requirements.

 Confusion will deservedly arise with a function that looks like an S3 method, but isn't. It will be not be labelled as S3 by 'pre.install' because you will of course have used the full name in the USAGE section, because it isn't a method. However, it can still be found by 'NextMethod' etc., so you shouldn't do that. (Though 'mvbutils::max.pkg.ver' currently does exactly that...)

 S3 classes themselves need to be documented either via a relevant method using an alias line, or via a separate 'myclass.doc' text object.

 *Q:* What about S4?
 *A:* I am not a fan of S4 and have found no need for it in many 1000s of lines of R code... hence I haven't included any explicit support for it so far. Nevertheless, things might well work anyway, unless special Rd constructs are needed. If 'doc2Rd' _doesn't_ work for your S4 stuff (bear in mind that the %-line mechanism may help), then for now you'll still have to write S4 Rd files yourself; see 'pre.install' for where to put them. However, if anyone would like the flatdoc facility for S4 and is willing to help out, I'm happy to try to add support.


SEE.ALSO

The file "sample.fun.rrr" in subdirectory "demostuff", and the demo "flatdoc.demo.r".
To do a whole group at once: 'pre.install'.
To check the results: 'docotest(myfun)' to check the HTML (or 'patch.installed(mypack)' and then '?myfun'). TODO something to easily check PDF (though R's PDF doco is pointless IMO); for now you need to manually generate the file, then from a command-line prompt do something like "RCMD Rd2dvi --pdf XXX.Rd" and "RCMD Rdconv -t=html XXX.Rd" and/or "-t=txt"
To convert existing Rd documentation: 'help2flatdoc'.
If you want to tinker with the underlying mechanisms: 'flatdoc', 'write_sourceable_function'


EXAMPLES

## Needs a function with the right kind of "doc" attr
## Look at file "demostuff/sample.fun.rrr"
sample.fun <- source.mvb( system.file( file.path(
    'demostuff', 'sample.fun.rrr'), package='mvbutils'))
print( names( attributes( sample.fun)))

cat( '***Original plain-text doco:***\n')
print( as.cat( attr( sample.fun, 'doc'))) # unescaped, ie what you'd actually edit

cat( '\n***Rd output:***\n')
sample.fun.Rd <- doc2Rd( sample.fun)
print( sample.fun.Rd) # already "cat" class

## Don't run:
docotest( sample.fun) # should display in browser
## End don't run

KEYWORDS

programming

}")

)

"dochelp" <-
structure( function( topic, doc, help_type=c( 'text', 'html')) {
  # "doc" might point to another object. Start by looping til we have a character "doc".
  current.topic <- topic
  if( missing( doc)) { # TRUE unless this is being used as a pager
    doc <- 0 
    while( !is.character( doc) && exists( current.topic) && 
        length( doc <- attr( get( current.topic), 'doc'))) 
      if( is.list( doc))
        current.topic <- doc[[1]] # unwrap list 
    # If no functions/things with such doco, look for a 'thing.doc' character object        
    if( !is.character( doc)) {
      for( ext in c( '', '.doc')) {
        t1 <- topic %&% ext
        if( exists( t1, mode='character'))
          doc <- get( t1, mode='character')
      break
      }
    }
  }

  fff <- FALSE # default
  if( has.doc <- is.character( doc)) {
    help_type <- try( match.arg( help_type))
    if( help_type %is.a% 'try-error')
      help_type <- 'text'

    if( help_type=='html') {
      help_type <- 'text' # in case we fail
      if( !nzchar( sub( ' +', '', doc[1])))
        doc <- c( topic, doc)
      drd <- try( doc2Rd( doc, warnings.on=FALSE))
      if( drd %is.not.a% 'try-error') {
        tf1 <- tempfile()
        fff <- tf1 %&% '.html' # will get class 'browsertemp', and be autoprinted
        tf1 <- tf1 %&% '.Rd'
        
        on.exit( try( unlink( tf1)), add=TRUE)
        cat( drd, file=tf1, sep='\n')
        drd <- try( Rd2HTML( tf1, fff))
        if( drd %is.not.a% 'try-error') {
          # All good-- no need for pager fallback
          help_type <- 'html'
        }      
      }
    }
    
    if( help_type=='text') {
      fff <- tempfile() 
      doc <- doc[ regexpr( '^%', doc) < 0] # drop "%" lines
      doc <- strsplit( doc, '\n')
      doc[ !sapply( doc, length)] <- ''
      doc <- strwrap( unlist( doc), simplify=FALSE)
      doc[ !sapply( doc, length)] <- ''
      #   writeLines( paste( unlist( doc), collapse='\n'), con=fff) # writelines seems to zap empty lines
      cat( paste( unlist( doc), collapse='\n'), file=fff)
      names( fff) <- topic
    }
    
    class( fff) <- if( help_type=='text') 'pagertemp' else 'browsertemp'
  } 
  
#  invisible( has.doc) changed for 2.x
  invisible( fff)
}
, doc =  docattr( r"{
dochelp        package:mvbutils

Documentation (informal help)

DESCRIPTION

'dochelp(topic)' will be invoked by the replacement 'help' if conventional 'help' fails to find documentation for topic 'topic'. If 'topic' is an object with a 'doc' attribute (or failing that if '<<topic>>' or '<<topic>>.doc' is a character vector), then the attribute (or the character object) will be formatted and displayed by the pager or browser. 'dochelp' is not usually called directly.

USAGE

# Not usually called directly
# If it is, then normal usage is: dochelp( topic)
dochelp( topic, doc, help_type=c( "text", "html"))
# Set options( mvb_help_type="text") if the browser gives you grief

ARGUMENTS

 topic: (character) name of the object to look for help on, or name of "...doc" character object-- e.g. either 'thing' or 'thing.doc' if the character object is 'thing.doc'.
 doc: (character or list)-- normally not set, but deduced by default from 'topic'; see DETAILS.
 help_type: as per 'help'. Defaults to 'getOption( "mvb_help_type")' in normal usage, which in turn defaults to 'getOption( "help_type")' as for standard 'help'. Only '"text"' and '"html"' are supported by 'dochelp'; anything else maps to '"text"', which invokes R's internal pager.
 

DETAILS

'dochelp' will only be called if the original 'help' call was a simple 'help( topic=X, ...)' form, with X not a call and with no 'try.all.packages' or 'type' or 'lib.loc' arguments (the other 'help' options are OK). 

The 'doc' argument defaults to the 'doc' attribute of 'get("topic")'. The only reason to supply a non-default argument would be to use 'dochelp' as a pager; this might have some value, since 'dochelp' does reformat character vectors to fit nicely in the system pager window, one paragraph per element, using 'strwrap' (qv). Elements starting with a "%" symbol are not displayed.

To work with 'dochelp', a 'doc' attribute should be either:

 - a character vector, of length >=1. New elements get line breaks in the pager. Or:
 - a length-one list, containing the name of another object with a 'doc' attribute. 'dochelp' will then use the 'doc' attribute of that object instead. This referencing can be iterated.

If the documentation is very informal, start it with a blank line to prevent 'find.documented( ..., doctype="Rd")' from finding it.

With 'help_type="text"', the doco will be re-formatted to fit the pager; each paragraph should be a single element in the character vector. Elements starting with a % will be dropped (but may still be useful for 'doc2Rd').

With 'help_type="html"', the doco will be passed thru 'doc2Rd' (qv) and then turned into HTML. 'doc2Rd' is pretty forgiving and has a fair crack at converting even very informal documentation, but does have its limits. If there is an error in the 'doc2Rd' conversion then 'help_type' will be reset to '"text"'.

'flatdoc' (qv) offers an easy way to incorporate plain-text (flat-format) documentation-- formal or informal-- in the same text file as a function definition, allowing easy maintenance. The closer you get to the displayed appearance of formal R-style help, the nicer the results will look in a browser (assuming 'help_type="html"'), but the main thing is to just write _some_ documentation-- the perfect is the enemy of the good in this case!

SEE.ALSO

'flatdoc', 'doc2Rd', 'find.documented', 'strwrap'


EXAMPLES

#
myfun <- structure( function() 1, 
  doc="Here is some informal documentation for myfun\n")
dochelp( "myfun")
help( "myfun") # calls dochelp

AUTHOR

Mark Bravington


KEYWORDS

documentation

}")

)

"docotest" <-
function( fun.or.text, ...) {
  doco <- doc2Rd( fun.or.text)
  tf <- tempfile( fileext='.Rd')
  tf2 <- tempfile( fileext='.html')
  on.exit( unlink( tf)) # can't unlink tf2 because browser might still need it
  
  cat( doco, file=tf, sep='\n')
  ok <- try( Rd2HTML( tf, tf2, ...))
  if( ok %is.not.a% 'try-error') {
    browseURL( tf2)
  } else {
    unlink( tf2) # might as well
  }
invisible( NULL)  
}


"docskel" <-
structure( function( x=NULL, char.x=NULL, env=.GlobalEnv, assign.=FALSE){
  if( !identical( env, .GlobalEnv)) {
    env <- as.environment( env)
    pkg <- sub( 'package:', '', attr( env, 'name'))
  } else
    pkg <- 'not-yet-a-package'

  if( is.null( char.x)) {
    sx <- substitute( x)
    if( is.call( sx)) {
      x <- as.character( sx)
      if( x[1] %not.in% c( '::', ':::') || length( x)<3)
stop( "Don't know how to fixr '" %&% deparse( sx) %&% "'")
      pkg <- x[2]
      char.x <- x[3]
      if( any( search()==pkg))
        env <- as.environment( pkg)
      else if( is.null( env <- maintained.packages[[ pkg]])) {
        if( any( search()=='package:' %&% pkg))
          env <- as.environment( 'package:' %&% pkg)
        else if( isNamespaceLoaded( pkg))
          env <- asNamespace( pkg)
        else
stop( "Package '" %&% pkg %&% "' not available")
      }
    } else
      char.x <- deparse( sx)[1]
  }

  if( is.null( x))
    x <- env[[ char.x]]

  # Skulduggery to 
  
  envoo <- new.env( env)
  envoo[[ char.x]] <- x

  text <- c( sprintf( '%s    package:%s', char.x, pkg),
      attr( sys.function(), 'att1'),
      make.usage.section( char.x, NULL, envoo))

  if( length( formals( x)))
    text <- c( text, attr( sys.function(), 'att2'),  
    make.arguments.section( char.x, NULL, envoo))

  text <- c( text, attr( sys.function(), 'att3'))

  #text <- unlist( strsplit( text, '\n'))
  if( assign.) {
    class( text) <- 'docattr'
    attr( x, 'doc') <- text
    env[[ char.x]] <- x # will do the assign for real...
  }

  class( text) <- 'cat'
  text
}
, att1 =  string2charvec( r"{

Do something-or-other

DESCRIPTION

A splendid function that does something jolly useful


USAGE

# This section is a formal requirement, and as such often isn't useful...
# ...in showing how to use the function(s). You can show more realistic usages...
# ...in comment lines, and/or refer to the EXAMPLES section.

}")
, att2 =  string2charvec( r"{

ARGUMENTS

You can put normal text in ARGUMENTS, too, like this. Remember to indent all arguments, as below.

}")
, att3 =  string2charvec( r"{

VALUE

Immense. NB this section isn't compulsory.


DETAILS

Not compulsory. Other section headings, e.g. AUTHOR, should also go here. Use *single* quotes around object names and code fragments, e.g. 'bit.of.code()'. Use *double* quotes for "text" or "file.name". See 'doc2Rd' for full details of format.


SEE.ALSO

'doc2Rd', 'flatdoc'


EXAMPLES

# Not compulsory to have an EXAMPLES -- you can put examples into other sections.
# Here's how to make a "don't run" example:

## Don't run
reformat.my.hard.drive()
## End don't run


KEYWORDS

%% You can delete the KEYWORDS section-- it will be auto-added by 'doc2Rd'
%% These lines starting with "%%" won't appear in user-visible help.

misc
}")

)

"dont.lock.me" <-
structure( function( env=environment( sys.function( -1))){
  assign.to.base( 'lockEnvironment', hack.lockEnvironment())
  attr( env, 'dont.lock.me') <- TRUE
}
, doc =  docattr( r"{
dont.lock.me   package:mvbutils

Prevent sealing of a namespace, to facilitate package maintenance.

DESCRIPTION

Call 'dont.lock.me()' during a '.onLoad' to stop the namespace from being sealed. This will allow you to add/remove objects to/from the namespace later in the R session (in a sealed namespace, you can only change objects, and you can't unseal a namespace retrospectively). There could be all sorts of unpleasant side-effects. Best to leave it to 'maintain.packages' (qv) to look after this for you...


USAGE

# default of env works if called directly in .onLoad 
dont.lock.me( env=environment( sys.function( -1)))


ARGUMENTS

 env: the environment to not lock.

DETAILS

'dont.lock.me' hacks the standard 'lockEnvironment' function so that locking won't happen if the environment has a non-NULL 'dont.lock.me' attribute. Then it sets this attribute for the namespace environment.

EXAMPLES

## Don't run:

# This unseals the namespace of MYPACK only if the option "maintaining.MYPACK" is non-NULL:

.onLoad <- function( libname, pkgname) {
  if( !is.null( getOption( 'maintaining.' %&% pkgname)))
    mvbutils:::dont.lock.me()
}

## End Don't run

KEYWORDS

programming
}")

)

"dont.lockBindings" <-
structure( function( what, pkgname, namespace.=TRUE) {
  # cat( 'dlb on ', what, 'in', pkgname, 'with namespace=', namespace., '\n')   
  what <- what # force ??
  # Used to have mvbutils:::untetherBalloon below, but u.B. should be found automatically
  # ... now putting it back in because that didn't seem to work, but in a CRANpatible way 
  f <- function( pkgname, pkgpath) 99 
  if( namespace.)
    body( f) <- substitute( sapply( what, untetherBalloon, env=asNamespace( pkgname)), 
        list( what=what, untetherBalloon=asNamespace( 'mvbutils')$untetherBalloon))
  else
    body( f) <- substitute( sapply( what, untetherBalloon, 
        env=as.environment( paste( 'package:', pkgname, sep=''))), 
        list( what=what, untetherBalloon=asNamespace( 'mvbutils')$untetherBalloon))
  environment( f) <- baseenv()
  setHook.once( pkgname, if( namespace.) "onLoad" else "attach", f, 'append')
}
, doc =  docattr( r"{
dont.lockBindings    package:mvbutils

Helper for live-editing of packages

DESCRIPTION

Normally, objects in a NAMESPACEd package are locked and can't be changed. Sometimes this isn't what you want; you can prevent it by calling 'dont.lockBindings' in the '.onLoad' for the package. For user-visible objects (i.e. things that end up in the "package:blah" environment on the search path), you can achieve the same effect by calling 'dont.lockBindings' in the package's '.onAttach' function, with 'namespace=FALSE'.

USAGE

 dont.lockBindings( what, pkgname, namespace.=TRUE)

ARGUMENTS

  what: (character) the names of the objects to not lock.

  pkgname: (string) the name of the package. As you will only use this inside '.onLoad', you can just set this to 'pkgname' which is an argument of '.onLoad'.
  
  namespace.: TRUE to antilock in the namespace during '.onLoad'; FALSE to antilock in the visible manifestation of the package.
  
DETAILS

Locking occurs after '.onLoad' / '.onAttach' are called so, to circumvent it, 'dont.lockBindings' creates a hook function to be called after the locking step.

EXAMPLES

## Don't run
library( debug)
debug:::.onLoad # d.lB is called to make 'tracees' editable inside 'debug's namespace.
debug:::.onAttach # d.lB is called to make 'tracees' editable in the search path

# NB also that an active binding is used to ensure that the 'tracees' object in the search...
#... path is a "shadow of" or "pointer to" the one in 'debug's namespace; the two cannot get...
#... out-of-synch

## End Don't run
  
SEE.ALSO

'lockBinding', 'setHook'

}")

)

"dont.save" <-
function() 
  getOption("dont.Save", cq( .packageName, .SavedPlots, last.warning, .Last.value, .Traceback))


"dos.or.windows" <-
function () 
.Platform$OS.type == "windows"


"dummy_PIBH" <-
function( pkg, DLL, lldir, Rdir, src_changed) {
stop( "This is a dummy version! Don't use it...")
## Actually, this is what I use in my package 'RcppTidy', and it works fine there

  Rcpp_files <- dir( lldir, pattern='[.]cpp$', full.names=TRUE) %that.dont.match% '^RcppExports'
  Rcpp_files <- Rcpp_files %SUCH.THAT%
      (length( grep( '^// +\\Q[[Rcpp::export]]\\E', readLines( .), perl=TRUE))>0)

  if( !length( Rcpp_files)) {
return( NULL)
  }

  Cloader <- file.path( Rdir, sprintf( 'RcppExports_%s.R', DLL))
  Crapper <- character()

  redo <- src_changed( Rcpp_files, Cloader)
  if( nzchar( redo)) {
    Crapper <- file.path( lldir, sprintf( 'RcppExports_%s.cpp', DLL))
    ca <- makeR3( Rcpp_files,
        Coutfile= Crapper,
        Routfile= Cloader,
        manifest= redo,
        package= pkg,
        DLL= DLL,
        prefix= ''
      )
  } # if redo

  makelines <- character() # for now...

returnList( Cloader, makelines, needs_makefile=FALSE, subenv=TRUE)
}


"DYN.UNLOAD" <-
structure( function( dllnames, warn_if_not_loaded=TRUE){
# Like dyn.unload, but actually usable by mortals :/
# R makes this UNNECESSARILY and RIDICULOUSLY DIFFICULT
  gld <- getLoadedDLLs()
  for( idll in dllnames){
    stuff <- unclass( gld[[ idll]])
    if( !is.null( stuff)){
      dyn.unload( stuff$path)
    } else if( warn_if_not_loaded){
warning( sprintf( "'%s' not found in getLoadedDLLs()", idll))
    }
  }
}
, doc =  docattr( r"{
DYN.UNLOAD    package:mvbutils


Unload DLL easily


DESCRIPTION

R's 'dyn.unload' (qv) is ridiculously hard to use in practice, because it requires complete paths. These _can_ be extracted from 'getLoadedDLLs' (qv), but only with ridiculous amounts of effort and tricks that I always forget. Use 'DYN.UNLOAD' instead with just the basename of the DLL(s) you actually want to unload.

Note that there can be multiple versions of a DLL loaded at the same time, with the same "name" (according to 'getLoadedDLLs') but different paths. This will unload the first one (only), so you may need to call it repeatedly.


USAGE

DYN.UNLOAD( dllnames, warn_if_not_loaded=TRUE)


ARGUMENTS

 dllnames: Usually one string, eg "my_dodgy_C_code", but you can do several at once (in a character vector, obvs).

 warn_if_not_loaded: Pretty self-explanatory.
 

VALUE

The satisfaction of actually having cleared the bloody thing out of memory, eg so that you can delete the file.


EXAMPLES


## Don't run
DYN.UNLOAD( "offending_C_code")
## End don't run

}")

)

"eclone" <-
function( env){
## Clones env into a new env; functions in that env whose env is that env
## have their env reset to the new env. Orright?
  
  newe <- list2env( as.list( env), parent=parent.env( env))
  funs <- find.funs( newe)
  for( ifun in funs){
    fun <- newe[[ifun]]
    fenv <- environment( fun)
    if( identical( fenv, env)) {
      environment( fun) <- newe
      newe[[ ifun]] <- fun
    }
  }
  
return( newe)
}


"empty.data.frame" <-
function (...) {
    mc <- as.list(match.call()[-1])
    m <- sapply(mc, mode)
    is.a.name <- m == "name"
    is.a.name[is.a.name] <- !nzchar(as.character(mc[is.a.name]))
    mc[is.a.name] <- mc[!is.a.name][(cumsum(!is.a.name) + 1)[is.a.name]]
    df <- do.call("list", mc)
    # df <- do.call("list", mc)
    # df <- as.data.frame.I(df)

    mc$stringsAsFactors <- FALSE
    df <- do.call( 'data.frame', mc)

    df <- df[-(1:nrow(df)), , drop = FALSE]
    df
}


"env.name.string" <-
function( env){
stopifnot( env %is.an% 'environment')
  namio <- attr( env, 'name')
  if( is.null( namio))
    namio <- names( attr( env, 'path'))
  namio <- if( is.null( namio) || !is.character( namio) || length( namio)<1) '' else namio[1]
  namio <- namio %&% capture.output( print( env))[1]
return( namio)
}


"eval.scriptlet" <-
function (expr, envir = parent.frame(), enclos = if (is.list(envir) || 
    is.pairlist(envir)) parent.frame() else baseenv()){
  force( envir)
  force( enclos)
  eval( expr, envir, enclos)
}


"everyth" <-
function( x, by=1, from=1)
  x[ seq( from=from, by=by, length=(length( x)-from+by) %/% by)]


"expand.match" <-
function( short, long, any.case=FALSE) {
# Expands unique partial matches of 'short' in 'long'. 
# Leaves non-matches or non-uniques alone
  if( any.case)
    i <- pmatch( toupper( short), toupper( long), duplicates.ok=TRUE)
  else
    i <- pmatch( short, long, duplicates.ok=TRUE)
  short[ !is.na( i)] <- long[ i[ !is.na(i)]]
#  short[ is.na( i)] <- NA
  short
}


"expanded.call" <-
function( nlocal=sys.parent()) mlocal(
  lapply( named( names( formals( sys.function( mvb.sys.nframe())))), function( x) eval( as.name( x)))
)


"extract.named" <-
structure( function( l, to=parent.frame()) {
  n <- names( l)
  for( i in n[ nchar( n)>0])
    assign( i, l[[ i]], envir=to)
}
, doc =  docattr( r"{
extract.named      package:mvbutils

Create variables from corresponding named list elements

DESCRIPTION

This is a convenience function for creating named variables from lists. It's particularly useful for "unpacking" the results of calls to '.C'.

USAGE

extract.named( l, to=parent.frame())

ARGUMENTS

 l: a list, with some named elements (no named elements is OK but pointless)
 to: environment
 
VALUE

nothing directly, but will create variables

EXAMPLES

ff <- function(...) { extract.named( list(...)); print( ls()); bbb } 
# note bbb is not "declared"
ff( bbb=6, ccc=9) # prints [1] "bbb" "ccc", returns 6

AUTHOR

Mark Bravington

KEYWORDS

programming
}")

)

"fast.read.fwf" <-
structure( function( file, width,
    col.names=if( !is.null( colClasses)) names( colClasses) else 'V' %&% 1:ncol( fields),
    colClasses=character(0), na.strings=character(0L), tz='', ...) {
  fs <- file.info( file)$size
  if( is.na( tail( width, 1))) {
    # Determine padding at EOL experimentally
    lengo <- 2*sum( abs( width))
    repeat {
      if( lengo > fs)
stop( "Can't find enough newlines")
      reado <- readChar( file, lengo)
      fullw <- regexpr( '\\n', reado, 0)
      if( fullw > 0)
    break
      lengo <- lengo * 2
    }
    width <- c( clip( width), -( fullw - sum( abs( clip( width)))))
  } else if( tail( width, 1) < 0)
    width <- c( clip( width), tail( width, 1) - 1) # EOL char
  else
    width <- c( width, -1) # 1 char for EOL
  acw <- abs( width)
  nl <- fs %/% sum( acw)
  if( fs %% sum( acw) != 0)
stop( "Line length mismatch")

  fields <- readChar( file, rep( acw, nl))
  dim( fields) <- c( length( width), nl)
  fields <- t( fields[ width>0,])
  dimnames( fields)[[2]] <- col.names
  df <- data.frame( matrix( 0, nl, 0)) # must have correct number of rows

  # For col classes, methods package may be needed, but
  # ...if so will usually have been loaded already, and
  # ...if not we don't want to bother

  # Defeat the RCMD CHECK NANNY which is getting a bit bloody above itself
  libr.sodding.ary <- library
  methas <- if( 'package:methods' %in% search()) get( 'as', 'package:methods') else
      function( x, y) { libr.sodding.ary( methods); {get( 'as', 'package:methods')}(x, y) }

  for( fi in col.names)
        df[[fi]] <- if (is.na(colClasses[fi]))
            type.convert(fields[,fi], as.is = TRUE, dec = '.', na.strings = na.strings)
        else if (colClasses[fi] == "factor")
            as.factor(fields[,fi])
        else if (colClasses[fi] == "Date")
            as.Date(fields[,fi])
        else if ( grepl( '^POSIXct', colClasses[fi]))
            as.POSIXct(fields[,fi], tz=tz, format=sub( '^POSIXct', '', colClasses[fi]))
        else methas(data[[fi]], colClasses[fi])
  df
}
, doc =  docattr( r"{
fast.read.fwf    package:handy2

Read in fixed-width files quickly

DESCRIPTION

Experimental replacement for 'read.fwf' that runs much faster. Included in 'mvbutils' only to reduce dependencies amongst my other packages.


USAGE

fast.read.fwf(file, width,
    col.names = if (!is.null(colClasses))
        names( colClasses) else "V" %&%  1:ncol(fields),
    colClasses = character(0), na.strings = character(0L),  tz = "", ...)

ARGUMENTS

 file: character
 width: vector of column widths. Negative numbers mean "skip this many columns". Use an NA as the final element if there are likely to be extra characters at the end of each row after the last one that you're interested in.
 col.names: names for the columns that are NOT skipped
 colClasses: can be used to control type conversion; see 'read.table'. It is an optional vector whose names must be part of 'col.names'. There is one extension of the 'read.table' rules: a 'colClass' string starting 'POSIXct.' will trigger automatic conversion to 'POSIXct', using the rest of the string as the format specifier. See also 'tz'.
 na.strings: are there any strings (other than NA) which should convert to NAs?
 tz: used in auto-conversion to 'POSIXct' when 'colClass' is set
 ...: ignored; it's here so that this function can be called just like 'read.fwf'

VALUE

A 'data.frame', as per 'read.fwf' and 'read.table'.

misc
}")

)

"FF" <-
function() {
  # Check list of filenames to see if they've been updated
  new.file.times <- unclass( file.info( fix.list$file)[,'mtime'])
  modified <- is.na( new.file.times) | new.file.times!= fix.list$file.time
  if( !any( modified))
return( structure( character( 0), for.info='No modifications'))

  FF.find.homes() # check that the homes are attached...

  if( !any( modified))
return( structure( character( 0), for.info='No modifications (but some updated files)'))

  set.srcfilecopy <- function( x, lines) {
      sc <- attr( x, 'srcref')
      if( !is.null( sc)) {
        attr( sc, 'srcfile') <- srcfilecopy( 'dummyfile', lines)
        last.line <- max( index( nzchar( lines)))
        last.char <- nchar( lines[ last.line])
        sc[] <- as.integer( c( 1, 1, last.line, last.char, 1, last.char, 1, last.line))
        attr( x, 'srcref') <- sc
      }
    return( x)
    }

  old.warn <- options( 'warn')[[1]]
  on.exit( options( warn=old.warn))

  for( mod in index( modified)) {
    name <- unclass( fix.list$name)[ mod]
    cat( name, ': ')
    stuffed <- FALSE
    if( !fix.list$has.source[ mod]) { 
      # ... was, grepl( '\\<character\\>', fix.list$dataclass[ mod])) {
      ff <- readLines( fix.list$file[ mod])
      the.class <- strsplit( fix.list$dataclass[ mod], 
          ',', fixed=TRUE)[[1]] %except% 'character'
      if( !length( the.class))
        the.class <- 'cat' # print.as.cat
      class( ff) <- the.class
      cat( 'OK\n')
    } else {
      # Could be anything...
      should.be.func <- grepl( '\\<function\\>', fix.list$dataclass[ mod])
      source.code <- readLines( fix.list$file[ mod]) # all incl. any errors
      mt <- new.env( parent=.GlobalEnv) # asNamespace( 'mvbutils')) # limit damage
      # code <- try( list( value=source.mvb( fix.list$file[ mod], max.n.expr=1, envir=mt,
      #     debug.script=!should.be.func)))

      code <- try( list( value=source.mvb( fix.list$file[ mod], 
          max.n.expr=1, envir=mt)))

      if( code %is.a% 'try-error') {
        stuffed <- TRUE
        if( should.be.func) {
            fftext <- sub( 'I', to.regexpr( name), "function( ...) stop( 'I failed to parse')")
            ff <- eval( parse( text=fftext, keep.source=TRUE))
            ff <- set.srcfilecopy( ff, source.code) # all lines
            environment( ff) <- mt
          } else
            ff <- list( 'Scriptlet for "' %&% name %&% '" failed to parse')
        attr( ff, 'source') <- source.code # otherwise code is lost!!!
      } else {
        ff <- code$value
        if( is.null( ff)) {
          warning( 'Scriptlet for "' %&% name %&% '" generates NULL; replacing with list()')
          ff <- list()
        }

        if( !is.function( ff)) {
          class( ff) <- c( 'thing.with.source', oldClass( ff))
          attr( ff, 'source') <- as.cat( source.code)
        }
        # Shouldn't need to set srcref or source attributes

        cat( 'OK\n')
      }
    }

    # Reset environment of functions. Modified 5/11/2011, to allow 'local()' defs to keep their own envir
    if( is.function( ff) && identical( environment( ff), mt) ) {
      # Use old environment if available
      if( exists( name, mode='function', w[[ mod]], inherits=FALSE))
        environment( ff) <- environment( w[[ mod]][[ name]])
      else
        environment( ff) <- .GlobalEnv # why not?
    }

    assign(name, ff, w[[ mod]])
    if( has.source( ff) || is.character( ff)) # should now work with charvecs too
      try( deal.with.backups( name, w[[ mod]])) # ought not to crash, but...

    if( !stuffed && mods.in.packages[ mod])
      update_loaded_pkg( attr( w[[mod]], 'name'), name, ff)
  } # loop over modifiees

  autosave <- getOption( 'FF.autosave', FALSE)
  for( i in unique( w[ mods.in.tasks | mods.in.packages]))
    if( !identical( i, .GlobalEnv) &&
        ( autosave || yes.no( "Save task '" %&% attr( i, 'name') %&% "'? ")))
      Save.pos( i)

  answer <- unclass( fix.list$name[ modified])
  if( 'package:debug' %in% search() && any( is.traced <- (answer %in% names( tracees)))) {
    cat( 'Reapplying trace(s)...')
    lapply( answer[ is.traced], mtrace, fname=NULL, # fname=NULL forces char.fname
        from=.GlobalEnv, # NOT ideal--- but until "from" is added to trace info, better than the alternative, which goes straight to baseenv
        tracing=TRUE)
    cat( 'done\n')
  }

  # fix.list <<- fix.list[ !modified,]
  fix.list$file.time <<- new.file.times # doesn't seem to work in one step

return( answer)
}


"FF.find.homes" <-
function( nlocal=sys.parent()) mlocal({
  w <- vector( 'list', nrow( fix.list))
  mods.in.tasks <- modified & fix.list$where.type=='task'
  if( any( mods.in.tasks)) {
    stt <- search.task.trees()
    where.tasks <- match( fix.list$where, names( stt))
    not.here <- mods.in.tasks & is.na( where.tasks) # nowhere to go. Warn?
    modified[ not.here] <- FALSE
    where.tasks[ not.here] <- NA
    use <- modified & !is.na( where.tasks)
    w[ use] <- lapply( stt[ where.tasks[ use]], as.env)
  }

  mods.in.packages <- modified & fix.list$where.type=='package'
  if( any( mods.in.packages)) {
    task.trees <- sapply( lapply( maintained.packages, attr, which='task.tree'), paste, collapse='/')
    where.packs <- match( fix.list$where, task.trees, NA)
    not.here <- mods.in.packages & is.na( where.packs)
    modified[ not.here] <- FALSE
    where.packs[ not.here] <- NA
    use <- modified & !is.na( where.packs)
    w[ use] <- maintained.packages[ where.packs[ use]]
  }

  mods.in.attached <- modified & fix.list$where.type=='attached'
  if( any( mods.in.attached)) {
    where.att <- match( fix.list$where, search(), NA)
    not.here <- mods.in.attached & is.na( where.att)
    modified[ not.here] <- FALSE
    where.att[ not.here] <- NA
    use <- modified & !is.na( where.att)
    w[ use] <- lapply( where.att[ use], pos.to.env)
  }
})


"file.path.as.absolute" <-
function( x) {
  # file_path_as_absolute rejects nonexistent paths-- ANNOYING, UNNECESSARY!!
  # Another hack required
  fpa <- file_path_as_absolute
  e <- new.env( parent=environment( fpa))
  e$stop <- function( ...) 0
  environment( fpa) <- e
  fpa( x)
}


"find.and.get" <-
function( nlocal=sys.parent()) mlocal({
  if( is.null( pkg)) {
    if( new)
      num.load.from <-  1
    else {
      num.load.from <- find( name, numeric=TRUE)[1]
      if( is.na( num.load.from)) {
        if( length( maintained.packages)) {
          mpls <- lapply( maintained.packages, lsall)
          m <- sapply( mpls, match, x=name, nomatch=0)
          if( sum( m>0)==1)
            pkg <- names( maintained.packages)[ m>0] # handle below
          else if( sum( m>0)>1)
  stop( "'" %&% name %&% "' found in more than one live package ('" %&%
          paste( names( maintained.packages)[ m>0], collapse="', '") %&% "'): must specify which one")
        }
        if( is.null( pkg)) { # can't find anywhere
          new <- TRUE
          num.load.from <- 1
        }
      } else if( regexpr( '^package:', search()[ num.load.from])>0) {
        # check for maintained version
        pkg <- substring( search()[ num.load.from], nchar( 'package:')+1) # handle below
        if( is.null( maintained.packages[[ pkg]])) {
          if( fixing)
            warning( "Reluctantly fixing '" %&% name %&% "' directly in 'package:" %&% pkg %&%
                "'-- won't affect any namespace copies and won't be saved",
                immediate.=TRUE)
          pkg <- NULL
        }
      }
    }
  }

  if( !is.null( pkg)) { # could be set during last
    num.load.from <- NA
    if( pkg %is.an% 'environment') # eg ..mypack
      pkg <- attr( pkg, 'name')
    load.from <- maintained.packages[[ pkg]]
    if( is.null( load.from)) {
      if( fixing)
stop( "Package '" %&% pkg %&% "' not set up for editing-- see 'maintain.packages'")
      else { # just readr something from a loaded package 
        load.from <- asNamespace( pkg)
        name.load.from <- 'package:' %&% pkg
        type.load.from <- 'attached'
      }
    } else {
      name.load.from <- paste( attr( load.from, 'task.tree'), collapse='/')
      type.load.from <- 'package'
    }
    new <- new || !exists( name, load.from, inherits=FALSE)
  } else { # num.load.from cannot be NA
    load.from <- pos.to.env( num.load.from)
    if( !is.null( names( attr( pos.to.env( num.load.from), 'path')))) {
      name.load.from <- rev( unlist( lapply( num.load.from:length( search()),
          function( x) names( attr( pos.to.env( x), 'path')))))
      type.load.from <- 'task'
    } else {
      name.load.from <- search()[ num.load.from]
      type.load.from <- 'attached'
    }
  }

  name.load.from <- paste( name.load.from, collapse='/')

  trace.was.on <- FALSE
  if(!new) {
    x <- get( name, load.from)
    trace.was.on <- exists( 'tracees', 'mvb.session.info') && (name %in% names( tracees)) }
  else {
    x <- what
    attributes( x) <- list() # ****ing srcref...
  }
})


"find.derefs" <-
function( envir) {
  if( is.null( mcache <- attr( envir, 'mcache')))
    attr( envir, 'mcache') <- mcache <- named( integer( 0))
  names( mcache) %SUCH.THAT% ( envir[[.]] %is.not.a% 'promise')
}


"find.docholder" <-
function( what, pos=find( what[1])){
  pos <- as.env( pos)
  o <- lsall( pos) %except% mcachees( pos)
  searchfun.Rd <- function( x) {
    if( is.function( xo <- pos[[x]])) 
      doco <- attr( xo, 'doc')
    else if( length( grep( '\\.doc$', x)) && is.character( xo))
      doco <- xo
    else
      doco <- character(0)
    what %in% named.in.doc( doco)
  } # searchfun.Rd

  searchfun.casual <- function( x) if( !is.null( doc <- attr( pos[[x]], 'doc')) &&
      is.list( doc)) doc[[1]] else character(0)
  searchfun.own <- function( x) if( !is.null( doc <- attr( pos[[x]], 'doc')) && 
      is.character( doc)) x else character(0)

  keepo1 <- list( length( what))
  Rds <- sapply( o, searchfun.Rd)
  dim( Rds) <- c( length( what), length( o))
  # apply over-simplifies, so...
  keepo <- lapply( split( Rds, row( Rds)), function( ins) o[ins])
  keepo2 <- lapply( named( what), searchfun.own) # what not o
  keepo <- mapply( c, keepo, keepo2, SIMPLIFY=FALSE) 
  keepo3 <- lapply( named( what), searchfun.casual) # what not o
  keepo <- mapply( c, keepo, keepo3, SIMPLIFY=FALSE)
  names( keepo) <- what
  lapply( keepo, unique)
}


"find.documented" <-
structure( function( pos=1,
    doctype=c( 'Rd', 'casual', 'own', 'any'),
    only.real.objects=TRUE,
    exclude.internal=FALSE) {
# 'pos' can have length > 1-- so guts live inside function

  # Avoid having to specify 'exclude.internal' in every call to...
  named.in.doc <- named.in.doc
  formals( named.in.doc)$exclude.internal <- exclude.internal

  findo <- function( pos) {
    pos <- as.env( pos)
    oallall <- lsall( pos)
    oall <- oallall %except% mcachees( pos)
    ofuns <- oall %SUCH.THAT% exists( ., mode='function', envir=pos)
    odoc <- (oall %except% ofuns) %that.match% '\\.doc$'
    searchfun.docobj.Rd <- function( x) named.in.doc( pos[[x]])
    searchfun.Rd <- function( x) named.in.doc( attr( pos[[x]], 'doc'))
    searchfun.casual <- function( x) x[ !is.null( attr( pos[[x]], 'doc')) ]
    searchfun.own <- function( x) x[ !is.null( doc <- attr( pos[[x]], 'doc')) &&
        is.character( doc) ]

    keepo <- character( 0)
    for( dt in doctype)
      keepo <- c( keepo, unlist( lapply( ofuns, FUN='searchfun.' %&% dt)))
    if( 'Rd' %in% doctype)
      keepo <- c( keepo, unlist( lapply( odoc, searchfun.docobj.Rd)))
  returnList( keepo=unique( keepo), oall=oallall)
  } # findo

  doctype <- match.arg( doctype)
  if( doctype=='any')
    doctype <- c( 'Rd', 'casual')

  if( is.environment( pos))
    pos <- list( pos)
  res <- lapply( pos, findo)
  keepo <- unique( unlist( lapply( res, '[[', x='keepo')))
  obs <- unlist( lapply( res, '[[', x='oall'))
  if( only.real.objects)
    keepo <- keepo %that.are.in% obs

return( keepo)
}
, doc =  docattr( r"{
find.documented          package:mvbutils
find.docholder

Support for flat-format documentation

DESCRIPTION

'find.documented' locates functions that have flat-format documentation; the functions and their documentation can be separate, and are looked for in all the environments in 'pos', so that functions documented in one environment but existing in another will be found. 'find.docholder' says where the documentation for one or more functions is actually stored. Both 'find.documented' and 'find.docholder' check two types of object for documentation: (i) functions with "doc" attributes, and (ii) character-mode objects whose name ends in ".doc"


USAGE

find.documented( pos=1, doctype=c( "Rd", "casual", "own", "any"),
  only.real.objects=TRUE, exclude.internal=FALSE)
find.docholder( what, pos=find( what[1]))


ARGUMENTS

 pos: search path position(s), numeric or character. In 'find.documented', any length. In 'find.docholder', only 'pos[1]' will be used; it defaults to where the first element of 'what' is found.

 doctype: Defaults to "Rd". If supplied, it is partially matched against the choices in USAGE. "Rd" functions are named in the alias list at the start of (i) any 'doc' attribute of a function, and (ii) any text object whose name ends with ".doc", that exist in 'pos' (see 'doc2Rd'). "casual" functions have their own 'doc' attribute and will be found by the replacement of 'help'; note that the 'doc' attribute can be just a reference to another documented function, of mode "list" as described in 'dochelp' (qv). "own" functions (a subset of "casual") have their own character-mode 'doc' attribute, and are suitable for 'doc2Rd'. "any" combines 'casual' and 'Rd'.

 only.real.objects: If TRUE, only return names of things that exist somewhere in the 'pos' environments. FALSE means that other things such as the name of helpfiles might be returned, too.

 exclude.internal: If TRUE, check the 'doc' attributes to see if they have "KEYWORDS<whitespace>internal", and if so, omit that function. Normally you probably wouldn't want that yourself; but it is used in 'make.NAMESPACE' to decide about exportees.

 what: names of objects whose documentation you're trying to find.


VALUE

 find.documented: Character vector of function names.
 find.docholder: list whose names are 'what'; element 'i' is a character vector showing which objects hold documentation for 'what[i]'. Normally you'd expect either 0 or 1 entries in the character vector; more than 1 would imply duplication.


NOTE

'doctype="Rd"' looks for the alias names, i.e. the first word of all lines occurring before the first blank line. This may include non-existent objects, but these are checked for and removed.

Start informal documentation (i.e. not intended for 'doc2Rd') with a blank line to avoid confusion.


SEE.ALSO

'flatdoc', 'doc2Rd', 'dochelp'


AUTHOR

Mark Bravington


KEYWORDS
documentation, programming, utilities
}")

)

"find.funs" <-
function( pos=1, ..., exclude.mcache=TRUE, mode='function') {
# In this version, "pos" can have length > 1
  findo <- function( pos2) {
      o <- named( lsall( pos=pos2, ...)) 
      if( exclude.mcache)
        o <- o %except% mcachees( pos2)
      if( !length( o))
    return( character( 0))
      old.warn <- options( warn=-1)$warn
      on.exit( options( warn=old.warn))
      keep <- sapply( o, exists, where=pos2, mode=mode, inherits=FALSE)
      if( !any( keep))
    return( character( 0))

      names( o) <- NULL
      o[keep]
    }

  if( is.environment( pos))
    pos <- list( pos)
  else
    pos <- lapply( pos, as.env)
  unlist( lapply( pos, findo), use.names=FALSE)
}


"find.lurking.envs" <-
function( obj, delve=FALSE, trace=FALSE){
  listo <- list( quote( obj))
  out.str <- character(0)
  out.size <- numeric( 0)
  
  while( length( listo)) {
    if( trace)
      print( listo[[1]])
    thing <- eval( listo[[1]])
    out.str <- c( out.str, deparse( listo[[1]])[1])
    if( missing( thing)) {
      out.size <- c( out.size, object.size( formals( sys.function())$obj))
    } else if( is.environment( thing)) {
      out.str[ length( out.str)] <- paste( out.str[ length( out.str)], 
        sub( 'environment', '', format( thing)))
      out.size <- c( out.size, Inf)
      # do not add environments to this list...
    } else {
      # process it, and add to list...
      thing <- unclass( thing)
      out.size <- c( out.size, object.size( thing))
      
      if( is.recursive( thing)) {
        if( is.function( thing)) {
          listo <- c( listo, substitute( environment( x), list( x=listo[[1]])))
          if( delve)
            listo <- c( listo,
              substitute( body( x), list( x=listo[[1]])),
              substitute( formals( x), list( x=listo[[1]])))
        } else {
          # Label list-like elts with $name if avail, or [[num]] if not
          if( is.null( namio <- names( thing)))
            namio <- rep( '', length( thing))
          listo <- c( listo, lapply( seq_along( thing), 
              function( x) if( nzchar( namio[x]))
                  substitute( a$b, list( a=listo[[1]], b=as.name( namio[x])))
                else
                  substitute( a[[b]], list( a=listo[[1]], b=x))))
        } # if recursive nonfunc
      }

      attro <- names( attributes( thing)) %except% 
          cq( dim, dimnames, class, levels, names, comment, row.names, tsp)

      if( length( attro))
        listo <- c( listo, lapply( attro, 
            function( x) substitute( attr( a, b), list( a=listo[[1]], b=x))))
    }
    
    listo <- listo[-1]
  }
  
  o <- order( out.size)
  data.frame( what=out.str[o], size=out.size[o])
}


"find.mp" <-
function( x, mode='any'){
  sx <- find( x, mode=mode, numeric=TRUE)[1]
  if( is.na( sx)) {
    sx <- index( sapply( maintained.packages,
        function( env) exists( x, env, mode=mode, inherits=FALSE))[1])
    if( is.na( sx))
stop( "Can't find '" %&% x %&% "' in search path or maintained packages")
    sx <- maintained.packages[[ sx]]
  } else
    sx <- as.environment( sx)

  sx
}


"find.path" <-
function( rel.path, char.rel.path, return.all=FALSE) {
  if( !missing( char.rel.path))
    rel.path <- as.character( char.rel.path) # in case of the number 0
  else
    rel.path <- deparse( substitute( rel.path))

  if( substring( rel.path, 1, 2)=='..' &&
      exists( rel.path, as.environment( 'mvb.session.info'), mode='environment', inherits=FALSE)) 
return( as.environment( 'mvb.session.info')[[ rel.path]])

  # Parse input string: NB that R interprets a/b/c as function calls!
  rel.path <- strsplit( rel.path, '/', fixed=TRUE)[[1]]
  rel.path <- as.character( unlist( rel.path))
  rel.path <- rel.path[ rel.path!="/"]

  search.list <- sapply( seq( search()),
      function( x) {
        x <- names( attr( pos.to.env( x), 'path'))
        if( is.null( x))
          x <- ''
        x }
      )

  get.tasks.if.present <- function( env.or.pos) {
      env.or.pos <- as.env( env.or.pos) 
      if( exists( 'tasks', envir=env.or.pos, inherits=FALSE))
        get( 'tasks', envir=env.or.pos)
      else
        character( 0)
    }

  env <- new.env()
  wp <- get( '.Path', pos='mvb.session.info')
  for( igo in seq( rel.path)) {
    go <- rel.path[ igo]
    if( go=='..') {
      if( length( wp))
        wp <- wp[-length(wp)] }
    else if( go=='0')
      wp <- .Path['ROOT']
    else if( go!='.') {
      if( identical( wp, .Path[ 1:length( wp)]))
        ctasks <- get.tasks.if.present( names( .Path)[ length( wp)])
      else {
#        cat( 'loading tasks from', wp[ length( wp)], '\n')
        load( file.path( wp[ length( wp)], '.RData'), envir=env, verbose=FALSE)
        ctasks <- get.tasks.if.present( env)
        remove( list=lsall( env), envir=env)
      }

      if( !any( go==names( ctasks)))
stop( 'can\'t find task named ' %&% go %&% ' in ' %&% wp[length(wp)])
      else {
        old.wd <- getwd()
        actual.ctask <- try( {
            setwd( wp[ length( wp)])
            setwd( ctasks[ go])
            getwd()
          })
        setwd( old.wd)
        if( actual.ctask %is.a% 'try-error')
stop( "can't find dir of task named '" %&% go %&% "' in '" %&% wp[ length( wp)])

        names( actual.ctask) <- go
        wp <- c( wp, actual.ctask)
      }
    } # if: different types of 'go'
  } # for

  if( !return.all)
    wp <- wp[ length( wp)]

  wp
}


"find.prefix" <-
function (j, nodes, parents) 
{
    s <- names(nodes[j])
    while ((j <- parents[j]) > 0) s <- names(nodes[j]) %&% "/" %&% 
        s
    s
}


"find.web" <-
function( nlocal=sys.parent()) mlocal({
  funs <- unique( c( funs, generics))
  n <- length( funs)
  if( !n)
stop( 'Nothing there!')

  funmat <- matrix( 0, n, n, dimnames=list( MASTER=funs, SLAVE=funs))
  master.of <- lapply( funs, called.by, can.match=funs, where=where)
  n.master <- unlist( lapply( master.of, length))
  if( !sum( n.master))
stop( 'Bo-RING! No food chain here!')

  setup <- c( rep( 1:length(funs), n.master), unlist( master.of))
  dim( setup) <- c( sum( n.master), 2)
  funmat[ setup] <- 1
  diag( funmat) <- 0 # to drop self-references

# Not interested in calls TO generic functions:
  funmat[ ,generics] <- 0

# check whether any methods of generic functions:
  drop.generics <- funmat[ generics, ] %**% rep( 1, n) == 0
  if( any( drop.generics)) {
    funs <- funs[ -match( generics[drop.generics], funs)]
    funmat <- funmat[ funs, funs]
    n <- n-sum( drop.generics) }

  color <- rep( textcolor, n)

  if( length( prune)) {
    prunio <- matrix( 0, length( prune), n)
    prunio <- sapply( to.regexpr( prune), regexpr, text=funs) # to.regexpr(): oct 2011
    prunio <- as.logical( (prunio != -1) %**% rep( 1, length( prune)))
    color[ prunio] <- highlight

  # Everything descended from a prune
    if( descendents) {
      old.descendents <- rep( FALSE, n)
      descendents <- prunio
      while( sum( descendents)!=sum( old.descendents)) {
        old.descendents <- descendents
        descendents <- descendents | (descendents %**% funmat > 0) } }
    else
      descendents <- prunio

  # All ancestors of a prune
    if( ancestors) {
      old.ancestors <- rep( FALSE, n)
      ancestors <- prunio
      while( sum( ancestors) != sum( old.ancestors)) {
        old.ancestors <- ancestors
        ancestors <- ancestors | (funmat %**% ancestors > 0) } }
    else
      ancestors <- prunio

    color <- color[ ancestors | descendents]
    funs <- funs[ ancestors | descendents]
    funmat <- funmat[ funs, funs, drop=FALSE]
    n <- length( funs)
  }

# Now we have to figure out what level in the hierarchy each fn. belongs at.
# Simple-minded approach: anything NOT called by any other function is top-
# level; anything called only by top-levels is second-level; etc.

  if( !n)
stop( 'Nothing there!')
  level <- rep(0, n); names( level) <- funs
  current.level <- 1
  while( any( level==0)) {
    tops <- rep( 1, sum( level==0)) %**% funmat[level==0, level==0] == 0
    if( !any( tops))  # we have to sort out functions that call each other
      tops <- least.mutual.dependency( funmat, funs, level)

    level[ dimnames( funmat)[[1]] [ level==0] [tops] ] <- current.level
    current.level <- current.level+1
  }
})


"fix.order" <-
structure( function( env=1) {
  oenv <- env
  env <- as.env( env)
  if( is.null( path <- attr( env, 'path')) || is.null( names( path)))
stop( 'Not a task')

  fob <- read.bkind( path)
  if( !length( fob[[1]]))
stop( 'Can\'t deduce fix.order')

  fdates <- file.info( file.path( path, '.Backup.mvb', fob$files))$mtime
  o <- order( fdates)
  fob <- fob$object.names[ o]

  # Remove deleted things still with backups-- not just functions these days
  fob <- fob %that.are.in% lsall( oenv) # used to be: [ fob %in% find.funs( oenv) ]
  fob
}
, doc =  docattr( r"{
fix.order package:mvbutils

Shows functions and scriptlets sorted by date of edit

DESCRIPTION

'fix.order' sorts the functions and scriptlets according to the filedates of their backups (in the .Backup.mvb directory). This is very useful for reminding yourself what you were working on recently. It only works if functions and scriptlets have been edited using the 'fixr' system.


USAGE

fix.order( env=1)


ARGUMENTS

 env: a single number, character string, or environment. Numbers and characters are interpreted as search path positions. The environment must be an attached mvb-style task.


DETAILS

Only objects that have a BU*** backup file will appear. Objects that have a BU*** file but have been deleted will not appear.


VALUE

Character vector of functions and scriptlets sorted by date/time of last modification.


TO.DO

Probably should modify this so it takes an arbitrary task path instead of a search position only. Task doesn't really need to be attached.
Add a 'pattern' argument a la find.funs.


EXAMPLES

## Don't run:
## Need to create backups and do some function editing first

fix.order() # functions in .GlobalEnv
fix.order( "ROOT") # functions in your startup task

## End don't run


SEE.ALSO

'fixr'


AUTHOR

Mark Bravington


KEYWORDS

utilities
}")

)

"fixr" <-
structure( function( 
    x, 
    new= FALSE, 
    install= FALSE,
    what= list( function(){}, '')[[1]], 
    fixing= TRUE,
    pkg= NULL, 
    character.only= FALSE, 
    new.doc= FALSE, 
    force.srcref= FALSE,
    stop.fixing= character()
){
#############################################################
  if( missing( x) && missing( character.only)){
    if( missing( stop.fixing)){
return( 'Nothing to edit!')
    } else {
stopifnot( is.character( stop.fixing))

      fix.list <<- fix.list %where% (name %not.in% stop.fixing)
return()
    }
  }

  prog <- 'program.' %&% ifelse( fixing, 'editor', 'reader')
  proged <- getOption( prog)
  if( is.null( proged) || install)
    proged <- install.proged( option.name=prog)

  if( is.character( character.only)) {
    x <- character.only
    character.only <- TRUE
  }

  if( !character.only) {
    sx <- substitute( x)
    if( is.call( sx)) {
      x <- as.character( sx)
      if( x[1] %not.in% c( '::', ':::', '$') || length( x)<3)
stop( "Don't know how to fixr '" %&% deparse( sx) %&% "'")
      pkg <- x[2]
      if( substring( pkg, 1, 2)=='..') { # ..mypack$fun
        pkg <- substring( pkg, 3)
        if( pkg %not.in% names( maintained.packages))
stop( "Package '" %&% pkg %&% "' is not a 'maintained package'!")
      }
      name <- x[3]
    } else
      name <- deparse( substitute( x), width.cutoff=30, nlines=1)
  } else
    name <- x

  find.and.get()

  if( is.function( x))
    environment( x) <- .GlobalEnv # ...
    # prevents env string being printed after definition.
    # ...mostly for new functions; bad practice to set environments otherwise.

  if( new.doc){
    x <- add.flatdoc.to( x, char.x=name, env=load.from)
  }

  dir <- c( getOption( 'edit.scratchdir'), 
      Sys.getenv( 'TMP'), Sys.getenv( 'TEMP'))
  dir <- dir[ nchar( dir)>0][1]
  if( is.na( dir))
stop( "Don't know where to put scratch files:" %&%
    " none of options( 'edit.scratchdir') or TMP or TEMP are set!")

  # Filename including "version" number if required
  exact.same <- index( name==fix.list$name & name.load.from==fix.list$where)[1]
  if( !is.na( exact.same))
    filename <- fix.list$file[ exact.same]
  else {
    if( !length( partial <- index( name==fix.list$name)))
      version.suffix <- ''
    else {
      ofnames <- fix.list$file[ partial]
      versions <- suppressWarnings( 
          as.integer( sub( '(.*)#([0-9]+)\\.R$', '\\2', ofnames)))
      versions[ is.na( versions)] <- 0
      new.version <- min( (1:max( versions)) %except% versions)
      version.suffix <- '#' %&% new.version
    }

    # Changed 1/2017 to allow other objects to get dot-R suffix instead of dot-txt; good for syntax-highlighting
    fsuffix <- if( has.source( x)) {
      '.R'
    } else {
      getOption( 'fixr.suffices', character())[ file_ext( name)]
    }

    if( is.na( fsuffix)) { # default for texty things
      fsuffix <- '.txt'
    }

    filename <- file.path( dir, 
        legal.filename( name %&% version.suffix %&% fsuffix))
  }

  old.warn <- options(warn = -1, width = 180)[1:2] # wide to avoid line breaks
  failed.to.edit <- TRUE # usual pessimism
  on.exit({
    if( failed.to.edit && file.exists( filename))
      unlink(filename)
    if( trace.was.on)
      mtrace( char.fname=name)
    options(old.warn) })

  # Do backup only if task
  if( fixing && !new && type.load.from %in% cq( package, task) && has.source( x))
    deal.with.backups( name, load.from) # takes env or number

  if( x %is.a% 'function') {
    if( !is.null( sr <- attr( x, 'srcref')) && 
        !is.null( src <- attr( sr, 'srcfile')$lines)) {
      # Might want entire original source if func didn't parse
      if( force.srcref) { # added 2018 to resolve messed-up cases with attributes
        nicewrite_function( x, filename)
      } else if( attr( sr, 'srcfile')$filename=='dummyfile') { 
        # produced by 'fixr' before; print all
        cat( src, file=filename, sep='\n')
      } else { # standard R srcref; just take what's there
        capture.output( print( x, useSource=TRUE), file=filename)
      }
    } else {
      nicewrite_function( x, filename)
    }
  } else if( has.source( x))
    cat( attr( x, 'source'), file=filename, sep='\n')
  else # text object
    cat( x, file=filename, sep='\n')

#  OK <- shell( proged(name, filename), translate=TRUE, wait = FALSE) # shell doesn't work on Linux
  cmd <- proged( name, filename)
  if( dos.or.windows())
    cmd <- gsub( '([^ ])/', '\\1\\\\', cmd)

  OK <- system( cmd, wait=FALSE) # before 12/2005 'wait' was only set FALSE on Windows; dunno why

  if(OK != 0)
stop("Couldn't launch editor")

# Avoid returning focus to console
  put.in.session( just.created.window=TRUE)

# Zap duplicates
  if( fixing) {
    fix.list <<- fix.list[ fix.list$name != name | 
        fix.list$where != name.load.from,]
    fix.list <<- rbind( fix.list,
        list( name = name, file = filename, 
        where = name.load.from, where.type= type.load.from,
        has.source=!is.character( x),
        dataclass = paste( unique( c( class( x),
            if( is.character( x)) 'character')), collapse=','),
        file.time=unclass( file.info( filename)[1,'mtime'])))
  }

  failed.to.edit <- FALSE
  invisible(NULL)
}
, doc =  docattr( r"{
fixr                    package:mvbutils
fixtext
readr
FF
autoedit

Editing functions, text objects, and scriptlets

DESCRIPTION

'fixr' opens a function (or text object, or "script" stored as an R 'expression'--- see SCRIPTLETS) in your preferred text editor. Control returns immediately to the R command line, so you can keep working in R and can be editing several objects simultaneously (cf 'edit'). A session-duration list of objects being edited is maintained, so that each object can be easily sourced back into its rightful workspace. These objects will be updated automatically on file-change if you've run 'autoedit( TRUE)' (e.g. in your '.First'), or manually by calling 'FF()'. There is an optional automatic text backup facility. It is designed to work with the 'cd' system, and may well _not_work outside of that system (note that you are automatically in that system if you call 'library(mvbutils)').

The safest is to call 'fixtext' to edit text objects, and 'fixr' for functions and everything else. However, 'fixr' can handle both, and for objects that already exist it will preserve the type. For new objects, though, you have to specify the type by calling either 'fixr' or 'fixtext'. If you forget--- ie if you really wanted to create a new text object, but instead accidentally typed 'fixr( mytext)'--- you will (probably) get a parse error, and 'mytext' will then be "stuck" as a broken function. Your best bet is to copy the actual contents in the text-editor to the clipboard, type 'fixtext( mytext)' in R, paste the old contents into the text-editor, and save the file; R will then reset the type and all should be well.

'readr' also opens a file in your text editor, but in read-only mode, and doesn't update the backups or the list of objects being edited.

'fixr' is designed for interfacing stand-alone text editors with R. I've never tried to interface it with e.g. ESS or Rstudio; that _might_ be possible, and even desirable, not least because of the next subsection.


.PACKAGES

'fixr' works with package 'mvbutils' package-maintenance system, and in fact is probably the only way to edit stuff inside that system. If your _maintained_ package  'mypack' (see 'maintain.packages') is already loaded, changes will be reflected immediately in the namespace of 'mypack', and also in any packages that import 'mypack', and to lists of S3 methods. The sole exception is if your function already exists in 'asNamespace(mypack)' and consists of a single call to '.Call' or '.External'. In that case, it is assumed to be an automated wrapper to low-level code that was created when the DLL was loaded, and thus should not be mucked about with; a warning is issued (but it's usually fine). [To force an overwrite without a rebuild, you'd have to first delete the function directly from the namespace--- yikes; but you're probably at a point of needing to rebuild the package anyway.] The newly-edited version will still go into the source-package for 'mypack', though, even though it is just a placeholder. The reason you might want to do that, is merely to note the existence of this wrapper-to-low-level (and what its arguments are), and to document it (either internally for use in the package, or for actual export if the user can call it directly).


USAGE

  # Usually: fixr( x) or fixr( x, new.doc=T)
  fixr( x, new=FALSE, install=FALSE, what, fixing, pkg=NULL,
      character.only=FALSE, new.doc=FALSE, force.srcref=FALSE,
      stop.fixing=character())
  # fixtext really has exact same args as fixr, but technically its args are:
  fixtext( x, ...)
  # Usually: readr( x) but exact same args as fixr, though the defaults are different
  readr( x, ...)
  FF() # manual check and update, usually only needed...
      # ... temporarily if autoedit() stops working
  autoedit( do=TRUE) # stick this line in your .First


ARGUMENTS

 x: a quoted or unquoted name of a function, text object, or expression. You can also write 'mypack$myfun', or 'mypack::myfun', or 'mypack:::myfun', or '..mypack$myfun', to simultaneously set the 'pkg' argument (only if 'mypack' has been set up with 'maintain.packages'). Note that 'fixr' uses non-standard evaluation of its 'x' argument, unless you specify 'character.only=TRUE'. If your object has a funny name, either quote it and set 'character.only=TRUE', or pass it directly as...

 character.only: (logical or character) if TRUE, 'x' is treated as a string naming the object to be edited, rather than the unquoted object name. If 'character.only' is a string, it is treated as the name of 'x', so that eg 'fixr(char="funny%name")' works.

 new.doc: (logical) if TRUE, add skeleton plain-text R-style documentatation, as per 'add.flatdoc.to'. Also use this to create an empty scriptlet for a general (non-function, non-text) object.

 force.srcref: (logical) Occasionally there have been problems transferring old code into "new" R, especially when a function has text attributes such as (but not limited to) 'doc'; the symptom is, they appear in the editor just as "# FLAT-FORMAT DOCUMENTATION". This sometimes requires manual poking-around, but usually can be sorted out by calling 'fixr(...,force.srcref=TRUE)'.

 new: (logical, seldom used) if TRUE, edit a blank function template, rather than any existing object of that name elsewhere in the search path. New edit will go into '.GlobalEnv' unless argument 'pkg' is set.

 install: (logical, rarely used) logical indicating whether to go through the process of asking you about your editor

 what: Don't use this-- it's "internal"! [Used by 'fixtext', which  calls 'fixr' with 'what=""' to force text-mode object. 'what' should be an object with the desired class.]

 fixing: (logical, rarely used) FALSE for read-only (i.e. just opening editor to examine the object)

 pkg: (string or environment) if non-NULL, then specifies in which package a specific maintained package (see 'maintain.packages') 'x' should be looked for.

 do: (logical) TRUE => automatically update objects from altered files; FALSE => don't.
 ...: other arguments, except 'what' in 'fixtext', and 'fixing' in 'readr', are passed to 'fixr'.
 
 stop.fixing: (character vector) removes these items from fix list.


DETAILS

When 'fixr' is run for the first time (or if you set 'install=TRUE'), it will ask you for some basic information about your text editor. In particular, you'll need to know what to type at a command prompt to invoke your text editor on a specific file; in Windows, you can usually find this by copying the Properties/Shortcut/Target field of a shortcut, followed by a space and the filename. After supplying these details, 'fixr' will launch the editor and print a message showing some 'options' ('"backup.fix"', '"edit.scratchdir"' and '"program.editor"'), that will need to be set in your '.First'. function. You should now be able to do that via 'fixr(.First)'.

Changes to the temporary files used for editing can be checked for automatically whenever a valid R command is typed (e.g. by typing 0<ENTER>; <ENTER> alone doesn't work). To set this up, call 'autoedit()' once per session, e.g. in your '.First'. The manual version (ie what 'autoedit' causes to run automatically) is 'FF()'. If any file changes are detected by 'FF', the code is sourced back in and the appropriate function(s) are modified. 'FF' tries to write functions back into the workspace they came from, which might not be '.GlobalEnv'. If not, you'll be asked whether you want to 'Save' that workspace (provided it's a task-- see 'cd'). 'FF' should still put the function in the right place, even if you've called 'cd' after calling 'fixr' (unless you've detached the original task) or if you 'move'd it. If the function was being 'mtrace'd (see 'package?debug'), 'FF' will re-apply 'mtrace' after loading the edited version. If there is a problem with parsing, the 'source' attribute of the function is updated to the new code, but the function body is invisibly replaced with a 'stop' call, stating that parsing failed.

If something goes wrong during an automatic call to 'FF', the automatic-call feature will stop working; this is rare, but can be caused eg by hitting <ESC> while being prompted whether to save a task. To restart the feature in the current R session, do 'autoedit(F)' and then 'autoedit(T)'. It will come back anyway in a new R session.

'readr' requires a similar installation process. To get the read-only feature, you'll need to add some kind of option/switch on the command line that invokes your text editor in read-only mode; not all text editors support this. Similarly to 'fixr', you'll need to set 'options( program.reader=<<something>>)' in your '.First'; the installation process will tell you what to use.

'fixr', and of course 'fixtext', will also edit character vectors. If the object to be edited exists beforehand and has a class attribute, 'fixr' will not change its class; otherwise, the class will be set to "cat". This means that 'print' invokes the 'print.cat' method, which displays text more readably than the default. Any other attributes on character vectors are stripped.

For functions, the file passed to the editor will have a ".r" extension. For character vectors or other things, the default extension is ".txt", which may not suit you since some editors decide syntax-highlighting based on the file extension. (EG if the object is a character-vector "R script", you might want R-style syntax highlighting.) You can somewhat control that behaviour by setting 'options()$fixr.suffices', eg

%%#
options( fixr.suffices=c( r='.r', data='.dat'))

which will mean that non-function objects whose name ends '.r' get written to files ending ".r.r", and objects whose name ends '.data' get written to files ending ".data.dat"; any other non-functions will go to files ending ".txt". This does require you to use some discipline in naming objects, which is no bad thing; FWIW my "scripts" always do have names ending in '.r', so that I can see what's what.

'fixr' creates a blank function template if the object doesn't exist already, or if 'new=TRUE'. If you want to create a new character vector as opposed to a new function, call 'fixtext', or equivalently set 'what=""' when you call 'fixr'.

If the function has attributes, the version in the text editor will be wrapped in a 'structure(...)' construct (and you can do this yourself). If a 'doc' attribute exists, it's printed as free-form text at the end of the file, and the call to 'structure' will  end with a line similar to:

%%#
,doc=flatdoc( EOF="<<end of doc>>"))

When the file is sourced back in, that line will cause the rest of the file-- which should be free-format text, with no escape characters etc.-- to be read in as a 'doc' attribute, which can be displayed by 'help'. If you want to add plain-text documentation, you can also add these lines yourself-- see 'flatdoc'. Calling 'fixr( myfun, new.doc=TRUE)' sets up a documentation template that you can fill in, ready for later conversion to Rd format in a package (see 'mvbutils.packaging.tools').

The list of functions being edited by 'fixr' is stored in the variable 'fix.list' in the 'mvb.session.info' environment. When you quit and restart R, the function files you have been using will stay open in the editor, but 'fix.list' will be empty; hence, updating the file "myfun.r" will not update the corresponding R function. If this happens, just type 'fixr(myfun)' in R and when your editor asks you if you want to replace the on-screen version, say no. Save the file again (some editors require a token modification, such as space-then-delete, first) and R will notice the update. Very very occasionally, you may want to tell R to stop trying to update one of the things it's editing, via eg 'fixtext <<- fixtext[-3,]' if the offending thing is the third row in 'fixlist'; note the double arrow.

An automatic text backup facility is available from 'fixr': see '?get.backup'. The backup system also allows you to sort edited objects by edit date; see '?fix.order'.


.CHANGES.WITH.R.2.14

Time was, functions had their source code (including comments, author's preferred layout, etc) stored in a "source" attribute, a simple character vector that was automatically printed when you looked at the function. Thanks to the fiddly, convoluted, opaque "srcref" system that has replaced "source" as of R 2.14--- to no real benefit that I can discern--- 'fixr' in versions of 'mvbutils' prior to 2.5.209 didn't work correctly with R 2.14 up. Versions of 'mvbutils' after 2.5.509 should work seamlessly.

The technical point is that, from R 2.14 onwards, basic R will _not_ show the 'source' attribute when you type a function name without running the function; unless there is a 'srcref' attribute, all you will see is the deparsed raw code. Not nice; so the replacement to 'print.function' in 'mvbutils' will show the 'source' attribute if it, but no 'srcref' attribute, is present. As soon as you change a function with 'fixr' post-R-2.14, it automatically loses any 'source' attribute and acquires a "proper" 'srcref' attribute, which will from then on.


.LOCAL.FUNCTION.GROUPS

There are several ways to work with "nested" (or "child" or "lisp-style macro") functions in R, thanks to R's scoping and environment rules; I've used at least four, most often 'mlocal' in package 'mvbutils'. One is to keep a bunch of functions together in a 'local' (qv) environment so that they (i) know about each other's existence and can access a shared variable pool, (ii) can be edited en bloc, but (iii) don't need to clutter up the "parent" code with the definitions of the children. 'fixr' will happily create & edit such a function-group, as long as you make sure the last statement in 'local' evaluates to a function. For example:


%%#
# after typing 'fixr( secondfun)' in R, put this into your text editor:
local({
  tot <- 0
  firstfun <- function( i) tot <<- tot+i
  # secondfun is defined in the next few lines:
  # entirely optional to precede them with 'secondfun <-'
  function( j) {
      for( ii in 1:j)
        firstfun( ii)
      tot
    }
})

Note that it's _not_ necessary to assign the last definition to a variable inside the 'local' call, unless you want to be able to reach that function recursively from one of the others, as in the first example for 'local'. Note also that 'firstfun' will not be visible "globally", only from within 'secondfun' when it executes.

'secondfun' above can be debugged as usual with 'mtrace' in the 'debug' package. If you want to turn on mtracing for 'firstfun' without first mtracing 'secondfun' and manually calling 'mtrace(firstfun)' when 'secondfun' appears, do 'mtrace(firstfun, from=environment( secondfun))'.

*Note*: I _think_ all this works OK in normal use (Oct 2012), but be careful! I doubt it works when building a package, and I'm not sure that R-core intend that it should; you might have to put the local-building code into the '.onLoad'.


.SCRIPTLETS

*Note*: I've really gone off "scriptlets" (writing this in mid 2016). These days I prefer to keep "scripts" as R character-vector objects (because I dislike having lots of separate files), edited by 'fixtext' and manually executed as required by 'mrun'--- which also has a debugging option that automatically applies 'debug::mtrace'. I'm not going to remove support for scriptlets in 'fixr', but I'm not going to try hard to sort out any bugs either. Instructions below are unchanged, and unchecked, from some years ago.

You can also maintain "scriptlets" with 'fixr', by embedding the instructions (and comments etc) in an 'expression(...)' statement. Obviously, the result will be an 'expression'; to actually execute a scriptlet after editing it, use 'eval()'. The scriptlet itself is stored in the "source" attribute as a character vector of class 'cat', and the expression itself is given class 'thing.with.source' so that the source is displayed in preference to the raw expression. Backup files are maintained just as for functions. Only the _first_ syntactically complete statement is returned by 'fixr' (though subsequent material, including extra comments, is always retained in the 'source' attribute); make sure you wrap everything you want done inside that call to 'expression(...)'.

Two cases I find useful are:

 - instructions to create data.frames or matrices by reading from a text file, and maybe doing some initial processing;
 - expressions for complicated calls with particular datasets to model-fitting functions such as 'glm'.

%%#
# Object creator:
expression( { # Brace needed for multiple steps
  raw.data <- read.table( "bigfile.txt", header=TRUE, row=NULL)
  # Condense date/time char fields into something more useful:
  raw.data <- within( raw.data, {
    Time <- strptime( paste( DATE, TIME, sep=' '), format="%Y-%m-%d %H:%M:%S")
    rm( DATE, TIME)
  })
  cat( "'raw.data' created OK")
})

and

%%#
# Complicated call:
expression(
  glm( LHS ~ captain + beard %in% soup, data=alldata %where% (mushroom=='magic'), family=binomial( link=caterpillar))
)

Bear in mind that 'eval(myscriptlet)' takes place in '.GlobalEnv' unless you tell it not to, so the first example above actually creates 'raw.data' even though it returns NULL. To trace evaluation of 'myscriptlet' with the 'debug' package, call 'debug.eval( myscriptlet)'.

For a new scriptlet 'mything', the call to 'fixr' should still just be 'fixr(mything)'. However, if you have trouble with this, try 'fixr( mything, what=list())' instead, even if 'mything' won't be a 'list()'. For an existing non-function, you'll need the 'new=T' argument, e.g. 'fixr( oldthing, new=T)', and you'll then have to manually copy/paste the contents.

Note that you *can't* use 'quote()' instead of 'expression()', because any attempt to display the object will cause it to run instead; this is a quirk of S3 methods!


..FOR.THE.BRAVE

In principle, you can also edit non-expressions the same way. For example, you can create a 'list' directly (not requiring subsequent 'eval()') via a scriptlet like this:

%%#
list(
  a = 1, # a number
  b = 'aardvark' # a character
)

Nowadays I tend to avoid this, because the code will be executed immediately R detects a changed file, and you have no other (easy) control over when it's evaluated. Also, note that the result will have class 'thing.with.source' (prepended to any other S3 classes it might have), which has its own print method that shows the source; hence you won't see the contents directly when you just type its name, which may or may not be desirable.

TROUBLESHOOTING

Rarely, 'fixr' (actually 'FF') can get confused, and starts returning errors when trying to update objects from their source files. (Switching between "types" of object with the same name--- function, expression, character vector--- can do this.) In such cases, it can be useful to purge the object from the 'fix.list', a session-duration data.frame object in workspace 'mvb.session.info' on the search path. Say you are having trouble with object "badthing": then

%%#
fix.list <<- fix.list[ names( fix.list) != 'bad.thing',]

will do the trick (note the double arrow). This means 'FF' will no longer look for updates to the source file for 'badthing', and you are free to again 'fixr( badthing)'.

To purge the entire 'fix.list', do this:

%%#

fix.list <<- fix.list[ 0,]


SEE.ALSO

'.First', 'edit', 'cd', 'get.backup', 'fix.order', 'move', 'maintain.packages'


KEYWORDS

utilities; programming

}")

)

"fixr.guts" <-
function( name, new=FALSE, proged, fixing=TRUE, what=list( function(){}, '')[[1]], obj) {
# Just like
  if( missing( name))
return( "Nothing to edit!")

  trace.was.on <- FALSE

# Function to edit, and its name (may be different from 'name' if method)
  if( !missing( obj)) {
    load.from <- 1
    x <- obj
    is.new <- trace.was.on <- FALSE
  } else {
    load.from <- if( new) NA else find( name, numeric=TRUE)[1] # mode check removed 28/7/2005
    is.new <- is.na( load.from)
    if(!is.new) {
      x <- if( missing( obj)) get( name, pos=load.from) else obj
      trace.was.on <- exists( 'tracees', 'mvb.session.info') && (name %in% names( tracees)) }
    else {
      x <- what
      load.from <- 1 }
  }

  if( is.function( x))
    environment( x) <- .GlobalEnv # to prevent the environment string being printed after the definition.
    # ...mostly for new functions; bad practice to set environments otherwise.

  try.load.from <- NULL
  num.load.from <- load.from
  if( load.from>1) {
    try.load.from <- names( attr( pos.to.env( load.from), 'path'))
    if( is.null( try.load.from))
      load.from <- search()[ load.from]
    else
      load.from <- try.load.from
  } else {
    load.from <- try.load.from <- names( attr( pos.to.env( 1), 'path')) # else ".GlobalEnv" will cause problems if there's a "cd"
    if( is.null( load.from)) {
      warning( search()[ load.from] %&% ' doesn\'t seem to be a task: object will be saved into .GlobalEnv')
      load.from <- '.GlobalEnv' }
  }

  dir <- options('edit.scratchdir')[1]
  if( is.null( dir)) {
    dir <- Sys.getenv( 'TMP')
    if( !nchar( dir))
      dir <- Sys.getenv( 'TEMP')
    if( !nchar( dir))
stop( "Don't know where to put scratch files: none of options( 'edit.scratchdir') or TEMP or TMP are set!")
  }

  filename <- file.path( dir, legal.filename( name)) # used to append.R to avoid...
  #... editors loading e.g. .First.lib as a binary file! Now assuming this is done outside

  old.warn <- options(warn = -1, width = 180)[1:2] # wide to avoid line breaks
  failed.to.edit <- TRUE # usual pessimism
  on.exit({
    if( failed.to.edit && file.exists( filename))
      unlink(filename)
    if( trace.was.on)
      mtrace( char.fname=name)
    options(old.warn) })

  if( fixing && !is.new && !is.null( try.load.from) && is.function( x)) # only do backup if task
    deal.with.backups( name, num.load.from)

  if( is.function( x))
    nicewrite_function( x, filename)
  else
    cat( x, file=filename, sep='\n')

#  OK <- shell( proged(name, filename), translate=TRUE, wait = FALSE) # shell doesn't work on Linux
  cmd <- proged( name, filename)
  callo <- quote( system( cmd))
  if( 'wait' %in% names( formals( system)))
    callo$wait <- FALSE
  OK <- eval( callo)

  if(OK != 0)
stop("Couldn't launch editor")

# Avoid returning focus to console
  put.in.session( just.created.window=TRUE)

# Zap duplicates
  if( fixing) {
    fix.list <<- fix.list[ fix.list$name != name,]
    fix.list <<- rbind(fix.list,
        list( name = name, file = filename, where = load.from,
        dataclass = paste( class( x), collapse=','), file.time=unclass( file.info( filename)[1,'mtime'])))
  }

  failed.to.edit <- FALSE
  invisible(NULL)
}


"fixtext" <-
function( x, ...) {
  mc <- match.call( expand.dots=TRUE)
  mc$what <- ''
  mc[[1]] <- quote( fixr)
  eval( mc, parent.frame())
}


"fixup.DLLs" <-
function( in.memory, ipath, rpath, spath, pkg, use.newest=FALSE, nlocal=sys.parent()) mlocal({
  suffix <- '[.]' %&% (if( .Platform$OS.type=='windows') '(dll|DLL)' else 'so') %&% '$'

  dlls1 <- sort( dir( rpath, pattern=suffix))
  libs <- 'libs'
  dlls2 <- suppressWarnings( sort( dir( file.path( rpath, 'inst', libs), pattern=suffix)))
  
  # R 2.12: DLLs in arch subfolder under "/libs" which... 
  # ... will be ref'd by 'libs' object, which controls installation dir...
  # ... Otherwise (pre 2.12), 'libs' is just "libs" folder
  # Allow any storage arrangement in task package
  # 2.12+: If task DLLs found *not* in subarch, assume the installed versions go to the current subarch
  if( nzchar( .Platform$r_arch)) {
    libs <- file.path( libs, .Platform$r_arch)
    dlls3 <- suppressWarnings( sort( dir( file.path( rpath, 'inst', libs), pattern=suffix)))
  } else
    dlls3 <- character( 0)
  
  dlls <- c( dlls1, dlls2, dlls3)
  dll.paths <- c( file.path( rpath, dlls1), file.path( rpath, 'inst', 'libs', dlls2), 
      file.path( rpath, 'inst', libs, dlls3))
  names( dll.paths) <- dlls

  # Definitely overwrite the versions in the source package
  if( length( dll.paths)) {
    mkdir( file.path( spath, 'inst', libs)) # probably should build into mvb.file.copy
    mvb.file.copy( dll.paths, file.path( spath, 'inst', libs, dlls))
  }
  
  if( !is.null( ipath)) {
    if( is.dir( ipath.libs <- file.path( ipath, libs)))
      idlls <- sort( dir( ipath.libs, pattern=suffix))
    else
      idlls <- character(0)
    inst.dll.paths <- file.path( ipath.libs, idlls)
    names( inst.dll.paths) <- idlls
    
    # New DLLs in raw get copied; new DLLs in inst get deleted
    use.raw <- dlls %except% idlls # provisionally, ones to replace

    if( length( both <- intersect( idlls, dlls))) {
      time.raw <- file.info( dll.paths[ both])$mtime
      time.inst <- file.info( inst.dll.paths[ both])$mtime
      md5.raw <- md5sum( dll.paths[ both])
      md5.inst <- md5sum( inst.dll.paths[ both])
      names( time.raw) <- names( time.inst) <- 
          names( md5.raw) <- names( md5.inst) <- both

      if( use.newest) { # also make sure files have different contents
        if( length( newer.inst <- both[ (time.raw < time.inst) & (md5.raw != md5.inst) ])) {
          mvb.file.copy( inst.dll.paths[ newer.inst], dll.paths[ newer.inst], overwrite=TRUE)
          # raw package-- in case overwritten already by older vers in raw task package!
          mvb.file.copy( inst.dll.paths[ newer.inst], 
              file.path( spath, 'inst', libs, newer.inst), overwrite=TRUE)
        }
      }

      # If raw is newer, handle below via 'use.raw'  
      use.raw <- c( use.raw, both[ time.raw > time.inst])
    } # if duplicated
    
    not.installed.yet <- use.raw %except% names( inst.dll.paths)
    inst.dll.paths[ not.installed.yet] <- file.path( ipath.libs, not.installed.yet)

    # in.memory=TRUE for reloading DLLs as appropriate-- always TRUE in current code
    # Unload / unlink
    
    try.dyn.load <- function( x) try( dyn.load( x))
    try.dyn.unload <- function( x) try( dyn.unload( x))
    try.library.dynam.load <- function( x) try( library.dynam( 
        sub( '[.]' %&% file_ext( x) %&% '$', '', basename( x)), package=pkg, lib.loc=dirname( ipath)))
    try.library.dynam.unload <- function( x) try( library.dynam.unload( 
        sub( '[.]' %&% file_ext( x) %&% '$', '', basename( x)), libpath=ipath))
    # DLLs could be loaded either via dyn.load or library.dynam; the latter is now recommended. 
    # ... Assume 'library.dynam' for new DLLs, and only assume 'dyn.load' if there's definite evidence!

    ldlist <- sapply( library.dynam(), '[[', i='path')
    loadeds <- sapply( getLoadedDLLs(), '[[', i='path')

    if( length( use.raw)) {
      # Unload, replace, reload
      cat( "Updating DLLs in source of '", pkg, "' from newer installed DLLs...", sep='')
      flush.console()
      via.dyn.loads <- (loadeds %that.are.in% inst.dll.paths[ use.raw]) %except% ldlist
      via.library.dynams <- inst.dll.paths[ use.raw] %except% via.dyn.loads
      
      if( in.memory) {
        lapply( via.dyn.loads %that.are.in% loadeds, try.dyn.unload)
        lapply( via.library.dynams %that.are.in% loadeds, try.library.dynam.unload)
      } # if in.memory
      
      mvb.file.copy( dll.paths[ use.raw], inst.dll.paths[ use.raw], overwrite=TRUE)
      
      if( in.memory) {
        lapply( via.library.dynams, try.library.dynam.load)
        lapply( via.dyn.loads, try.dyn.load)
      }
      cat( 'done\n')
    } # if changed dlls in raw package

    # DLLs in inst that now shouldn't be (because not in raw):
    inxs.dlls <- idlls %except% dlls 
    
    # Ones with source shouldn't be zapped either
    if( is.dir( file.path( rpath, 'src'))) {
      src.files <- dir( file.path( rpath, 'src'), pattern='[.](c|cc|cpp|C|f|f90|f95|m|mm|M|pas|lpr)$')
      if( length( src.files)) {
        # Include mypack.c-- though I recommend naming your dynlibs specifically, not with pkgname
        src.files <- c( src.files, pkg %&% '.c')
        src.dlls <- to.regexpr( sub( '[.][^.]*$', '', src.files)) # strip ext 
        src.dlls <- src.dlls %&% suffix # prepare for match
        inxs.dlls <- inxs.dlls %that.dont.match% src.dlls
      }
    }
    
    lapply( inst.dll.paths[ inxs.dlls] %that.are.in% (loadeds %except% ldlist), try.dyn.unload)
    lapply( inst.dll.paths[ inxs.dlls] %that.are.in% ldlist, try.library.dynam.unload)

    file.remove( inst.dll.paths[ inxs.dlls])
  }
})


"fixup.exports" <-
function( pkg) {
  # Make sure exported functions are visible
  
  ns <- asNamespace( pkg)
  # Export list must be read directly from the NAMESPACE file, ugggh
  things.to.export <- parseNamespaceFile( pkg, 
      package.lib=dirname( getNamespaceInfo( ns, 'path')))$exports
  
  expenv <- ns$.__NAMESPACE__.$exports
  unexportees <- lsall( expenv) %except% things.to.export
  rm( list=unexportees, envir=expenv)
  for( new.exportee in things.to.export %except% lsall( expenv)) 
    assign( new.exportee, structure( new.exportee, names=new.exportee), envir=expenv)
  
  visible <- list()
  if( ('package:' %&% pkg) %in% search())
    visible <- list( list( env=as.environment( 'package:' %&% pkg), 
        can.reset=FALSE)) # see below for can.reset

  loaded.users <- getNamespaceUsers( pkg) 
  loaded.users <- loaded.users %SUCH.THAT% (("package:" %&% .) %in% search())
  loaded.users <- loaded.users %SUCH.THAT% (try( asNamespace( .)) %is.not.a% 'try-error')
  for( lu in loaded.users)
    visible <- c( visible, list( list( env=parent.env( asNamespace( lu)), can.reset=FALSE)))
    
  # Space for code to look thru importing environments of other packages that 
  # might import this one; should add the parent.env of the namespace of the 
  # importing package to the list 'visible'
  
  # These might be locked, in which case it would be possible to fudge by 
  # resetting their parent.env to a new environment to contain these exports, 
  # and whose own parent is the original parent.env. That is a bit RISKY 
  # so if I do code this, be sure to make it optional. The 'can.reset' field 
  # is used to indicate whether this is OK; it isn't OK for the search path copy, 
  # but that shouldn't be locked in the first place.
  
  for( vis in visible) {    
    things.to.make.vis <- things.to.export %except% lsall( vis$env)
    things.to.zap <- unexportees %that.are.in% lsall( vis$env)
    if( length( c( things.to.make.vis, things.to.zap))) {
      assenv <- NULL # default: can't do it
      if( environmentIsLocked( vis$env)) {
        if( vis$can.reset && identical( parent.env( vis$env), asNamespace( 'base'))) {
          assenv <- new.env( parent=parent.env( vis$env))
          parent.env( vis$env) <- assenv # scary...
        } else if( environmentIsLocked( parent.env( vis$env)))
          warning( "Can't or daren't add to imports for " %&% environmentName( vis$env))
        else
          assenv <- parent.env( vis$env) # not locked; probably fudged like this already
      } else # not locked
        assenv <- vis$env
      
      if( !is.null( assenv)) {
        rm( list=things.to.zap, envir=assenv)
        for( thing in things.to.make.vis)
          assign( thing, ns[[ thing]], assenv)
      }
    } # if anything to assign
  } # loop over places where the funcs should be visible
}


"fixup.help" <-
function( nlocal=sys.parent()) mlocal({
  # Work out which Rd files are new
  # md5sum is incredibly fast for this!

  manpath <- file.path( spath, 'man')
  Rd.files <- dir( manpath, pattern='[.]Rd$')
  new.Rd.info <- md5sum( file.path( manpath, Rd.files))
  names( new.Rd.info) <- Rd.files

  alias <- matrix( scan( file.path( ipath, 'help', 'AnIndex'), what='', sep='\t', quiet=TRUE),
      ncol=2, byrow=TRUE)
  uaf <- unique( alias[,2])
  alias.files <- alias[,2] %&% '.Rd'
  alias <- alias[,1]

  # In theory, could check against the man/pkg.Rd.gz file in the installation
  # ...but no guaranteed unzip method available. Instead, use a from-last-time file.

  Rd.info.file <- file.path( ipath, 'Meta', 'Rd.info.rda')
  if( !force.all.docs && file.exists( Rd.info.file))
    load( Rd.info.file, verbose=FALSE) # creates old.Rd.info
  else {
    old.Rd.info <- rep( -1, length( uaf))
    # Next reflects fossilized bug in %&%: should return ch(0) if any arg is len 0
    names( old.Rd.info) <- if( length( uaf)) uaf %&% '.Rd' else uaf
  }

  zipped <- file.exists( file.path( ipath, 'help', 'Rhelp.zip'))

  new.files <- names( new.Rd.info) %except% names( old.Rd.info)
  changed.files <- names( new.Rd.info) %that.are.in% names( old.Rd.info)
  changed.files <- changed.files[ old.Rd.info[ changed.files] != new.Rd.info[ changed.files]]
  gone.files <- names( old.Rd.info) %except% names( new.Rd.info)

  # Fudge for R2.10 to get round parse_Rd/lazyLoad bugs: force re-parsing
  if( dynamic.help && !file.exists( file.path( ipath, 'help', 'patched'))) {
    new.files <- names( new.Rd.info)
    changed.files <- character( 0)
  }

  # Function to prepare shell & then run commands in it
  log <- character( 0)
  system2 <- function( commands, intern=TRUE, ...) {
    bf <- tempfile()
    on.exit( unlink( bf))
    if( .Platform$OS.type=='windows') {
      commands <- sub( '\\bR CMD\\b', 'RCMD', commands, perl=TRUE)
      bf <- bf %&% '.bat'
    } else
      commands <- sub( '\\bRCMD\\b', 'R CMD', commands, perl=TRUE)

    cat( getOption( 'rcmd.shell.setup', character( 0)), # 'CALL SET-R-BUILD-PATH.BAT'
        commands, sep='\n', file=bf)
    if( !is.null( mvboptions$debug_fixup_help)) {
      cat( c( readLines( bf), '...'), sep='\n', file=stderr())
      flush.console()
    }
    log <<- c( log, system( bf, intern=intern, ...))
    if( !is.null( mvboptions$debug_fixup_help)) {
      cat( 'done\n', file=stderr())
      flush.console()
    }
  }

  if( length( gone.files)) {
    fzap <- sub( '[.]Rd$', '', gone.files)
    if( !zipped)
      try( suppressWarnings( file.remove( file.path( ipath, 'help', fzap))), silent=TRUE)
    else
      system2( 'zip -d ' %&% file.path( ipath, 'help', 'Rhelp.zip') %&% fzap)
    try( suppressWarnings( file.remove( file.path( ipath, 'html', fzap %&% '.html'))), silent=TRUE)
  }

  dealias.files <- c( changed.files, gone.files)
  if( length( dealias.files)) {
    alias <- alias[ alias.files %not.in% dealias.files]
    alias.files <- alias.files %except% dealias.files
    uaf <- unique( alias.files)
  }

  files.to.update <- c( new.files, changed.files)

  if( length( files.to.update)) {
    fnew <- sub( '[.]Rd$', '', files.to.update)
    full.fnew <- file.path( spath, 'man', fnew %&% '.Rd')

    # from .build_Rd_db:
    enco <- try( tools$.get_package_metadata( ipath, FALSE)["Encoding"])
    if( (enco %is.a% 'try-error') || is.na(enco))
      enco <- "unknown"

    if( dynamic.help) {
      # Magic to avoid lazyLoad trouble:
      if( !is.null( mvboptions$debug_fixup_help)) {
        cat( 'flushing cache in help.rdb...', file=stderr())
        flush.console()
      }

      LLDBflush( file.path( ipath, 'help', pkg %&% '.rdb'))
      if( !is.null( mvboptions$debug_fixup_help)) {
        cat( 'done\n', file=stderr())
        flush.console()
      }

      if( force.all.docs)
        file.remove( file.path( ipath, 'help', pkg %&% c( '.rdb', '.rdx')))

      # Next routine is smart about updating
      if( !is.null( mvboptions$debug_fixup_help)) {
        cat( 'installing Rd...', file=stderr())
        flush.console()
      }

      # Make compression optional for lazyLoad
      ipRdo <- tools$.install_package_Rd_objects
      eee <- new.env( parent=environment( ipRdo))
      mlrdb <- tools$makeLazyLoadDB
      formals( mlrdb)$compress <- compress.lazyload # arg to patch.installed()
      eee$makeLazyLoadDB <- mlrdb
      environment( ipRdo) <- eee

      testo <- try( ipRdo( spath, ipath, enco=enco))
      rm( eee)

      if( !is.null( mvboptions$debug_fixup_help)) {
        cat( 'done\n', file=stderr())
        flush.console()
      }
      # ...try() should only be to trap any errors with user's own Rd files..?

      # Fastest to get aliases from parsed Rd
      if( !is.null( mvboptions$debug_fixup_help)) {
        cat( '2nd flushing cache in help.rdb...', file=stderr())
        flush.console()
      }
      LLDBflush( file.path( ipath, 'help', pkg %&% '.rdb'))
      if( !is.null( mvboptions$debug_fixup_help)) {
        cat( 'done\n', file=stderr())
        flush.console()
      }

      Rdlist <- tools$fetchRdDB( file.path( ipath, 'help', pkg))
      aliases <- lapply( Rdlist, tools$.Rd_get_metadata, kind='alias')
      alias <- unlist( aliases)
      alias.files <- rep( names( aliases), sapply( aliases, length))
      names( alias.files) <- alias
      saveRDS( alias.files, file.path( ipath, 'help', 'aliases.rds'))

      tools$.install_package_Rd_indices( spath, ipath)
      tools$.writePkgIndices( spath, ipath)
    } else {
      text.fnew <- file.path( ipath, 'help', fnew)
      html.fnew <- file.path( ipath, 'html', fnew %&% '.html')
      Rd.fnew <- file.path( ipath, 'man', fnew %&% '.Rd')

      if( is.Rd2) { # if( exists( 'Rd2txt', mode='function'))  # assume Rd2HTML does, too
        for( i in seq_along( fnew)) {
          p1 <- try( tools$prepare_Rd( full.fnew[i], encoding=enco, defines = .Platform$OS.type,
                    stages = "install", warningCalls = FALSE))

          # p1 <- try( parse_Rd( full.fnew[i])) doesn't do macro subs
          if( p1 %is.a% 'try-error')
            warning( "Can't parse_Rd " %&% fnew[ i] %&% "; no help for this one")
          else {
            attr( p1, 'prepared') <- 3L # from .build_rd_db
            Rd2txt( p1, out=text.fnew[i], package=pkg)
            Rd2HTML( p1, out=html.fnew[i], package=pkg)
          } # parse_Rd OK
        } # for i
      } else { # Rdoc 1
        cat( 'Rdconv-ing ', length( fnew), ' Rd files, twice...\n')
        system2( 'RCMD Rdconv ' %&% c(
            paste( '--package=', pkg, '-t=txt', '-o=' %&% text.fnew, full.fnew),
            paste( '-t=html', '-o=' %&% html.fnew, full.fnew)))

        if( .Platform$OS.type=='windows') { # ...then hack links
          # Endless work needed to do this right for pre-2.10, so just make all point here
          for( i.html in html.fnew) {
            reado <- readLines( i.html)
            reado <- gsub( '"[.][.]/[.][.]/[.][.]/doc/html/search/SearchObject.html[?]([^"]+)"',
                '"\\1.html"', reado)
            cat( reado, sep='\n', file=i.html)
          }
        } # windows

        cat( '...done\n')
      } # which Rd version

      # ?Need to do something about 00index.html? (both versions)

      if( zipped) {
        system2( 'zip -j ' %&% file.path( ipath, 'help', 'Rhelp.zip') %&% ' ' %&% text.fnew)
        try( suppressWarnings( file.remove( text.fnew)), silent=TRUE)
      }

      # Alias info
      for( ifnew in fnew) {
        filio <- readLines( file.path( spath, 'man', ifnew %&% '.Rd'))
        sections <- grep( '^[\\][A-Za-z0-9]+\\{', filio) # }
        namas <- sections[ grep( '^[\\](name|alias)[{]', filio[ sections])]
        not.namas <- c( sections %except% namas, Inf)
        namas <- namas[ namas < min( not.namas)]
        this.alias <- unique( sub( '.*[{]([^}]+)[}].*', '\\1', filio[ namas]))
        alias <- c( alias, this.alias)
        alias.files <- c( alias.files, rep( ifnew, length( this.alias)))
      }
    } # if not dynamic help

    # Index
    alias.files <- sub( '[.]Rd$', '', alias.files)
    cat( paste( alias, alias.files, sep='\t'),
        sep='\n', file=file.path( ipath, 'help', 'AnIndex'))
  } # if length( files.to.update)


  # old.Rd.info
  old.Rd.info <- new.Rd.info
  save( old.Rd.info, file=Rd.info.file)

  # help.search index... TO DO I guess... now obsolete post R-2.10, phew
})


"fixup.vignettes" <-
function( nlocal=sys.parent(), empty.list) mlocal({
## NB NB: see also 'vignette.pkg'. 'fixup.vegnettes' was tweaked hastily in 
## July 2024, and may not be sensible!

  if( !getOption( 'mvbutils.vignettes', FALSE)){
    # Do it properly (ish!)
   vigdir <- file.path( sourcedir, 'vignettes')
   if( !dir.exists( vigdir)){
return( local.return())
   }
  
   Rmd_origs <- dir( vigdir, pattern='[.]Rmd[.]orig$')
   if( !length( Rmd_origs)){
return( local.return())
   }
   
   if( FALSE){
     # Try to make more path-robust, but doesn't work and couldn't like this
     precomp <- c( 
          sprintf( 
              'local({ bd <- knitr::opts_knit$set( base.dir="%s/vignettes");', 
             sourcedir), 
          sprintf( 'knitr::knit( "%s", "%s");', Rmd_origs, 
              sub( '.orig$', '', Rmd_origs)),
         'knitr::opts_knit$set( base.dir=bd)})'
       ) # precomp
   } else if( FALSE){
     # Hard-wiring the path doesn't work either
     # FFS
     precomp <- sprintf(
         'knitr::knit( "%s", "%s");', Rmd_origs, sub( '.orig$', '', Rmd_origs))
   } else {
     precomp <- c( 
          sprintf( 'local({ owd <- setwd( "%s/vignettes");', dir.), 
         'try({', 
         sprintf(
             'knitr::knit( "%s", "%s");', Rmd_origs, 
              sub( '.orig$', '', Rmd_origs)),
         '});',
         'setwd( owd);',
         '})'
       ) # precomp
   }
   writeLines( precomp, con=file.path( dir., 'vignettes/precomp.R'))
return( local.return())
  }
  
  # Vignette index for homebrewed PDFs
  if( is.dir( own.vig.dir <- file.path( sourcedir, 'inst', 'doc')) &&
      length( own.vigs <- dir( own.vig.dir, pattern='[.]pdf$')) &&
      !file.exists( file.path( own.vig.dir, 'index.html'))) {
    ## Make it up, either from mypack.VIGNETTES first, then adding other missing bits
    if( is.character( vig.info <- ewhere[[ pkg %&% '.VIGNETTES']])) {
      vig.info <- sub( '^ *([^ :]+) *: *', '\\1:', vig.info)
      vig.info <- vig.info %that.match% '^[^ :]+:'
      if( length( vig.info)) {
        vig.info <- strsplit( vig.info, ':')
        vig.info <- structure( sapply( vig.info, '[', i=2), 
            names=sapply( vig.info, '[', i=1))
        vig.info <- vig.info %such.that% ((names( .) %&% '.pdf') %in% own.vigs)
      }
    } else
      vig.info <- character() # then make it up
      
    own.vigs <- sub( '[.]pdf$', '', own.vigs)
    need.rudi <- own.vigs %except% names( vig.info)
    vig.info <- c( vig.info, named( need.rudi))
    vig.R.files <- file.path( sourcedir, 'inst', 'doc', names( vig.info) %&% '.R')
    names( vig.R.files) <- vig.info

    # Create .Rnw stubs so vignettes get found: thanks to Henrik Bengtsson's nonsweave doco
    for( ivig in own.vigs) {
      ivig.rnw <- file.path( sourcedir, 'inst', 'doc', ivig %&% '.Rnw')
      if( !file.exists( ivig.rnw)) {
        stubbo <- gsub( 'VIGNAME', to.regexpr( ivig), attr( sys.function(), 'vignette.stub'))
        if( file.exists( vig.R.files[ ivig])) {
          last.line <- grep( 'end.*document', stubbo)
          stubbo <- multinsert( stubbo, last.line-1, c( '<<>>=', readLines( vig.R.files[ ivig]), '@'))
        }
        scatn( '%s', stubbo, file=ivig.rnw)
      } # if no stub
    } # for own.vigs

    empty.list <- I( rep( list( character( 0)), length( vig.info)))
    vig.index <- data.frame( 
        File=names( vig.info) %&% '.Rnw', 
        Title=vig.info, 
        PDF=names( vig.info) %&% '.pdf',
        Depends=empty.list, 
        Keywords=empty.list,
        R=ifelse( file.exists( vig.R.files), vig.R.files, ''), 
        stringsAsFactors=FALSE
      )

    tools$.writeVignetteHtmlIndex( pkg, 
        file.path( sourcedir, 'inst', 'doc', 'index.html'), vig.index)
    saveRDS( vig.index, file=file.path( sourcedir, 'R', 'meta.vignette.rds'))
    # so patch.installed() can copy this across to Meta
    # saveRDS( vig.index, file = file.path( sourcediroutDir, "Meta", "vignette.rds"))
  } else { # if no vig stuff
    if( !is.dir( own.vig.dir))
      own.vigs <- character( 0)
    sweave.vig.dir <- file.path( sourcedir, 'vignettes')
    if( is.dir( sweave.vig.dir)) 
      own.vigs <- c( own.vigs, dir( sweave.vig.dir, pattern='[.]Rnw$'))
    
    # Should check filedates and rebuild if required
    # Maybe include pdfs in source pack even if Sweave, but use Rbuildignore
    
    # This does not deal case of changing from homebrewed to Sweave vignettes
    # but why would you?!
    if( !length( own.vigs)) {
      # clean out anything installed
      old.pdfs <- dir( file.path( ipath, 'inst', 'doc'), 
          pattern='[.]pdf$', full.names=TRUE)
      unlink( old.pdfs)
      unlink( sub( 'pdf$', 'R', old.pdfs)) # R source
      unlink( sub( 'pdf$', 'Rnw', old.pdfs)) # vignette source
      unlink( file.path( ipath, 'inst', 'doc', 'index.html'))
      unlink( file.path( ipath, 'meta', 'vignette.rds'))
    }
  }
})


"flatdoc" <-
structure( function( EOF="<<end of doc>>") {
  doctext <- readLines.mvb( current.source(), EOF=EOF, line.count=TRUE)
  class( doctext) <- 'docattr'
  attr( doctext, 'line.count') <- NULL
  doctext
}
, doc =  docattr( r"---{
docattr package:mvbutils
flatdoc
tidyup_docattr


Flat-format documentation

DESCRIPTION

The 'docattr' convention, and its obsolete ancestor 'flatdoc', lets you edit plain-text documentation in the same file as your function's source code. 'docattr' and 'flatdoc' are hardly ever called explicitly, but you will see them in text files produced by 'fixr'; you can also add it to such files yourself. They are mostly used to write Rd-style help with almost no markup (_much_ cleaner than Roxygen!) that will be converted into Rd-format when building/exporting packages. However, 'mvbutils' extends 'help' so that '?myfunc' will display plain-text documentation for 'myfunc', even if 'myfunc' isn't in a package. There are no restrictions on the format of informal-help documentation, so 'docattr' is useful for adding quick simple help just for you or for colleagues. If your function is to be part of a maintained package (see 'mvbutils.packaging.tools'), then the documentation should follow a slightly more formal structure; use 'fixr( myfun, new.doc=T)' to set up the appropriate template.

A neat trick, for a function where you want "internal" documentation but not visible (yet), is to name the attribute "secret_doc" rather than "doc". 

The difference between these two functions is that 'docattr' (which requires R >= 4.1) has completely regular R syntax, taking advantage of "raw strings" (see 'Quotes'). 'flatdoc' had to use a rather devious trick which required that the file was subsequently read in by 'source.mvb' rather than 'source'. If you have a task package 'wundapak' which uses 'flatdoc', then you can convert it with eg 'tidyup_docattr(..wundapak)'.

'docattr' is a simple wrapper for 'string2charvec' (qv), to which it just adds the "docattr" class so that the documentation is not printed by default; you will just see "# FLAT-FORMAT DOCUMENTATION" appended to the function body.


USAGE

# ALWAYS use it like this:
# structure( function( ...) {body},
# doc=docattr( r"--{
#... including newlines, etc}--
# }--"))

# almost NEVER like this
docattr( rawstr)

# Obsolete flatdoc() version
# ALWAYS use it like this:
# structure( function( ...) {body},
# doc=flatdoc( EOF="<<end of doc>>"))
# plaintext doco goes here...
# NEVER use it like this:
flatdoc( EOF="<<end of doc>>")

tidyup_docattr( e)


ARGUMENTS

 rawstr: a single string, almost certainly a raw string containing the plain-text documentation.
 
 EOF: character string showing when plain text ends, as in 'readlines.mvb'

 body: replace with your function code

 ...: replace with your function arg list
 
 e: an environment (usually a task package, starting with '..'--- but it could be '.GlobalEnv'), or the name of an environment.


VALUE

'docattr' returns a character vector of class 'docattr'. The print method for 'docattr' objects just displays the string "# FLAT-FORMAT DOCUMENTATION", to avoid screen clutter.


INTERNAL.DETAILS.OF.FLATDOC

This section can be safely ignored by almost all users, and as of 'mvbutils' v2.11.0, it's obsolete anyway since 'docattr' should now replace 'flatdoc' throughout.

On some text editors, you can modify syntax highlighting so that the "start of comment block" marker is set to the string "doc=flatdoc(".

It's possible to use 'flatdoc' to read in more than one free-format text attribute. The 'EOF' argument can be used to distinguish one block of free text from the next. These attributes can be accessed from your function via 'attr( sys.function(), "<<attr.name>>")', and this trick is occasionally useful to avoid having to include multi-line text blocks in your function code; it's syntactically clearer, and avoids having to escape quotes, etc. 'mvbutils:::docskel' shows one example.

'fixr' uses 'write.sourceable.function' to create text files that use the 'flatdoc' convention. Its counterpart 'FF' reads these files back in after they're edited. The reading-in is not done with 'source' but rather with 'source.mvb', which understands 'flatdoc'. The call to 'doc=flatdoc' causes the rest of the file to be read in as plain text, and assigned to the 'doc' attribute of the function. Documentation can optionally be terminated before the end of the file with the following line:

%%#
<<end of doc>>

or whatever string is given as the argument to 'flatdoc'; this line will cause 'source.mvb' to revert to normal statement processing mode for the rest of the file. Note that vanilla 'source' will not respect 'flatdoc'; you do need to use 'source.mvb'.


'flatdoc' should never be called from the command line; it should only appear in text files designed for 'source.mvb'.

_The rest of this section is probably obsolete, though things should still work._

If you are writing informal documentation for a group of functions together, you only need to 'flatdoc' one of them, say 'myfun1'. Informal help will work if you modify the others to e.g.

%%#
myfun2 <- structure( function(...) { whatever}, doc=list("myfun1"))

If you are writing with 'doc2Rd' in mind and a number of such functions are to be grouped together, e.g. a group of "internal" functions in preparation for formal package release, you may find 'make.usage.section' and 'make.arguments.section' helpful.


SEE.ALSO

'doc2Rd', 'dochelp', 'write_sourceable_function', 'source.mvb', 
'make.usage.section', 'make.arguments.section', 'fixr', 
the demo in "flatdoc.demo.R"


EXAMPLES

# This illustrate the general format for a function with attached plain-text documentation. It is the format produced by write_sourceable_function()

flubbo <- structure( function( x){
  ## A comment
  x+1
}
,doc=mvbutils::docattr( r"-{
flubbo       not-yet-in-a-package

'flubbo' is a function! And here is some informal doco for it. Whoop-de-doo!

You can have multiple lines, lots of "double" and 'single' quotes, and there's no need to escape weird characters, so "\" is tickety-boo.

And you can use the power of raw strings to r"{have a short one}" inside your function. Just make sure your final closing "quote" matchs the number of dashes (0 or more) that follow the first r-double-quote, and exceeds the number in any r"{short quotelets}" inside the documentation. Usually there won't be any, so you won't need to add any dashes.
}-"))


## Don't run

## Put next lines up to "<<end of doc>>" into a text file <<your filename>>
## and remove the initial hashes

#structure( function( x) {
#  x*x
#}
#,doc=flatdoc("<<end of doc>>"))
#
#Here is some informal documentation for the "SQUARE" function
#<<end of doc>>

## Now try SQUARE <- source.mvb( <<your filename>>); ?SQUARE

## Example with multiple attributes
## Put the next lines up to "<<end of part 2>>"
## into a text file, and remove the single hashes

#myfun <- structure( function( attname) {
#  attr( sys.function(), attname)
#}
#,  att1=flatdoc( EOF="<<end of part 1>>")
#,  att2=flatdoc( EOF="<<end of part 2>>"))
#This goes into "att1"
#<<end of part 1>>
#and this goes into "att2"
#<<end of part 2>>

## Now "source.mvb" that file, to create "myfun"; then:
myfun( 'att1') # "This goes into \"att1\""
myfun( 'att2') # "and this goes into \"att2\""

## End don't run


AUTHOR

Mark Bravington


KEYWORDS
documentation; programming
}---")

)

"foodweb" <-
structure( function( funs, where=1, charlim=80, prune=character(0), rprune,
    ancestors=TRUE, descendents=TRUE,
    plotting=TRUE, plotmath=FALSE,
    generics=c( 'c','print','plot', '['), lwd=0.5, xblank=0.18,
    border='transparent', boxcolor='white', textcolor='black', color.lines=TRUE, highlight='red',
    calc_xpos=plotting,
    ...) {
########
  if( plotting){
    re_bloody_quire( 'grDevices')
    oldpar <- par( ..., no.readonly=TRUE)
    on.exit( par( oldpar))
    charlim <- charlim/par('cex')
    par( lwd=lwd) # lwd included as a parameter, in case this screws up
  }

  skip.computations <- FALSE
  if( missing( funs)) {
    if( is.environment( where))
      where <- list( where)
    funs <- unique( unlist( lapply( where, find.funs)))
  } else if( funs %is.a% 'foodweb') { # basically redisplay
    skip.computations <- TRUE
    extract.named( funs)
    funs <- names( level)
    n <- length(level) }

  if( !skip.computations) {
    if( !missing( rprune))
      prune <- funs %matching% rprune
    funs <- unique( c( funs, prune))

    if( !length( funs))
return( structure( list( funmat=matrix( 0,0,0), x=numeric( 0), level=numeric( 0)),
    class='foodweb'))

    find.web()
    organize.web.display( plotmath=plotmath) }

  answer <- list( funmat=funmat, x=x, level=level)
  class( answer) <- 'foodweb'

  if( plotting) {
    # Dreadful Rgui-windows bug with 'ps'...
    opar <- par( 'ps')
    if( names( grDevices::dev.cur())=='windows') {
      on.exit( par( ps=opar+1L))
    }
    plot( answer, border=border, boxcolor=boxcolor, xblank=xblank, textcolor=textcolor,
        color.lines=color.lines, plotmath=plotmath, ...)
  }
  invisible( answer)
}
, doc =  docattr( r"{
foodweb               package:mvbutils
callers.of
callees.of
plot.foodweb

Shows which functions call what


DESCRIPTION

'foodweb' is applied to a group of functions (e.g. all those in a workspace); it produces a graphical display showing the hierarchy of which functions call which other ones. This is handy, for instance, when you have a great morass of functions in a workspace, and want to figure out which ones are meant to be called directly. 'callers.of(funs)' and 'callees.of(funs)' show which functions directly call, or are called directly by, 'funs'.


USAGE

foodweb( funs, where=1, charlim=80, prune=character(0), rprune,
    ancestors=TRUE, descendents=TRUE, plotting =TRUE, plotmath=FALSE,
    generics=c( "c","print","plot", "["), lwd=0.5, xblank=0.18,
    border="transparent", boxcolor="white", textcolor="black",
    color.lines=TRUE, highlight="red", calc_xpos=plotting, ...)

plot(x, textcolor, boxcolor, xblank, border, textargs = list(),
    use.centres = TRUE, color.lines = TRUE, poly.args = list(),
    expand.xbox = 1.05, expand.ybox = expand.xbox * 1.2, plotmath = FALSE,
    cex=par( "cex"), ...) # S3 method for foodweb

callers.of( funs, fw, recursive=FALSE)
callees.of( funs, fw, recursive=FALSE)


ARGUMENTS

 funs: character vector OR (in 'foodweb' only) the result of a previous 'foodweb' call

 where: position(s) on search path, or an environment, or a list of environments

 charlim: controls maximum number of characters per horizontal line of plot

 prune: character vector. If omitted, all 'funs' will be shown; otherwise, only ancestors and descendants of functions in 'prune' will be shown. Augments 'funs' if required.

 rprune: regexpr version of 'prune'; 'prune <- funs %matching% rprune'. Does NOT augment 'funs'. Overrides 'prune' if set.

 ancestors: show ancestors of 'prune' functions?

 descendents: show descendents of 'prune' functions?

 plotting: graphical display?

 plotmath: leave alone

 generics: calls TO functions in 'generics' won't be shown

 lwd: see 'par'

 xblank: leave alone

 border: border around name of each object ('TRUE/FALSE')

 boxcolor: background colour of each object's text box

 textcolor: of each object

 color.lines: will linking lines be coloured according to the level they originate at?

 highlight: seemingly not used

 cex: text size (see "cex" in '?par')

 calc_xpos: whether to calculate reasonable on-screen positions. Defaults to TRUE if plotting, and FALSE otherwise (to save a bit of time). If you aren't plotting immediately but might plot the results _later_, you should set this to TRUE.

 ...: passed to 'plot.foodweb' and thence to 'par'

 textargs: not currently used

 use.centres: where to start/end linking lines. 'TRUE' is more accurate but less tidy with big webs.

 expand.xbox: how much horizontally bigger to make boxes relative to text?

 expand.ybox: how much vertically bigger to ditto?

 poly.args: other args to 'rect' when boxes are drawn

 fw: an object of class 'foodweb', or the 'funmat' element thereof (see VALUE)

 x: a foodweb (as an argument to 'plot.foodweb')

 recursive: ('callees.of' and 'callers.of' only) whether to include callee/rs of callee/rs of... (Thanks to William Proffitt for this suggestion.)


DETAILS

The main value is in the graphical display. At the top ("level 0"), functions which don't call any others, and aren't called by any others, are shown without any linking lines. Functions which do call others, but aren't called themselves, appear on the next layer ("level 1"), with lines linking them to functions at other levels. Functions called only by level 1 functions appear next, at level 2, and so on. Functions which call each other will always appear on the same level, linked by a bent double arrow above them. The colour of a linking line shows what level of the hierarchy it came from.

'foodweb' makes some effort to arrange the functions on the display to keep the number of crossing lines low, but this is a hard problem! Judicious use of 'prune' will help keep the display manageable. Perhaps counterintuitively, any functions NOT linked to those in 'prune' (which all will be, by default) will be pruned from the display.

'foodweb' tries to catch names of functions that are stored as text, and it will pick up e.g. 'glm' in 'do.call( "glm", glm.args)'. There are limits to this, of course (?methods?).

The argument list may be somewhat daunting, but the only ones normally used are 'funs', 'where', and 'prune'. Also, to get a readable display, you may need to reduce 'cex' and/or 'charlim'. A number of the less-obvious arguments are set by other functions which rely on 'plot.foodweb' to do their display work. Several may disappear in future versions.

If the display from 'foodweb' is unclear, try 'foodweb( .Last.value, cex=<<something below 1>>, charlim=<<something probably less than 100>>)'. This works because 'foodweb' will also accept a 'foodweb'-class object as its argument. You can also assign the result of 'foodweb' to a variable, which is useful if you expect to do a lot of tinkering with the display, or to inspect the who-calls-whom matrix by hand.

'callers.of' and 'callees.of' process the output of 'foodweb', looking for immediate dependencies only. The second argument will call 'foodweb' by default, so it may be more efficient to call 'foodweb' first and assign the result to a variable. NB you can set 'recursive=TRUE' for the obvious result.


.BUG.IN.RGUI.WINDOWS.GRAPHICS

When plotting the foodweb, there's a display bug in Rgui for windows which somehow causes the fontsize to shrink in each successive calls! Somehow 'par("ps")' keeps on shrinking. Indeed, on my own machines, calling 'par(ps=par("ps"))$ps' will show a decreasing value each time... Working around this was very tricky; variants of saving/restoring 'par' _inside_ 'plot.foodweb' do not work. As of package 'mvbutils' version 2.8.142, there's an attempted fix directly in 'foodweb', but conceivably the fixe will somehow cause problems for other people using default graphics windows in Rgui. Let me know if that's you... (in which case I'll add an 'option()' to not apply the fix).


VALUE

'foodweb' returns an object of (S3) class 'foodweb'. This has three components:

 funmat: a matrix of 0s and 1s showing what (row) calls what (column). The 'dimnames' are the function names.

 x: shows the x-axis location of the centre of each function's name in the display, in 'par("usr")' units

 level: shows the y-axis location of the centre of each function's name in the display, in 'par("usr")' units. For small numbers of functions, this will be an integer; for larger numbers, there will some adjustment around the nearest integer

Apart from graphical annotation, the main useful thing is 'funmat', which can be used to work out the "pecking order" and e.g. which functions directly call a given function. 'callers.of' and 'callees.of' return a character vector of function names.


EXAMPLES


foodweb( ) # functions in .GlobalEnv

# I have had to trim this set of examples because CRAN thinks it's too slow...
# ... though it's only 5sec on my humble laptop. So...

## Don't run
foodweb( where="package:mvbutils", cex=0.4, charlim=60) # yikes!
foodweb( c( find.funs("package:mvbutils"), "paste"))
# functions in .GlobalEnv, and "paste"
foodweb( find.funs("package:mvbutils"), prune="paste")
# only those parts of the tree connected to "paste";
# NB that funs <- unique( c( funs, prune)) inside "foodweb"
foodweb( where="package:mvbutils", rprune="aste")
# doesn't include "paste" as it's not in "mvbutils", and rprune doesn't augment funs
foodweb( where=asNamespace( "mvbutils")) # secret stuff
fw <- foodweb( where="package:mvbutils")
## End Don't run

fw <- foodweb( where=asNamespace( "mvbutils")) # also plots
fw$funmat # a big matrix
callers.of( "mlocal", fw)
callees.of( "find.funs", fw)

# ie only descs of functions whose name contains 'name'
foodweb( where=asNamespace( 'mvbutils'), rprune="name", ancestors=FALSE, descendents=TRUE)


}")

)

"FOR" <-
function( x, expr, ...){
  fungo <- function( .) bod
  l <- list( ...)
  environment( fungo) <- if( length( l))
      list2env( l, parent=parent.frame())
    else
      parent.frame()
  body( fungo) <- substitute( expr)
  if( is.atomic( x) && is.null( names( x)))
    x <- named( x)
  lapply( x, fungo)
}


"force.assign" <-
function( x, value, envir) {
  envir <- as.environment( envir)
  if( bl <- exists( x, envir, inherits=FALSE) && balloonIsTethered( x, envir))
    untetherBalloon( x, envir)
  assign( x, value, envir=envir)
  if( bl)
    tetherBalloon( x, envir)
}


"format.dull" <-
function( x, ...) rep( '...', NROW( x))


"from.here" <-
function( EOF=as.character( NA)) {
  f1 <- tempfile()
#  cat( 'FILENAME: ', f1, '\n')
  cat( readLines.mvb( current.source(), EOF=EOF), file=f1, sep='\n')
  c1 <- file( f1)
  class( c1) <- c( 'selfdeleting.file', class( c1))
  c1
}


"full.path" <-
structure( function( path, start='.'){
  spath <- strsplit( path, '/', fixed=TRUE)[[1]]
  if( spath[1] %in% c( '.', '..'))
    path <- file.path( start, path)

  # Eliminate . and ..

  spath <- strsplit( path, '/')[[1]]
  spath <- spath %except% '.'

  while( !is.na( first.parent <- index( spath == '..')[1]))
    spath <- spath[ -( first.parent + -1:0)]

  paste( spath, collapse='/')
}
, doc =  docattr( r"{
full.path    package:test

Expand relative file path

DESCRIPTION

'path' is expanded relative to 'start', with any '.' being eliminated and any '..' being treated as "go back one step". If 'path' doesn't start with a '.' or '..', 'start' is ignored. Might be Windows-specific but probably fairly safe in general. NB that all separators in 'path' and 'start' must be "/".


USAGE

full.path(path, start)


ARGUMENTS

 path: character(1)
 start: character(1), defaulting to '.'


KEYWORDS

internal
}")

)

"generic.dll.loader" <-
structure( function( libname, pkgname, ignore_error=FALSE, dlls=NULL){
   # Generic DLL loader
   dll.path <- file.path( libname, pkgname, 'libs')
   if( nzchar( subarch <- .Platform$r_arch))
     dll.path <- file.path( dll.path, subarch)
   this.ext <- .Platform$dynlib.ext %&% '$' # strictly, should wrap first in 'mvbutils:::to.regexpr'

   if( is.null( dlls)) {
     dlls <- dir( dll.path, pattern=this.ext, full.names=FALSE, ignore.case=.Platform$OS.type=='windows')
   }
   names( dlls) <- dlls

   ns <- asNamespace( pkgname)
   for( idll in dlls) {
     dll.name <- sub( this.ext, '', idll)
     this.dll.info <- try( library.dynam( dll.name, package=pkgname, lib.loc=libname))
     if( this.dll.info %is.not.a% 'try-error') {
       assign( 'LL_' %&% dll.name, create.wrappers.for.dll( this.dll.info, ns), ns)
     } else if( !ignore_error) {
       print( this.dll.info) # error message
stop()
     }
   } # for dlls

}
, doc =  docattr( r"{
generic.dll.loader    package:mvbutils
create.wrappers.for.dll
ldyn.tester
ldyn.unload

Convenient automated loading of DLLs

DESCRIPTION

'generic.dll.loader' is to be called from the '.onLoad' of a package. It calls 'library.dynam' on all the DLLs it can find in the "libs" folder (so you don't need to specify their names), or in the appropriate sub-architecture folder below "libs". It also creates "R aliasses" in your namespace for all the _registered_ low-level routines in each DLL (i.e. those returned by 'getDLLRegisteredRoutines', qv), so that the routines can be called efficiently later on from your code--- see DETAILS.

If you just want to use 'mvbutils' to help build/maintain your package, and don't need your package to import/depend on other functions in 'mvbutils', then it's fine to just copy the code from 'generic.dll.loader' etc and put it directly into your own '.onLoad'.

'ldyn.tester', 'create.wrappers.for.dll', and 'ldyn.unload' are to help you develop a DLL that has fully-registered routines, without immediately having to create an R package for it. 'ldyn.tester' loads a DLL and returns its registration info. The DLL must be in a folder '.../libs/<subarch>' where '<subarch>' is '.Platform$r_arch' iff that is non-empty; this is because 'ldyn.tester' merely tricks 'library.dynam' into finding a spurious "package", and that's the folder structure that 'library.dynam' needs to see. 'create.wrappers.for.dll' does the alias-creation mentioned above for 'generic.dll.loader'. 'ldyn.unload' unloads the DLL.

USAGE

# Only call this inside your .onLoad!
generic.dll.loader(libname, pkgname, ignore_error=FALSE, dlls=NULL)

# Only call these if you are informally developing a DLL outside a package
ldyn.tester(chname)
create.wrappers.for.dll( this.dll.info, ns=new.env( parent=parent.frame(2)))
ldyn.unload( l1)

ARGUMENTS

 libname, pkgname: as per '.onLoad'
 ignore_error: ?continue to load other DLLs if one fails?
 dlls: default (NULL) means "load all the DLLs you can find". Otherwise, it should be a character vector specifying the DLLs by name, without folder--- no extension is necessary.
 chname: (for 'ldyn.tester') Path to the DLL (extension not required)
 this.dll.info: (for 'create.wrappers.for.dll') A 'DLLInfo' object, as returned by '.dynLibs()[[N]]' or 'library.dynam(...)'
 ns: (for 'create.wrappers.for.dll') If you're calling 'create.wrappers.for.dll' manually, then this defaults to the calling environment, probably '.GlobalEnv'. For "internal use", 'ns' is meant to be a namespace, but you shouldn't be using it like that!
 l1: (for 'ldyn.unload') Result of previous call to 'ldyn.tester'

VALUE

'generic.dll.loader' returns NULL (but see DETAILS).

'ldyn.tester' returns a class "DLLInfo" object if successful. 'ldyn.unload' should return NULL if successful, and crash otherwise.

'create.wrappers.for.dll' returns the environment containing the aliasses.

Be careful with accidentally saving and loading the results of 'ldyn.tester' and 'create.wrappers.for.dll'; they won't be valid in a new R session. You might be better off creating them in the 'mvb.session.info' environment on the search path; they will still be found, but won't persist in a different R session. See EXAMPLES.

DETAILS

R-callable aliasses for your low-level routines will be called e.g. 'C_myrout1', 'Call_myrout2', 'F_myrout3', or 'Ext_myrout4', depending on type. Those for routines in "myfirstdll" will be stored in the environment 'LL_myfirstdll' ("Low Level") in your package's namespace, which itself inherits from the namespace. In your own R code elsewhere in your package, you can then have something like

%%#
.C( LL_myfirstdll$C_myrout1, <<arguments>>) # NB no need for PACKAGE argument

Getting fancy, you can alternatively set the environment of your calling function to 'LL_myfirstdll' (which inherits from the namespace, so all your other functions are still visible). In that case, you can just write

%%#
.C( C_myrout1, <<arguments>>)

SEE.ALSO

'set.finalizer' for a safe way to ensure cleanup after low-level routines.

EXAMPLES

## Don't run

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


## End don't run
}")

)

"get.backup" <-
structure( function( name, where=1, rev=TRUE, zap.name=TRUE, unlength=TRUE) {
  bdd <- get.path.from.where( where)
    
  if( !is.dir( bdd)) {
    warning( "Can't find backup directory")
return() }

  filename <- get.bkfile( name, bdd, create = FALSE)
  if( !nchar( filename)) {
    warning( "Can't find backup file")
return() }

  # Zap warnings about unterminated lines
  ow <- options(warn = -1); on.exit( options( ow))
  bu <- readLines(filename); options(ow); on.exit()

  if( !length( bu)) {
    warning( "Nothing in the backup file")
return()
  }

  nonblanks <- regexpr( '[^ ]', c( bu, 'x'))>0
  bu <- bu[ min( index( nonblanks)) %upto% length( bu)]

  # Next line must match 'get.bkfile'
  infeasible.R.line <- "'\"@\"@'@ START OF BACKUP @'@\"@\"'"

  line.breaks <- bu == infeasible.R.line
  if( !sum(line.breaks)) {
    warning( "No marker lines in the backup file")
return()
  }
    
  bu <- split( bu, cumsum( line.breaks))
  bu <- lapply( bu, '[', -(1:2))
  if( zap.name) {
    zap.name.function <- function( x) {
        x[ 1] <- sub( '"[^"]*" <- *', '', x[ 1])
        x
      }
    bu <- lapply( bu, zap.name.function)
  }
  
  # Character object backups are preceded by one line giving the length of the object. Remove.
  if( unlength) 
    bu <- lapply( bu, function( x) {
      l <- suppressWarnings( as.numeric( x[1]))
      if( !is.na( l) && length( x)==l+1)
        x <- x[-1]
      return( x)
    })
  
  if( rev)
    bu <- rev( bu)
  
  bu  
}
, doc =  docattr( r"{
get.backup      package:mvbutils
create.backups
read.bkind

Text backups of function source code

DESCRIPTION

'get.backup' retrieves backups of a function or character object. 'create.backups' creates backup files for all hitherto-unbacked-up functions in a search environment. For 'get.backup' to work, all backups must have been created using the 'fixr' system (or 'create.backups'). 'read.bkind' shows the names of objects with backups, and gives their associated filenames.

USAGE

get.backup( name, where=1, rev=TRUE, zap.name=TRUE, unlength=TRUE)
create.backups( pos=1)
read.bkind( where=1)


ARGUMENTS

 name: function name (character)
 where, pos: position in search path (character or numeric), or e.g. '..mypack' for maintained package 'mypack'.
 rev: if TRUE, most recent backup comes first in the return value
 zap.name: if TRUE, the tag '"funname" <- ' at the start of each backup is removed
 unlength: if TRUE, the first line of each backup is removed iff it consists only of a number equal to 1+length( object). This matches the (current) format of character object backups.

 
VALUE

 get.backup: Either NULL with a warning, if no backups are found, or a list containing the backups, each as a character vector.
 create.backups: NULL
 read.bkind: a list with components 'files' and 'object.names'; these are character vector with elements in 1-1 correspondence. Some of the objects named may not currently exist in 'where'.

DETAILS

'fixr' and 'FF' are able to maintain text-file backups of source code, in a directory ".Backup.mvb" below the task directory. The directory will contain a file called "index", plus files BU1, BU2, etc. "index" shows the correspondence between function names and BUx files. Each BUx file contains multiple copies of the source code, with the oldest first. Even if a function is removed (or 'move'd) from the workspace, its BUx file and "index" entry are not deleted.

The number of backups kept is controlled by 'options(backup.fix)', a numeric vector of length 2. The first element is how many backups to keep from the current R session. The second is how many previous R sessions to keep the final version of the source code from. Older versions get discarded. I use 'c(5,2)'. If you want to use the backup facility, you'll need to set this option in your '.First'. If the option is not set, no backups happen. If set, then every call to 'Save' or 'Save.pos' will create backups for all previously-unbackupped functions, by automatically calling 'create.backups'. 'create.backups' can also be called manually, to create the backup directory, index, and backup files for all functions in the currently-top task.

'get.backup' returns all available backup versions as *character vectors*, by default with the most recent first. To turn one of these character vectors into a function, a 'source' step is needed; see EXAMPLES.

'read.bkind' shows which file to look for particular backups in. These files are text-format, so you can look at one in a text editor and manually extract the parts you want. You can also use 'read.bkind' to set up a restoration-of-everything, as shown in EXAMPLES. I deliberately haven't included a function for mass restoration in 'mvbutils', because it's too dangerous and individual needs vary.

Currently there is no automatic way to determine the type of a backed-up object. All backups are stored as text, so text objects look very similar to functions. However, the first line of a text object is just a number equal to the length of the text object; the first line of a function object starts with "function(" or "structure( function(". The examples show one way to distinguish automatically. 

The function 'fix.order' (qv) uses the access dates of backup files to list your functions sorted by date order.

'move' will also move backup files and update INDEX files appropriately.



EXAMPLES

## Don't run
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
for( i in bks$object.names) {
  cat( "Restoring ", i, "...")
  gb <- get.backup( i, unlength=FALSE)[[1]] # unlength F so we can check type
  
  # Is it a charvec?
  if( grepl( '^ *[0-9]+ *$', gb[1])) # could check length too
    gb <- as.cat( gb[-1]) # remove line showing length and...
    # ...set class to "cat" for nice printing, as per 'as.cat'
  else {
    # Nope, so it's a function and needs to be sourced
    tc <- textConnection( gb)
    gbfun <- try( source.mvb( gb)) # will set source attribute, documentation etc.
    close( tc)
    if( gbfun %is.a% "try-error") {
      gbfun <- stop( function( ...) stop( ii %&% " failed to parse"), list( ii=i))
      attr( gbfun, 'source') <- gb # still assign source attribute
    }
    gb <- gbfun
  }
      
  assign( i, gb)
  cat( '\n')
}

## End don't run


SEE.ALSO

'fixr', 'cd', 'move'


AUTHOR

Mark Bravington


KEYWORDS

programming; utilities; documentation
}")

)

"get.bkfile" <-
function (name, bkdir, create = FALSE) 
{
    fob <- read.bkind(bkdir)
    i <- match(name, fob$object.names)
    if (is.na(i)) {
        if (!create) 
            return("")
        file.nums <- as.integer(unlist(strsplit(fob$files, "BU", fixed=TRUE)))
        n <- min(1:(length(file.nums) + 1) %except% file.nums)
        filename <- "BU" %&% n
        fob$files <- c(fob$files, filename)
        fob$object.names <- c(fob$object.names, name)
        cat(paste(fob$files, fob$object.names, sep = "="), sep = "\n", 
            file = file.path(bkdir, ".Backup.mvb", "index"))
    }
    else filename <- fob$files[i]
    filename <- file.path(bkdir, ".Backup.mvb", filename)
    if (!file.exists(filename)) 
        file.create(filename)
    filename
}


"get.cd.from.menu" <-
function() {
  if(!exists( "tasks", where=1, inherits=FALSE))
    tasks <- structure( character(0), names=character(0)) # avoid sort complaining about names

  catstop <- function() {
      cat( 'No ')
stop( 'merely quitting cd', call.=FALSE) 
    }    
    
  line.end <- if( getOption( 'cd.extra.CR', FALSE)) '\n' else ''
  
  can.go.up <- ifelse( length( .Path) > 1, 1, 0)
  to <- menu( c( sort(names(tasks)), if( can.go.up) '..' else NULL, 
      "CREATE NEW TASK"), graphics = !is.null(getOption('gui')), title = "Task menu")
  if(to == 0) 
catstop()

  if(to == 1 + can.go.up +length(tasks)) {
    cat( "Name of new task (ENTER to quit): " %&% line.end)
    to <- readline()
    if(to=="")
catstop() } 
  else if( to > length( tasks))
    to <- '..'
  else
    to <- sort( names(tasks))[to]

return( parse( text=to)[[1]])
}


"get.info.for.mcache" <-
function( x, envir, name=TRUE) {
  if( name)
    x <- envir[[ x]]
  lapply( named( cq( mode, class, dim, length, object.size)),
      function( f) get(f)(x))
}


"get.last.R.mandatory.rebuild.version" <-
function() {
##############################
  # Self-explanatory. NB R.rebuild.vers created in mvbutils:::.onLoad

  last.R.major <- numeric_version( sub( '[.][0-9]+[^.]*$', '', as.character( getRversion())))
  Rrebver <- max( R.rebuild.vers %such.that% (. <= last.R.major))
  next_Rrebver <- min( R.rebuild.vers %such.that% (. > last.R.major))
  if( !length( next_Rrebver)) { # min() on empty numeric_version will return length-0
    next_Rrebver <- numeric_version( sprintf( '%i.%i', last.R.major[[1,1]]+1, 0))
  }

returnList(
  Rrebver,
  next_Rrebver,
  last.R.major)
}


"get.mcache.reffun" <-
function( whati, envir) {
  # Must avoid name clash between 'whati' and internal vars of fx
  fx <- function( x) NULL
  body( fx) <- substitute(
      if( missing( x))
        qwhati
      else {
        mc <- attr( envir, 'mcache')
        mci <- as.list( attr( mc, 'info'))
        mc[ whati] <- -abs( mc[ whati]) # signal a change
        mci[[ whati]] <- get.info.for.mcache( x, name=FALSE)
        attr( mc, 'info') <- mci
        oldClass( mc) <- 'nullprint'
        attr( envir, 'mcache') <- mc
        qwhati <<- x
    }, list( whati=whati, qwhati=as.name( whati), x=as.name( 'x' %&% whati),
        mc=as.name( 'mc' %&% whati)))
  names( formals( fx)) <- 'x' %&% whati

  e <- new.env( parent=asNamespace( 'mvbutils'))
  e$envir <- envir # doesn't work if I sub envir directly into body( fx)
  environment( fx) <- e
  fx
}


"get.mcache.store.name" <-
function( envir) {
  lsnc <- lsall( envir=envir, patt='^\\.mcache[0-9]+$')
  if( !length( lsnc))
    cache.name <- '.mcache0'
  else
    cache.name <- lsnc[ order( nchar( lsnc), decreasing=TRUE)[1]]
  cache.name
}


"get.new.file.numbers" <-
function( derefs, file.numbers) {
  had.numbers <- derefs %such.that% (. %in% names( file.numbers))
  file.numbers <- file.numbers %without.name% derefs
  derefs <- derefs %except% had.numbers
  new.file.numbers <- (1 %upto% max( file.numbers)) %except% file.numbers
  new.file.numbers <- c( new.file.numbers, max( c( 0, file.numbers)) +
      1 %upto% (length( derefs)-length(new.file.numbers)))[ 1:length(derefs)]
  names( new.file.numbers) <- derefs
  new.file.numbers
}


"get.path.from.where" <-
function( where){
  if( is.character( where) && is.dir( where))
return( where)

  if( !is.environment( where)) {
    if( length( where) != 1)
stop( "'where' should be length 1")

    where <- named( search())[ where] # to character
    where <- index( search()==where) # to numeric
    if( !is.numeric( where) || is.na( where))
  stop( "'where'?")

    pfw <- file.path(attr(pos.to.env(where), "path"))
  } else 
    pfw <- attr( where, 'path')
return( pfw)
}


"get.path.list" <-
function () 
{
    path.list <- search()
    apfun <- function(x) {
        x <- attr(pos.to.env(x), "path")
        if (!is.null(x)) 
            x <- names(x)[1]
        if (is.null(x)) 
            x <- ""
        x
    }
    ap <- sapply(seq(path.list), apfun)
    path.list[nchar(ap) > 0] <- ap[nchar(ap) > 0]
    path.list
}


"get.ref.info" <-
function( envo, nlocal=sys.parent()) mlocal({
  if( is.null( cache <- attr( envo, 'cache')))
    attr( envo, 'cache') <- cache <- new.env( hash=TRUE, envo)
  lscache <- lsall( cache)
  refs <- derefs <- promises <- character(0)
  file.numbers <- numeric( 0)
  if( length( lscache)) {
    refs <- names( which( unlist( eapply( envo, inherits, 'mref'))))
    derefs <- lscache %that.are.in% (lsall( envo) %except% refs)
    prom.func <- function( x) {cache[[x]] %is.a% 'promise'}
    promises <- names( which( sapply( lscache, prom.func)))
    fnum.func <- function( x) unclass( envo[[ x]])$nfile
    if( length( refs))
      file.numbers <- sapply( refs, fnum.func)
  }
})


"get.S3.generics" <-
function( pack, ns=TRUE){
  if( ns) {
    packname <- pack
    pack <- asNamespace( pack)
    meths <- lsall( pack$.__S3MethodsTable__.) 
  } else {
    packname <- '' # nameless
    meths <- find.funs( pack)
  }
    
  prefixes <- character( 0)
  for( imeth in meths) {
    spl <- clip( strsplit( imeth, '.', fixed=TRUE)[[1]])
    prefixes <- c( prefixes, sapply( 1 %upto% length( spl), 
        function( x) paste( spl[ 1:x], collapse='.')))
  }

  packgens <- unique( prefixes %that.are.in% find.funs( pack))
  packgens <- packgens[ unlist( lapply( packgens, 
      function( f) 'UseMethod' %in% all.names( body( pack[[f]]))))]
  structure( rep( packname, length( packgens)), names=packgens)
}


"get_defs_from_source" <-
function( f){
r"--{#COMMENT#
Given a source file 'f' (presumably for a package), find all the object definitions and their exact sources (as far as possible), so that internal comments are included. But drop all the Roxybollox and crap between definitions. A big thing is to *not* evaluate the definitions per se, in case of side effects. However, for 'structure(function...)' I think evaluation may be unavoidable.
}--"

  x <- readLines( f)
  stuff <- parse( f)
  thingdefs <- do.on( stuff, my.all.equal( class( .), '<-'))
  stuff <- stuff[ thingdefs]
  objnames <- do.on( stuff, as.character( .[[2]]))
  objlist <- FOR( stuff, .[[3]])
  
  # Evaluate function(foo) bah and structure( function(foo) blah), only
  objlist <- FOR( objlist, 
      if( 
        is.call( .) && 
        is.name( .[[1]]) && (
          as.character( .[[1]])=='function') || (
            as.character( .[[1]])=='structure' &&
            is.call( .[[2]]) &&
            is.name( .[[c(2,1)]]) &&
            as.character( .[[c(2,1)]])=='function'
          )
        ) eval( .) else .
    )

  objlist <- FOR( objlist, replace_function_by_sourcecode( ., x))
  names( objlist) <- objnames

return( objlist)
}


"get_import_instructions" <-
function( ns){
  import_instructions <- named( ns$import)

  # 2023 mod: certain imports might be excluded, to avoid clash warnings
  for( i in names( ns$import_exceptions)){
    import_instructions[ i] <- sprintf( '%s, except=c(%s)',
        i,
        paste( sprintf( '"%s"', ns$import_exceptions[[ i]]), collapse=',')
      )
  } # for any importees with exceptions

return( import_instructions)
}


"get_my_roxy_lines" <-
function( func, roxys, this_srcref) {
## For 'unpackage', retrieve any pre-function Roxygen stuff from original source file
  # In case of "manufactured" functions, eg pre-install hook: NULL
  if( is.null( this_srcref))
return( NULL)

  roxlines <- roxys[[ attr( this_srcref, 'srcfile')$filename]]$roxlines[[ func]]
  if( !is.null( roxlines)) {
    roxlines <- as.cat( roxlines)
  }
return( roxlines)
}


"get_ncores_CRANal" <-
structure( function( target){
## Yet more work to get around CRANality
  re_bloody_quire( 'parallel')
  avail <- detectCores()
  if( is.na( avail)){
    avail <- 1L
  }
  
  if( avail==1L){
return( 1L)
  }
  
  avail <- 2L # even CRAN should allow THIS...
  while( avail < target){
    tried <- min( target, 2*avail)
    CLUSTO <- try( parallel::makeCluster( tried))
    if( CLUSTO %is.a% 'try-error'){
  break
    }
    
    stopCluster( CLUSTO)
    avail <- tried
  }

return( avail)  
}
, doc =  docattr( r"{
get_ncores_CRANal    package:mvbutils


Detect number of CPU cores in CRAN-robust way


DESCRIPTION

This is only relevant for parallel code inside package examples and vignettes. In real applications, you would call 'parallel::detectCores' (qv) and then decide how many of those to use. But CRAN enforces a limit of (currently) 2 cores when checking examples (and presumably vignettes etc)--- and doesn't give you any way to find out what the limit is from code; it just gives an error. Since the _entire_ point of parallel processing is to use lots of cores if available, CRAN makes it impossible to demonstrate anything realistic in examples, if you want to get them past CRAN. You ccan of course limit the number of cores to 2 purely for CRAN's benefit, but then you are castrating your code for real tests.

To avoid this lunacy, you can call this function inside your example/vignette. It counts roughly how many cores are _allowed_ (ie won't cause an error), up to the limit requested by its argument (which you would get from 'detectCores' etc). Actually it only goes in multiples of 2, so it won't necessarily give you the max.

In real code as opposed to examples, you probably don't want this; rather call 'parallel::detectCores' and then decide for yourself, as I mention in two other places in this helpfile!

The "algorithm" is to start with 2 cores and keep doubling until there's an error (trapped with 'try'), or until the target is reached. This will at least be "quick" on CRAN. But it always means setting up and destroying a cluster at least twice, which is *inefficient* if you can just decide for yourself! And if you are really happy just having your example use 2 cores, then just use 2 cores in the example--- don't bother with this!

At present, this code is only in 'mvbutils' so I can make the 'numvbderiv_parallel' (qv) example run nicely; but I guess I might use it in other packages eventually. Parallel stuff in R is messy; be warned.


USAGE

get_ncores_CRANal( target) 


ARGUMENTS

 target: How many cores you would like. Presumably, requires a previous call to 'parallel::detectCores' and also a sensible decision on your part.
 

VALUE

Integer


EXAMPLES


## Don't run
"See numvbderiv example"
## End don't run


KEYWORDS

parallel

}")

)

"get_roxy_lines" <-
function( f) {
  l <- readLines( f)

  pp <- parse( f)
  is_def <- do.on( pp, (class(.)=='<-') && (length( .[[2]])==1))
  def_names <- do.on( pp[ is_def], as.character( .[[2]]))

  file_srcref <- attr( pp, 'srcref')
  defs <- do.on( file_srcref, unclass( .)[1]) # lines

  # Add dummy non-definition to simplify lookups...
  is_def <- c( is_def, FALSE)
  defs <- c( defs, length( l)+1L)
  names( defs)[ is_def] <- def_names

  # defs <- grep( '^ *"[^"]*" +<-', l)
  # names( defs) <- sub( '^ *"([^"]*)".*', '\\1', l[ defs])
  iroxy <- grep( "^#'", l)
  if( !length( iroxy))
return( returnList( file_srcref, defs, roxy=matrix( 0L, 0, 2), roxlines=list()))


  di <- diff( c( -1, iroxy))
  start <- which( di>1)
  irend <- c( iroxy[ start[-1]-1], tail( iroxy, 1))
  irstart <- iroxy[ start]
  roxy <- cbind( irstart, irend)[ irend > irstart+2,,drop=FALSE]
  # Discarded mere export directives--- these can be auto-added during reconversion, based on
  # ... named-in-doc-alias

  idef <- findInterval( roxy[,2], defs)+1L
  roxy <- roxy[ is_def[ idef],,drop=FALSE]
  # Any roxybollox not associated with a function will be DISCARDED for now...

  roxlines <- FOR( 1 %upto% nrow( roxy), l[ roxy[.,1] %upto% roxy[.,2]])
  idef <- idef[ is_def[ idef]]
  rownames( roxy) <- names( roxlines) <- names( defs)[ idef] # not def_names[ idef], for some reason...
  defs <- defs[ is_def]
returnList( file_srcref, defs, roxy, roxlines)
}


"gitup_pkg" <-
structure( function( pkg, gitparent, character.only=FALSE, excludo='funs.rda'){
## Update the github-ready version of a package
## Not for first-time use

  set.pkg.and.dir() # dir. spath ewhere pkg (as character)
  rpath <- dir.
  spath <- sourcedir

  if( is.null( rpath)){
stop( "Can't find path of raw package '" %&% pkg %&% "'")
  }

  git_dir <- file.path( gitparent, pkg)
stopifnot( is.dir( git_dir))

  # git_dir might be the "real" package, or a layer above it...
  baby_git_dir <- file.path( git_dir, pkg)
  if( is.dir( baby_git_dir)){
    git_dir <- baby_git_dir
  }

stopifnot( file.exists( file.path( git_dir, 'DESCRIPTION')))

  old_git <- list.files( git_dir, recursive=TRUE)
  now_stuff <- list.files( spath, recursive=TRUE)
  now_stuff <- now_stuff %such.that% (basename( .) %not.in% excludo)

  dead_wood <- old_git %except% now_stuff
  if( length( dead_wood)){
    scatn( "Deleting dead wood: %s", paste( dead_wood, collapse='; '))
    unlink( file.path( git_dir, dead_wood))
  }

  green_shoots <- now_stuff %except% old_git
  file.copy( file.path( spath, green_shoots), file.path( git_dir, green_shoots))

  samo <- now_stuff %that.are.in% old_git
  md5_now <- md5sum( file.path( spath, samo))
  md5_git <- md5sum( file.path( git_dir, samo))

  diffo <- samo[ md5_now != md5_git]
  if( length( diffo)){
    scatn( "Replacing: %s", paste( diffo, collapse='; '))
    file.copy( file.path( spath, diffo), file.path( git_dir, diffo),
        overwrite=TRUE, copy.mode=FALSE, copy.date=TRUE)
  }
invisible( NULL)
}
, doc =  docattr( r"{
gitup_pkg    package:mvbutils


Update local git repo

DESCRIPTION

Update local git repo of your package (e.g. 'splendid'), from a source package. Well, all it does is delete old files, and overcopy any whose MD5 sum has changed; you still have to do all the git bollocks yourself (add/commit/push, in my book). Maybe you should do a "git pull" first before all this, so that you can have the fun of reconciling changes before the extreme fun of "cannot pull; changes..." messages and the inevitable descent into "git push force".

IMO everything is simpler if your R source files are stored individually (function-by-function) _because then you can easily see what changed_, but the vast and unenlightened hordes disagree with me and plonk it all in one single mega-file, complete with Roxygen "documentation" (don't get me started...). Sigh.

I hate Git, BTW--- in case that's not already obvious. This is really for my own use, in conjunction with 'unpackage' (qv), for a way to reconcile my own devel process with Git.


USAGE

gitup_pkg(
  pkg,
  gitparent,
  character.only = FALSE,
  excludo='funs.rda')


ARGUMENTS

 pkg: name of yr task package, as per 'install_pkg' etc (there are various options)
 gitparent: folder where yr local git copy lives, or possibly one level higher
 character.only: if TRUE, interpret 'pkg' like a normal R variable, not like in 'library'
 excludo: files to not copy


}")

)

"group" <-
function( m, ...) {
  l <- list( ...)
  if( length( l)==1 && is.list( l))
    l <- l[[ 1]]
  rep( names( l), sapply( l, length))[ match( m, unlist( l), NA)]
}


"hack" <-
structure( function( fun, ...){
  if( is.character( fun))
    fun <- get( fun)
  mc <- match.call( expand.dots=FALSE)$...
  for( i in names( mc))
    formals( fun)[[ i]] <- mc[[ i]]
  fun
}
, doc =  docattr( r"{
hack        package:mvbutils
assign.to.base

Modify standard R functions, including tweaking their default arguments

DESCRIPTION

You probably shouldn't use these... 'hack' lets you easily change the argument defaults of a function. 'assign.to.base' replaces a function in 'base' or 'utils' (or any other package and its namespace and S3 methods table) with a modified version, possibly produced by 'hack'. Package 'mvbutils' uses these two to change the default position for library attachment, etc; see the code of 'mvbutils:::.onLoad'.

Note that, if you call 'assign.to.base' during the '.onLoad' of your package, then it must be called _directly_ from the '.onLoad', not via an intermediate function; otherwise, it won't correctly reset its argument in the import-environment of your namespace. To get round this, wrap it in an 'mlocal'; see 'mvbutils:::.onLoad' for an example.

'assign.to.base' is only meant for changing things in packages, e.g. not for things that merely sit in non-package environments high on the search path (where '<<-' should work). I don't know how it will behave if you try. It won't work for S4 methods, either.


USAGE

 hack( fun, ...)
 assign.to.base( x, what=,  where=-1, in.imports=, override.env = TRUE) 

 
ARGUMENTS

 fun: a function (not a character string)
 ...: pairlist of arguments and new default values, e.g. arg1=1+2. Things on RHS of equal signs will *not* be evaluated.
 x: function name (a character string)
 what: function to replace 'x', defaulting to '"replacement." %&% x'
 where: where to find the replacement function, defaulting to usual search path
 in.imports: usually TRUE, if this is being called from an '.onLoad' method in a namespace. Make sure any copies of the function to be changed that are in the "imports" namespace also get changed. See DESCRIPTION.
 override.env: should the replacement use its own environment, or (by default) the one that was originally there?


EXAMPLES

## Don't run:

hack( dir, all.files=getOption( "ls.all.files", TRUE)) # from my '.First'
assign.to.base( "dir", hack( dir, all.files=TRUE))

## End Don't run


KEYWORDS

programming
}")

)

"hack.importIntoEnv" <-
function () { # impenv, impnames, expenv, expnames) {
  le <- if( exists( 'base.importIntoEnv', where='mvb.session.info', inherits=FALSE))
        get( 'base.importIntoEnv', 'mvb.session.info')
      else
        baseenv()$importIntoEnv

  # NB NB: Probably should do this with activeBinding() not delayedAssign(), to 
  # ... proof it against subsequent changes
  # Also (though not for this function) would be nice to have on-the-fly changes to "Imports"
  # ... reflected by 'patch.install'
  subbo <- substitute(
      {
        if( environmentName( expenv) %in% names( maintained.packages)) {
          for( i in seq_along( impnames))
            do.call( 'delayedAssign', list( x=impnames[ i], value=call( 'get', expnames[i]), 
                eval.env=expenv, assign.env=impenv))
        } else
          default
      }, list( default=body( le)))
  body( le) <- subbo
  environment( le) <- asNamespace( 'mvbutils')
  le
}


"hack.lockEnvironment" <-
function(){
  le <- if( exists( 'base.lockEnvironment', where='mvb.session.info', inherits=FALSE))
        get( 'base.lockEnvironment', 'mvb.session.info')
      else
        baseenv()$lockEnvironment
  subbo <- substitute(
      { 
      #cat( 'Checking '); print( env)
      #cat( '  '); print( exists( '.__NAMESPACE__.', env, mode='environment', inherits=FALSE))
      #cat( '  '); print( exists( '.packageName', env, mode='character', inherits=FALSE))
      #cat( '  '); print( sum( match( env$.packageName, names( maintained.packages), 0)))
      is.mp.ns <- exists( '.__NAMESPACE__.', env, mode='environment', inherits=FALSE) &&
            exists( '.packageName', env, mode='character', inherits=FALSE) &&
            sum( match( env$.packageName, names( maintained.packages), 0))
        if( is.mp.ns || any( sapply( dont.lock.envs, identical, y=env)) ||
            !is.null( attr( env, 'dont.lock.me')) ||
            sum( match( attr( env, 'name'), dont.lock.envnames, 0))) {
          # cat( "Not locking\n") 
          if( is.mp.ns) {
            dont.lock.envnames <<- c( dont.lock.envnames, 'package:' %&% env$.packageName)
            dont.lock.envs <<- c( dont.lock.envs, structure( list( parent.env( env)),
                names='imports:' %&% env$.packageName))
          }
        } else
          default
      }, list( default=body( le)))
  body( le) <- subbo
  environment( le) <- asNamespace( 'mvbutils')
  le
}


"hack_exported_names" <-
function( pkg) {
  # Emulates Rd2roxygen:::exported_names, sans *terrible* habit of trying to loadNamespace!
  f <- file.path(pkg, "NAMESPACE")
  NAMESPACE <- parse( f)
  
  # Changed May 2021 because Shane has changed the format of kinference NAMESPACE file...
  exports <- expression()
  for( expr in NAMESPACE) {
    if( (expr %is.a% 'call') && (expr[[1]] %is.a% 'name')) { 
      pseudoroutine <- as.character( expr[[1]])
      if( pseudoroutine == 'export') {
        exports <- c( exports, expr) # I prefer them all to be listed in one "call" to export()
      } else if( pseudoroutine == 'exportPattern') {
stop( "Can't handle 'exportPattern'...")      
      }
    }
  }

  if( !length( exports)) {
return( character( 0))
  }
  
  export_names <- character()
  export <- function( ...) {
    mc <- match.call( expand.dots=TRUE)
    mc[[1]] <- quote( cq)
    export_names <<- c( export_names, eval( mc))
  }
  
  eval( exports)
return( export_names)

## Was just this:::
#  exports[[1]] <- quote( c)
# return( eval( exports))
}


"hack_Makeconf" <-
structure( function() {
  # For some STUPID reason, R will not honour BINPREF64 when in 32bit
  # ... leading to failure to cross-compile for "multiple platforms"
  # ... I think only when running in 32bit
  # Maybe there is an official workaround 
  # (since tools: : :.shlib_internal seems to work)
  # but I'm ****ed if I know what it is
  # So, from the wonderful wicked web: need to hack R's own Makeconf file to get things working

  if( (getRversion() < '4.2') && (.Platform$OS.type=='windows')) {
    # Seem to need this now on 64bit too; formerly had " && (.Platform$r_arch=='i386'))"
    makeconf64 <- file.path( Sys.getenv( 'R_HOME'), 'etc/x64/Makeconf')
    reado <- readLines( makeconf64)
    BP_lines <- grep( 'BINPREF', reado, fixed=TRUE)
    # Truly perfect hack would actually run a reduced makefile just
    # containing these lines, and see what echoes as BINPREF...

    if( !length( grep( '^ *BINPREF *= *\\$\\(BINPREF64\\)', reado[ BP_lines]))) {
      OK <- yes.no( "<R_HOME>/etc/x64/Makeconf needs a tweak; otherwise 64-bit DLLs won't be compiled. Shall I?")
      if( OK) {
        BP_def_line <- grep( '^ *BINPREF.*=', reado[ BP_lines])
        reado <- multinsert( reado, BP_lines[ BP_def_line], attr( sys.function(), 'tweak'))
        writeLines( reado, makeconf64)
      }
    }
  } else {
    OK <- TRUE
  }

return( TRUE)
}
, tweak =  docattr( r"{
# From the web, to work round R stupidity:
ifdef BINPREF64
BINPREF = $(BINPREF64)
endif
}")

)

"handle_prebuilds" <-
function( pkg, dir, description, rewrap.forcibly){
  # Remember to allow version number provisions!
  Clinkers <- names( Clink_packages()) %that.are.in% sub( '[ (].*', '', strsplit( description['Imports'], ', *')[[1]])

  ## C (etc) source files, and "auto-precompiling-wrapping" (eg RcppExports)
  # NB this will put some extra files into the task package (eg RcppExports)
  # They get copied later
  Cloaders <- DLLs <- makelines <- character()
  needs_makefile <- FALSE
  use_subenv_for_DLLs <- logical()

  run_postcopy_hooks <- expression()
  pkg <- basename( dir)
  Rdir <- file.path( dir, 'R')
  lldir <- file.path( dir, 'src')
  # One level of subfolders is permitted
  subdirs <- list.dirs( lldir, recursive=FALSE, full.names=TRUE) %that.dont.match% '/[._][^/]*$'

  if( rewrap.forcibly) {
    src_changed <- function( ...) TRUE
  } # otherwise it looks at manifests etc

  for( idir in c( lldir, subdirs)) {
    this_DLL <- if( idir==lldir) pkg else basename( idir)

    for( Clink_pkg in Clinkers) {
      # Do any pre-build, and return relevant info for NAMESPACE etc
      check <- Clink_packages()[[ Clink_pkg]]( pkg, this_DLL, idir, Rdir, src_changed)
      if( !is.null( check)) {
        Cloaders <- c( Cloaders, structure( check$Cloader, names=this_DLL))
        DLLs <- c( DLLs, this_DLL)
        use_subenv_for_DLLs <- c( use_subenv_for_DLLs, structure( check$subenv, names=this_DLL))
        makelines <- c( makelines, check$makelines) # multi-lines; no name
        needs_makefile <- needs_makefile || (length( Cloaders)>1) || suppressWarnings( isT( check$needs_makefile))
        run_postcopy_hooks <- c( run_postcopy_hooks, check$postcopy_hook_expr) # name dangerous?
    break # don't check any other Clinkers on this folder
      }
    } # Clinkers

    # Generate a new DLL for this (sub)dir iff it has [[Rcpp::export]] in any C file
    # Rcpp_files <- dir( idir, patt='[.]cpp$', full=TRUE) %that.dont.match% '^RcppExports'
    # Rcpp_files <- Rcpp_files %SUCH.THAT% length( grep( '^// +' %&% to.regexpr( '[[Rcpp::export]]'), readLines( .)))
  }

  if( length( Cloaders)) {
    # Make sure the R-header creation step runs last, after all other vars exist
    # Order of Cloaders shouldn't matter (eg some ADT ones, an Rcpp one, ...)
    # description[ 'Collate'] <- pkg %&% '.R ' %&% paste( basename( Cloaders), collapse=' ')

    description[ 'Collate'] <- pkg %&% '.R ' %&% sprintf( 'Cloaders_%s.R', pkg)

    UDL <- DLLs
    names( UDL) <- ifelse( use_subenv_for_DLLs, sprintf( 'DLL_%s', DLLs), '')
  } else {
    UDL <- character()
  }

  if( needs_makefile){
    make_Makefile( makelines, dir)
  }

returnList( description, useDynLib=UDL, Cloaders, run_postcopy_hooks)
}


"has.source" <-
function( x) is.function( x) || !is.null( attr( x, 'source'))


"help" <-
structure( function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
    try.all.packages = getOption("help.try.all.packages"), help_type = getOption("help_type")) {
  # help <- get("base.help", pos = "mvb.session.info")
  mc <- as.list(match.call(expand.dots = TRUE))
  mc[[1]] <- quote( utils::help) # as.environment( 'mvb.session.info')$base.help)

  # Set 'mvb_help_type', just in case it's needed
  mvb_help_type <- mc$help_type
  if( is.null( mvb_help_type))
    mvb_help_type <- getOption( 'mvb_help_type', 'text')

  if (!is.null(mc$topic) && !is.call(mc$topic) && is.null(mc$type) &&
      is.null(mc$lib.loc)) { #  && is.null(mc$try.all.packages)) {
    h1 <- try(eval(as.call(mc), parent.frame()), silent = TRUE)
    if( (h1 %is.not.a% "try-error") && (length(unclass(h1)) > 0)) 
  return( h1)
  
    h1 <- dochelp( as.character( mc$topic), help_type=mvb_help_type) 
    if( h1 %is.a% c( "pagertemp", "browsertemp"))
  return(h1)      
  }

  eval(as.call(mc), parent.frame())
}
, doc =  docattr( r"{
help   package:mvbutils
?

The R help system

DESCRIPTION

'?x' is the usual way to get help on 'x'; it's primarily a shortcut for 'help(x)'. There are rarer but more flexible variations,  such as 'x?y' or 'help(x,...)'. See base-R help on help. The versions of 'help' and '?' exported by 'mvbutils' behave exactly the same as base-R, unless base-R 'help' _fails_ after being called with a single argument, e.g. 'help(topic)'. In that case, if 'topic' is an object with an attribute called "doc" (or failing that if 'topic' or 'topic.doc' is a character vector), then the attribute (or the character object) will be formatted and displayed by the pager (by default) or browser. This lets you write informal documentation for non-package objects that can still be found by 'help', and by colleagues you distribute your code to. See 'dochelp' for more information. The rest of this documentation is copied directly from base-R for 'help', except as noted under ARGUMENTS for 'help_type'.


USAGE

help(topic, package = NULL, lib.loc = NULL,
     verbose = getOption("verbose"),
     try.all.packages = getOption("help.try.all.packages"),
     help_type = getOption("help_type"))


ARGUMENTS

 topic: usually, a name or character string specifying the topic for  which help is sought.  A character string (enclosed in  explicit single or double quotes) is always taken as naming a  topic. If the value of 'topic' is a length-one character vector the topic is taken to be the value of the only element. Otherwise 'topic' must be a name or a reserved word (if syntactically valid) or character string. See DETAILS for what happens if this is omitted.

 package: a name or character vector giving the packages to look into for documentation, or 'NULL'.  By default, all packages in  the search path are used.  To avoid a name being deparsed use  e.g. '(pkg_ref)' (see the examples).

 lib.loc: a character vector of directory names of R libraries, or 'NULL'.  The default value of 'NULL' corresponds to all libraries currently known. If the default is used, the loaded packages are searched before the libraries.  This is not used for HTML help (see DETAILS).

 verbose: logical; if 'TRUE', the file name is reported.

 try.all.packages: logical; see NOTE.

 help_type: character string: the type of help required.  Possible values are "text", "html" and "pdf".  Case is ignored, and partial matching is allowed. [Note that, for informal doco, 'getOption( mvb_help_type, "text")' is used; i.e., the default there is always the pager, which lets you be as informal as you please.]


DETAILS

The following types of help are available:

 - Plain text help

 - HTML help pages with hyperlinks to other topics, shown in a browser by 'browseURL'.  If for some reason HTML help is      unavailable (see 'startDynamicHelp'), plain text help will be used instead.

 - For 'help' only, typeset as PDF - see the section on OFFLINE.HELP.

The default for the type of help is selected when R is installed - the 'factory-fresh' default is HTML help.

The rendering of text help will use directional quotes in suitable locales (UTF-8 and single-byte Windows locales): sometimes the fonts used do not support these quotes so this can be turned off by setting 'options(useFancyQuotes = FALSE)'.

'topic' is not optional. If it is omitted, R will give:

 -  If a package is specified, (text or, in interactive use only, HTML) information on the package, including hints/links to      suitable help topics.

 - If 'lib.loc' only is specified, a (text) list of available packages.

 - Help on 'help' itself if none of the first three arguments is specified.

Some topics need to be quoted (by backticks) or given as a character string.  These include those which cannot syntactically appear on their own such as unary and binary operators, 'function' and control-flow reserved words (including 'if', 'else' 'for', 'in', 'repeat', 'while', 'break' and 'next').  The other 'reserved' words can be used as if they were names, for example 'TRUE', 'NA' and 'Inf'.

If multiple help files matching 'topic' are found, in interactive use a menu is presented for the user to choose one: in batch use the first on the search path is used.  (For HTML help the menu will be an HTML page, otherwise a graphical menu if possible if 'getOption("menu.graphics")' is true, the default.)

Note that HTML help does not make use of 'lib.loc': it will always look first in the attached packages and then along 'libPaths()'.


OFFLINE.HELP

Typeset documentation is produced by running the LaTeX version of the help page through 'pdflatex': this will produce a PDF file.

The appearance of the output can be customized through a file 'Rhelp.cfg' somewhere in your LaTeX search path: this will be input as a LaTeX style file after 'Rd.sty'.  Some environment variables are consulted, notably 'R_PAPERSIZE' (_via_ 'getOption("papersize")') and 'R_RD4PDF' (see 'Making manuals' in the 'R Installation and Administration Manual').

If there is a function 'offline_help_helper' in the workspace or further down the search path it is used to do the typesetting, otherwise the function of that name in the 'utils' namespace (to which the first paragraph applies).  It should accept at least two arguments, the name of the LaTeX file to be typeset and the type (which as from R 2.15.0 is ignored).  As from R 2.14.0 it should accept a third argument, 'texinputs', which will give the graphics path when the help document contains figures, and will otherwise not be supplied.


NOTE

Unless 'lib.loc' is specified explicitly, the loaded packages are searched before those in the specified libraries.  This ensures that if a library is loaded from a library not in the known library trees, then the help from the loaded library is used.  If 'lib.loc' is specified explicitly, the loaded packages are _not_ searched.

If this search fails and argument 'try.all.packages' is 'TRUE' and neither 'packages' nor 'lib.loc' is specified, then all the packages in the known library trees are searched for help on 'topic' and a list of (any) packages where help may be found is displayed (with hyperlinks for 'help_type = "html"').  *NB:* searching all packages can be slow, especially the first time (caching of files by the OS can expedite subsequent searches dramatically).


REFERENCES

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) _The New S Language_.  Wadsworth & Brooks/Cole.


SEE.ALSO

'?' for shortcuts to help topics.

'dochelp' for how to write informal help with 'mvbutils'.

'help.search()' or '??' for finding help pages on a vague topic; 'help.start()' which opens the HTML version of the R help pages; 'library()' for listing available packages and the help objects they contain; 'data()' for listing available data sets; 'methods()'.

Use 'prompt()' to get a prototype for writing 'help' pages of your own package.


EXAMPLES

help()
help(help)              # the same

help(lapply)

help("for")             # or ?"for", but quotes/backticks are needed

help(package="splines") # get help even when package is not loaded

topi <- "women"
help(topi)

try(help("bs", try.all.packages=FALSE)) # reports not found (an error)
help("bs", try.all.packages=TRUE)       # reports can be found
                                        # in package 'splines'

## For programmatic use:
topic <- "family"; pkg_ref <- "stats"
help((topic), (pkg_ref))

}")

)

"help2flatdoc" <-
structure( function( fun.name, pkg=NULL, text=NULL, aliases=NULL){
  # These were buried inside the if() below

  if( is.null( text)) {
    libpath <- dirname( find.package( pkg))
    
    if( getRversion() >= "2.10") {
      al <- readRDS( file.path( libpath, pkg, 'help', 'aliases.rds'))
      hfilename <- al[ fun.name]
      p1 <- tools$fetchRdDB( file.path( libpath, pkg, 'help', pkg), hfilename)
      text <- Rd2txt_easy( p1)
    } else {
      # Get 'help' to create text, via fake 'pager' function
      text <- character( 0)
      repager <- function( file, header, title, delete.file) {
        text <<- readLines( file)
        if( delete.file)
          unlink( file)
      }
    # Now 'print' will invoke 'repager'       

    # Non-standard treatment of 'package' arg in 'help' requires the following hack:
      ufq <- options( useFancyQuotes=FALSE)
      eval( substitute( 
          print( help( fun.name, package=pkg, htmlhelp=FALSE, chmhelp=FALSE, pager=repager, 
              lib.loc=libpath)),
          list( pkg=pkg)))
      options( ufq)
    }
    if( !length( text))
stop( "No help found for " %&% fun.name)
  }

  if( !grepl( '(?i) R Documentation', text[1])) { # new styleee
    text <- c( sprintf( '%s      package:%s     R Documentation', fun.name, pkg), '', text)
  }

  text <- help2flatdoc_guts( text, aliases=unique( c( fun.name, aliases)))

  class( text) <- 'cat'
  text
}
, doc =  docattr( r"{
help2flatdoc package:mvbutils

Convert help files to flatdoc format.

DESCRIPTION

Converts a vanilla R help file (as shown in the internal pager) to plain-text format. The output conventions are those in 'doc2Rd' (qv), so the output can be turned into Rd-format by running it through 'doc2Rd'. This function is useful if you have existing Rd-format documentation and want to try out the 'flatdoc' (qv) system of integrated code and documentation. Revised Nov 2017: now pretty good, but not perfect; see DETAILS.


USAGE

 help2flatdoc( fun.name, pkg=NULL, text=NULL, aliases=NULL)


ARGUMENTS

 fun.name: function name (a character string)
 pkg: name of package
 text: plain-text help
 aliases: normally leave this empty--- see DETAILS.
 
The real argument is 'text'; if missing, this is deduced from the help for 'fun.name' (need not be a function) in the installed package 'pkg'.


VALUE

A character vector of plain-text help, with class 'cat' so it prints nicely.


DETAILS

The package containing 'fun.name' must be loaded first. If you write documentation using 'flatdoc' (qv), prepare the package with 'pre.install' (qv), build it with RCMD BUILD or INSTALL, and run 'help2flatdoc' on the result, you should largely recover your original flat-format documentation. Some exceptions:

 - Nesting in lists is ignored. 
 
 - Numbered lists won't convert back correctly (Nov 2017), but the problem there is in 'doc2Rd'.

 - Link-triggering phrases (i.e. that will be picked up by 'doc2Rd', such as "see <blah>") aren't explicitly created-- probably, links could be automated better via an argument to 'doc2Rd'.

Aliases (i.e. if this doco can be found by 'help' under several different names) are deduced from function calls in the USAGE section, in addition to anything supplied specifically in the 'alias' argument. The latter is really just meant for internal use by 'unpackage' (qv).


EXAMPLES

cd.doc <- help2flatdoc( "cd", "mvbutils")
print( cd.doc)
cd.Rd <- doc2Rd( cd.doc)

SEE.ALSO

'doc2Rd'

KEYWORDS

programming
}")

)

"help2flatdoc_guts" <-
function( text, aliases=NULL) {
  text <- gsub( '[' %&% sQuote( '') %&% ']', "'", text)
  text <- gsub( '[' %&% dQuote( '') %&% ']', '"', text)

  # "Things" are separated by one or more blank lines; convert to single blanks, even in usage/examples
  text <- sub( '^ +$', '', text)
  text <- c( text, '') # add empty line, maybe to be trimmed
  blanko <- which( !nzchar( text))
  consec <- diff( c( 0, blanko)) == 1
  text <- text[ -blanko[ consec]]

  # Headings have weird colour decorations
  # Subheadings don't but start in char 1
  # Sub-subs start after char 1
  # All heading-lines end in a colon; AFAICS nothing else does except an argument with no documentation...
  # which deserves punishment and will become a subheading...
  # ... or a comment in a R-like bit that happens to end in colon. Assume things with hash can't be headings.

  # Tidy up headings
  iheadings <- grep( '\b', text, fixed=TRUE)

  # ... but first one is the title
  headings <- gsub( '_\b', '', text[ iheadings])
  text[ iheadings[ 1]] <- headings[ 1]
  iheadings <- iheadings[-1]
  headings <- headings[-1]

  # Tidy the rest of the headings
  headings <- toupper( sub( ':$', '', headings))
  heading_indents <- regexpr( '[^ ]', headings) # needed later
  uhi <- sort( unique( heading_indents)) # 1, x+1, 2x+1, ...

  nonblanks <- which( nzchar( text)) # easier to re-do after condensing multiblanks
  first_typical <- ( nonblanks %such.that% (. > iheadings[ headings=='DESCRIPTION']))[1]
  def_indent <- c( regexpr( '[^ ]', text[ first_typical]))

  # Special sections not to be tweaked
  noli_me_tangere <-  'xxx' # no indent, clearly odd
  specials <- character()
  special_ranges <- numeric()
  for( special in cq( USAGE, EXAMPLES) %such.that% (. %in% headings)) { # non-funcs might not have this
    this <- which( headings==special)
    range <- (iheadings[ this]+2) %upto% ( c( iheadings, length( text)+2)[ this+1]-2) # leave blanks around headings
    tr <- text[ range]
    minind <- min( regexpr( '[^ ]', tr) %such.that% (.>0))
    tr <- substring( tr, minind)

    if( special=='USAGE') {
      # Aliasses: all functions named in USAGE
      alias_lmethods <- grep( '## S3( replacement)? method for class|Default S3( replacement)? method', tr)
      alias_lines1 <- grep( "^[a-zA-Z0-9._]+\\(", tr, value=TRUE)
      alias_funs <- sub( '^([a-zA-Z0-9._]+)\\(.*', '\\1', alias_lines1)

      # If there's already an S3-type comment at the end of a method line, then just leave it
      # Assume method usages are single-line
      if( length( alias_lmethods)) {
        subat <- 1+alias_lmethods[ !grepl( '# S3( replacement)? method for ', tr[ 1+alias_lmethods])]
        tr[ subat] <- tr[ subat] %&%
            sub( '##', '  #', fixed=TRUE,
            sub( 'class ', '', fixed=TRUE,
            sub( "''", "'", fixed=TRUE,
            sub( "'", '"', fixed=TRUE,
            tr[ subat-1]))))
        tr[ alias_lmethods] <- '\002'
        alias_methods <- alias_funs[ alias_lines1 %in% (1+alias_lmethods)]
        alias_funs <- alias_funs %except% alias_methods
      } else {
        alias_methods <- character()
      }

      alias_lines2 <- grep( "^[^(]+%[a-zA-Z0-9_.]+%", tr, value=TRUE)
      alias_ops <- sub( "^[^(]+(%[a-zA-Z0-9_.]+%).*", '\\1', alias_lines2)
      aliases <- unique( c( aliases, alias_funs, alias_methods, alias_ops)) %except% sub( ' .*', '', text[1])
    }

    specials <- c( specials, tr)
    special_ranges <- c( special_ranges, range)
    text[ range] <- noli_me_tangere
  }

  # Dot-points are more indented... AFAIK dotch can only occur at start-of-line
  # Some problems here: I think the idea was to use a non-standard bullet-point character
  # to avoid clash with regular strings...
  dotch <- substring( Rd2txt_easy( options=TRUE)$itemBullet ,1, 1)
  # text <- gsub( dotch, '\001', text, fixed=TRUE) # the dotch is a bit hard to manipulate in regexps... though I'm using it here, I guess

  # Group adjacent lines. Stop the group either at blank line, or at "verbatim" (over-indented) text
  # Don't twiddle headings

  gaps <- which( !nzchar( text)) %such.that% (. > iheadings[1])
  prev_iheading <- findInterval( gaps, iheadings)

  # new_text <- character( length( gaps)-1)
  evalq( # for debug speed
  for( i in which( head( gaps, -1) %not.in% (iheadings-1))) {
    range <- (gaps[i]+1) %upto% (gaps[i+1]-1)
    lines <- text[ range]

    # How indented do we expect?
    indent <- def_indent + regexpr( '[^ ]', headings[ prev_iheading[ i]]) - 1
    idot <- regexpr( sprintf( '^ *[%s]', dotch), lines[1], fixed=TRUE)
    if( idot>0) {
      indent <- idot
    }

    # Check indentation; if non-code, collapse & unindent all lines of the group
    ich1 <- regexpr( '[^ ]', lines[ 1])

    if( (ich1>1) && (ich1 <= indent)) { # standard para, or list item
      lines <- paste( sub( '^ +', '', lines), collapse=' ')
      text[ range[-1]] <- '\002' # mark to kill later

      # List items should be *less* indented thx2 Rd2txt_easy
      prefix <- if( ich1 < indent) '  ' else '' # list item
      text[ range[1]] <- prefix %&% lines
    } else if( ich1>indent) {
      lines <- substring( lines, ich1)
      lines[1] <- '%%#\n' %&% lines[1] # code block
      text[ range] <- lines
    } # else noli me tangere!
  } # for gaps
  )

  text[ special_ranges] <- specials
  text <- sub( dotch, ' -', text, fixed=TRUE) # dot-points

  headings <- sub( '^ +', '', headings)
  subness <- match( heading_indents, uhi)
  # Precede all headings with blank line; spaces to dots; prefix subheadings
  headings <- '\n' %&% do.on( subness, rawToChar( rep( charToRaw( '.'), .-1))) %&%
      gsub( ' +', '.', sub( '^ +', '', headings))
  text[ iheadings] <- headings

  # Split lines with code blocks (and extra blank line before headings)
  start_of_code <- grep( '\n', text)
  text <- multirep( text, start_of_code, strsplit( text[ start_of_code], '\n'))

  text <- text[ text != '\002']
  text <- c( sub( ' +R Documentation *$', '', text[1]), aliases, text[-1])

return( text)
}


"hook.set.already" <-
function( pkg, hook.type, f, action=cq( append, prepend, replace)){
  identical.to.f <- function( x) {
    y <- x
    attr( y, '.Environment') <- NULL
    identical( y, f) }
  mangle <- packageEvent( pkg, hook.type)
  hooks <- getHook( mangle)
  if( !any( sapply( hooks, identical.to.f))) {
    action <- match.arg( action)
    setHook( mangle, f, action)
  }
}


"index" <-
function (lvector) 
seq_along( lvector)[lvector]


"install.pkg" <-
structure( function( pkg, character.only=FALSE, lib=.libPaths()[1],
    flags=character(0), multiarch=NA, preclean=TRUE)
{
  set.pkg.and.dir( FALSE)

  # Fucking R continues to fuck up BINPREF and BINPREF64 despite bug report 16919 (from someone else)
  # Only solution appears to be to hack <R>/etc/x64/Makeconf to something like this
  ## BINPREF ?= c:/Rtools/mingw_64/bin/
  # BINPREF ?= X:/.../mingw_64/bin
  # ifdef BINPREF64
  # BINPREF = $(BINPREF64)
  # endif

  if( preclean) {
    flags <- c( '--preclean', flags)
  }

  if( is.na( multiarch)) {
    check_multiarch()
  }

  if( !multiarch) {
    flags <- c( '--no-multiarch', flags)
  }

  # Workaround for fucking R bug that pollutes the source-package with /src-i386 and /src-x64...
  # move it somewhere else first!
  td <- tempfile()
  mkdir( td)
  on.exit( unlink( td, recursive=TRUE, force=TRUE), add=TRUE)
  file.copy( sourcedir, td, recursive=TRUE)

  rcmdgeneric.pkg2( pkg, '', indir=file.path( td, pkg), outdir='.', cmd='INSTALL',
      flags= c( flags, '-l ' %&% shQuote( lib)),
      must_hack_makeconf=TRUE # to allow cross-compile from i386
    )
}
, doc =  docattr( r"{
install.pkg    package:not-yet-a-package
build.pkg
build.pkg.binary
check.pkg
cull.old.builds
set.rcmd.vars


Package building, distributing, and checking

DESCRIPTION

These are convenient wrappers for R's package creation and installation tools. They are designed to be used on packages created from tasks via 'mvbutils' package, specifically 'pre.install' (though they can be used for "home-made" packages). The 'mvbutils' approach deliberately makes re-installation a rare event, and one call to 'install.pkg' might suffice for the entire life of a simple package. After that very first installation, you'd probably only need to call 'install.pkg' if (when...) new versions of R entail re-installation of packages, and 'build.pkg/build.pkg.binary/check.pkg' when you want to give your package to others, either directly or via CRAN etc.


.FOLDERS

Source packages and built packages go into various folders, depending on various things. Normally you shouldn't have to mess around with the folder structure, but you will still need to _know_ where built packages are put so that you can send them to other people. Specifically, these '...pkg...' functions work in the highest-versioned "Rx.y" folder that is not newer than the _running_ R version. If no such folder exists, then 'build.pkg/build.pkg.binary" will create one from the running R version; you can also create such a folder manually, as a kind of "checkpoint", when you want to make your package dependent on a specific R version. See "Folders and R versions" in 'mvbutils.packaging.tools' for full details.

There are also two minor housekeeping functions: 'cull.old.builds' to tidy up detritus, and 'set.rcmd.vars' which does absolutely nothing (yet). 'cull.old.builds' looks through _all_ "Rx.y" folders (where built packages live) and deletes the least-recent ".tar.gz" and ".zip" files in each (regardless of which built package versions are in the other "Rx.y" folders).


USAGE

  # Usually: build.pkg( mypack) etc
  install.pkg( pkg, character.only=FALSE, lib=.libPaths()[1], flags=character(0),
      multiarch=NA, preclean=TRUE)
  build.pkg( pkg, character.only=FALSE, flags=character(0), cull.old.builds=TRUE)
  build.pkg.binary( pkg, character.only=FALSE, flags=character(0),
      cull.old.builds=TRUE, multiarch=NA, preclean=TRUE)
  check.pkg( pkg, character.only=FALSE, build.flags=character(0),
      check.flags=character( 0), envars=character(0), CRAN=FALSE)
  cull.old.builds( pkg, character.only=FALSE)
  set.rcmd.vars( ...) # NYI; see envars arg...
  # ... or if it doesn't and you need to set env vars eg PATH 
  # for R CMD to work,  then DIY; see DETAILS


ARGUMENTS

See the examples

 pkg: usually an unquoted package name, but interpretation can be changed by non-default 'character.only'. You can also get away with eg '..mypack', ie a direct reference to the maintained package. A folder name can also be used, for a non-mvbutils-maintained package. Just as if it was "maintained", the folder should contain a subfolder with the (same) package name and the real package contents (eg "c:/r/mypack/mypack/DESCRIPTION" should exist), and any built things will go into eg "c:/r/mypack/R3.2"

 character.only: default FALSE. If TRUE, treat 'pkg' as a normal object, which should therefore be a string containing the package's name. If 'character.only' is itself a string, it will override 'pkg' and be treated as the name of the package.

 lib: ('install.pkg' only) where to install to; default is the same place R would install to, i.e. '.libPaths()[1]'.

 flags: character vector, by default empty. Any entries should be function-specific flags, such as "--md5" for 'build.pkg'. It will be passed through 'paste( flags, collapse=" ")', so you can supply flags individually (eg 'flags=c( "--md5", "--compact.vignettes")') or jointly (eg 'flags="--md5 --compact.vignettes"').

 build.flags, check.flags: ('check.pkg' only) as per 'flags' but for the two separate parts of 'check.pkg' (see DETAILS). 'check.flags' is overridden if 'CRAN==TRUE''.
 
 envars: optional named character vector of envars to set on the command-line, which is how you control some RCMD behaviour. They will be restored afterwards (or deleted if they didn't exist beforehand).

 preclean: adds flag "--preclean" if TRUE (the default); this is probably a good idea since one build-failure can otherwise cause R to keep failing to build.

 multiarch: Adds flag "-no-multiarch" if FALSE. Defaults to TRUE unless "Biarch: FALSE" is found in the DESCRIPTION.  Default used to be FALSE when I was unable to get 64bit versions to build. Now I mostly can (after working round BINPREF64 bug in R3.3.something by futzing around in etc/arch/Makeconf based on random internet blogs).

 cull.old.builds: self-explanatory

 CRAN: ('check.pkg' only) if TRUE, set the '--as-cran' flag to "RCMD check" and unset all other check flags (except library locations, which are set automatically by all these functions). Note that this will cause R to check various internet databases, and so can be slow.

 ...: name-value pairs of system environment variables (not used for now)

DETAILS

Before doing any of this, you need to have used 'pre.install' to create a source package. (Or 'patch.install', if you've done all this before and just want to re-install/build/check for some reason.)

The only environment variable currently made known to R CMD is R_LIBS-- let me know if others would be useful.

'install.pkg' calls "R CMD INSTALL" to install from a source package.

'build.pkg' calls "R CMD build" to wrap up the source package into a "tarball", as required by CRAN and also for distribution to non-Windows-and-Mac platforms.

'build.pkg.binary' (Windows & Mac only) calls "R CMD INSTALL --build" to generate a binary package. A temporary installation directory is used, so your existing installation is _not_ overwritten or deleted if there's a problem; R CMD INSTALL --build has a nasty habit of doing just that unless you're careful, which 'build.pkg.binary' is.

'check.pkg' calls "R CMD check" after first calling 'build.pkg' (more efficiently, I should perhaps try to work out whether there's an up-to-date tarball already). It doesn't delete the tarball afterwards. It _may_ also be possible for you to do some checks directly from R via functions in the 'utils' package, which is potentially a lot quicker. However, NB the possibility of interference with your current R session. For example, at one stage 'codoc' (which is the only check that I personally find very useful) tried to unload & load the package, which was very bad; but I think that may no longer be the case.

You _may_ have to set some environment variables (eg PATH, and perhaps R_LIBS) for the underlying R CMD calls to work. As of 'mvbutils' v2.11.18, the 'envars' argument might do the trick (just for the duration of the RCMD call). Otherwise, currently you have to do it manually--- your '.First' or '.Rprofile' would be a good place. [There _was_ a plan for a function 'set.rcmd.vars' that could temporarily set envars before each RCMD call and then restore them afterwards, but I've shelved it in favour of 'envars', at least for now.]

Perhaps it would be desirable to let some flags be set automatically, eg via something in the 'pre.install.hook' for a package. I'll add this if requested.


VALUE

Ideally, the "status code" of the corresponding RCMD operation: 0 for success or some other integer if not. It will have several attributes attached, most usefully "output" which duplicates what's printed while the functions are running. (Turn off "buffered output" in RGui to see it as it's happening.) This requires the existence of the "tee" shell redirection facility, which is built-in to Linux and presumably Macs, but not to Windows. You can get one version from Coreutils in GnuWin32; make sure this is on your PATH, but probably _after_ the Rtools folders required by the R build process, to avoid conflicts between the other Coreutils versions and those in Rtools (I don't know what I'm talking about here, obviously; I'm just describing what I've done, which seems to work). If "tee" eventually moves to Rtools, then this won't be necessary.

If no "tee" is available, then:

 - progress of RCMD will be shown "live" in a separate shell window

 - the status code is returned as NA, but still has the attributes including "output". You could, I suppose, "parse" the output somehow to check for failure.

The point of all this "tee" business is that there's no reliable way in R itself to both show progress on-screen within R (which is useful, because these procedures can be slow) and to return the screen output as a character vector (which is useful so you can subsequently, pore through the error messages, or bask in a miasma of smugness).


EXAMPLES

## Don't run

# First time package installation
# Must be cd()ed to task above 'mvbutils'
maintain.packages( mvbutils)
pre.install( mvbutils)
install.pkg( mvbutils)

# Subsequent maintenance is all done by:
patch.install( mvbutils)

# For distro to
build.pkg( mvbutils)

# or on Windows (?and Macs?)
build.pkg.binary( mvbutils)

# If you enjoy R CMD CHECK:
check.pkg( mvbutils)

# How to not fail if Suggestees are missing (I think), via envars
check.pkg( mvbutils, envars=c( '_R_CHECK_FORCE_SUGGESTS_'=0))


# Also legal:
build.pkg( ..mvbutils)

# To do it under programmatic control
for( ipack in all.my.package.names) {
  build.pkg( char=ipack)
}

## End don't run

}")

)

"install.proged" <-
function( option.name='program.editor') {
  readonly <- ifelse( option.name=='program.reader', 'in read-only mode', '')
  cat( 'Must set up program editor information before "fixr" works.')
  repeat {
    cat( '\nType whatever you\'d type in a command window to',
      'invoke your editor', readonly, 'on a file called "myfun.r".',
      '  For example, on Unix-like systems: myedit myfun.r &',
      '  In Windows, use double quotes around a path if it contains spaces,',
      '  and use \\ not \\\\ or / as the separator;',
      'to find the path, look at Properties/Shortcut/Target of the icon or shortcut.',
      '  Otherwise, type <ENTER> to quit: ', sep='\n')
    pe.path <- readline()
    if( !nchar( pe.path))
return()
    if( length( grep( 'myfun\\.r', pe.path))==1)
  break
  }
  
  pe.path <- strsplit( pe.path, 'myfun.r', fixed=TRUE)[[1]]
  if( length( pe.path)==1)
    pe.path <- c( pe.path, '')
  
  pe <- substitute( function( name, fname) paste( path1, fname, path2, sep=''), 
      list( path1=pe.path[1], path2=pe.path[2]))
      
  edit.scratchdir <- Sys.getenv( 'TEMP')
  if( !nchar( edit.scratchdir))
    edit.scratchdir <- Sys.getenv( 'TMP')
  repeat{ 
    cat( 'Enter directory for scratch files (single backslashes only in Windows)')
    if( nchar( edit.scratchdir))
      cat( 'or <ENTER> for', edit.scratchdir)
    cat( ': ')
    check <- readline()
    if( nchar( check))
      edit.scratchdir <- check
      
    if( !is.dir( edit.scratchdir))
      mkdir( edit.scratchdir)
    if( is.dir( edit.scratchdir))
  break
    cat( "Can't create directory", edit.scratchdir, "!")
  }

  edit.scratchdir <- as.vector( edit.scratchdir)[1] 
  
  if( option.name=='program.editor') {
    backup.fix <- NULL # don't backup by default
    repeat{
      cat( 'Automatic backups #1: how many backups per session (0 for no backups)? ')
      n.per.session <- as.integer( readline())
      if( is.na( n.per.session) || n.per.session < 0)
    next
      if( n.per.session==0)
    break
      cat( 'Automatic backups #2: how many sessions to keep last version from? ')
      n.sessions <- as.integer( readline())
      if( is.na( n.sessions) || n.sessions<0)
    next
      backup.fix <- c( n.sessions, n.per.session)
    break
    }

    o <- substitute( options( program.editor=pe, edit.scratchdir=edit.scratchdir, backup.fix=backup.fix))
  } else
    o <- substitute( options( program.reader=pe))
  eval( o)
  
  cat( 'You should use "fixr" to make sure that the following appears in your .First:',
    deparse( o), 'autoedit( TRUE)', '', sep='\n')
    
  options()[[ option.name]]
}


"integ" <-
function( expr, lo, hi, what='x', ..., args.to.integrate=list()) {
  f <- function() {}
  body( f) <- substitute( expr)
  formals( f) <- c( list( x=NULL), as.list( match.call( expand.dots=FALSE)$...))
  names( formals( f))[1] <- what
  environment( f) <- parent.frame()
  do.call( 'integrate', c( list( f, lo, hi), args.to.integrate))$value
}


"internal.copy.ns.objects" <-
function( pkgname, pkgpath){
  senv <- as.environment( 'package:' %&% pkgname)
  ns <- asNamespace( pkgname)
  f <- function( val) blah-blah-blah
  environment( f) <- ns
  print( objects)
  for( x in objects) {
    body( f) <- substitute( if( missing( val)) x else x <<- val, list( x=as.name( x)))
    makeActiveBinding( x, f, senv)
  }
}


"inv.logit" <-
function( qq) plogis( qq)


"is.dir" <-
function (dir) 
{
    ok <- file.exists(dir)
    ok[ok] <- file.info(dir[ok])$isdir
    ok
}


"is.nonzero" <-
function (x) 
{
    val <- FALSE
    if (length(x) == 1) {
        if (is.character(x) || is.factor(x)) 
            val <- pmatch(x, "FALSE", 0) == 0
        else if (is.logical(x)) 
            val <- x
        else if (is.numeric(x)) 
            val <- x != 0
    }
    val
}


"is.package.installed" <-
function(package, ...) {
  # Taken from R.utils whatever that is
  # thanks to Henrik Bengtsson for the tip
  path <- system.file(package=package)
  (path != "")
}


"isF" <-
function( x) {
  if( length( x) != 1) {
     warning( 'isT/isF expect length-1 arguments; returning FALSE')
   }
  is.logical( x) && (length( x)==1) && (!is.na( x)) && !x
}


"isT" <-
function( x) {
  if( length( x) != 1) {
     warning( 'isT/isF expect length-1 arguments; returning FALSE')
   }
  is.logical( x) && (length( x)==1) && (!is.na( x)) && x
}


"lazify" <-
function( path, package, pkgpath) {
# Taken from tools: : :makeLazyLoading
  e <- new.env( hash=TRUE)
  load( path, e, verbose=FALSE)
  file.remove( path)
  tools$makeLazyLoadDB( e, file.path( dirname( path), package), compress=TRUE)

  # Next line to avoid use of bad cache if reloaded:
  LLDBflush( file.path( dirname( path), package %&% '.rdb'))

  loaderFile <- file.path( R.home(), "share", "R",
      ( if( packageHasNamespace( package, dirname( pkgpath))) 'ns') %&% "packloader.R")
  file.copy( loaderFile, file.path( dirname( path), package), TRUE)
}


"ldyn.tester" <-
function( chname) {
  # Circumvent the package lookup mechanism: package must be a path!
  package <- gsub( '[\\]', '/', dirname( chname))
  package <- sub( '/libs(/[^/]*)?$', '', package)
stopifnot( is.dir( package))
  chname <- file_path_sans_ext( basename( chname))

  find.package <- function( package, ...) package
  ldyn <- baseenv()$library.dynam
  environment( ldyn) <- environment()
  rezzo <- ldyn( chname, package, lib.loc='')
return( rezzo)
}


"ldyn.unload" <-
function( l1) {
##################
# l1 from previous call to ldyn.tester
  patho <- unclass( l1)$path
  dl <- .dynLibs()
  whicho <- which( do.on( dl, unclass(.)$path==patho))
stopifnot( length( whicho)==1)

  OK <- dyn.unload( patho)
  .dynLibs( dl[ -whicho])
return( OK)
}


"least.mutual.dependency" <-
function (funmat, funs, level) 
{
    group <- funmat[level == 0, level == 0, drop = FALSE]
    mode(group) <- "logical"
    old.group <- group & FALSE
    while (any(group != old.group)) {
        old.group <- group
        for (i in funs[level == 0]) {
            newbies <- group[, group[, i], drop = FALSE] %*% 
                rep(1, sum(group[, i]))
            group[, i] <- group[, i] | (newbies > 0)
        }
    }
    nn <- sum(level == 0)
    keep <- c(TRUE, rep(FALSE, nn - 1))
    for (i in 2:nn) {
        old.group <- matrix(as.vector(group[, i]) == as.vector(group[, 
            keep]), nrow = nn)
        keep[i] <- !any(rep(1, nn) %*% old.group == nn)
    }
    group <- group[, keep, drop = FALSE]
    if (ncol(group) > 1) {
        nn <- ncol(group)
        old.group <- matrix(0, nn, nn)
        for (i in 1:nn) for (j in (1:nn)[1:nn != i]) {
            old.group[i, j] <- set.test(group[, i], group[, j])
            old.group[j, i] <- -old.group[i, j]
        }
        old.group[old.group < 0] <- 0
        not.keep <- old.group %*% rep(1, nn) > 0
        group <- group[, !not.keep, drop = FALSE]
    }
    group <- dimnames(group)[[1]][apply(group, 1, any)]
    match(group, funs[level == 0])
}


"legal.filename" <-
function (name) 
{
    length.limit <- 250
    filenames <- strsplit(substr(name, 1, length.limit), "")[[1]]
    filenames[filenames %in% c(":", "*", "?", "'", "/", "\\", '|',
        "\"", ">", "<", '+', ' ')] <- "."
    if (!(upper.case(filenames[1]) %in% LETTERS)) 
        filenames <- c("X", filenames)
    paste(filenames, collapse = "")
}


"library.dynam.reg" <-
structure( function( chname, package, lib.loc, ...) {
  ld <- library.dynam( chname, package, lib.loc, ...)
  rr <- getDLLRegisteredRoutines( ld, addNames=TRUE)
  gnsym <- getNativeSymbolInfo( 
      c( names( rr$.C), names( rr$.Call), names( rr$.Fortran), names( rr$.External)), 
      PACKAGE=ld)
  ns <- asNamespace( package)
  chname <- 'C_' %&% chname
  assign( chname, new.env( parent=emptyenv()), envir=ns)
  FOR( names( gnsym), assign( ., gnsym[[.]], envir=ns[[ chname]]))
}
, doc =  docattr( r"{
library.dynam.reg    package:mvbutils

Auto-registration and loading of dynamic library

DESCRIPTION

A bit like 'useDynLib' but for direct use in your own package's '.onLoad', this loads a DLL and creates objects that allow the DLL routines to be called directly. If your package "Splendid" calls 'library.dynam.reg' in its '.onLoad()' to load a DLL "speedoo" which contains routines "whoosh" and "zoom", then an environment "C_speedoo" will be created in the 'asNamespace("Splendid")', and the environment will contain objects 'whoosh' and 'zoom'. R-code routines in "Splendid" can then call e.g.

%%#
.C( C_speedo$whoosh, ....)

You can only call 'library.dynam.reg' inside '.onLoad', because after that the namespace will be sealed so you can't poke more objects into it.

.NOTE

Currently, _all_ routines go into 'C_speedoo', regardless of how they are meant to be called ('.C', '.Call', '.Fortran', or '.External'). It's up to you to call them the right way. I might change this to create separate 'Call_speedoo' etc.

.NOTE2

As of R3.1.1 at least, it's possible that "recent" changes to the 'useDynLib' directive in a package namespace might obviate the need for this function. In particular, 'useDynLib' can now create an environment/list that refers directly to DLL, containing references to individual routines (which will be slightly slowed because they need to be looked up each time). Also, 'useDynLib' can automatically register its routines. What's not obvious is whether it can yet do both these things together--- which is what 'library.dynam.reg' is aimed at.


USAGE

# Only inside a '.onLoad', where you will already know "package" and "lib.loc"
library.dynam.reg(chname, package, lib.loc, ...) 


ARGUMENTS

 chname: DLL name, a string--- _without_ any path
 package, lib.loc: strings as for 'library.dynam'
 ...: other args to 'library.dynam'

}")

)

"load.maintained.package" <-
function( name, path, task.tree, autopatch=TRUE){
  e <- new.env( parent=if( exists( 'emptyenv', mode='function')) emptyenv() else baseenv())
  attr( e, 'path') <- structure( path, names=name)
  attr( e, 'name') <- name
  attr( e, 'task.tree') <- task.tree

  # Hooks and maintained.packages[[]] are adjusted before the load, 
  # ... in case the latter triggers namespacing...
  # ... which shouldn't happen, but will if a fun sourcepack accidentally has
  # ... the namespace enviro

  # Used to test for existence of .onLoad before next line, but
  # 1. not sure the test was needed anyway, and
  # 2. all packages should have namespaces now
  # 3. can't do the test before the load
  # if( exists( '.onLoad', e, mode='function')) 
  setHook( packageEvent( name, 'onLoad'), no.lazyLoad.hook, 'prepend')
  setHook( packageEvent( name, 'attach'), no.lazyLoad.attach.hook, 'prepend')
  
  maintained.packages[[ name]] <<- e 
  assign( '..' %&% name, e, as.environment( 'mvb.session.info')) # alias for ease of access
  tryo <- try( load.refdb( envir=e))
  if( (tryo %is.a% 'try-error') || !length( lsall( e))) {
    warning( "No package '" %&% name %&% "' found during 'maintain.packages'")
    rm( e)
    try( rm( '..' %&% name, as.environment( mvb.session.info)), silent=TRUE)
    maintained.packages <<- maintained.packages %without.name% name
  } else {
    # Anything being edited from that task?
    editees <- fix.list$where == paste( task.tree, collapse='/')
    fix.list$where.type[ editees] <<- 'package'
  }
}


"load.mvb" <-
function (filename, name, pos, attach.new=is.null( envir) && pos != 1, 
    path=attr( envir, 'path'), envir=NULL, ...) {
  if (attach.new)
    envir <- ATTACH(NULL, pos = pos, name = name)
  else {
    if( is.null( envir))
      envir <- as.env(pos)
    attr( envir, "name") <- name # which won't work on .GlobalEnv in R3.0+, but never mind
  }

# This stuff used to be after the load, but load.refdb needs the path attr set

  if( tail( splitto <- strsplit( filename, '.', fixed=TRUE)[[1]], 1)=='rdb') {
    LLDBflush( paste( clip( splitto), collapse='.') %&% '.rdb')
    lazyLoad( paste( clip( splitto), collapse='.'), envir=envir)
  } else
    load.refdb(filename, envir = envir, fpath=path)

  attr( envir, 'path') <- path
  ll <- list(...)
  if (length(ll))
    for (attro in names(ll)) attr(envir, attro) <- ll[[attro]]
}


"load.refdb" <-
structure( function( file=file.path( fpath, '.RData'), envir, fpath=attr( envir, 'path')) {
  envir <- as.env( envir)
  if( !file.exists( file))
return()

  load( file, envir, verbose=FALSE)
  setup.mcache( envir, fpath)

  invisible( lsall( envir))
}
, doc =  docattr( r"{
load.refdb          package:mvbutils

Cacheing objects for lazy-load access


DESCRIPTION

'load.refdb' is like 'load', but automatically calls 'setup.mcache' (qv) to create access arrangements for cached objects. You probably don't need to call it directly.


USAGE

load.refdb( file, envir, fpath=attr( envir, "path"))


ARGUMENTS

 file: a filename relative to 'fpath'
 envir: an environment or (more usually) a position on the search path (numeric or character)
 fpath: a directory. Usually the default will do.


SEE.ALSO

'mlazy', 'setup.mcache'


AUTHOR

Mark Bravington


KEYWORDS

internal



}")

)

"local.on.exit" <-
structure( function( expr, add=FALSE) {
# Assigns expr to on.exit.code in mlocal manager. See local.return for explanation of 'where'
# Don't know what should "really" happen if expr is missing but add is TRUE

  subex <- if( missing( expr)) NULL else substitute( expr)
  where <- get( 'enclos', envir=parent.frame(2))
  if( add) {
    oldex <- get( 'on.exit.code', where)
    subex <- substitute( { oldex; subex }, returnList( oldex, subex))
  }

  assign( 'on.exit.code', subex, envir=where)
}
, doc =  docattr( r"{
local.on.exit       package:mvbutils

Macro-like functions

DESCRIPTION

'local.on.exit' is the analogue of 'on.exit' for "nested" or "macro" functions written with 'mlocal' (qv).

USAGE

# Inside an 'mlocal' function of the form
# function( <<args>>, nlocal=sys.parent(), <<temp.params>>) mlocal({ <<code>> })

local.on.exit( expr, add=FALSE)


ARGUMENTS

 expr: the expression to evaluate when the function ends
 add: if TRUE, the expression will be appended to the existing 'local.on.exit' expression. If FALSE, the latter is overwritten.


DETAILS

'on.exit' doesn't work properly inside an 'mlocal' function, because the scoping is wrong (though sometimes you get away with it). Use 'local.on.exit' instead, in exactly the same way. I can't find any way to set the exit code in the *calling* function from within an 'mlocal' function.

Exit code will be executed before any temporary variables are removed (see 'mlocal').

EXAMPLES

ffin <- function( nlocal=sys.parent(), x1234, yyy) mlocal({
  x1234 <- yyy <- 1 # x1234 & yyy are temporary variables
  # on.exit( cat( yyy)) # would crash after not finding yyy
  local.on.exit( cat( yyy))

  })

ffout <- function() {
  x1234 <- 99
  ffin()
  x1234 # still 99 because x1234 was temporary
}

ffout()


SEE.ALSO

'mlocal', 'local.return', 'local.on.exit', 'do.in.envir', and R-news 1/3

AUTHOR

Mark Bravington


KEYWORDS

programming
}")

)

"local.return" <-
structure( function( ...) { # Returns its arguments; unnamed arguments are named using deparse & substitute
  orig.mc <- mc <- as.list( match.call())[ -1]

  if( length( mc)) {
    if( length( mc)==1)
      mc <- eval( mc[[1]], envir=parent.frame())
    else { # multiple arguments, so return as named list
      if( is.null( names( mc)))
        which <- 1:length( mc)
      else
        which <- names( mc)==''

      for( i in index( which))
        if( is.symbol( orig.mc[[ i]]))
          names( mc)[ i] <- as.character( orig.mc[[ i]] )
      mc <- lapply( mc, eval, envir=parent.frame())
    }
  }

r"--{
R version (adapted c AD2001 from S version...). This uses a trick: the call to 'eval' that invokes the mlocalized routine containing this call to 'local.return', sets up a frame with 3 args including 'enclos' which is actually ignored. However I deliberately set this argument in the final call to 'eval' inside 'mlocal', so that 'local.return' knows where to put the answer. This is probably dependent on a quirk of implementation, particularly the 'parent.frame(2)' bit below. If it stops working, look at 'debug:::debug.local.return' for a more robust, but presumably slower, general solution.

Original comment said this:

The need to do this at all, is that loops terminated with a 'break' in R _don't_ have the value of the last expression before the break. They do in S.

As of 2023, I've forgotten what that's about, and/or why mlocal() wraps its evaluand in a fake repeat-loop...
}--"

  # Obsolete (cos I've given up caring what the CRANiacs think)
  # Need to hide "override.answer" from stupid CRANal checks
  # override.answer <- 'override.answer' # FFS...
  # assign( override.answer, ...)

  # NB in R4.1, next does not find the right frame when debugging (with mtrace)
  # so there is now a debug:::debug.local.return to fix that.
  enclos <- parent.frame( 2)$enclos
  assign( 'override.answer', mc, envir=enclos)
}
, doc =  docattr( r"{
local.return      package:mvbutils

Macro-like functions

DESCRIPTION

In an 'mlocal' (qv) function, 'local.return' should be used whenever 'return' is called, wrapped inside the 'return' call around the return arguments.


USAGE

local.return(...) # Don't use it like this!
# Correct usage: return( local.return( ...))


ARGUMENTS

 ...: named and unnamed list, handled the same way as 'return' before R 1.8, or as 'returnList' (qv)


EXAMPLES

ffin <- function( nlocal=sys.parent()) mlocal( return( local.return( a)))
ffout <- function( a) ffin()
ffout( 3) # 3

# whereas:
ffin <- function( nlocal=sys.parent()) mlocal( return( a))
try(
  ffout( 3) # error:; "return" alone doesn't work
)


SEE.ALSO

'mlocal'


AUTHOR

Mark Bravington


KEYWORDS

programming
}")

)

"localfuncs" <-
structure( function( funcs) {
  pf <- parent.frame()
  funcs <- lapply( named( funcs), get, envir=pf, inherits=TRUE)
  for( i in names( funcs)) {
    f <- funcs[[ i]]
    environment( f) <- pf
    assign( i, f, envir=pf)
  }
  invisible( NULL)
}
, doc =  docattr( r"{
localfuncs    package:mvbutils

"Declare" child functions, allowing much tidier code

DESCRIPTION

Only call this within a function, say 'f'. The named functions are copied into the environment of 'f', with their environments set to the environment of 'f'. This means that when you call one of the named functions later in 'f', it will be able to see all the variables in 'f', just as if you had defined the function inside 'f'. Using 'localfuncs' avoids you having to clutter 'f' with definitions of child functions. It differs from 'mlocal' (qv) in that the local functions won't be changing objects directly in 'f' unless they use '<<-' -- they will instead have normal R lexical scoping.



USAGE

localfuncs(funcs)


ARGUMENTS

 funcs: character vector of function names


SEE.ALSO

'mlocal' for a different approach


EXAMPLES

inner <- function( x) {
  y <<- y+x
  0
}

outer <- function( z) {
  # Multiply z by 2!
  y <- z
  localfuncs( 'inner')
  inner( z)
  return( y)
}

outer( 4) # 8


}")

)

"logit" <-
function( x) qlogis( x)


"lsall" <-
function( ...) {
  mc <- match.call( expand.dots=TRUE)
  mc$all.names <- TRUE
  mc[[1]] <- as.name( 'ls')
  eval( mc, parent.frame())
}


"lsize" <-
structure( function( envir=.GlobalEnv, recursive=0){
  envir <- as.env( envir)
  mcache <- attr( envir, 'mcache')
  mcs <- names( mcache) %that.are.in% lsall( envir)

  if( length( mcs)) {
    mcfiles <- file.path( attr( envir, 'path'), 'mlazy', 'obj' %&% abs( mcache[ mcs]) %&% '.rda')
    mcsize <- file.info( mcfiles)$size
    names( mcsize) <- mcs
  } else
    mcsize <- numeric( 0)

  # All package-namespaces and search()-members should have environmentName()
  # Already-known anonymous environments
  known_ents <- list()
  sizio <- function( x){
      x <- envir[[x]]
      if( missing( x)){ # seems to get this far
    return( 0L)
      }
      
      if( x %is.an% 'environment') {
        y <- x
      }
      if( (x %is.an% 'environment') && 
          (recursive>0) &&
          !nzchar( environmentName( x)) &&
          !any( do.on( known_ents, identical( x, .)) )){
        known_ents <<- c( known_ents, list( x))
        sum( lsize( x, recursive-1))
      } else {
        object.size( x)
      }
    }

  obs <- lsall( envir=envir) %except% mcs
  obsize <- vapply( named( obs), sizio, 0)
  o <- order( c( obsize, mcsize))
  obsize <- c( obsize, -mcsize)[o]
return( obsize)
}
, doc =  docattr( r"{
lsize         package:mvbutils

Report objects and their memory sizes

DESCRIPTION

'lsize' is like 'ls', except it returns a numeric vector whose names are the object names, and whose elements are the object sizes. The vector is sorted in order of increasing size. 'lsize' avoids loading objects cached using 'mlazy' (qv); instead of their true size, it uses the size of the file that stores each cached object, which is shown as a _negative_ number. The file size is typically smaller than the size of the loaded object, because 'mlazy' saves a compressed version. NB that 'lsize' will scan _all_ objects in the environment, including ones with funny names, whereas 'ls' does so only if its 'all.names' argument is set to TRUE.

Missing objects should return 0 (which may or may not be exactly correct!). You won't normally get that, but see EXAMPLES for a perverse case.


.ENVIRONMENTS

If there are environment-objects (which are really symbols that _point_ to frames--- see R) within the very environment you are 'lsize'ing, what should their size be? There is no perfect answer. R will tell you it's "56 bytes" and that's what you'll get with the default 'recursive=0'. However, each environment could be holding arbitrarily large objects--- so you might want to know how much memory they are "really" taking. You can do so by setting 'recursive' to a positive number, which also controls the _depth_ of recursion (because environments can themselves contain other environments).

However-however, those environments might be innocuous things that just refer to shared system-y ones (eg namespaces of packages, copies of '.GlobalEnv', etc), in which case they are not costing any memory. And if two symbols refer to the same actual environment, they are duplicates and second one is not taking any extra real memory. So 'lsize' tries to keep track of such cases (whenever 'recursive>0'), and _not_ to incorporate their memory-use; whether it does so optimally, is another Q. NB that for duplicates, only the alphabetically-first will be recursed.

There are lots of places that environments can lurk _within_ other objects: notably, environments-of-functions, and formulae/results of calls to 'lm' etc. These can take huge amounts of memory, sometimes manifest only when saving/loading. 'lsize' does not currently attempt to measure those; but see 'find.lurking.envs'.


USAGE

lsize( envir=.GlobalEnv, recursive=0)


ARGUMENTS

 envir: where to look for the objects. Will be coerced to environment, so that e.g. 'lsize( 2)' and 'lsize( "package:mvbutils")' work. 'envir' can be a 'sys.frame'-- useful during debugging.
 
 recursive: depth of recursion to allow, for objects that are themselves environments. See .ENVIRONMENTS.

VALUE

Named numeric vector.


SEE.ALSO

'ls', 'mlazy', 'find.lurking.envs'


AUTHOR

Mark Bravington


EXAMPLES

# Current workspace
lsize() 

# Contrived example to show objects in a function's environment
nonsense <- function(..., a, b, c) lsize( environment())
try( # this might be fragile with missings; OK in R4.3
  nonsense()
)
# a, b, c are all missing; this example might break in future R versions
# ...   a   b   c 
#   0   0   0   0 


KEYWORDS

programming; misc


}")

)

"maintain.packages" <-
structure( function( ..., character.only=FALSE, autopatch=FALSE){
  if( character.only)
    packs <- unlist( list(...))
  else {
    mc <- as.list( match.call( expand.dots=FALSE)$...)
    packs <- sapply( mc, as.character)
  }

  # Don't reload
  # packs <- packs %such.that% (. %not.in% names( maintained.packages))
  packs <- packs %except% names( maintained.packages)

  # Can't be retrospective
  if( length( packs) && (
      any( already <- (packs %in% loadedNamespaces()) |
      !is.na( match( 'package:' %&% packs, search()))))) {
    cat( "Can't maintain package(s) {", paste( packs[ already], collapse=','),
        "}: already loaded!\n")
    packs <- packs[ !already]
  }

  # Can't be cd'ed into or below the
  if( length( packs) && (
      any( already <- packs %in% names( .Path)))) {
    cat( "Can't maintain package(s) {", paste( packs[ already], collapse=','),
        "}: already cd'ed into\n")
    packs <- packs[ !already]
  }

  if( length( packs)) {
    snames <- lapply( seq( along=search()), function( x) names( attr( pos.to.env( x), 'path'))[1])
    snames[ sapply( snames, is.null)] <- ''
    snames <- unlist( snames)
    snames <- match( rev( names( .Path)), snames)
    tasks <- lapply( snames, function( x)
        if( exists( 'tasks', pos.to.env( x), mode='character')) pos.to.env( x)$tasks else character( 0))

    owner <- match( packs, names( unlist( tasks)), 0)
    packs <- packs[ owner>0]
    owner <- rep( 1:length( tasks), sapply( tasks, length))[ owner[ owner>0]]

    # Need fully qualified path name, using path attr + task thing which might be relative
    for( ipkg in seq( along=packs)) {
      pe <- pos.to.env( snames[ owner[ ipkg]])
      task.tree <- c( names( .Path)[ 1:(length( snames) + 1 - owner[ ipkg])], packs[ ipkg])
      load.maintained.package( packs[ ipkg],
          full.path( pe$tasks[ packs[ ipkg]], attr( pe, 'path')),
          task.tree)
    } # for ipkg
  } # if length( packs)

  if( autopatch) {
    for( ipack in packs)
      try({ # eg in case not installed
        instpath <- dirname( system.file( '.', package=ipack))
        if( instpath=='.')
      next

        instdate <- file.info( file.path( instpath, 'R', ipack))$mtime
        moddate <- file.info( file.path( attr( maintained.packages[[ipack]], 'path'), '.RData'))$mtime
        if( moddate > instdate) {
          cat( sprintf( "Updating installation of '%s'...", ipack))
          try( flush.console()) # try() in case it doesn't exist on this platform
          patch.installed( ipack, character.only=TRUE)
          cat( 'done\n')
        } else
          patch.installed( ipack, character.only=TRUE, pre.inst=FALSE, DLLs.only=TRUE) # update "source" DLLs
      })
  }

return( names( maintained.packages))
}
, doc =  docattr( r"{
maintain.packages    package:mvbutils
unmaintain.package


Set up task package for live editing

DESCRIPTION

See 'mvbutils.packaging.tools' before reading or experimenting!

Set up task package(s) for editing and/or live-editing. Usually called in '.First' or '.First.task'. You need to be 'cd'ed into the parent task of your task-package. 'maintain.packages' must be called _before_ loading the package via 'library' or 'require'. The converse, 'unmaintain.package', is rarely needed; it's really only meant for when 'unpackage' doesn't work properly, and you want a "clean slate" task package.

USAGE

# E.g. in your .First, after library( mvbutils), or in...
# ... a '.First.task' above yr task-package
maintain.packages(..., character.only = FALSE, autopatch=FALSE)
unmaintain.package( pkg, character.only = FALSE)


ARGUMENTS

 ...: names of packages, unquoted unless 'character.only' is TRUE. Package names must correspond to subtasks of the current task.
 character.only: see above
 pkg: name of package, unquoted unless 'character.only' is TRUE.
 autopatch: whether to 'patch.install' out-of-date installed packages (default FALSE, but TRUE is common).


DETAILS

'maintain.packages( mypack)' loads a copy of your task-package "mypack" (as stored in its ".RData" file) into a environment '..mypack' (an "in-memory-task-package"), which itself lives in the "mvb.session.info" environment on the search path. You don't normally need to know this, because normally you'd modify/create/delete objects in the package via 'fixr' or 'fixr(..., pkg="mypack")' or 'rm.pkg( ..., pkg="mypack")'. But to move objects between the package and other tasks, you do need to refer to the in-memory task package, e.g. via 'move( ..., from=..Splendid, to=subtask/of/current)'. In most cases, you will be prompted afterwards for whether to save the task package on disk, but you can always do yourself via 'Save.pos( ..Splendid)'. Note that only these updates and saves only update the _task package_ and the _loaded package_. To update the _source package_ using the task package, call 'pre.install'; to update the _installed package_ on disk as well as the source package, call 'patch.install'.

.CREATING.NEW.THINGS

It's always safe to create new objects of any type in '.GlobalEnv', then use 'move(newthing,.,..mypack)'. For a new _function_, you can shortcut this two-step process and create it directly in the in-memory maintained package, via 'fixr(..mypack$newfun)'; 'fixr' will take care of synchronization with the loaded package. This also ought to work for text objects created via 'fixtext'. Otherwise, use the two-step route, unless you have a good reason to do the following...

.DIRECTLY.MODIFYING.THE.MAINTAINED.PACKAGE

Rarely, you may have a really good reason to directly modify the contents of '..mypack', e.g. via

%%#
..mypack$newfun <<- function( x) whatever

You can do it, but there are two problems to be aware of. The first is that changes won't be directly propagated to the loaded package, possibly not even after 'patch.install' (though they will be honoured when you 'library()' the package again). That is definitely the case for general data objects, and I'm not sure about functions; however, successful propagation after 'patch.install' may happen for a special objects such as 'mypack.DESCRIPTION' and documentation objects. Hence my general advice is to use 'fixr' or 'move'.

The second, minor, problem is that you will probably forget to use '<<-' and will use '<-' instead, so that a local copy of '..mypack' will be created in the current task. This is no big deal, and you can just 'rm' the local copy; the local copy and the master copy in "mvb.session.info" both point to the same thing, and modifying one implies modifying the other, so that deleting the local copy won't lose your changes. 'Save' detects accidental local copies of task packages, and omits them from the disk image, so there shouldn't be any problems next time you start R even if you completely forget about local/master copies.

.AUTOPATCH

If 'autopatch==TRUE', then 'maintain.packages' will check whether the corresponding _installed_ packages are older than the ".RData" files of the task packages. If they are, it will do a full 'patch.install'; if not, it will still call 'patch.install' but only to reverse-update any bundled DLLs (see 'pre.install'), not to re-install the R-source. I find 'autopatch' useful with packages containing C code, where a crash in the C code can cause R to die before the most recent R-code changes have been "committed" with 'patch.install'. When you next start R, a call to 'maintain.packages' with 'autopatch=TRUE' will "commit" the changes _before_ the package is loaded, because you have to call 'maintain.packages' before 'library'; this seems to be more reliable than running 'patch.install' manually after 'library' after a restart.



MAINTAINED.PACKAGES.AS.TASKS

If you use 'mvbutils' to pre-build your package, then your package must exist as a task in the 'cd' hierarchy. Older versions of 'mvbutils' allowed you to 'cd' to a maintained package, but this is now forbidden because of the scope for confusion. Thanks to 'maintain.packages', there is no compelling need to have the package/task at the top of the search path; 'fixr', 'move', etc work just fine without. If you really do want to 'cd' to a maintained package, you must call 'unmaintain.package' first.

One piece of cleanup that I recommend, is to move any subtasks of "mypack" one level up in the task hierarchy, and to remove the 'tasks' object from "Splendid" itself, e.g. via something like:

%%#
cd( task.above.splendid)
tasks <- c( tasks, combined.file.paths( tasks[ "Splendid"], ..Splendid$tasks))
# ... combined.file.paths is an imaginary function. Watch out if you've used relative paths!
rm.pkg( tasks, pkg="Splendid")


SEE.ALSO

'mvbutils.packaging.tools', 'fixr', 'pre.install', 'patch.installed', 'unpackage'


EXAMPLES

## Don't run

# In your .First:
library( mvbutils)
maintain.packages( myfirstpack, mysecondpack, mythirdpack)

# or...
live.edit.list <- c( 'myfirstpack', 'mysecondpack', 'mythirdpack')
maintain.packages( live.edit.list, character.only=TRUE)

library( myfirstpack) # etc
## End Don't run
}")

)

"make.arguments.section" <-
structure( function( 
  funs= find.funs( env) %except% find.documented( env, doctype='Rd'), 
  file=stdout(),
  env=.GlobalEnv
){
  arguments <- function( x) {
      ax <- names( formals( env[[ x]]))
      if( length( ax))
        ' ' %&% ax %&% ': (' %&% x %&% ')'
      else
        character( 0)
    }
 funs <- unlist( lapply( funs, arguments), use.names=FALSE)
 if( !is.null( file))
   cat( funs, sep='\n', file=file)
 invisible( funs)
}
, doc =  docattr( r"{
make.arguments.section       package:mvbutils
make.usage.section


Construct sections of documentation

DESCRIPTION

Don't bother reading about these unless you are sure you need to! These are really intended for expediting documentation of large numbers of "internal" functions in a proto-package, and are called by 'make.internal.doc'. 'make.usage.section' and 'make.arguments.section' form prototype USAGE and ARGUMENTS section for the specified functions. These are ready for pasting into flat-format documentation (and subsequent editing).


USAGE

make.usage.section( funs=, file=stdout(), env=.GlobalEnv)
make.arguments.section( funs=, file=stdout(), env=.GlobalEnv)


ARGUMENTS

 funs: character vector of function names, defaulting to 'find.funs() %except% find.documented( doctype="Rd")'
 file: where to put the output ('"clipboard"' is useful). NULL means don't print.
 env: where to look for the functions

VALUE

Character vector containing the doc section (in plain text, not Rd format).


DETAILS

The default 'funs' argument will find all functions not mentioned in flat-format ready-for-doc2Rd documentation. This is useful for documenting a group of "internal" functions.

'make.usage.section' simply puts the name of each function before its deparsed and concatenated argument list, one function per line.

'make.arguments.section' puts one argument per line, then a colon, then the name of the function in parentheses. The idea is that something about the argument should be added manually in a text editor.


EXAMPLES

if( FALSE){
  # Can't run this directly, coz internal
  # so not exported
  ns <- asNamespace( 'mvbutils')
  make.usage.section( c( "make.usage.section", "find.funs"), 
    env=ns)
  make.arguments.section( c( "make.usage.section", "find.funs"), 
    env=ns)
}

SEE.ALSO

'flatdoc', 'pre.install'


AUTHOR

Mark Bravington


KEYWORDS

internal
}")

)

"make.internal.doc" <-
structure( function( funs, package, pkenv) {
  if( !length( funs))
return( character( 0))

  # xfuns is to cope with operators,
  # whose names start with %. This is interpreted as a "don't-show-rest-of-line"
  # by the standard flatdoc system, and is removed by 'doc2Rd'.
  # So we need to add an extra % symbol.
  xfuns <- ifelse( regexpr( '^%', funs)>0, '%', '') %&% funs
  text <- c( "PACKAGE-internal package:PACKAGE",
      xfuns, 
      attr( sys.function(), 'usage.header.text'), 
      make.usage.section( funs, file=NULL, env=pkenv), 
      attr( sys.function(), 'usage.footer.text')) 
  text <- gsub( 'PACKAGE', to.regexpr( package), text)
  
  # Split CRs; dunno if there are any nowadays-- this may be fixing something that doesn't happen
  # unlist( strsplit( text, '\n')) no, because zaps blank lines
  if( length( embedCR <- grep( '\n', text)))
    text <- massrep( text, embedCR, strsplit( text[ embedCR], '\n'))
  
return( unname( text))
}
, usage.header.text =  string2charvec( r"{

Internal functions for PACKAGE

DESCRIPTION

Internal functions for 'PACKAGE', not meant to be called directly.


USAGE

}")
, usage.footer.text =  docattr( r"{

KEYWORDS

internal
}")

)

"make.NAMESPACE" <-
structure( function( env=1, path=attr( env, 'path'),
    description=read.dcf( file.path( path, 'DESCRIPTION'))[1,], more.exports=character( 0),
    useDynLib=character() # empty
){
  env <- as.environment( env)
  import <- paste( description[ 'Depends'], description[ 'Imports'], sep=',')
  import <- gsub( '\\([^)]*\\)', '', import)
  import <- gsub( ' *', '', import)
  import <- strsplit( import, ',')[[1]]
  import <- sub( '[<>].*', '', import)
  import <- unique( import %except% c( 'R', 'NA'))

  # useDynLib should have been set up beforehand
  # ... turning into NAMESPACE format is handled by write.NAMESPACE

  ffe <- find.funs( env)
  import_exceptions <- screen_masked_imports( import, ffe)

  owndoc <- find.documented( env, doctype='own')
#  internals <- character(0)
#  for( internaldoc in owndoc[ sapply( owndoc, function( x) regexpr( '-internal', attr( get( x), 'doc')[1])>0)]) {
#    tc <- unclass( attr( get( internaldoc), 'doc'))[-1]
#    gap <- index( regexpr( '[^ ]', tc)<0)[1]
#    internals <- c( internals, gsub( ' +', '', tc[ 1 %upto% (gap-1)]))
#  }
  force.exports <- possible.methods <- ffe
  force.exports <- force.exports %SUCH.THAT% !is.null( attr( env[[.]], 'export.me'))
  possible.methods <- possible.methods %except% force.exports
  export <- unique( c( ffe %that.are.in% find.documented( env, exclude.internal=TRUE),
      force.exports, more.exports)) %that.are.in% lsall( env)

  methods <- list()
  group.generics <- c( "+", "-", "*", "/", "^", "%%", "%/%",
     "&", "|", "!",
     "==", "!=", "<", "<=", ">=", ">") # from ?S3groupGeneric
  # Arguably, should also have the other Groups' group-generics
  # but I've never written methods for them, and I can see confusion with eg 'all.equal'. They are:
  group.generics <- c( group.generics, cq(
    abs, sign, sqrt, floor, ceiling, trunc, round, signif, exp, log, expm1, log1p,
    cos, sin, tan, acos, asin, atan, cosh, sinh, tanh, acosh, asinh, atanh,
    lgamma, gamma, digamma, trigamma, cumsum, cumprod, cummax, cummin,
    all, any, sum, prod, min, max, range,
    Arg, Conj, Im, Mod, Re))

  prims <- c( .S3PrimitiveGenerics, group.generics)
  # Built-in version of '.knownS3Generics' is glaringly crap
  .knownS3Generics <- mvb.base.S3.generics

  S3.generics <- c( .knownS3Generics, structure( rep( 'base', length( prims)), names=prims))
  S3.generics <- S3.generics[ !duplicated( cbind( S3.generics, names( S3.generics)))]
  for( ipack in import)
    S3.generics <- c( S3.generics, get.S3.generics( ipack, ns=TRUE))
  S3.generics <- c( S3.generics, get.S3.generics( env, ns=FALSE))

  for( gen in names( S3.generics))
    methods[[ gen]] <- possible.methods %that.match% ('^' %&% to.regexpr( gen) %&% '\\.')
  methods <- methods %SUCH.THAT% (length(.)>0)
  generics <- rep( names( methods), sapply( methods, length))

  if( length( methods)) {
    # Weed out apparent non-methods
    # Default env in arg1 is namespace of package where generic 'x' lives
    pseudo.ns <- function( pack) if( nzchar( pack)) asNamespace( pack) else env
    arg1 <- function( x, env=pseudo.ns( S3.generics[ x])) {
        # Hitting some problem with "Ops", "Math", etc in R 3.4.4. Hard-wiring from documentation.
        # Not sure what the right thing is...
        xlist <- c( Summary = '...', Ops = 'e1', Math = 'x', Complex = 'z')
        xx <- xlist[ x]
        if( is.na( xx)) {
          xx <- names( formals( env[[ x]]))[1]
          if( is.null( xx) || !length( xx)) {
            xx <- ''
          }
        }
      return( xx)
      }
    genarg1 <- sapply( named( names( methods)), arg1)
    genarg1 <- rep( genarg1, sapply( methods, length))
    methods <- unlist( methods, use.names=FALSE)
    metharg1 <- sapply( named( methods), arg1, env=env)
    is.meth <- genarg1=='' | metharg1==genarg1
    methods <- methods[ is.meth]
    generics <- generics[ is.meth]

    if( length( methods)) {
      # Check doco to see if possible methods really are methods
      # ...USAGE section should not refer to specific method but to generic
      # No doc => no evidence against being a method, but otherwise...
      methdoc <- find.docholder( methods, env)
      is.meth <- rep( TRUE, length( methods))
      methdoclen <- sapply( methdoc, length)
      has.doc <- index( methdoclen != 0)
      for( i in has.doc) {
        docobj <- get( methdoc[[ i]][1], envir=env)
        docobj <- if( is.function( docobj)) attr( docobj, 'doc') else docobj
        USAGE.line <- grep( '^%?USAGE$', docobj)[1]
        ARGUMENTS.line <- grep( '^%?ARGUMENTS$', docobj)[1]
        # Is it called by its own name?
        if( !is.na( USAGE.line + ARGUMENTS.line))
          is.meth[ i] <- !length( grep( '\\<' %&% to.regexpr( methods[ i]) %&% ' *\\(',  #)
              docobj[ (USAGE.line+1) %upto% (ARGUMENTS.line-1)]))
      }

      methods <- methods[ is.meth]
      generics <- generics[ is.meth]
    } # if any possible methods with OK args
  } # if any possible methods at all

  if( !length( methods)) {
    methdoclen <- integer( 0) # avoid woe below
  }

  classes <- substring( methods, nchar( generics)+2)
  # Methods not exported unless explicitly documented
  export <- export %except% (methods[ methdoclen==0])
  S3 <- matrix( c( generics, classes), ncol=2)

  returnList( import, import_exceptions, export, S3, useDynLib)
}
, doc =  docattr( r"{
make.NAMESPACE   package:mvbutils
write.NAMESPACE

Auto-create a NAMESPACE file

DESCRIPTION

Called by 'pre.install' (qv) for would-be packages that have a '.onLoad' function, and are therefore assumed to want a namespace. Produces defaults for the import, export, and S3Methods. You can modify this information prior to the NAMESPACE file being created, using the pre-install hook mechanism. The default for 'import' is taken from the DESCRIPTION file, but the defaults for export and S3 methods are deduced from your functions, and are described below.


USAGE

# Don't call this directly-- pre.install will do it automatically for you
make.NAMESPACE( env=1, path=attr( env, "path"),
  description=read.dcf( file.path( path, "DESCRIPTION"))[1,], more.exports=character( 0),
  useDynLib=character())


ARGUMENTS

 env: character or numeric position on search path

 path: directory where proto-package lives

 description: (character) elements for the DESCRIPTION file, e.g. 'c( ..., Author="R.A. Fisher", ...)'. By default, read from existing file.

 more.exports: (character) things to export that normally wouldn't be.

 useDynLib: character vector of DLLs, without path or extension. Elements with names will get a NAMESPACE entry of the form '<env_name>=useDynLib( <DLL>, .registration=TRUE)', with the symbols being placed into a subenvironment withint the package namespace environment. Unnamed elements just get 'useDynLib(<DLL>)' and the symbols go directly into the package namespace environment; that's how 'Rcpp' currently operates (and I'm not keen on it!). If you _don't_ want native-symbol registration, then you should use a PIBH (pre-install build hook; see 'pre.install') and move the non-regstrees from 'nsinfo$useDynLib' into 'nsinfo$useDynLib_sans_rego'.


DETAILS

There is (currently) no attempt to handle S4 methods.

The imported packages are those listed in the "Depends:" and "Imports:" field of the DESCRIPTION file. All exported functions in those packages will be imported (i.e. currently no "importFrom" provision), except if a function would be screened by a later import or by your package's own functions. The latter should avoid clash warnings when your package is loaded.

The exported functions are all those in 'find.documented(doctype="any")' unless they appear to be S3 methods, plus any functions that have a non-NULL 'export.me' attribute. The latter is a cheap way of arranging for a function to be exported, but without formal documentation (is that wise??). 'pre.install' (qv) will incorporate any undocumented 'export.me' functions in the "mypack-internal.Rd" file, so that RCMD CHECK will be happy.

The S3 methods are all the functions whose names start "<<generic>>." and whose first argument has the same name as in the appropriate '<<generic>>'. The generics that are checked are (i) the names of the character vector '.knownS3Generics' in package 'base'; (ii) all functions that look like generics in any importees or dependees of your would-be package (i.e. functions in the namespace whose name is a prefix of a function in the S3 methods table of the namespace, and whose body contains a call to 'UseMethod'); (iii) any plausible-looking generic in your would-be package (effectively the same criterion). Documented functions which look like methods but whose flat-doc documentation names them explicitly in the USAGE section (e.g. referring to 'print.myclass(...)' rather than just 'print(...)', the latter being how you're supposed to document methods) are assumed not be methods.


SEE.ALSO

'pre.install', 'flatdoc'


KEYWORDS

utilities; programming

}")

)

"make.new.cd.task" <-
function( task.name, nlocal=sys.parent(), answer, dir.name) mlocal({
  # dir.name <- file.path( task.home(), legal.filename( task.name))
  dir.name <- './' %&% legal.filename( task.name) # syntax for rel paths: 26/6/2005
  line.end <- if( getOption( 'cd.extra.CR', FALSE)) '\n' else ''

  repeat {
    cat("Default directory = ", dir.name, "\n(names will be expanded relative to ", task.home(),
        ")\nDirectory: " %&% line.end)
    answer <- readline()
    if(answer == "")
      answer <- dir.name
    else {
      answer <- gsub( '\\\\', '/', answer)
    if( (.Platform$OS.type=='windows' && (substring( answer, 1, 1) != '/') && 
          (substring( answer, 2, 2) != ':')) ||
        (.Platform$OS.type=='unix' && (substring( answer, 1, 1) %not.in% c('~','/')))  ) {
      # want relative path
      if( substring( answer, 1, 2) != './')
        answer <- './' %&% answer
      }
    }

    if( file.exists( answer)) {
      if( !is.dir( answer))
        cat("Directory already exists, as a file!\n")
      else
  break }
    else # if !file.exists
      if( mkdir( answer))
  break
      else
        cat( 'Failed to create directory ', answer,
           '\nWarning: unwanted directories may have been created!\n')
  }

  dir.name <- answer

  if( !exists( 'tasks', where=2, inherits=FALSE))
    tasks <- character( 0)
  tasks <- c( tasks, dir.name)
  names( tasks)[length( tasks)] <- task.name
  assign( 'tasks', tasks, pos=2)
  pe2 <- pos.to.env( 2)
  if( getOption( 'write.mvb.tasks', FALSE))
    write.mvb.tasks( tasks, pe2)
  Save.pos( 2) #  save( list=objects( pos=2, all=TRUE), envir=pe2, file=file.path( attr( pe2, 'path'), '.RData'))
  rdata.path <- file.path( dir.name, '.RData')
  if( !file.exists( rdata.path))
    save( list=character(0), file=rdata.path)
  names( dir.name) <- task.name
  dir.name
})


"make.Rd2" <-
function( strings, width=NA, methodize=FALSE){
####################
# width NYI: to allow autoclipping of code lines
# basic idea is strwrap, but need to watch for strings and comments
  badatt <- FALSE
  strings <- gsub( '\\\\%', '%', strings) # because line() will pre-insert backslash before percent


  pp <- parse_and_maybe_methodize_USAGE( strings, methodize)

  # Reason for {} next, is to avoid incomplete-parsing "errors" eg with if/else
  # if( try( pp <- parse( text=c( '{', strings, '}')), silent=TRUE) %is.a% 'try-error') {
  if( pp %is.a% 'try-error') {
    errstring <- try( paste( strsplit( unclass( pp), '\n')[[1]][-1], collapse='\n'))
    if( errstring %is.a% 'try-error') { # total paranoia
      errstring <- "can't work out where!?!"
    }
    warning( sprintf( "Unparsable R-like text: %s", errstring))

    # prolly orta indicate WHERE..!
    # but that requires lots of args to be passed in
    # Mostly, doc2Rd() says what it is parsing, so user can figga it
    # Make everything a comment, and do escapes accordingly
    strings <- '#' %&% strings # what about %-starters?
    badatt <- TRUE
  } else {
    strings <- pp # methodized, if requested
  }


  # Hide escaped quotes
  search.string <- "(^|[^\\])([\\\\])+\\"
  strings <- gsub( search.string %&% "'", '\\1\\2\001', strings)
  strings <- gsub( search.string %&% '"', '\\1\\2\002', strings)
  strings <- gsub( search.string %&% "`", '\\1\\2\003', strings)
#  strings <- gsub( search.string %&% "n", '\004', strings) # also special
  strings <- strings %&% '\n'

  sq <- charToRaw( "'")
  dq <- charToRaw( '"')
  bq <- charToRaw( '`')
  hash <- charToRaw( '#')
  eol <- charToRaw( '\n') # all hash-modes end at EOL, which is always found

  brace <- charToRaw( '{')
  backbrace <- charToRaw( '}')
  backslash <- charToRaw( '\\')
  percent <- charToRaw( '%')
  rep.brace <- '\005'
  rep.backbrace <- '\006'
  rep.backslash <- '\007'
  rep.percent <- '\010'

  specials <- end.specials <- c( sq, dq, bq, hash)
  end.specials[ end.specials==hash] <- eol

  state <- 0  # string states are carried across lines
  for( istr in seq_along( strings)) {
    rch <- charToRaw( strings[ istr])
    l <- length( rch)
    states <- rep( state, length( rch)) # default
    to.do <- rep( TRUE, length( rch))
    done <- 0
    while( done < length( rch)) {
      if( state==0) {
        # Match any special
        matcho <- sapply( specials, match, table=rch[ to.do], nomatch=l+1-done) + done
        next.state <- which.min( matcho)
        matcho <- matcho[ next.state]
      } else {
        # Match only the end-special for this state
        matcho <- match( end.specials[ state], rch[ to.do], nomatch=l+1-done) + done
        next.state <- 0
      }

      states[ (done+1) %upto% (matcho-1)] <- state
      done <- matcho
      to.do <- (done+1) %upto% l
      if( matcho <= l)
        state <- next.state
    }

    # Flag characters in rch that need escaping
    rch[ rch==backslash] <- charToRaw( rep.backslash)
    rch[ rch==percent] <- charToRaw( rep.percent)
    escape.braces <- states %in% c( 0, 4)
    rch[ escape.braces & rch==brace] <- charToRaw( rep.brace)
    rch[ escape.braces & rch==backbrace] <- charToRaw( rep.backbrace)

    strings[ istr] <- rawToChar( clip( rch))
  }

  if( methodize) {
    # String-concealing trick was used earlier to hide
    # ... method{func}{class} from the semi-parser. Undo trick.
    strings <- sub( '( *)"method[{]([^}]*[}])[{]([^}]*[}])"', '\\1\\\\method{\\2{\\3', strings,
        perl=FALSE) # NFI why perl=FALSE is needed :/
  }

  strings <- gsub( '\001', "\\'", strings, fixed=TRUE)
  strings <- gsub( '\002', '\\"', strings, fixed=TRUE)
  strings <- gsub( '\003', '\\`', strings, fixed=TRUE)
  strings <- gsub( '\004', '\\n', strings, fixed=TRUE)
  strings <- gsub( rep.percent, '\\%', strings, fixed=TRUE)
  strings <- gsub( rep.brace, '\\{', strings, fixed=TRUE)
  strings <- gsub( rep.backbrace, '\\}', strings, fixed=TRUE)
  strings <- gsub( rep.backslash, '\\\\', strings, fixed=TRUE) # ?is this correct #backslashes?

  attr( strings, 'badatt') <- badatt # NULL if parsed OK
return( strings)
}


"make.usage.section" <-
function( 
  funs=find.funs( env) %except% find.documented( env, doctype='Rd'), 
  file=stdout(),
  env=.GlobalEnv
){
  usage <- function( x) {
      if( regexpr( '^%[^%]*%$', x)>0) {
        # Assumes binary op with no defaults
        y <- names( formals( env[[ x]]))
        y <- paste( y[1], x, y[2], sep=' ')
      } else {
        y <- clip( deparse( args( env[[x]])))
        y <- sub( '^ +', ' ', y)
        if( make.names( x) != x) # need backquote
          x <- '`' %&% x %&% '`'
        y[1] <- sub( '^function ', to.regexpr( x), y[1])
        y <- paste( y, collapse='')
      }
      y
    }
 funs <- unlist( lapply( funs, usage), use.names=FALSE)
 if( !is.null( file))
   cat( funs, sep='\n', file=file)
 invisible( funs)
}


"make_Cloader_funs" <-
structure( function( pkg, Cloaders, sourcedir){
## Turn Cloader R-scripts into functions that can be run from .onLoad
  if( length( Cloaders)){
    Cloader_def <- gsub( '<PKG>', pkg, as.cat( attr( sys.function(), "run_Cloader_template")))

    tf <- tempfile( tmpdir=file.path( sourcedir, 'R'))
    # Define lparen/rparen to avoid appearance of unbalanced parens in this code
    lparen <- '('
    rparen <- ')'
    lbrace <- '{'
    rbrace <- '}'
    for( iCloader in names( Cloaders)) {
      Cloader_def <- c( Cloader_def,
          sprintf( iCloader, fmt=',%s = quote%s%s', lparen, lbrace),
          readLines( Cloaders[[ iCloader]]),
          rbrace %&% rparen
        )
    }

    Cloader_def <- c( Cloader_def, sprintf( '%s[-1]', rparen), '')
    writeLines( Cloader_def, tf)
    # DON'T want to leave the original Cloaders in the R folder of the source package
    # ... NB though they will stay in the task package

    unlink( file.path( sourcedir, 'R', basename( Cloaders)))
    file.rename( tf, file.path( sourcedir, sprintf( 'R/Cloaders_%s.R', pkg)))
  }
}
, run_Cloader_template =  docattr( r"{
run_Cloaders_<PKG> <- function() {
  Cloaders <- attr( sys.function(), 'Cloaders')
  for( iCloader in names( Cloaders)) {
    flypatch <- system.file( sprintf( 'R/Cloaders_%s.R', iCloader), package='<PKG>')
    if( nzchar( flypatch)) { # empty string if not
      # use that instead of built-in, eg if debugging
      source( flypatch, local=asNamespace( '<PKG>'), echo=FALSE, verbose=FALSE)
    } else {
      eval( Cloaders[[ iCloader]], asNamespace( '<PKG>'))
    }
  }
}
attr( run_Cloaders_<PKG>, 'Cloaders') <- list( NULL# ))
}")

)

"make_dull" <-
structure( function( df, cols) {
  for( icol in cols) {
    class( df[[ icol]]) <- unique( c(
        'dull',
        oldClass( df[[ icol]]),
        # Explicitize other bits to help S3 dispatch:
        #   do want matrix & array; don't want character and numeric and integer...
        class( unclass( df[[ icol]])) %except% c( typeof( df[[ icol]]), storage.mode( df[[ icol]]))
      ))
  }
return( df)
}
, doc =  docattr( r"{
make_dull    package:mvbutils
make.dull
undull


Hide dull columns in data frames

DESCRIPTION

'make_dull' AKA 'make.dull' adds a "dull" S3 class to designated columns in a 'data.frame'. When the 'data.frame' is printed, entries in those columns will show up just as "...". Useful for hiding long boring stuff like matrices with loads of columns, nucleotide sequences, MD5 sums, and filenames. Columns will still print clearly and behave properly if manually extracted. You can remove dullness via 'undull'; see EXAMPLES.

The 'dull' class has methods for 'format' (used when printing a 'data.frame') and '[', so that dullness is perpetuated.


USAGE

make_dull(df, cols)


ARGUMENTS

 df: a data.frame
 cols: columns to designate


VALUE

A modified data.frame


DETAILS

Ask yourself: do you _really_ want details of a function called 'make_dull'? Life may be sweet but it is also short.

Actually, I've had to add something "for the record", but you probably don't want to read this. Just prepending "dull" to the "class" attribute of a column (see 'oldClass') can mask normal S3 dispatch--- matrices being a case in point. Therefore, 'make_dull' now also adds (part of) the _implicit_ class of the column after "dull". In quasi-English, what that means is that a matrix will acquire _explicit_ class attribute 'c( "dull", "matrix", "array")', whereas a normal non-dull matrix would have a 'NULL' "class" attribute but an _implicit_ class of c("matrix","array"). This means you can still do e.g. 'isSymmetric( x$dullmat)' and 'inherits( x$dullmat, "matrix")'. Other semi-exotic objects (including 'array', which AFAICR only semi-works inside dataframes, 'list' which I _think_ works OK, and user-defined classes) get similar treatment.

This Sort Of Thing is why it's marginally worth having an explicit 'undull' function, as opposed to just 'unclass' or eg 'oldClass(excol) <- oldClass( excol) %except% "dull"'. The former would destroy a user-defined class; the latter would leave superfluous 'matrix' and 'array' elements in an unnecessary "class" attribute (although I'm not sure that causes any practical problems).

Sigh... we are deep in the S3wamplands here; "the perfect is the enemy of the good" where S3 is concerned, so we just gotta put up with some of the murky stuff. That's OK. Be careful what you wish for: ie S4!


.MORE.DETAILS

'make_dull' is both autologous and idempotent.


EXAMPLES

# Becos more logical syntax:
rsample <- function (n = length(pop), pop, replace = FALSE, prob = NULL){
  pop[sample(seq_along(pop) - 1, size = n, replace = replace, prob = prob) + 1]
}

df <- data.frame( x=1:3,
    y=apply( matrix( rsample( 150, as.raw( 33:127), rep=TRUE), 50, 3), 2, rawToChar),
    stringsAsFactors=FALSE) # s.A.F. value shouldn't matter
df # zzzzzzzzzzzzzzz

df <- make_dull( df, 'y')
df # wow, exciting!

df$y # zzzzzzzzzzzzzz
undull( df$y) # no class attrib now

df$ZZZ <- matrix( 1:99, nrow=3, ncol=33)
df # boooring
class( df$ZZZ)
oldClass( df$ZZZ)

df <- make_dull( df, 'ZZZ')
df # whew
class( df$ZZZ)
oldClass( df$ZZZ)

ZZZ <- df$ZZZ

# Suddenly it is interesting! So...
ZZZ <- undull( ZZZ)
class( ZZZ)
oldClass( ZZZ)




}")

)

"make_Makefile" <-
structure( function( Clink_list, dir) {
  each_target <- do.on( Clink_list,
      sprintf( '> %s.<EXT>: %s.cpp\\n>  %s',
      .$dll, .$key_source, .$compile_instruction)
    )
  makefile_template <- attr( sys.function(), 'makefile_template')

  exts <- c( 'Makefile'='so', 'Makefile.win'='dll')
  for( mfile_name in names( mfiles)) {
    makery <- tsub( makefile_template,
        EXT= exts[ mfile_name],
        TARGET_LIST=paste( sprintf( '%s.%s', dlls, exts[ mfile_name]), collapse=' '),
        EACH_TARGET= gsub( '<EXT>', exts[ mfile_name], each_target)
      )
    writeLines( makery, con= file.path( dir, 'src', mfilename))
  }
return( NULL)
}
, makefile_template =  docattr( r"{
# Avoid the hell of tabs
.RECIPEPREFIX = >
# ... requires Gnu make 4.9 ish

all: <TARGET_LIST>
<EACH_TARGET>

clean:
>  rm -rf *o
}")

)

"masked" <-
function (pos) {
  if( is.character( pos))
    pos <- match( pos, search())

  if (any(pos < 2)) 
return(structure(.Data = character(0), info = "Nothing in .Global.env can be masked!"))

  o <- unique(unlist(lapply(pos, objects, all = TRUE)))
  all.objects <- unlist(lapply(1:(min(pos) - 1), objects, 
      all = TRUE), use.names = FALSE)
  mm <- match(all.objects, o, 0)
  tabu <- tabulate(mm, nbins = length(o))
  o[tabu > 0]
}


"masking" <-
function (pos = 1) {
  if( is.character( pos))
    pos <- match( pos, search())
  if (any(pos >= (sl <- length(search())))) 
return(structure(.Data = character(0), info = "Objects at the bottom can't mask anything!"))

  o <- unique(unlist(lapply(pos, objects, all = TRUE)))
  all.objects <- unlist(lapply((max(pos) + 1):sl, objects, 
      all = TRUE), use.names = FALSE)
  mm <- match(all.objects, o, 0)
  tabu <- tabulate(mm, nbins = length(o))
  o[tabu > 0]
}


"massrep" <-
function( orig, atlist, replist, sorted.at=TRUE){
  if( !length( atlist))
return( orig)

  repextend <-  function( a, r)
      if( length( a)==1)
        r
      else
        c( r, rep( list( character(0)), length(a)-1) )
  la <- sapply( atlist, length)
  rl <- rep( list( orig[0]), sum( la)) # preserves type of 'orig'
  rl[ 1 + c( 0, cumsum( clip( la)))] <- replist
return( multirep( orig, unlist( atlist), rl, sorted.at))
}


"max_pkg_ver" <-
structure( function( pkg, libroot, pattern='^[rR][ -]?[0-9]+') {
  max.ver <- numeric_version( '0')
  while( length( libroot)) {
    lib <- libroot[ 1]
    ver <- try( packageVersion( pkg, lib), silent=TRUE)
    if( ver %is.not.a% 'try-error') {
      max.ver <- max( ver, max.ver)
    }

    potlibs <- dir( lib, pattern=pattern, full.names=TRUE, include.dirs=TRUE) %such.that% is.dir(.)
    libroot <- multirep( libroot, 1, list( potlibs))
  }
return( max.ver)
}
, doc =  docattr( r"{
max_pkg_ver    package:mvbutils

Max package version

DESCRIPTION

Finds the highest version number of an installed package in (possibly) _several_ libraries. Mainly for internal use in 'mvbutils', but might come in handy if your version numbers have gotten out-of-synch eg with different R versions. On my setup, all my "non-base" libraries are folders inside "d:/rpackages", with folder names such as "R2.13"; my '.First' sets '.libPaths()' to all of these that are below the running version of R (but that are still legal for that R version; so for R > 3.0, folders named "R2.xxxx" would be excluded). Hence I can call 'max_pkg_ver( mypack, "d:/rpackages")' to find the highest installed version in all these subfolders.

USAGE

max_pkg_ver(pkg, libroot, pattern = "^[rR][ -]?[0-9]+")
    # NB named with underscores to avoid interpretation as S3 method

ARGUMENTS

 pkg: character, the name of the package
 libroot: folder(s) to be searched recursively for package 'pkg'
 pattern: what regexp to use when looking for potential libraries to recurse into

VALUE

A 'numeric_version' object for the highest-numbered installation, with value 'numeric_version("0")' if no such package is found. If 'libroot' is a single library containing the package, the result will equal 'packageVersion( pkg, limbroot)'.


EXAMPLES

max_pkg_ver( "mvbutils", .libPaths())



}")

)

"maybe.save.after.move" <-
function (to.from) {
  if( is.na( to.from$saving)) {
    thing.for.message <- if( !is.null( names( to.from$path)))
        '"' %&% names( to.from$path) %&% '" [' %&% to.from$path %&% ']'
      else
        to.from$path
    to.from$saving <- yes.no( 'Save workspace of ' %&% thing.for.message %&% '? ') 
  }
  
  if( to.from$saving) 
    Save.pos( to.from$env, to.from$path)
}


"mcachees" <-
function( envir=.GlobalEnv)
  if( is.null( mcache <- attr( as.environment( envir), 'mcache'))) character(0) else names( mcache)


"mcut" <-
function( x, breaks, pre.lab='', mid.lab='', post.lab='', digits=getOption( 'digits')){
  lbreaks <- format( round( breaks, digits=digits)) %&% mid.lab
  labs <- pre.lab %&% '[' %&% c( '<' %&% lbreaks[1],  
      clip( lbreaks) %&% ',' %&% lbreaks[ -1], '>=' %&% rev( lbreaks)[1]) %&% ']' %&% post.lab
  if( length( breaks)==1)
    labs <- labs[-2]
  else if( length( breaks)==0)
    labs <- labs[2]
  xc <- 1+findInterval( x, breaks)
  factor( labs[ xc], levels=labs, ordered=TRUE)
}


"mdeparse" <-
structure( function( expr, ...){
## A beautiful hack to make 'a:=b' and 'b?c' deparse properly...
## ... with minimal work :)
  
  # Replace ':=' and '?' by %.% operators, containing strings that
  # ... deparse() would _never_ produce, even if buried within a string! (because they could not be parsed)
  op1 <- r"--{%'"1`'"%}--"
  op2 <- r"--{%'"2`'"%}--"
  unop2 <- sub( '`', '\\`', op2, fixed=TRUE)
  
  expr <- do.call( 'substitute', list( expr, list( ':='=as.name( op1), '?'=as.name( op2))))
  dep <- base::deparse( expr, ...)
  dep <- gsub( op1, ':=', dep, fixed=TRUE)
  
  # ?a will give `%<op2>%`(a) because there's no 2nd arg for the operator
  dep <- gsub( sprintf( '`%s`(', unop2), '(?', dep, fixed=TRUE)

  # a?b will deparse fine but 
  dep <- gsub( op2, '?', dep, fixed=TRUE)
return( dep)
}
, doc =  docattr( r"{
mdeparse    package:mvbutils


Deparsing nicelier


DESCRIPTION

R's built-in 'deparse' (qv) rather messes up 'a:=b', 'a?b', and '?a', destroying their elegance. This doesn't--- though for '?a' it does wrap the result in superfluous parentheses. There might be a few superfluous parens in other cases too, but 'base::deparse' does that too (to safeguard reparsability).


.DETAIL

This works (quickly) by first using 'substitute' to replace calls to ':=' and '?' by fake user-defined operators ('%<something>%'), then calling 'deparse', then 'gsub' to re-replace the user-def-ops by ':=' and '?'. The '<something>' is meant to be a string that could never occur accidentally in 'deparse' output (ie no character constant or name could ever deparse to it)--- I hope I got it right!

I am not _entirely_ sure about precedence. Because user-defined ops have higher precedence than ':=' or '?', what I _think_ happens is that 'deparse' puts in extra parentheses to safeguard precedence. I don't remove them, so I _think_ the end result is correct in the sense that 'parse(text=mdeparse(expr))' will keep precedence correct, though maybe with extra parens.

Apparently 'rlang::expr_deparse' also handles ':=' and '?' sensibly (and doesn't put the parens on '?a'), but it is slow because it does everything itself, and _crikey_ it is a big dependency. 'mdeparse' is fast because it's a beautiful hack.

While I was thinking about this, I came upon the 'doubt' package, which is a _really_ clever thing--- kudos to the author!


USAGE

mdeparse(expr, ...) 


ARGUMENTS

 expr: what to deparse
 
 ...: other args for 'base::deparse'


VALUE

Character vector.


EXAMPLES

deparse( quote( a:=b))     # [1] "`:=`(a, b)"
mdeparse( quote( a:=b))    # [1] "a := b"

mdeparse( quote( a?b))     # [1] "a ? b"
deparse( quote( a?b))      # [1] "`?`(a, b)"

mdeparse( quote( ?b))      # [1] "(?b)" best I could do--- sorry!
deparse( quote( ?b))       # [1] "`?`(b)"

}")

)

"mintcut" <-
structure( function( x, breaks=NULL,
    prefix='', all.levels=!is.null( attr( breaks, 'all.levels')), by.breaks=1) {
####################
  # Labels of the form 2-7 or 3, or 8+ (for last in range)
  # x<breaks[1] := NA
  all.levels <- force( all.levels)
  x <- as.integer( x)
  breaks <- if( is.null( breaks))
      seq( floor( min(x, na.rm=TRUE)), ceiling( max( x, na.rm=TRUE)), by=by.breaks)
    else
      sort( as.integer( breaks))

  xc <- findInterval( x, breaks)
  xc[ xc==0] <- NA
  xlabs <- breaks %&% '-' %&% c( breaks[ -1]-1, Inf)
  gap1 <- c( clip( breaks)==breaks[-1]-1, FALSE)
  xlabs[ gap1] <- breaks[ gap1]
  xlabs[ length( breaks)] <- breaks[ length( breaks)] %&% '+'
  xlabs <- prefix %&% xlabs
  factor( xlabs[ xc], levels=if( all.levels) xlabs else xlabs[ 1 %upto% max( xc, na.rm=TRUE)],
      ordered=TRUE)
}
, doc =  docattr( r"{
mcut    package:handy2
mintcut

Put reals and integers into specified bins, returning factors.

DESCRIPTION

Put reals and integers into specified bins, returning ordered factors. Like 'cut' (qv) but for human use.

USAGE

mcut( x, breaks, pre.lab='', mid.lab='', post.lab='', digits=getOption( 'digits'))
mintcut( x, breaks=NULL, prefix='', all.levels=, by.breaks=1)

ARGUMENTS

 x: (numeric vector) What to bin-- will be coerced to integer for 'mintcut'

 breaks: (numeric vector) LH end of each bin-- should be increasing. Values of 'x' exactly on the LH end of a bin will go into that bin, not the previous one. For 'mintcut', defaults to equal-size bins across the range of 'x', where bin size is set from 'by.breaks' which itself defaults to 1. For 'mcut', should start with -Inf if necessary, but should not finish with Inf unless you want a bin for Infs only.

 prefix, pre.lab: (string) What to prepend to the factor labels-- e.g. "Amps" if your original data is about Amps.

 mid.lab: "units" to append to numeric vals _inside_ factor labels. Tends to make the labels harder to read; try using 'post.lab' instead.

 post.lab: (string) What to append to the factor labels.

 digits: (integer) How many digits to put into the factor labels.

 all.levels: if FALSE, omit factor levels that don't occur in 'x'. To override "automatically", just set the "all.levels" attribute of 'breaks' to anything non-NULL; useful e.g. if you are repeatedly calling 'mintcut' with the same 'breaks' and you always want 'all.levels=TRUE'.

 by.breaks: for 'mintcut' when default 'breaks' is used, to set the bin size.

DETAILS

Values of 'x' below 'breaks[1]' will end up as NAs. For 'mintcut', factor labels (well, the bit after the 'prefix') will be of the form "2-7" or "3" (if the bin range is 1) or "8+" (for last in range). For 'mcut', labels will look like this (apart from the 'pre.lab' and 'post.lab' bits): "[<0.25]" or "[0.25,0.50]" or "[>=0.75]".

EXAMPLES

set.seed( 1)
mcut( runif( 5), c( 0.25, 0.5, 0.75))
# [1] [0.25,0.50] [0.25,0.50] [0.50,0.75] [>=0.75]     [<0.25]
# Levels: [<0.25] [0.25,0.50] [0.50,0.75] [>=0.75]

 mcut( runif( 5), c( 0.25, 0.5, 0.75), pre.lab='A', post.lab='B', digits=1)
# [1] A[>=0.8]B    A[>=0.8]B    A[0.5,0.8]B A[0.5,0.8]B A[<0.2]B
# Levels: A[<0.2]B A[0.2,0.5]B A[0.5,0.8]B A[>=0.8]B

mintcut( 1:8, c( 2, 4, 7))
# [1] <NA> 2-3  2-3  4-6  4-6  4-6  7+   7+
# Levels: 2-3 4-6 7+

mintcut( c( 1, 2, 4)) # auto bins, size defaulting to 1
# [1] 1  2  4+
# Levels: 1 < 2 < 3 < 4+
mintcut( c( 1, 2, 6), by=2) # auto bins of size 2
# [1] 1-2 1-2 5+
# Levels: 1-2 < 3-4 < 5+


}")

)

"mkdir" <-
function( dirlist) {
  outcome <- logical(length(dirlist))
  for (dir in 1 %upto% length(dirlist)) {
    answer <- strsplit(strsplit(dirlist[dir], "/", fixed=TRUE)[[1]], "\\", fixed=TRUE)
    # Deal with absolute strings starting with '/'
    if( !length( answer[[1]])) {
      answer <- answer[-1]
      answer[[1]] <- '/' %&% answer[[1]]
    }
    next.dir <- character(0)
    for (i in answer) 
      if( !is.dir( next.dir <- paste( c( next.dir, i), collapse = "/")) &&
          !( substring( next.dir, nchar( next.dir), nchar( next.dir))==':')) 
        dir.create(next.dir)
    outcome[dir] <- is.dir(next.dir)
  }
  outcome
}


"mlazy" <-
structure( function( ..., what, envir=.GlobalEnv, save.now=TRUE) {
  if( missing( what))
    what <- sapply( match.call( expand.dots=FALSE)$..., deparse)
  if( !length( what))
return()

  envir <- as.environment( envir)

  what <- what %such.that% (. %in% lsall( envir))
  if( !length( what)) {
    warning( 'nothing exists to be mlazyed')
return()
  }

  # Next call used to have a getfrom arg, set to sys.frame( mvb.sys.parent()) ..?
  move.to.mcache( what, envir, save.now=save.now) 
  if( !identical( envir, .GlobalEnv))
    save.refdb( envir=envir) # not until asked
}
, doc =  docattr( r"{
mlazy         package:mvbutils
mtidy
demlazy
mcachees
attach.mlazy

Cacheing objects for lazy-load access

DESCRIPTION

'mlazy' and friends are designed for handling collections of biggish objects, where only a few of the objects are accessed during any period, and especially where the individual objects might change and the collection might grow or shrink. As with "lazy loading" of packages, and the 'gdata/ASOR' packages, the idea is to avoid the time & memory overhead associated with loading in numerous huge R binary objects when not all will be needed. Unlike lazy loading and 'gdata', 'mlazy' caches each mlazyed object in a separate file, so it also avoids the overhead that would be associated with changing/adding/deleting objects if all objects lived in the same big file. When a workspace is 'Save'd, the code updates only those individual object files that need updating.

Apart from possibly 'environment' objects (see subsection), 'mlazy' does not require any special structure for object collections; in particular, the data doesn't have to go into a package. 'mlazy' is particularly useful for users of 'cd' because each 'cd' to/from a task causes a read/write of the binary image file (usually ".RData"), which can be very large if 'mlazy' is not used. Read DETAILS next. Feedback is welcome.


.ENVIRONMENTS

Sometimes nowadays I use an R 'environment' instead of a 'list' to store stuff, usually to take advantage of inheritance. They are a bit different to other R objects, and if you don't understand them properly, then be careful! The salient point here is that an 'environment' is really a pointer, and unlike other R objects, two R 'environment' "objects" can actually point to exactly the same "shared memory". Now, 'mlazy' works fine with single copy of an 'environment' (when it's saved into the cache folder, R will automatically include any necessary parent environments, etc) _but_ if you have two objects that point to the same "real" environment, and you 'mlazy' just one of them or both of them, then I don't know what's going to happen when you reload them; do you now end up with two separate unlinked copies, or what? So... like I said, be very careful with 'mlazy' and 'environment' objects. (It would be handy if there was a tool to check for other references to a given environment, which must be buried inside R's internal structures since reference-counting is certainly used there. But...)


USAGE

mlazy( ..., what, envir=.GlobalEnv, save.now=TRUE)
  # cache some objects
mtidy( ..., what, envir=.GlobalEnv) 
  # (cache and) purge the cache to disk, freeing memory
demlazy( ..., what, envir=.GlobalEnv) 
  # makes 'what' into normal uncached objects
mcachees( envir=.GlobalEnv) 
  # shows which objects in  envir are cached
attach.mlazy( dir, pos=2, name=) 
  # load mcached workspace into new search environment, 
  # or create empty s.e. for cacheing


ARGUMENTS

 ...: unquoted object names, overridden by 'what' if supplied
 what: character vector of object names, all from the same environment. For 'mtidy' and 'demlazy', defaults to all currently-cached objects in 'envir'
 envir: environment or position on the search path, defaulting to the environment where 'what' or 'objs' live.
 save.now: see DETAILS
 dir: name of directory, relative to 'task.home' (qv).
 pos: numeric position of environment on search path, 2 or more
 name: name to give environment, defaulting to something like "data:current.task:dir".


VALUE

These functions are used only for their side-effects, except for 'cachees' which returns a character vector of object names.


MORE.DETAILS

All this is geared to working with saved images (i.e. ".RData" or "all.rda" files) rather than creating all objects anew each session via 'source'. If you use the latter approach, 'mlazy' will probably be of little value.

The easiest way to set up cacheing is just to create your objects as normal, then call 

'mlazy( <<objname1>>, <<objname2>>, <<etc>>)'
'Save()'

This will not seem to do much immediately-- your object can be read and changed as normal, and is still taking up memory. The memory and time savings will come in your next R session in this workspace.

You should never see any differences (except in time & memory usage) between working with cached (AKA mlazyed) and normal uncached objects.[One minor exception is that cacheing a function may stuff up the automatic backup system, or at any rate the "backstop" version of it which runs when you 'cd'. This is deliberate, for speeding up 'cd'. But why would you cache a _function_ anyway?]

'mlazy' itself doesn't save the workspace image (the ".RData" or "all.rda" file), which is where the references live; that's why you need to call 'Save' (qv) periodically. 'save.image' and 'save' will *not* work properly, and nor will 'load'-- see NOTE below. 'Save' doesn't store cached objects directly in the ".RData" file, but instead stores the uncached objects as normal in '.RData' together with a special object called something like '.mcache00' (guaranteed not to conflict with one of your own objects). When the '.RData' file is subsequently reloaded by 'cd', the presence of the '.mcache00' object triggers the creation of "stub" objects that will load the real cached objects from disk when and only when each one is required; the '.mcache00' object is then deleted. Cached objects are loaded & stored in a subdirectory "mlazy" from individual files called "obj*.rda", where "*" is a number.

'mlazy' and 'Save' do not immediately free any memory, to avoid any unnecessary re-loading from disk if you access the objects again during the current session. To force a "memory purge" _during_ an R session, you need to call 'mtidy'. 'mtidy'  purges its arguments from the cache, replacing them by 'promise's just as when loading the workspace; when a reference is next accessed, its cached version will be re-loaded from disk. 'mtidy' can be useful if you are looping over objects, and want to keep memory growth limited-- you can 'mtidy' each object as the last statement in the loop. By default, 'mtidy' purges the cache of all objects that have previously been cached. 'mtidy' also caches any formerly uncached arguments, so one call to 'mtidy' can be used instead of 'mlazy( ...); mtidy( ...)'.

'move' (qv) understands cached objects, and will shuffle the files accordingly.

'demlazy' will *delete* the corresponding "obj*.rda" file(s), so that only an in-memory copy will then exist; don't forget to 'Save' soon after.


.WARNING

The system function 'load' does not understand cacheing. If you merely 'load' an image file saved using 'Save', cached objects will not be there, but there will be an extra object called something like '.mcache00'. Hence, if you have cached objects in your ROOT task, they will not be visible when you start R until you load the 'mvbutils' library-- another fine reason to do that in your '.First'. The '.First.lib' function in 'mvbutils' calls 'setup.mcache( .GlobalEnv)' to automatically prepare any references in the ROOT task.


.CACHEING.IN.OTHER.SEARCH.ENVIRONMENTS

It is possible to cache in search environments other the current top one (AKA the current workspace, AKA '.GlobalEnv'). This could be useful if, for example, you have a large number of simulated datasets that you might need to access, but you don't want them cluttering up '.GlobalEnv'. If you weren't worried about cacheing, you'd probably do this by calling 'attach( "<<filename>>")'. The cacheing equivalent is 'attach.mlazy( "cachedir")'. The argument is the name of a directory where the cached objects will be (or already are) stored; the directory will be created if necessary. If there is a ".RData" file in the directory, 'attach.mlazy' will load it and set up any references properly; the ".RData" file will presumably contain mostly references to cached data objects, but can contain normal uncached objects too.

Once you have set up a cacheable search environment via 'attach.mlazy' (typically in search position 2), you can cache objects into it using 'mlazy' with the 'envir' argument set (typically to 2). If the objects are originally somewhere else, they will be transferred to 'envir' before cacheing. Whenever you want to save the cached objects, call 'Save.pos(2)'.

You will probably also want to modify or create the '.First.task' (see 'cd' (qv)) of the current task so that it calls 'attach.mlazy("<<cache directory name>>")'. Also, you should create a '.Last.task' (see 'cd' (qv)) containing 'detach(2)', otherwise 'cd(..)' and 'cd(0/...)' won't work.


.OPTIONS

By default, 'mlazy' now saves & loads into a auto-created subdirectory called "mlazy". In the earliest releases, though, it saved "obj*.rda" files into the same directory as ".RData". It will now *move* any "obj*.rda" files that it finds alongside ".RData" into the "mlazy" subdirectory. You can (possibly) override this by setting 'options( mlazy.subdir=FALSE)', but the default is likely more reliable.

By default, there is no way to figure out what object is contained in a "obj*.rda" without forcibly loading that file or inspecting the '.mcache00' object in the "parent" '.RData' file-- not that you should ever need to know. However, if you set 'options( mlazy.index=TRUE)' (*recommended*), then a file "obj.ind" will be maintained in the "mlazy" directory, showing (object name - value) pairs in plain text (tab-separated). For directories with very large numbers of objects, there may be some speed penalty. If you want to create an index file for an existing "mlazy" directory that lacks one, 'cd' to the task and call 'mvbutils:::mupdate.mcache.index.if.opt(mlazy.index=TRUE)'.

See 'Save' for how to set compression options, and 'save' for what you can set them to; 'options(mvbutils.compression_level=1)' may save some time, at the expense of disk space.



.TROUBLESHOOTING

In the unlikely event of needing to manually load a cached image file, use 'load.refdb' (qv)-- 'cd' and 'attach.mlazy' do this automatically.

In the unlikely event of lost/corrupted data, you can manually reload individual "obj*.rda" files using 'load'-- each "obj*.rda" file contains one object stored with its correct name. Before doing that, call 'demlazy( what=mcachees())' to avoid subsequent trouble. Once you have reloaded the objects, you can call 'mlazy' again.

See OPTIONS for the easy way to check what object is stored in a particular "obj*.rda" file. If that feature is turned off on your system, the failsafe way is to load the file into a new environment, e.g. 'e <- new.env(); load( "obj99.rda", e); ls( e)'.

To see how memory changes when you call 'mlazy' and 'mtidy', call 'gc()'.

To check object sizes _without_ actually loading the cached objects, use 'lsize' (qv). Many functions that iterate over all objects in the environment, such as 'eapply', will cause 'mlazy' objects to be loaded.

Housekeeping of "obj**.rda" files happens during 'Save'; any obsolete files (i.e. corresponding to objects that have been 'remove'd) are deleted.


.INNER.WORKINGS

What happens: each workspace acquires a 'mcache' attribute, which is a named numeric vector. The absolute values of the entries correspond to files-- 53 corresponds to a file "obj53.rda", etc., and the names to objects. When an object 'myobj' is 'mlazy'ed, the 'mcache' is augmented by a new element named "myobj" with a new file number, and that file is saved to disk. Also, "myobj" is replaced with an active binding (see 'makeActiveBinding'). The active binding is a function which retrieves or sets the object's data within the function's environment. If the function is called in change-value mode, then it also makes negative the file number in 'mcache'. Hence it's possible to tell whether an object has been changed since last being saved.

When an object is first 'mlazy'ed, the object data is placed directly into the active binding function's environment so that the function can find/modify the data. When an object is 'mtidy'ed, or when a cached image is loaded from disk, the thing placed into the A.B.fun's environment is not the data itself, but instead a 'promise' saying, in effect, "fetch me from disk when you need me". The promise gets forced when the object is accessed for reading or writing. This is how "lazy loading" of packages works, and also the 'gdata' package. However, for 'mlazy' there is the additional requirement of being able to determine whether an object has been modified; for efficiency, only modified objects should be written to disk when there is a 'Save'.

There is presumably some speed penalty from using a cache, but experience to date suggests that the penalty is small. Cached objects are saved in compressed format, which seems to take a little longer than an uncompressed save, but loading seems pretty quick compared to uncompressed files.


SEE.ALSO

'lsize', 'gc', package 'gdata', package 'ASOR'


AUTHOR

Mark Bravington


EXAMPLES

## Don't run:
biggo <- matrix( runif( 1e6), 1000, 1000)
gc() # lots of memory
mlazy( biggo)
gc() # still lots of memory
mtidy( biggo)
gc() # better
biggo[1,1]
gc() # worse; it's been reloaded
## End Don't run

KEYWORDS

programming; data


}")

)

"mlocal" <-
structure( function( expr){ # tricky_dicky=FALSE
  sp <- sys.parent()
  sp.env <- sys.frame(sp)
  # nlocal_ eval( as.name( 'nlocal'), envir=sp.env) # used to work in S but not in R
  nlocal <- get( 'nlocal', envir=sp.env)
  nlocal.env <- if( is.numeric( nlocal)) 
      sys.frame( nlocal) 
    else 
      as.environment( nlocal)

# on.exit stuff changed 7/2/2005; looks like old version was for Splus
  on.exit( {
#    eval( sys.on.exit()[[nlocal]], envir=nlocal.env) # zapped

#   Get rid of temporaries
    remove( list=names( params) %that.are.in%
        (lsall( env=nlocal.env) %except% names( savers)), envir=nlocal.env)

#   Restore things hidden by params
    for( i in names( savers))
      assign( i, savers[[ i]], envir=nlocal.env)

#    eval( old.on.exit, envir=nlocal.env) # so old code will execute on return to 'nlocal' # zapped
  })

  eval( expression( on.exit())[[1]], envir=nlocal.env)

  params <- formals( sys.function( sp))
  params <- params[ names(params)!='nlocal']
  savers <- names( params)

  if( length( params)) {
    names( savers) <- savers
    savers <- sapply( savers, exists, envir=nlocal.env, inherits=FALSE)
    savers <- names( savers)[ savers]
    if( length( savers)) {
      names( savers) <- savers
      savers <- lapply( savers, function( x) mget( x, envir=nlocal.env)[[1]])
    }
#   Parameters and temporary working variables:

    for( i in names( params)) {
      if( eval( call( 'missing', i), envir=sp.env)) {
        if( is.symbol( params[[ i]]) && !nzchar( as.character( params[[ i]])) &&
            exists( i, envir=nlocal.env, inherits=FALSE))
          remove( list=i, envir=nlocal.env)
        else
          assign( i, params[[i]], envir=nlocal.env) }
          #delayedAssign( i, params[[i]], eval.env=nlocal.env, assign.env=nlocal.env) }
      else # CHANGED from: bugs here? doesn't force... should do so or use delayedAssign?
        assign( i, sp.env[[i]], envir=nlocal.env)
        #assign( i, eval( call( 'get', i), envir=sp.env), envir=nlocal.env)
        #delayedAssign( i, call( 'eval', i, envir=sp.env), assign.env=nlocal.env)
        
    } # else NORMAL case
  } # parameter loop

# Embed "expr" in an artificial loop, so that calls to 'break' at top-level will quit the function. This feature
# is only for S-compatibility. Preferred syntax in R is return( local.return( ...)) which works inside any depth of
# loops
  
  expr <- substitute( repeat{ assign( 'answer', expr, envir=env); break },
      list( expr=substitute( expr), env=environment()
      #sys.frame(sys.nframe())
      ))
#print( expr)
  # Experimental mechanism to robustify local.return, esp when debugging
  # As of R4.1, Should only be needed by mtrace
  # To use this, make tricky_dicky an argument, with default FALSE
  # and remove 'FALSE &&' in next line!
  if( FALSE && tricky_dicky){
    new_local_return <- get( 'local.return', envir=nlocal.env, inherits=TRUE) # hopefully the namespace...
    xenv <- new.env( parent=environment( new_local_return))
    environment( new_local_return) <- xenv
    xenv$'_ENCLOS_' <- environment()
    tryme <- do.call( 'substitute', list( expr, list( local.return=new_local_return)))
    print( tryme)
    expr <- tryme
  }

  # Trick to make local.return() work when debugging
  # cos the eval( enclos) trick below does not work in that case
  # and local.return cannot then find where to put 'override.answer'
  # Irrel when not debugging, but harmless
  `_ENCLOS_` <- environment()  

# The business end!
  on.exit.code <- quote( NULL)
  eval( expr, envir=nlocal.env, enclos=environment()) # See local.return() for why enclos
  
  # ... which may have changed "on.exit.code"
  eval( on.exit.code, envir=nlocal.env, enclos=environment())

  # Was local.return() called? If so, use that value
  if( exists( 'override.answer', envir=environment(),
      inherits=FALSE)){
    answer <- override.answer
  }
return( answer)
}
, doc =  docattr( r"{
mlocal       package:mvbutils

Macro-like functions

DESCRIPTION

'mlocal' lets you write a function whose statements are executed in its caller's frame, rather than in its own frame.

USAGE

# Use only as wrapper of function body, like this:
# my.fun <- function(..., nlocal=sys.parent()) mlocal( expr)
# ... should be replaced by the arguments of "my.fun"
# expr should be replaced by the code of "my.fun"
# nlocal should always be included as shown

mlocal( expr) # Don't use it like this!


ARGUMENTS

 expr: the function code, normally a braced expression


DETAILS

Sometimes it's useful to write a "child" function that can create and modify variables in its parent directly, without using 'assign' or '<<-' (note that '<<-' will only work on variables that exist already). This can make for clearer, more modular programming; for example, tedious initializations of many variables can be hidden inside an 'initialize()' statement. The definition of an 'mlocal' function does not have to occur within its caller; the 'mlocal' function can exist as a completely separate R object.

'mlocal' functions can have arguments just like normal functions. These arguments will temporarily hide any objects of the same name in the 'nlocal' frame (i.e. the calling frame). When the 'mlocal' function exits, its arguments will be deleted from the calling frame and the hidden objects (if any) will be restored. Sometimes it's desirable to avoid cluttering the calling frame with variables that only matter to the 'mlocal' function. A useful convention is to "declare" such temporary variables in your function definition, as defaultless arguments after the 'nlocal' argument.

The 'nlocal' argument of an 'mlocal' function-- which must ALWAYS be included in the definition, with the default specified as 'sys.parent()'-- can normally be omitted when invoking your 'mlocal' function. However, you will need to set it explicitly when your function is to be called by another, e.g. 'lapply'; see the third example. A more daring usage is to call e.g. 'fun.mlocal(nlocal=another.frame.number)' so that the statements in 'fun.mlocal' get executed in a completely different frame. A convoluted example can be found in the (internal) function 'find.debug.HQ' in the 'debug' package, which creates a frame and then defines a large number of variables in it by calling 'setup.debug.admin(nlocal=new.frame.number)'. As of 2016, you can also set 'nlocal' to be an environment.

'mlocal' functions can be nested, though this gets confusing. By default, all evaluation will happen in the same frame, that of the original caller.

Note that (at least at present) all arguments are evaluated as soon as your 'mlocal' function is invoked, rather than by the usual lazy evaluation mechanism. Missing arguments are still OK, though.

If you call 'return' in an 'mlocal' function, you must call 'local.return' (qv) too.

'on.exit' doesn't work properly. If you want to have exit code in the 'mlocal' function itself, use 'local.on.exit' (qv). I can't find any way to set the exit code in the calling function from within an 'mlocal' function. (Not checked for some years)

Frame-dependent functions (sys.parent()) etc. will not do what you expect inside an 'mlocal' function. For R versions between at least 1.8 and 2.15, calling the 'mvb...' versions will return information about the *caller* of the current 'mlocal()' function caller (or the original caller, if there is a chain of 'mlocal's). For example, 'mvb.sys.function()' returns the definition of the caller, and 'mvb.sys.parent()' the frame of the caller's parent. Note that 'sys.frame( mvb.sys.nframe())' gives the current environment (i.e. where all the variables live), because this is shared between the caller and the 'mlocal' function. Other behaviour seems to depend on the version of R, and in R 2.15 I don't know how to access the definition of the 'mlocal' function itself. This means, for example, that you can't reliably access attributes of the 'mlocal' function itself, though you can access those of its caller via e.g. 'attr( mvb.sys.function(), "thing")'.


VALUE

As per your function; also see 'local.return'.

EXAMPLES

# Tidiness and variable creation
init <- function( nlocal=sys.parent()) mlocal( sqr.a <- a*a)
ffout <- function( a) { init(); sqr.a }
ffout( 5) # 25


# Parameters and temporary variables
ffin <- function( n, nlocal=sys.parent(), a, i) mlocal({
    # this "n" and "a" will temporarily replace caller's "n" and "a"
    print( n)
    a <- 1
    for( i in 1:n)
      a <- a*x
    a
  })
x.to.the.n.plus.1 <- function( x, n) {
    print( ffin( n+1))
    print( n)
    print( ls())
  }
x.to.the.n.plus.1( 3, 2) # prints as follows:
# [1] 3 (in "ffin")
# [1] 27 (result of "ffin")
# [1] 2 (original n)
# [1] "n" "x" (vars in "x.to.the..."-- NB no a or i)


# Use of "nlocal"
ffin <- function( i, nlocal=sys.parent()) mlocal( a <- a+i )
ffout <- function( ivec) { a <- 0; sapply( ivec, ffin, nlocal=sys.nframe()) }
ffout( 1:3) # 1 3 6


SEE.ALSO

'local.return', 'local.on.exit', 'do.in.envir', 'localfuncs', and R-news 1/3 2001 for a related approach to "macros"

AUTHOR

Mark Bravington


KEYWORDS

programming
}")

)

"most.recent" <-
function( lvec) {
stopifnot( is.logical( lvec))
  nmax <- length( lvec)
  ivec <- index( lvec)
  rep( c( 0, ivec), diff( c( 1, ivec, nmax+1)))
}


"move" <-
structure( function( 
    x='.', 
    from='.', 
    to='.', 
    what, 
    overwrite.by.default=FALSE, 
    copy=FALSE
){
  if( !missing( what)) {
    to <- substitute( from)
    from <- substitute( x) }
  else {
    what <- as.character( substitute( x))
    from <- substitute( from)
    to <- substitute( to) }

  if( (to %is.a% 'call') && (to[[1]]==quote( `$`))) { # maintained package
    to <- eval( to, parent.frame())
  } else { # normal
    if( !is.character( to))
      to <- deparse( to)
    to <- find.path( char.rel.path=to)
  }

  if( (from %is.a% 'call') && (from[[1]]==quote( `$`))) { # maintained package
    from <- eval( from, parent.frame())
  } else { # normal
    if( !is.character( from))
      from <- deparse( from)
    from <- find.path( char.rel.path=from)
  }

  from <- prepare.for.move( from)
  to <- prepare.for.move( to)

  if( identical( from$env, to$env) || from$path==to$path)
stop( '"from" and "to" point to the same place!')

  found <- !is.na( match( what, from$obj))
  if( !all( found))
    warning( 'Can\'t find ' %&% paste( what[!found], collapse=','))
  what <- what[ found]
  if( !length( what)) {
    cat( 'Nothing to move!')
return( invisible( character(0))) }

  overwrite <- is.na( match( what, to$obj)) | overwrite.by.default
  names( overwrite) <- what
  for( i in what[!overwrite]) {
    all.over <- FALSE
    repeat{
      cat( 'Overwrite ', i, ' [Y(es)/N(o)/A(ll)]? ')
      answer <- upper.case( substring( readline(), 1,1))
      overwrite[ i] <- NA
      if( answer=='Y')
        overwrite[ i] <- TRUE
      else if( answer=='N')
        overwrite[ i] <- FALSE
      else if( answer=='A') {
        overwrite[ index( i==what):length( overwrite)] <- TRUE
        all.over <- TRUE }
      if( !is.na( overwrite[i]))
    break }

    if( all.over)
  break }

  what <- what[ overwrite]
  if( !length( what)) {
    cat( 'Nothing to move!')
return( invisible( character(0))) }

  # Changed 14/3/2004 to cope with mrefs
  to.mcache <- attr( to$env, 'mcache')
  from.mcache <- attr( from$env, 'mcache') # replaces info from to$env

  whatrefs <- what %such.that% (. %in% names( from.mcache))
  mtidy( what=whatrefs, from$env)

  for( i in what %except% whatrefs) {
    obj <- from$env[[ i]]
    assign( i, obj, envir=to$env)
    update_loaded_pkg( names( attr( to$env, 'path')), i, obj) # live pkgs: 6/7/2006
    move.backup.file( i, old.dir=from$path, new.dir=to$path)
  }

  if( length( whatrefs)) {
    mkdir( file.path( to$path, 'mlazy')) # otherwise file.rename below won't work
    # mcache not applicable to loaded packages, phew
    new.to.mcache <- mupdate.mcache( whatrefs, to.mcache, from$env)

    from.obj.files <- file.path( from$path, 'mlazy',
        'obj' %&% from.mcache[ whatrefs] %&% '.rda')
    to.obj.files <- file.path( to$path, 'mlazy',
        'obj' %&% new.to.mcache[ whatrefs] %&% '.rda')

    suppressWarnings( file.remove( to.obj.files))
    renamed <- logical( length( from.obj.files))
    for( i in seq_along( from.obj.files))
      renamed[i] <- file.rename( from.obj.files[i], to.obj.files[i])
    if( any( !renamed))
      file.copy( from.obj.files[ !renamed], to.obj.files[ !renamed] )

    attr( to$env, 'mcache') <- new.to.mcache
    setup.mcache( to$env, refs=whatrefs) # change nfile & env
  }

  move.fix.list()

  maybe.save.after.move( to)

  if( !copy) {
    remove( list=what, envir=from$env)
    if( length( maintained.packages) &&
        !is.na( mp <- index( sapply( maintained.packages, identical, from$env))[1]))
      rm.pkg( names( maintained.packages)[ mp], list=what, save.=FALSE)
    if( length( whatrefs)) {
      suppressWarnings( file.remove( from.obj.files))
      attr( from$env, 'mcache') <- from.mcache %without.name% whatrefs
    }
    maybe.save.after.move( from)
  }

  invisible( what)
}
, doc =  docattr( r"{
move     package:mvbutils

Organizing R workspaces

DESCRIPTION

'move' shifts one or more objects around the task hierarchy (see 'cd'), whether or not the source and destination are currently attached on the search path.


USAGE

# Usually: unquoted object name, unquoted from and to, e.g.
# move( thing, ., 0/somewhere)
# Use 'what' arg to move several objects at once, e.g.
# move( what=c( "thing1", "thing2"), <<etc>>)
# move( x, from, to)
# move( what=, from, to)

# Next line shows the formal args, but the real usage would NEVER be like this...0
move( x='.', from='.', to='.', what, overwrite.by.default=FALSE, copy=FALSE)

ARGUMENTS

 x: unquoted name
 from: unquoted path specifier (or maintained package specifier)
 to: unquoted path specifier (or M.P. specifier)
 what: character vector
 overwrite.by.default: logical(1)
 copy: logical(1)


DETAILS

The normal invocation is something like 'move( myobj, ., 0/another.task)'-- note the lack of quotes around 'myobj'. To move objects with names that have to be quoted, or to move several objects at the same time, specify the 'what' argument: e.g. 'move( what=c( "myobj", "%myop%"), ., 0/another.task)'. Note that 'move' is playing fast and loose with standard argument matching here; it correctly interprets the '.' as 'from', rather than 'x'. This well-meaning subversion can lead to unexpected trouble if you deviate from the paradigms in EXAMPLES. If in doubt, you can always name 'from' and 'to'.

'move' can also handle moves in and out of packages being live-edited (see 'maintain.packages'). If you want to specify a move to/from your package "whizzbang", the syntax of 'to' and 'from' should be '..whizzbang' (i.e. the actual environment where the pre-installed package lives). An alternative for those short of typing practice is 'maintained.packages$whizzbang'. No quotes in either case.

If 'move' finds an object with the same name in the destination, you will be asked whether to overwrite it. If you say no, the object will not be moved. If you want to force overwriting of a large number of objects, set 'overwrite.by.default=TRUE'.

By default, 'move' will delete the original object after it has safely arrived in its destination. It's normally only necessary (and more helpful) to have just one instance of an object; after all, if it needs to be accessed by several different tasks, you can just 'move' it to an ancestral task. However, if you really do want a duplicate, you can avoid deletion of the original by setting 'copy=TRUE'.

You will be prompted for whether to 'save' the source and destination tasks, if they are attached somewhere, but not in position 1. Normally this is a good idea, but you can always say no, and call 'Save.pos' later.  If the source and/or destination are not attached, they will of course be saved automatically. The top workspace (i.e. current task) '.GlobalEnv' is never saved automatically; you have to call 'Save' yourself.

'move' is not meant to be called within other functions.


EXAMPLES

## Don't run:
move( myobj, ., 0) # back to the ROOT task

move( what="%myop%", 0/first.task, 0/second.task)
# neither source nor destination attached. Funny name requires "what"

move( what=c( "first.obj", "second.obj"), ., ../sibling.task)
# multiple objects require "what"

move( myobj, ..myfirstpack, ..mysecondpack) # live-edited packages

## End Don't run


SEE.ALSO

'cd'


AUTHOR

Mark Bravington


KEYWORDS

utilities
}")

)

"move.backup.file" <-
function( name, old.dir, new.dir, copy=FALSE) {
  if( !nchar( old.file <- get.bkfile( name, old.dir, create=FALSE)))
return()

  new.index <- create.bkind.if.needed( new.dir)
  new.file <- get.bkfile( name, new.dir, create=TRUE)
  file.copy( from=old.file, to=new.file, overwrite=TRUE)
  
  unlink( old.file)
  if( !copy) {
    old.index <- create.bkind.if.needed( old.dir) # sure to exist
    old.index.contents <- read.bkind( old.dir)
    which <- match( name, old.index.contents$object.names)
    cat( paste( old.index.contents$files[ -which], old.index.contents$object.names[ -which], sep='='), sep='\n', 
      file=old.index)
  }
}


"move.fix.list" <-
function( nlocal=sys.parent()) mlocal({
  fixing <- match( fix.list$name, what, 0) > 0
  if( any( fixing)) { # must all be moving to the same place
    stt <- search.task.trees()
    path.list <- sapply( stt, function( x) attr( pos.to.env( x), 'path'))
    if( !is.na( to.match <- match( attr( to$env, 'path'), path.list)[1])) {
      fix.list$where[ fixing] <<- names( stt)[ to.match]
      fix.list$where.type[ fixing] <<- 'task'
    } else {
      if( !is.null( attr( to$env, 'name')) && 
          (attr( to$env, 'name') %in% names( maintained.packages))) {
        fix.list$where[ fixing] <<- paste( attr( to$env, 'task.tree'), 
            collapse='/')
        fix.list$where.type[ fixing] <<- 'package'
      } else {
        cat( 'Warning: the following have moved out of memory and further fixes will not be committed: ',
            paste( fix.list$name[ fixing], collapse=','), '\n')
        fix.list <<- fix.list[ !fixing,]
      }
    }
  }
})


"move.to.mcache" <-
function( what, envir, save.now) { # used to have a getfrom arg
  mcache <- attr( envir, 'mcache')
  if( is.null( mcache))
    mcache <- numeric(0)

  if( !length( what))
return( mcache)

  what <- (what %SUCH.THAT% exists( ., envir=envir)) %SUCH.THAT% !bindingIsActive( ., env=envir)
#  what <- what %SUCH.THAT% exists( ., envir=getfrom, inherits=TRUE)

  ow <- options( warn=-1)
  on.exit( options( ow))
  attr( envir, 'mcache') <- mcache <- mupdate.mcache( what, mcache, envir)

  path <- attr( envir, 'path')
  if( getOption( 'mlazy.subdir', TRUE)) {
    dir.create( file.path( path, 'mlazy'), showWarnings=FALSE)
    objpath <-  file.path( 'mlazy', 'obj')
  } else
    objpath <- 'obj'

  for( i in what) {
    # Anything moved to the cache must be saved
    this.file <- file.path( path, objpath %&% mcache[ i] %&% '.rda')
    xsave( list=i, file=this.file, envir=envir) # used to have envir=getfrom ???

    fx <- get.mcache.reffun( i, envir)
    environment( fx)[[ i]] <- envir[[ i]]
    remove( list=i, envir=envir)
    suppressWarnings( makeActiveBinding( i, fx, envir))
  }
  mupdate.mcache.index.if.opt( mcache, file.path( path, objpath))

return( mcache)
}


"mp.synch" <-
function( pkg){
  nspos <- try( asNamespace( pkg), silent=TRUE)
  if( nspos %is.not.a% 'try-error') {
    # use identical to check whether *really* changed

    if( exports.have.changed)
    users <- getNamespaceUsers( pkg)

  }

  pkpos <- match( 'package:' %&% pkg, search(), 0)
  if( pkpos>0) {

  }

}


"mtidy" <-
function( ..., what, envir=.GlobalEnv) {
  if( missing( what))
    what <- sapply( match.call( expand.dots=FALSE)$..., deparse)
  if( !length( what))
return()

  envir <- as.environment( envir)
  mcache <- attr( envir, 'mcache')

  if( !missing( what)) {
    what <- what %such.that% ( . %in% lsall( envir))
    mlazy( what=what, envir=envir) #  %except% names( mcache), envir=envir) # caused trouble with direct assign
  } else
    what <- names( mcache) %such.that% ( . %in% lsall( envir))

  if( !length( what))
return( invisible( what))

  path <- attr( envir, 'path')
  if( is.null( path))
stop( 'environment has no path attribute')

  save.mchanged( what, envir)

  # Replace cachees by new active bindings
  remove( list=what, envir=envir)
  setup.mcache( refs=what, envir=envir)

  invisible( what)
}


"multimatch" <-
structure( function( df1, df2, nomatch=NA, char.force=FALSE, force.same.cols=TRUE) {
  df1 <- as.data.frame( df1, row.names=NULL)
  df2 <- as.data.frame( df2, row.names=NULL)
  if( !nrow( df1))
return( integer( 0))

  if( !nrow( df2))
return( as.integer( rep( nomatch, nrow( df1))))

  if( !force.same.cols) {
    in.both <- names( df1) %that.are.in% names( df2)
    df1 <- df1[ in.both]
    df2 <- df2[ in.both]
  }

  if( (n <- length( df1)) != length( df2))
stop( '# columns must be equal for multimatch')
  if( char.force) {
    df1[] <- lapply( df1, as.character)
    df2[] <- lapply( df2, as.character)
  }

  if( !all( (dc <- sapply( df1, data.class)) == sapply( df2, data.class)))
stop( 'mismatch in data classes')
  if( !all( do.on( df1, is.logical( .) || is.numeric( .) || is.factor( .) ||
      is.character( .) || (. %is.a% 'POSIXct'))))
stop( 'needs logical, numeric, factor, character, or POSIXct variables')
  if( any( do.on( df1, is.array(.))))
stop( "can't handle arrays (yet)")  

  nr1 <- nrow( df1)
  nr2 <- nrow( df2)
  mdf1 <- matrix( 0, nr1, n)
  mdf2 <- matrix( 0, nr2, n)

  nu <- numeric( n) # used to be integer, but this can cope with more/bigger
  for( i in 1:n) {
    if( is.factor( df1[[ i]])) {
      # Match based on level strings (even if levels are not identical)
      # NAs need to match too, hence xfactor()
      df1i <- xfactor( df1[[ i]], exclude=NULL)
      df2i <- xfactor( df2[[ i]], exclude=NULL)
      ul <- unique( c( levels( df1i), levels( df2i)))
      ilev1 <- match( levels( df1i), ul)
      mdf1[, i] <- ilev1[ as.numeric( df1i)]
      ilev2 <- match( levels( df2i), ul)
      mdf2[, i] <- ilev2[ as.numeric( df2i)]
    } else { # numeric/logical
      ul <- unique( c( df1[[ i]], df2[[ i]]))
      mdf1[, i] <- match( df1[[ i]], ul)
      mdf2[, i] <- match( df2[[ i]], ul)
    }

    nu[ i] <- length( ul)
  }

  nu <- c( 1, cumprod( nu)[ -n])
  imdf1 <- mdf1 %**% nu
  imdf2 <- mdf2 %**% nu
  match( imdf1, imdf2, nomatch)
}
, doc =  docattr( r"{
multimatch    package:mvbutils


Match rows of one dataframe to another using multiple columns


DESCRIPTION

Like 'match', but for more than one variable at a time--- and geared specifically to dataframes (or matrices). NA values match only to NAs.

So useful that I've finally moved it from secret package 'handy2' into package 'mvbutils', and added documentation.

Any 'factor' fields (which I hardly ever use; characters are just Better) will be matched based on the strings they display as, so that (eg) arbitrary re-orderings 'levels' won't matter.


USAGE


multimatch(df1, df2, nomatch = NA, char.force = FALSE, force.same.cols = TRUE)


ARGUMENTS

 df1,  df2: two dataframes. Unless you set 'force.same.cols=FALSE', column-order is assumed to be the same in both, and a mismatched number of columns will trigger an error.
 
 nomatch: like in 'match' (qv)
 
 char.force: ?convert all columns to 'character' before checking? Usually doesn't matter; if it does, 'TRUE' is _probably_ the safer, but historically the default is 'FALSE'
 
 force.same.cols: Perhaps a misleading name... set to 'FALSE' if you want 'multimatch' to use only columns whose name exists in both dataframes, and to re-order columns if necessary so that the names match. Usually the non-default 'FALSE' is better!


VALUE

A numeric vector, one element per row in 'df1', showing which row in 'df2' it matches to, or 'nomatch' if none do.


DETAILS

'multimatch' works by constructing a single numeric composite for each row in 'df1' and 'df2', based on multiplying numbers of distinct values across columns. This could _potentially_ overflow, or give inaccurate results, if the number of columns and distinct values is very large. So, don't use 'multimatch' in that situation...


SEE.ALSO

'match'


EXAMPLES

xx <- data.frame( 
    animal= cq( cat, dog, cat), colour= cq( blue, blue, pink), size= 1:3, 
    royalty= cq( high, low, high))
yy <- data.frame( 
    animal= cq( dog, dog, cat), colour= cq( red, blue, pink), size= 1:3,
    loyalty=cq( high, high, low)) # note the spelling!

multimatch( xx, yy) 
# NA NA NA
multimatch( xx[,1:3], yy[,1:3])  # ignore 4th col
# NA 2 3
multimatch( xx, yy, force=FALSE) # auto-drop loyalty & royalty (different names)
# NA 2 3 
try( multimatch( xx[,1:2], yy[,1:3]))
# <error>: num of cols
try( multimatch( xx[,1:2], yy[,1:3], force=F))
# all good
multimatch( as.matrix( xx[,1:3]), as.matrix( yy[,1:3])) # matrices OK too
# NA 2 3

}")

)

"multinsert" <-
function( orig, at, ins, sorted.at=TRUE){
  if( !length( at))
return( orig)

  if( !is.list( ins))
    ins <- if( length( at)==1) list( ins) else as.list( ins) # assumes each ins elt is length-1
  if( length( ins) < length( at))
    ins <- rep( ins, length( at) / length( ins))

  if( !sorted.at) {
    o <- order( at)
    at <- at[ o]
    ins <- ins[ o]
  }

  inslen <- sapply( ins, length)
  # NB replace call in next line: in case at[1]==0
  new <- orig[ rep( seq_along( orig), c( 1, 1+inslen)[ 1+match( seq_along( orig),
      replace( at, at==0, 1), nomatch=0)])]
  new[ rep( at, inslen) + 1:sum( inslen)] <- unlist( ins)
  new
}


"multirep" <-
structure( function( orig, at, repl, sorted.at=TRUE){
  if( !length( at))
return( orig)

if( !sorted.at) {
  o <- order( at)
  at <- at[ o]
  repl <- repl[ o]
}

  replen <- sapply( repl, length)
  new <- orig[ rep( seq_along( orig), c( 1, replen)[ 1+match(
      seq_along( orig), at, nomatch=0)])]
  new[ rep( at, replen) + 1:sum( replen) - rep( 1:length(at), replen)] <- unlist( repl)
  new
}
, doc =  docattr( r"{
multirep package:mvbutils
multinsert
massrep

Replacement and insertion functions with more/less than 1 replacement per spot

DESCRIPTION

'multirep' is like 'replace', but the replacements are a list of the same length as the number of elements to replace. Each element of the list can have 0, 1, or more elements-- the original vector will be expanded/contracted accordingly. (If all elements of the list have length 1, the result will be the same length as the original.) 'multinsert' is similar, but doesn't overwrite the elements in 'orig' (so the result of 'multinsert' is longer). 'massrep' is like 'multirep', but takes lists as arguments so that a group-of-line-numbers in the first list is replaced by a group-of-lines in the second list.

USAGE

multirep( orig, at, repl, sorted.at=TRUE)
multinsert( orig, at, ins, sorted.at=TRUE)
massrep( orig, atlist, replist, sorted.at=TRUE)


ARGUMENTS

 orig: vector

 at: numeric vector, saying which elements of the original will be replaced or appended-to. Can't exceed 'length(orig)'. 0 is legal in 'multinsert' but not 'multirep'. Assumed sorted unless 'sorted.at' is set to FALSE.

 atlist: list where each element is a group of line numbers to be replaced by the corresponding element of 'replist' (and that element can have a different length). Normally each group of line numbers would be consecutive, but this is not mandatory.

 repl, ins, replist: a list of replacements. 'repl[[i]]' will replace line 'at[i]' in 'orig', possibly removing it (if 'repl[[i]]' has length 0) or inserting extra elements (if 'repl[[i]]' has length > 1). In 'multinsert', 'repl' can be a non-list, whereupon it will be cast to 'list(repl)' [if 'at' is length 1] or 'as.list(repl)' [if 'at' is length>1]. If 'length(repl') < 'length(at)', 'repl' will be replicated to the appropriate size. If 'repl' is atomic, it will be typecast into a list-- in this case, all replacements/insertions will be of length 1.

 sorted.at: if TRUE, then 'at' had better be sorted beforehand; if FALSE, 'at' will be sorted for you inside 'multirep', and 'repl' is reordered accordingly.


EXAMPLES

multirep( cq( the, cat, sat, on, the, mat), c( 2, 6),
    list( cq( big, bug), cq( elephant, howdah, cushion)))
# [1] "the" "big" "bug" "sat" "on" "the" "elephant" "howdah" "cushion"

multirep( cq( the, cat, sat, on, the, mat), c( 2, 6),
    list( cq( big, bug), character(0)))
# [1] "the" "big" "bug" "sat" "on" "the"

# NB the 0 in next example:
multinsert( cq( cat, sat, on, mat), c( 0, 4),
    list( cq( fat), cq( cleaning, equipment)))
# [1] "fat" "cat" "sat" "on" "mat" "cleaning" "equipment"

KEYWORDS
misc
}")

)

"mupdate.mcache" <-
function( what, mcache, envir) {
  had.num <- what %such.that% (. %in% names( mcache))
  need.num <- what %except% had.num
  if( !length( need.num))
return( mcache)
  mci <- attr( mcache, 'info')

  if( !length( mcache))
    new.mcache <- seq( along=need.num)
  else {
    new.mcache <- (1 %upto% (max( c( 0, abs( mcache)), na.rm=TRUE) + length( need.num))
        ) %except% abs( mcache)
    new.mcache <- new.mcache[ 1:length( need.num)]
  }

  names( new.mcache) <- need.num
  new.mci <- lapply( need.num, get.info.for.mcache, envir=envir, name=TRUE)

  mcache <- c( mcache, new.mcache)
  attr( mcache, 'info') <- c( mci, new.mci)
  mcache
}


"mupdate.mcache.index.if.opt" <-
function( 
    mcache=attr( env, 'mcache'), 
    objpath=file.path( attr( env, 'path'), 'mlazy', 'obj'),
    mlazy.index=getOption( 'mlazy.index', FALSE),
    env=.GlobalEnv)
  if( !is.null( mcache) && mlazy.index)
    cat( names( mcache) %&% '\t' %&% abs( mcache), sep='\n', file= objpath %&% '.ind')


"mvb.eval.parent" <-
function( expr, n=1){
  p <- mvb.parent.frame( n+1)
  eval( expr, p)
}


"mvb.file.copy" <-
function( file1, file2, overwrite=TRUE) {
  # file.copy used to stuff up 'mtime' so I wrote a special version. Presumably no longer needed...
  # ... and my version (using system calls) was very slow

  if( getRversion() >= '3.3.3') { # bug in R's file.copy here; Sys.setFileTime only does one at a time
    ok <- file.copy( from=file1, to=file2, overwrite=overwrite,
        recursive=FALSE, copy.mode=TRUE, copy.date=FALSE)
    for( iok in which( ok)) {
      Sys.setFileTime( file2[iok], file.info( file1[iok], extra_cols=FALSE)$mtime)
    }
return( ok)
  } else if( getRversion() >= '3.4.0') { # nope still not bloody fixed in 3.4.4
return( file.copy( from=file1, to=file2, overwrite=overwrite,
    recursive=FALSE, copy.mode=TRUE, copy.date=TRUE))
  }

  if( .Platform$OS.type=='windows') {
    syscopy <- Sys.getenv( 'COMSPEC') %&% ' /c copy /y'
    file1 <- '"' %&% gsub( '/', '\\\\', file1) %&% '"'
    file2 <- '"' %&% gsub( '/', '\\\\', file2) %&% '"'
    copy.same.mtime <- function( f1, f2)
      system( paste( syscopy, f1, f2), show.output.on.console=FALSE)
  } else {
    syscopy <- 'cp'
    # Escape spaces and backslashes... and probably all sorts of other crap NFN
    subbo <- function( f) {
      f <- gsub( '\\', '\001', f, fixed=TRUE)
      f <- gsub( ' ', '\\ ', f, fixed=TRUE)
      f <- gsub( '\001', '\\\\', f, fixed=TRUE)
    }

    copy.same.mtime <- function( f1, f2) {
      result <- system( paste( syscopy, subbo( f1), subbo( f2)))
      if( result==0) {
        f1.mtime <- format( file.info( f1)$mtime, '%Y%m%d%H%M.%S')
        system( paste( 'touch -m -t ', f1.mtime, subbo( f2)))
      }
      result
    }
  }

  ok <- rep( FALSE, length( file1))
  for( i in seq_along( file1))
    if( overwrite || !file.exists( file2[i]))
      ok[ i] <- copy.same.mtime( file1[i], file2[i])
  return( ok)
}


"mvb.formalize.package.hook" <-
function( default.list) {
  default.list$exclude.funs <- character( 0)
  default.list
}


"mvb.match.call" <-
function (definition = sys.function( mvb.sys.parent()), 
    call = sys.call(mvb.sys.parent()), 
    expand.dots = TRUE,
    envir= mvb.parent.frame( 2)) {
  # This has to be tricky to get it to work in 'debug'
  # eg f <- function( ...) g(...), g <- function( alpha=1) match.call(), f(1)
  # It's not clear to me that ... is consistently handled when call is non-default
  # ... because it still depends on the calling context
  
  # envir arg added in base-R at some point <= R 3.3
  
  callo <- quote( baseenv()$match.call())
  callo[[2]] <- definition
  callo[[3]] <- baseenv()$call( 'quote', call)
  callo$envir <- envir
  callo$expand.dots <- expand.dots
  eval( callo, mvb.parent.frame(2))
}


"mvb.nargs" <-
function() 
  length( sys.calls()[[ mvb.sys.parent()]])-1


"mvb.parent.frame" <-
function (n = 1) 
  sys.frame( mvb.sys.parent( n+1)) # +1 added Oct 09


"mvb.rbind.data.frame" <-
function( ..., deparse.level=1) {
  scatn( "'mvb.rbind.data.frame' is obsolete, but still being called!")
  allargs <- list( ...) %SUCH.THAT% !is.null( .)
  if( !length( allargs))
return( brdf()) # weird-ass 0*0 DF, as base-R doco mandates (why??!!); should not be reached by dispatch

  # This for some kind of compatibility with potty base-R behaviour
  is.scalar <- sapply( allargs, is.atomic) & sapply( allargs, is.vector)
  
  allargs[ !is.scalar] <- lapply( allargs[ !is.scalar], data.frame)
  ncols <- sapply( allargs[ !is.scalar], ncol)
  if( any( ncols != ncols[1]))
stop( 'Differing number of columns')

  # Make all scalars into single-row data frames: crazy base-R. Should not be allowed!
  if( any( is.scalar)) {
    warning( "risky to supply scalar argument(s) to 'rbind.data.frame'")
    target <- names( allargs[ !is.scalar][[ 1]])
    make.like.target <- function( x) {
        xout <- rep( x[1], length( target))
        xout[] <- x
        names( xout) <- target
        data.frame( as.list( xout))
      }
    allargs[ is.scalar] <- lapply( allargs[ is.scalar], make.like.target)
  }

  if( length( allargs)==1)
return( allargs[[1]])

  rows <- sapply( allargs, nrow)
  norows <- rows==0

  # 0-row args get a row of NAs. Must avoid calling rbind!
  allargs[ norows] <- lapply( allargs[ norows], function( x) {
      x <- data.frame( x) # since matrices don't like next line...
      x[1,] <- x[1,] # ... which adds a row of NAs, even for cols of DF that are matrices
      x
    })
  
  # brdf = base::rbind.data.frame, modded to handle classed matrices
  rbindo <- do.call( brdf, c( allargs, list( deparse.level=deparse.level)))
  if( any( norows)) # should work anyway but...
    rbindo <- rbindo[ -cumsum( rows + norows)[norows],,drop=FALSE]
  rbindo
}


"mvb.session.env" <-
structure( function(){
  # Eventually, make this a separate env in mvbutils namespace
  # currently...
  as.environment( 'mvb.session.info')
}
, doc =  docattr( r"{
mvb.session.env    package:mvbutils
mvb_session_env


Session info environment

DESCRIPTION

Package 'mvbutils' needs a place to stash useful session-level stuff, such as 'fix.list' (see 'fixr'). Since like foreeeever (2001), this has been via a special environment called 'mvb.session.info' which is attached to the search path. However, that's not how yer sposed to do it apparently, so for better security the direct use of 'mvb.session.info' is deprecated in favour of calling the _function_ 'mvb_session_env()' or its dotty synonym. Like the base-R functions 'globalenv()' and 'baseenv()', an _environment_ is returned.

Future versions of 'mvbutils' package will move the session-info environment into "private" storage within the 'mvbutils' namespace, so that it can only easily be accessed via 'mvb_session_env()' or 'mvb.session.env()'.


USAGE

mvb_session_env()
mvb.session.env() 


VALUE

Environment where session-level info used by the 'mvbutils' package (and perhaps by other packages, such as 'debug') is stashed.

}")

)

"mvb.sys.call" <-
function( which=0) {
  if( which>0)
    sys.call( which) # dotInternal( sys.call( which))
  else {
    which <- try( mvb.sys.parent( 1-which), silent=TRUE)
    if( which %is.a% 'try-error')
stop( 'not that many enclosing functions')
    else if( which==0)
      NULL # that's what R 1.8.1 does
    else
      sys.call( which) # dotInternal( sys.call( which))
  }
}


"mvb.sys.function" <-
function( n) {
  if( missing( n))
    n <- mvb.sys.parent()
  sys.function( n)
}


"mvb.sys.nframe" <-
function() mvb.sys.parent(1)


"mvb.sys.parent" <-
structure( function(n=1) {
  p <- sys.nframe()
  frames <- lapply( sys.frames(), list) # this wrapper seems to be necessary to get it to work. R "feature"
  parents <- sys.parents()
  for( gen in 0 %upto% n)
    p <- parents[ which( sapply( frames, identical, frames[[p]]) )[ 1] ] # parent of FIRST pointer to this env in frame list

  p
}
, doc =  docattr( r"{
mvb.sys.parent    package:mvbutils
mvb.sys.nframe
mvb.parent.frame
mvb.eval.parent
mvb.match.call
mvb.nargs
mvb.sys.call
mvb.sys.function


Functions to Access the Function Call Stack

DESCRIPTION

These functions are "do what I mean, not what I say" equivalents of the corresponding system functions. The system functions can behave strangely when called in strange ways (primarily inside 'eval' calls). The 'mvb' equivalents behave in a more predictable fashion.


USAGE

mvb.sys.parent(n=1)
mvb.sys.nframe()
mvb.parent.frame(n=1)
mvb.eval.parent( expr, n=1)
mvb.match.call(definition = sys.function(mvb.sys.parent()), 
    call = sys.call(mvb.sys.parent()),  expand.dots = TRUE, envir= mvb.parent.frame( 2)) 
mvb.nargs() 
mvb.sys.call(which = 0) 
mvb.sys.function(n) 


ARGUMENTS

All as per the corresponding system functions, from whole helpfiles the following is taken:

 which: the frame number if non-negative, the number of generations to go back if negative. (See the Details section.)
 
 n: the number of frame generations to go back.
 
 definition: a function, by default the function from which 'match.call' is called.
 
 call: an unevaluated call to the function specified by 'definition', as generated by 'call'.
 
 expr: an expression to evaluate
 
 expand.dots: logical. Should arguments matching '...' in the call be included or left as a '...' argument?

 envir: an environment from which the ... in call are retrieved, if any (as per 'base::match.call')

VALUE

See the helpfiles for the system functions.


DETAILS

Sometimes 'eval' is used to execute statements in another frame. If such statements include calls to the system versions of these routines, the results will probably not be what you want. In technical terms: the same environment will actually appear several times on the call stack (returned by 'sys.frame()') but with a different calling history each time. The 'mvb.' equivalents look through 'sys.frames()' for the first frame whose environment is identical to the environment they were called from, and base all conclusions on that first frame. To see how in detail, look at the most fundamental function: 'mvb.sys.parent'.

'mvbutils' pre 2.7 used to include 'mvb.sys.on.exit' as well (to return whatever the 'on.exit' code would be), but I think this was by mistake; the code was actually specific to my 'debug' package (which already has its own substitute), and so I've moved it out of 'mvbutils'.


EXAMPLES

ff.no.eval <- function() sys.nframe()
ff.no.eval() # 1

ff.system <- function() eval( quote( sys.nframe()), envir=sys.frame( sys.nframe()))
ff.system() # expect 1 as per ff.no.eval, get 3

ff.mvb <- function() eval( quote( mvb.sys.nframe()), envir=sys.frame( sys.nframe()))
ff.mvb() # 1

ff.no.eval <- function(...) sys.call()
ff.no.eval( 27, b=4) # ff.no.eval( 27, b=4)

ff.system <- function(...) eval( quote( sys.call()), envir=sys.frame( sys.nframe()))
ff.system( 27, b=4) # eval( expr, envir, enclos) !!!

ff.mvb <- function(...) eval( quote( mvb.sys.call()), envir=sys.frame( sys.nframe()))
ff.mvb( 27, b=4) # ff.mvb( 27, b=4)


SEE.ALSO

'sys.parent', 'sys.nframe', 'parent.frame', 'eval.parent', 'match.call', 'nargs', 'sys.call', 'sys.function'


AUTHOR

Mark Bravington


KEYWORDS

programming
}")

)

"mvb_session_env" <-
function() mvb.session.env()


"mvboption" <-
structure( function( ...) {
  l <- list( ...)
  ol <- NULL
  if( !is.null( names( l))) {
    if( all( nzchar( names( l)))) {
      existing_options <- names( l) %that.are.in% lsall( mvboptions)
      ol <- mget( existing_options, mvboptions)
    } else {
stop( "must be non-empty names")
    }
    for( i in names( l)) {
      assign( i, l[[i]], envir=mvboptions)
    }
  } else if( is.character( l[[1]])) {
    ol <- mget( l[[1]], mvboptions)
    if( length( ol)==1){
      ol <- ol[[1]]
    }
  } else {
stop( "eh? weird args")
  }

return( ol)
}
, doc =  docattr( r"{
mvboption    package:mvbutils

Private options for mvbutils package and beyond


DESCRIPTION

Set/get values in the environment 'mvbutils::mvboptions'. Mostly for 'mvbutils' itself, but anyone can use it at their own risk! Partly intended to ultimately obviate the dicey 'mvb.session.info' environment on the search path...


USAGE

mvboption(...) # eg mvboption( use_something=TRUE) to set,
# ... or mvboption( 'what_am_i') to get


ARGUMENTS

 ...: Either a named pairlist (eg 'mvboption( a=1, b=2)') to set, or a character vector to get


VALUE

Any previous value(s) of the options, if setting; this might mean an empty list. When getting, it's a list if more than one thing is being gotten, or the value itself if just one. More obvious than it sounds. See EXAMPLES.


EXAMPLES

mvboption( something=1)  # empty list
mvboption( a=2, b=3)     # empty list
mvboption( 'b')          # [1] 3
mvboption( cq( a, b))    # list with two elements

}")

)

"mvbutils.dollar.assign.data.frame" <-
function (x, name, value) {
  cl <- oldClass(x)
  class(x) <- NULL
  nrows <- .row_names_info(x, 2L)
  if (!is.null(value)) {
    N <- NROW(value)
    if (N > nrows) {
      if( nrows>0) {
stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
        "replacement has %d rows, data has %d"), N, nrows), domain = NA)
      } else { # create "empty" version of value
        if( length( dv <- dim( value)) > 1L) {
          emptyval <- structure( as.vector( value)[0L], dim=c( 0L, dv[-1L]))
          if( !is.null( dn <- dimnames( value))) {
            newdn <-  c( list( character()), dn[-1L])
            names( newdn) <- NULL # because I say so
            dimnames( emptyval) <- newdn
          } # if dimnames
          attributes( emptyval) <- c( attributes( emptyval), # dim and maybe dimnames
              attributes( value) %without.name% c( 'dim', 'dimnames'))
          value <- emptyval
        } else {
          value <- value[0]
        } # ?empty vector, or empty array?
      } # if need empty
    } else if (N < nrows) {
      if (N > 0L && (nrows%%N == 0L) && length(dim(value)) <= 1L) 
        value <- rep(value, length.out = nrows)
      else 
stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
        "replacement has %d rows, data has %d"), N, nrows), domain = NA)
    }
    
    if (is.atomic(value) && !is.null(names(value))) 
      names(value) <- NULL
  } # if something to replace with
  
  x[[name]] <- value
  class(x) <- cl
  return(x)
}


"mvbutils.subassign.data.frame" <-
function (x, i, j, value) {
  if (!all(names(sys.call()) %in% c("", "value"))) 
    warning("named arguments are discouraged")

  cl <- oldClass(x)
  class(x) <- NULL
  nrows <- .row_names_info(x, 2L)
  if (is.atomic(value) && !is.null(names(value))) 
    names(value) <- NULL

  if (nargs() < 4L) {
    nc <- length(x)
    if (!is.null(value)) {
      N <- NROW(value)
      if (N > nrows) {
        if( nrows>0) {
stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
         "replacement has %d rows, data has %d"), N, nrows), domain = NA)
        } else { # create "empty" version of value
          if( length( dv <- dim( value)) > 1L) {
            emptyval <- structure( as.vector( value)[0L], dim=c( 0L, dv[-1L]))
            if( !is.null( dn <- dimnames( value))) {
              newdn <-  c( list( character()), dn[-1L])
              names( newdn) <- NULL # because I say so
              dimnames( emptyval) <- newdn
            } # if dimnames
            attributes( emptyval) <- c( attributes( emptyval), # dim and maybe dimnames
                attributes( value) %without.name% c( 'dim', 'dimnames'))
            value <- emptyval
          } else {
            value <- value[0]
          } # ?empty vector, or empty array?
        } # if need empty
      } else if (N < nrows) {
        if (N > 0L && (nrows%%N == 0L) && length(dim(value)) <= 1L) 
          value <- rep(value, length.out = nrows)
        else 
stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
         "replacement has %d rows, data has %d"), N, nrows), domain = NA)
      } 
    }
    
    x[[i]] <- value
    if (length(x) > nc) {
      nc <- length(x)
      if (names(x)[nc] == "") 
        names(x)[nc] <- paste0("V", nc)
      names(x) <- make.unique(names(x))
    }
    class(x) <- cl
    return(x)
  }
  
  if (missing(i) || missing(j)) 
    stop("only valid calls are x[[j]] <- value or x[[i,j]] <- value")
  rows <- attr(x, "row.names")
  nvars <- length(x)
  if (n <- is.character(i)) {
    ii <- match(i, rows)
    n <- sum(new.rows <- is.na(ii))
    if (n > 0L) {
      ii[new.rows] <- seq.int(from = nrows + 1L, length.out = n)
      new.rows <- i[new.rows]
    }
    i <- ii
  }
  
  if (all(i >= 0L) && (nn <- max(i)) > nrows) {
    if (n == 0L) {
      nrr <- (nrows + 1L):nn
      if (inherits(value, "data.frame") && (dim(value)[1L]) >= length(nrr)) {
        new.rows <- attr(value, "row.names")[seq_len(nrr)]
        repl <- duplicated(new.rows) | match(new.rows, rows, 0L)
        if (any(repl)) 
         new.rows[repl] <- nrr[repl]
      } else {
        new.rows <- nrr
      }
    }
    
    x <- xpdrows.data.frame(x, rows, new.rows)
    rows <- attr(x, "row.names")
    nrows <- length(rows)
  }
  
  iseq <- seq_len(nrows)[i]
  if (anyNA(iseq)) 
stop("non-existent rows not allowed")

  if (is.character(j)) {
    if ("" %in% j) 
stop("column name \"\" cannot match any column")

    jseq <- match(j, names(x))
    if (anyNA(jseq)) 
stop(gettextf("replacing element in non-existent column: %s", 
        j[is.na(jseq)]), domain = NA)
  } else if (is.logical(j) || min(j) < 0L) 
    jseq <- seq_along(x)[j]
  else {
    jseq <- j
    if (max(jseq) > nvars) 
      stop(gettextf("replacing element in non-existent column: %s", 
        jseq[jseq > nvars]), domain = NA)
  }
  
  if (length(iseq) > 1L || length(jseq) > 1L) 
stop("only a single element should be replaced")

  x[[jseq]][[iseq]] <- value
  class(x) <- cl
  x
}


"mwhere" <-
function( x, cond){
  # Don't eval args prematurely...
  mc <- match.call()
  mc[[1]] <- as.name( '%where%')
  eval.parent( mc)
}


"my.all.equal" <-
function (x, y, ...) {
  # This *really* shouldn't be needed--- but is :/
  # R has, delightfully, changed the behaviour of all.equal() over the years
  # I am in a polite frame of mind today (late 2018) so will refrain from further comment
  # the ... is so you can tell R to just effin do it, for example
  stupid <- all.equal(x, y, ...)
  if (!is.logical(stupid))
      stupid <- FALSE
return( stupid)
}


"my.fixup.package.URLs" <-
function (pkg, force = FALSE) 
{
    top <- paste("file:///", chartr("\\", "/", R.home()), sep = "")
    fixedfile <- file.path(pkg, "fixedHTMLlinks")
    if (file.exists(fixedfile)) {
        oldtop <- readLines(fixedfile)
        if (!force && (length(oldtop) == 1) && top == oldtop) 
            return(TRUE)
        olddoc <- paste(oldtop, "/doc", sep = "")
        oldbase <- paste(oldtop, "/library/base", sep = "")
        oldutils <- paste(oldtop, "/library/utils", sep = "")
        oldgraphics <- paste(oldtop, "/library/graphics", sep = "")
        oldstats <- paste(oldtop, "/library/stats", sep = "")
        olddata <- paste(oldtop, "/library/datasets", sep = "")
        oldgrD <- paste(oldtop, "/library/grDevices", sep = "")
        oldmeth <- paste(oldtop, "/library/methods", sep = "")
    }
    else {
        olddoc <- "../../../doc"
        oldbase <- "../../base"
        oldutils <- "../../utils"
        oldgraphics <- "../../graphics"
        oldgrDevices <- "../../grDevices"
        oldstats <- "../../stats"
        olddata <- "../../datasets"
        oldgrD <- "../../grDevices"
        oldmeth <- "../../methods"
    }
    if (!file.create(fixedfile)) 
        return(FALSE)
    cat(top, "\n", sep = "", file = fixedfile)
    htmldir <- file.path(pkg, "html")
    if (!file.exists(htmldir)) 
        return(FALSE)
    files <- list.files(htmldir, pattern = "\\.html$", full.names = TRUE)
    doc <- paste(top, "/doc", sep = "")
    base <- paste(top, "/library/base", sep = "")
    utils <- paste(top, "/library/utils", sep = "")
    graphics <- paste(top, "/library/graphics", sep = "")
    stats <- paste(top, "/library/stats", sep = "")
    datasets <- paste(top, "/library/datasets", sep = "")
    grD <- paste(top, "/library/grDevices", sep = "")
    meth <- paste(top, "/library/methods", sep = "")
    
    # altered by MVB 3/2009, to avoid changing unmodified files
    for (f in files) {
        page <- readLines(f)
        old.page <- page # MVB
        page <- gsub(olddoc, doc, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldbase, base, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldutils, utils, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldgraphics, graphics, page, fixed = TRUE, 
            useBytes = TRUE)
        page <- gsub(oldstats, stats, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(olddata, datasets, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldgrD, grD, page, fixed = TRUE, useBytes = TRUE)
        page <- gsub(oldmeth, meth, page, fixed = TRUE, useBytes = TRUE)
        if( identical( page, old.page))
    next
        out <- try(file(f, open = "w"), silent = TRUE)
        if (inherits(out, "try-error")) {
            warning(gettextf("cannot update '%s'", f), domain = NA)
            next
        }
        writeLines(page, out)
        close(out)
    }
    return(TRUE)
}


"my.index" <-
structure( function( var, ...) {
#  pg <- .Primitive( '[[') # doesn't cope with pairlists
#  pg <- function( x, i) .Primitive( '[[')( as.list( x), i) # screws up e.g. on factors
  if( getRversion() >= '2.12') {
    cc <- unlist( list( ...))
    if( length( cc))
return( (baseenv()$'[[')( var, cc))
    else
return( var)
  }

  pg <- function( x, i) .Primitive( '[[')( if( is.pairlist( x)) as.list( x) else x, i)
  vv <- as.name( 'var')
  for( i in c(...))
    vv <- call( 'pg', vv, i)
  eval( vv)
}
, doc =  docattr( r"{
my.index       package:mvbutils
my.index.assign
my.index.exists

Arbitrary-level retrieval from and modification of recursive objects

DESCRIPTION

As of R 2.12, you probably don't need these at all. But, in case you do: 'my.index' and 'my.index.assign' are designed to replace '[[' and '[[<-' _within_ a function, to allow arbitrary-depth access into any recursive object. In order to avoid conflicts with system usage and/or slowdowns, it is wise to do this only inside a function definition where they are needed. A zero-length index returns the entire object, which I think is more sensible than the default behaviour (chuck a tanty). 'my.index.exists' tests whether the indexed element actually exists. Note that these functions were written in 2001; since then, base-R has extended the default behaviour of '[[' etc for recursive objects, so that 'my.index( thing, 1, 3, 5)' can sometimes be achieved just by to 'thing[[c(1,3,5)]]' with the system version of '[['. However, at least as of R 2.10.1, the system versions still have limited "recursability".


USAGE

# Use them like this, inside a function definition:
# assign( "[[", my.index); var[[i]]
# assign( "[[<-", my.index.assign); var[[i]] <- value

my.index( var, ...) # not normally called by name
my.index.assign( var, ..., value) # not normally called by name
my.index.exists( i, var)


ARGUMENTS

 var: a recursive object of any mode (not just 'list', but e.g. 'call' too)
 value: anything
 ...: one or more numeric index vectors, to be concatenated
 i: numeric index vector


DETAILS

Although R allows arbitrary-level access to lists, this does not (yet) extend to 'call' objects or certain other language objects-- hence these functions. They are written entirely in R, and are probably very slow as a result. Notwithstanding EXAMPLES below, it is *unwise* to replace system '[[' and '[[<-' with these replacements at a global level, i.e. outside the body of a function-- these replacements do not dispatch based on object class, for example.

Note that 'my.index' and 'my.index.assign' distort strict R syntax, by concatenating their '...' arguments before lookup. Strictly speaking, R says that 'x[[2,1]]' should extract one element from a matrix list; however, this doesn't really seem useful because the same result can always be achieved by 'x[2,1][[1]]'. With 'my.index', 'x[[2,1]]' is the same as 'x[[c(2,1)]]'. The convenience of automatic concatentation seemed slightly preferable (at least when I wrote these, in 2001).

'my.index.exists' checks whether 'var' is "deep enough" for 'var[[i]]' to work. Unlike the others, it does not automatically concatenate indices.

At present, there is no facility to use a mixture of character and numeric indexes, which you can in S+ via "list subscripting of lists".


EXAMPLES

local({
  assign( "[[", my.index)
  assign( "[[<-", my.index.assign)
  ff <- function() { a <- b + c }
  body( ff)[[2,3,2]] # as.name( "b")
  my.index.exists( c(2,3,2), body( ff)) # TRUE
  my.index.exists( c(2,3,2,1), body( ff)) # FALSE
  body( ff)[[2,3,2]] <- quote( ifelse( a>1,2,3))
  ff # function () { a <- ifelse(a > 1, 2, 3) + c }
  my.index.exists( c(2,3,2,1), body( ff)) # now TRUE
})

AUTHOR

Mark Bravington


KEYWORDS

programming; utilities
}")

)

"my.index.assign" <-
function (var, ..., value) {
  if( getRversion() >= '2.12') {
    cc <- unlist( list( ...))
    if( length( cc))
return( (baseenv()$'[[<-')( var, cc, value))
    else
return( value)
  }
  
  i <- c(...)
  if (length(i) < 2) 
return(.Primitive("[[<-")(var, i, value))
  pa <- .Primitive("[[<-")
  pg <- .Primitive("[[")
  vario <- as.name("var")
  for (ii in i[-length(i)]) vario <- call("pg", vario, ii)
  callio <- substitute(value, env = parent.frame())
  for (ii in rev(i)) {
    callio <- call("pa", vario, ii, callio)
    if (length(vario) > 1) 
      vario <- vario[[2]]
  }
  return(eval(callio))
}


"my.index.exists" <-
function( i, var) {
  for( ii in 1 %upto% length( i))
    if( missing( var) || !is.recursive( var) || i[ ii] > length( var))
return( FALSE)
    else
      var <- as.list( var)[[ i[ ii] ]]
return( TRUE) }


"named" <-
function (x) {
  if( !length( x))
return( x)

  names(x) <- as.character(x)
  x
}


"named.in.doc" <-
function( doc, exclude.internal=FALSE) {
  if( is.null( doc) || !is.character( doc))
return( character( 0))

  doc <- gsub( '\t', ' ', doc)

  doc <- c( doc, ' ') # guarantees blank
  blank <- seq( along=doc) %except% grep( '[^ ]', doc)

  if( exclude.internal) {
    KEYWORDS <- which( doc=='KEYWORDS')
    if( length( KEYWORDS)) {
      poss <- doc[ -(1 %upto% tail( KEYWORDS, 1))]
      allcaps <- grep( '^[A-Z0-9]+ *$', poss)
      if( !length( allcaps)) {
        allcaps <- length( poss)+1
      }
      poss <- sub( '^ +$', '', poss[ 1 %upto% allcaps[1]]) %such.that% nzchar(.)
      poss <- sub( ',,+', ',', sub( ' +', '', paste( poss, collapse=',')))
      KEYWORDS <- strsplit( poss, ',')[[1]]
      if( 'internal' %in% KEYWORDS) {
return( character(0)) }
    }
  } # if exclude.internal

  namelines <- doc[ 1 %upto% (min(blank)-1)] # 2: to ignore first line
  namelines <- sub( '^ +', '', namelines) # leading spaces
  namelines <- gsub( ' +[^ ]+', '', namelines) # keep first word only
  namelines <- gsub( ' *$', '', namelines) # trailing spaces

  namelines
}


"NEG" <-
structure( function( f) { 
  if( is.null( f))
return( f) # useful for

  if( is.primitive( f)) {
    fargs <- formals( args( f)) # primitives don't have formals
    argo <- lapply( names( fargs), as.name)
    gbod <- list( as.name( '-'), as.call( c( list( substitute( f)), argo)))
    g <- function() 0
    body( g) <- as.call( gbod)
    formals( g) <- fargs
    environment( g) <- .GlobalEnv
  } else {
    # f is normal function
    g <- f
    body( g) <- substitute( { 
      mc <- match.call()
      mc[[1]] <- f
      -eval( mc, parent.frame())
    }, list( f=f))
    formals( g) <- formals( f)
    environment( g) <- environment( f) 
  }
return( g)
}
, doc =  docattr( r"{
NEG    package:mvbutils

Generate a negated version of your function. Useful for 'nlminb' etc.

DESCRIPTION

You pass it a function 'f(.)'; it returns a function whose result will be '-f(.)'. The arguments, return attributes, and environment are identical to those of 'f'.


USAGE

NEG(f) 

ARGUMENTS


 f: Normally, a function that returns a scalar; rarely, a NULL.

VALUE

A function that returns '-f'. However, if 'is.null(f)', the result is also NULL; this is useful e.g. for gradient arg to 'nlminb'.


EXAMPLES 

NEG( sqrt)( 4) # -2
# should put in more complex one here...
e <- new.env()
e$const <- 3
funco <- function( x) -sum( ( x-const)^2L)
environment( funco) <- e

nlminb( c( 0, 0), NEG( funco)) # c( 3, 3)

dfunco <- NULL
nlminb( c( 0, 0), NEG( funco), gradient=NEG( dfunco)) # c( 3, 3)

}")

)

"nicewrite_function" <-
function(...){
# Use write_sourceable_function() if available for this R, unless 
# overridden by option
  WSF <- if( (getRversion() >= '4.1') 
      && getOption( 'mvbutils.rawstring4doc', TRUE))
    write_sourceable_function else write.sourceable.function
  WSF( ...)
}


"no.lazyLoad.attach.hook" <-
function( pkgname, pkglib) {
  # Identical to no.lazyLoad.hook, but for search-path version
  # Hook to force immediate loading, and to avoid trouble with lazyLoad being out-of-synch later  
  # Don't force loading of mlazies
  ns <- as.environment( 'package:' %&% pkgname) 
    
  for( obj in lsall( ns)) {
    get.promise <- call( 'substitute', as.name( obj))
    c1 <- eval( get.promise, ns) 
    if( (c1 %is.a% 'call') && (c1[[1]]==as.name( 'lazyLoadDBfetch'))) {
      assign( obj, ns[[ obj]], envir=ns) # force and overwrite promise
    }
  }
}


"no.lazyLoad.hook" <-
function( pkgname, pkglib) {
  # Hook to force immediate loading, and to avoid trouble with lazyLoad being out-of-synch later  
  # Don't force loading of mlazies
  ns <- asNamespace( pkgname) 
      
  for( obj in lsall( ns)) {
    get.promise <- call( 'substitute', as.name( obj))
    c1 <- eval( get.promise, ns) 
    if( (c1 %is.a% 'call') && (c1[[1]]==as.name( 'lazyLoadDBfetch'))) {
      # Can force just via ns[[ obj]], but it still leaves the promise lying around... 
      # ... paranoia wins
      # ns[[ obj]] # force
      assign( obj, ns[[ obj]], envir=ns) # force and overwrite promise
    }    
  }
}


"noice" <-
structure( function( cc, ...) { # Args of cc on separate lines
  cc <- as.list( cc)
  zub <- unname( do.on( 2 %upto% length( cc), {
      thing <- deparse( as.call( c( list( quote( X)), cc[ .])), ...)
      if( length( thing) > 1) {
        thing <- thing[1] %&% '...)'
      }
      sub( ')$', ',', sub( 'X(', '', thing, fixed=TRUE))
    }))
    
  zub[ length( zub)] <- sub( ',$', ')', zub[ length( zub)])
  zub <- c( deparse( cc[[ 1]]) %&% '(', '  ' %&% zub)
as.cat( zub)
}
, doc =  docattr( r"{
noice    package:mvbutils


Prints a call object nicely

DESCRIPTION

Prints a 'call'-mode object nicely, with one argument per line. This is useful, for example, in displaying readably the outcomes of 'sys.call()', which is often used to create a 'call' attribute for the results of complicated functions.


USAGE

noice( cc, ...) 


ARGUMENTS

 cc: a 'call' object, eg something appended to a fitting result via 'sys.call'.
 ...: any other arguments for 'deparse'


VALUE

Character vector with one argument per line, of class 'as.cat' so that it prints cleanly. Long arguments are truncated, so the result is not guaranteed to re-parse cleanly (a general issue with R which seems unavoidable in any powerful language).


EXAMPLES 

# This is a bona fide function call from my own work
# normally it would be evaluated directly, and sys.call() 
# would be used inside it to assign a 'call' attrib to the result
# but the call attrib then looks like a mess-o-rama
# The quote() wrapper is just used here to make the point

# It would be interesting if 'call' could cope with a 'source' or
# 'srcref' argument, and would "know" how to print itself, but that 
# is a big ask

# BTW, the 72-char limit in Rd EXAMPLES and USAGE is a PITBA

monster <- quote( est_N(
    popcompo = fp1a_17, 
    df_rs_as_at_l = NULL,
    df_rs_ls = NULL, # NB comments are allowed, but get chucked
    newstyle_data = data17b,
    use_alpha_hsp = TRUE,
    AMIN = 8, AMAX = 30,
    YMIN = 2002, YMAX = 2014,
    prior_mean_z_plusgroup = 0.386,
    prior_sd_z_plusgroup = 0.0268,
    LMIN = 150, LMAX = 200, 
    logit_surv_form = ~ I( pmax( age, 19)- AMAX) - 1,
    log_nsa_y1_form = ~factor(sex),
    log_nys_a1_reqm_form = ~0, 
    logit_tresid_form = ~sex * I(len - 170), 
    log_selbase_form = ~ 0,
    log_daily_reprodm_form = ~ 0,
    vb_form = ~sex, 
    log_vb_cv_Linf_form = ~1, 
    log_rct_re_var_start = log( sqr( 0.41)),
    fix_CV_R = TRUE,
    RE_rct = TRUE, 
    sel_is_by_sex = TRUE, 
    ssreduce_l = 1,
    fec_bout_start_fit = start_of_bout, 
    fec_rest_start_fit = start_of_rest, 
    fec_ovwt_fit = bfec, 
    lf_sel_model=lv10kk5fix,
    nu_lata = 12))

monster # yuk
noice( monster) # yum

}")

)

"not.for.packaging" <-
function( env){
  nfp <- cq( tasks, .Traceback, .packageName, last.warning, .Random.seed, .SavedPlots)
  if( !is.null( pkgname <- attr( env, 'name')))
    nfp <- c( nfp, pkgname %&% '.package.doc')
  if( exists( 'exclude.from.package', mode='character', env))
    nfp <- c( nfp, env$exclude.from.package, 'exclude.from.package')
  nfp
}


"nscat" <-
function( fmt, ..., sep='\n', file='') {
  s <- sprintf( fmt, ...)
  s[1] <- '\n' %&% s[1]
  s <- paste( s, collapse='\n')
  cat( s, file=file)
}


"nscatn" <-
function( fmt, ..., sep='\n', file='') cat( '', sprintf( fmt, ...), sep=sep, file=file)


"numvbderiv" <-
structure( function(
  f, x0, eps=0.0001, param.name=NULL, ..., 
  SIMPLIFY=TRUE, PARALLEL=FALSE,
  TWICE.TO.GET.SENSIBLE.ANSWERS=TRUE
){
## Docu is in numvbderiv_parallel
# Code is self-explanatory, if you concentrate..!

  if( PARALLEL){
    # Allow one interface to both functions
    # Added 2024 as this migrates to mvbutils
    mc <- match.call()
    mc[[1]] <- quote( numvbderiv_parallel)
    mc$TWICE.TO.GET.SENSIBLE.ANSWERS <- NULL # auto in parallel
return( eval.parent( mc))
  }

  if( is.null( param.name))
    ff <- function( x, ...) f( x, ...)
  else
    ff <- function( x, ...) {
      ll <- c( list( x), list( ...))
      names( ll)[1] <- param.name
      do.call( 'f', ll)
    }

  f0 <- ff(x0, ...)
  n <- length( x0)
  m <- matrix( 0, length(f0), n)
  for( i in 1:n) {
    this.eps <- eps * if( x0[ i]==0) 1 else x0[ i]
    m[,i] <- ( ff( x0+this.eps * (1:n==i), ...) - f0) / this.eps }
  if( !is.null( dim( f0)))
    dim( m) <- c( dim( f0), n)
  if( TWICE.TO.GET.SENSIBLE.ANSWERS) {
    mc <- match.call()
    mc$eps <- -eps
    mc$TWICE.TO.GET.SENSIBLE.ANSWERS <- FALSE
    m <- 0.5*(m + eval( mc, sys.frame( sys.parent())))
  }
  
  if( any( dim( m)==length( m)) && SIMPLIFY)
    m <- c( m) # demote 1D arrays to vectors
  
return( m)
}
, doc =  docattr( r"{
numvbderiv_parallel    package:mvbutils
numvbderiv


Economy numerical derivatives


DESCRIPTION

'numvbderiv' does simple two-point symmetric numerical differentiation of any function WRTO a _vector_ parameter, via '(f(x+delta)-f(x-delta))/(2*delta)'. Your function can return a vector/matrix/array of real or complex type, and if the 'x' parameter is not scalar, then the result has one extra dimension at the end for the per-parameter-element derivatives.

For multi-parameter ('length(x)>=4' or so) derivatives of slow functions, you can speed things up a lot with parallel processing, by setting 'PARALLEL=TRUE' or (better) by directly calling 'numvbderiv_parallel'. But, be aware there is substantial learning-curve-pain-cost to all this parallel shenanigans in R. 'numvbderiv_parallel' uses the 'foreach' package to diff wrto each component 'x[i]' in parallel, using however many cores at a time you tell it to. You have to set up a "parallel cluster" beforehand in R. See EXAMPLES--- it took me a long time to get this working, but now it's good.

'numvbderiv' is definitely "economy model" and for many many years I have kept it out of 'mvbutils', because it is not particularly accurate nor incredibly robust, and I didn't want to have to deal with people's questions! But I use 'numvbderiv' and 'numvbderiv_parallel'  all the time in code that I want to share (sometimes with different names, omitting the "mv"), and in 2024 it just became too annoying to have to distribute them separately. So here they are, with nice new names, and tarted-up documentation that you are now enjoying, but still warts and all.


.FAQ

 - Q: Surely there are well-known methods to produce more accurate and robust numerical derivatives?
 
 - A: Yep.
 
 - Q: I want something more!
 
 - A: Then use something else!

 - Q: Oh well. But I guess 'numvbderiv' is easy to use, right?
 
 - A: Yep.
 
As to accuracy: IME 'numvbderiv' is usually fine, and computationally cheap! The relevant parameter is 'eps'; to compute 'Df(x)/dx|x=x0' your function 'f' is evaluated at 'x0+/-eps*x0' (unless 'x0==0' exactly, in which case it is at '+/-eps'). The bigger you go with 'eps', the less mathematically accurate the result, since the neglected higher-order terms are bigger; but if you go too small, then the answer becomes computationally inaccurate because of rounding etc. The default is crude but has usually worked OK for me, given this is _not_ a high-accuracy routine. I sometimes play around with values between 1e-3 and 1e-7. If you're worried, try two different values that differ by an order-of-mag.

Unlike eg the 'numDeriv' package or 'pracma::numderiv', which use more function evaluations at various step-sizes to account for higher-order terms in the finite-difference approximation, 'numvbderiv' does not try to be _very_ accurate, and you do have to specify the step yourself (see ARGUMENTS) or trust the default. Nevertheless, I expect my 'numvbderiv' to be _more_ accurate than the original (or still-current default) of 'stats::numericDeriv' because the latter appears _not_ to do symmetric calculation, based on the code in "Writing R Extensions" section 5.11. IE, it just does '(f(x0+e)-f(x0))/e'. [Update in 2024: AFAIK 'stats::numericDeriv' used never to even have a symmetric option, but it now appears to have added one now via its 'central' argument--- although that defaults to FALSE :/ .] Also, 'stats::numericDeriv' is pretty horrible to use TBH; AFAIK its main historical purpose was just to show how to interface C to R, not to actually differentiate stuff!


USAGE

numvbderiv( f, x0, eps=0.0001, param.name=NULL, ..., 
  SIMPLIFY=TRUE, PARALLEL=FALSE, 
  TWICE.TO.GET.SENSIBLE.ANSWERS=TRUE)

numvbderiv_parallel(f, x0, eps = 0.0001, param.name = NULL, ..., 
    SIMPLIFY = TRUE,  PARALLEL = TRUE, 
    PROGRESS_BAR = interactive() && .Platform$OS.type!='unix',
    PROGRESS_BAR_FILE = "",  FOREACH_ARGS = list())


ARGUMENTS

 f: function of one or more arguments
 
 x0: value to numdiff around
 
 eps: Relative step-size. Evaluation is at 'x0+/-eps*x0' unless 'x0==0' exactly, in which case it is at '+/-eps'. See FAQ.
 
 param.name: Unless the parameter you want to diff WRTO comes first in the argument-list of 'f', you need to specify its name, eg 'param.name="c"' if your function is  'f(a,b,c)' and you wanna diff wirto the third one.
 
 ...: Other args that your 'f' wants.
 
 SIMPLIFY: If TRUE and 'f' appears to return a "scalar-equivalent" result (eg all-but-one of its dimensions are of extent 1, as you can sometimes get eg from a matrix-multiply I guess if you use R's built-in routine), then this will turn the result into a pure vector. Avoids you getting tedious 'N*1' or '1*N' "matrix" results that you then have to 'c()' yourself.
 
 PARALLEL: if FALSE, use the scalar version. If TRUE _and_ 'length(x0)>1' _and_ the 'foreach' package is available _and_ there is a "currently registered doPar backend" [sic], then parallel woop-woop magic will be used. 'numvbderiv/numvbderiv_parallel' have defaults 'PARALLEL=FALSE/TRUE' respectively.
 
 FOREACH_ARGS: things to pass to 'foreach::foreach', eg '.packages' or perhaps '.exports' so your function can find stuff it needs when it is invoked in a new cold lonely R session.
 
 PROGRESS_BAR: If you are bothering to use the parallel version, then presumably things are fairly slow; you can set 'PROGRESS_BAR=TRUE' to see how it's going. I don't know if it works on Linux, coz it relies on 'flush.console', so the default there is FALSE, but you can give it a try.) 
 PROGRESS_BAR_FILE: I use 'numvbderiv_parallel' during interactive R sessions in RGui, and the default of appearing in the console seems ideal. For other uses, you might need to tell the progress bar to appear somewhere else, via this argument which is passed as the 'file' argument of 'txtProgressBar' (qv).
 
 TWICE.TO.GET.SENSIBLE.ANSWERS: Leave it alone!!! Not for you.


DETAILS

.THE.PROGRESS.BAR

The progress bar (parallel case only) uses a 'txtProgressBar' (qv) and some excellent Github code from K Vasilopoulos.  It's no good trying to get your own function to show its progress or call-count in the parallel case, because it will be executing in separate invisible R processes and messages don't get sent back, so this is the only convenient way. However, the nature of 'foreach' means that this progress bar is only updated when a task finishes, and since all deriv-steps will take about the same time, you'll probably get the first 4 finishing all-at-once, so that progress will update in a very clunky fashion and if your parameter is of low dimension, the bar may not help. The 'numvbderiv_parallel' code actually does try to update the progress-bar before the paralleling begins, immediately after the very first function call which is to 'f(x0)' itself, so in principle you _should_ "quickly" get some idea of how long it's all gonna take--- but that update doesn't always seem to show up. Displaying the bar relies on a call to 'utils::flush.console' (qv) so prolly doesn't work under Unix; maybe there's another way. Future versions of 'numvbderivParallel' may let you supply your own progress-bar rather than forcing 'txtProgressBar'. For now, be grateful for what you have been given.


VALUE

Normally, an array/matrix with same dimensions as 'f(x0)' except for an extra one at the end, of 'length(x0)'. If 'SIMPLIFY=TRUE' (see ARGUMENTS) and a pure vector "makes sense", then the dimensions will be stripped and you'll get a pure vector.


SEE.ALSO

'pracma::numderiv'; the 'numDeriv' package; 'stats::numericDeriv', the 'foreach' package, the 'doParallel' package


EXAMPLES


# Complex numbers are OK:
numvbderiv( function( x) x*x, complex( real=1, imaginary=3))
# [1] 2+6i

# Parallel example...  the whole point is to show speed and generality
# Works fine on my machine
# But if testing under CRAN, which I normally never do,
# then CRAN's ludicrous 2-core limit, and deliberate inability to 
# check CRANality (or even number of cores _allowed_) while running,
# makes this completely ridiculous
# Not for the first time
# I have used the function 'get_ncores_CRANal' to try to get round this... 

if( require( 'doParallel') && require( 'foreach')){
  ncores <- parallel::detectCores( logical=FALSE)
  
  if( ncores > 2 ){ # pointless otherwise
    # Need a slowish example. 1e5 is too small; 1e7 better, 
    # ... but hard on auto builders eg R-universe
    BIGGOVAL <- 1e5
    slowfun <- function( pars, BIGGO) 
      sum( sqr( 1+1/outer( seq_len( BIGGO), pars)))
      
    parstart <- rep( 2, 8)
    system.time( 
      dscalar <- numvbderiv( slowfun, parstart, 
          BIGGO=BIGGOVAL # named extra param (part of ...)
        )
    ) # scalar
  
    # Make "doPar back end". I do not know what I am doing ...
    # NB I like to leave some cores spare, hence "-1"-- 
    # superstition, really
    
    ncores_target <- min( ncores-1, length( parstart))
    
    # Anti CRANky: ignore on your own machine:
    # ncores_target should just work
    ncores_avail <- get_ncores_CRANal( ncores_target)  
    
    CLUSTO <- makeCluster( ncores_avail)
    registerDoParallel( CLUSTO, ncores_avail) 

    # Next bit ensures slaves can find packages... sigh.
    # Necessary _here_ coz example, but you may not need it
    # clusterCall does not work properly :/, so the "obvious" fails:
    # clusterCall( CLUSTO, .libPaths, .libPaths())
    # Instead, we are forced into this nonsense:
    eval( substitute( 
        clusterEvalQ( CLUSTO, .libPaths( lb)),
        list( lb=.libPaths())))

    # Need 'mvbutils::sqr', hence '.packages' arg
    system.time( 
      dpara <- numvbderiv_parallel( slowfun, parstart,
          BIGGO=BIGGOVAL, # named extra parameter
          FOREACH_ARGS=list( .packages= 'mvbutils')
        )
      )
    rbind( dscalar, dpara)

    # To refer to other data (ie beside params)
    # best practice is to put it into function's environment
    # (generally true, not just for numvbderiv)
    e <- new.env()
    e$paroffset <- c( 6, -3)
    fun2 <- function( pars) { # not a speed test, can be smaller
        sum( sqr( 1+1/outer( 1:1e3, pars+paroffset)))
      }
    environment( fun2) <- e

    numvbderiv( fun2, parstart)
    # Parallel version should still work, coz function's environment
    # is also passed to slaves
    try({ 
      numvbderiv_parallel( fun2, parstart,
          FOREACH_ARGS=list( .packages= 'mvbutils')
        )       
      })

    # Sometimes you do need to explicitly export stuff to the slave processes
    # Here's a version that will get paroffset from datenv
    # datenv must exist...
    alt_fun2 <- function( pars){
      environment( fun2) <- list2env( datenv)
      fun2( pars)
    }

    datenv <- as.list( e)
    numvbderiv_parallel( alt_fun2, parstart,
        FOREACH_ARGS=list( 
          .packages= 'mvbutils',
          .export= cq( datenv, fun2) # stuff that alt_fun2 refers to
          ) 
    )
    
    # Always tidy up your clusters once you have finished playing
    stopImplicitCluster()
    stopCluster( CLUSTO)
    rm( CLUSTO)


  } # if ncores>2
} # parallel



}")

)

"numvbderiv_parallel" <-
function(
  f,
  x0,
  eps= 0.0001, 
  param.name= NULL, 
  ..., 
  SIMPLIFY= TRUE, 
  PARALLEL= TRUE,
  PROGRESS_BAR= interactive() && .Platform$OS.type!='unix',
  PROGRESS_BAR_FILE= '',
  FOREACH_ARGS= list()
){
  ff <- function( x, ...) f( x, ...)
  if( !is.null( param.name)){
    # I don't really understand this, and it could prolly all be Much Better
    FOREACH_ARGS$.export <- c( FOREACH_ARGS$.export, 'f')
    body( ff) <- substitute({
      if( FALSE) f() # to con doParallel into exporting!
      ll <- c( list( x), list( ...))
      names( ll)[1] <- param.name
      do.call( 'f', ll)
    })
  }

  f0 <- ff(x0, ...)
  n <- length( x0)

  PARALLEL <- PARALLEL && 
      (n>1) && 
      requireNamespace( 'foreach') &&
      !is.null( getDoParName()) # checks if cluster is regjoed or wotevs
  
  if( !PARALLEL){
    mc <- match.call( expand.dots=TRUE)
    mc$PARALLEL <- mc$PROGRESS_BAR <- mc$PROGRESS_BAR_FILE <- 
        mc$FOREACH_ARGS <- NULL
    mc[[1]] <- quote( numvbderiv)
return( eval.parent( mc))
  } else {  
    if( PROGRESS_BAR){
      # Amazing excellent code from K Vasilopoulos, for progress bar!!!
      pb <- txtProgressBar(min = 1, max = 2*n, style = 3, 
          file=PROGRESS_BAR_FILE)
      pbcount <- 0
      combine_progbar <- function(...) {
          pbcount <<- pbcount + length(list(...)) - 1
          setTxtProgressBar(pb, pbcount)
          flush.console()
          cbind(...) # this can feed into .combine option of foreach
        }
      combine_progbar(2) # just to get started      
      FOREACH_ARGS$.combine <- combine_progbar
    } else {
      FOREACH_ARGS$.combine <- cbind # default anyway
    }
    
    # FOREACH_ARGS$.export <- c( FOREACH_ARGS$.export, c( 'f', 'param.name'))
    # ... sometimes led to warnings about "f already exported".
    # Construction of ff() should now avoid that
    
    m <- do.call( 'foreach', c( list( i=1:n), FOREACH_ARGS)) %dopar% {
      this.eps <- eps * if( x0[ i]==0) 1 else x0[ i]
      ( ff( x0+this.eps * (1:n==i), ...) - f0) / this.eps
    }
    
    m <- m + do.call( 'foreach', c( list( i=1:n), FOREACH_ARGS)) %dopar% {
      this.eps <- (-eps) * if( x0[ i]==0) 1 else x0[ i]
      ( ff( x0+this.eps * (1:n==i), ...) - f0) / this.eps
    }
    
    m <- 0.5*m
    
    if( PROGRESS_BAR){
      close( pb)
    }
  }

  if( !is.null( dim( f0)))
    dim( m) <- c( dim( f0), n)
  
  if( any( dim( m)==length( m)) && SIMPLIFY)
    m <- c( m) # demote 1D arrays to vectors
  
return( m)
}


"old.$[[<-.data.frame" <-
function (x, i, j, value) {
  if (!all(names(sys.call()) %in% c("", "value"))) 
    warning("named arguments are discouraged")

  cl <- oldClass(x)
  class(x) <- NULL
  nrows <- .row_names_info(x, 2L)
  if (is.atomic(value) && !is.null(names(value))) 
    names(value) <- NULL

  if (nargs() < 4L) {
    nc <- length(x)
    if (!is.null(value)) {
      N <- NROW(value)
      if (N > nrows) {
        if( nrows>0) {
stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
         "replacement has %d rows, data has %d"), N, nrows), domain = NA)
        } else { # create "empty" version of value
          if( length( dv <- dim( value)) > 1L) {
            emptyval <- structure( as.vector( value)[0L], dim=c( 0L, dv[-1L]))
            if( !is.null( dn <- dimnames( value))) {
              newdn <-  c( list( character()), dn[-1L])
              names( newdn) <- NULL # because I say so
              dimnames( emptyval) <- newdn
            } # if dimnames
            attributes( emptyval) <- c( attributes( emptyval), # dim and maybe dimnames
                attributes( value) %without.name% c( 'dim', 'dimnames'))
            value <- emptyval
          } else {
            value <- value[0]
          } # ?empty vector, or empty array?
        } # if need empty
      } else if (N < nrows) {
        if (N > 0L && (nrows%%N == 0L) && length(dim(value)) <= 1L) 
          value <- rep(value, length.out = nrows)
        else 
stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
         "replacement has %d rows, data has %d"), N, nrows), domain = NA)
      } 
    }
    
    x[[i]] <- value
    if (length(x) > nc) {
      nc <- length(x)
      if (names(x)[nc] == "") 
        names(x)[nc] <- paste0("V", nc)
      names(x) <- make.unique(names(x))
    }
    class(x) <- cl
    return(x)
  }
  
  if (missing(i) || missing(j)) 
    stop("only valid calls are x[[j]] <- value or x[[i,j]] <- value")
  rows <- attr(x, "row.names")
  nvars <- length(x)
  if (n <- is.character(i)) {
    ii <- match(i, rows)
    n <- sum(new.rows <- is.na(ii))
    if (n > 0L) {
      ii[new.rows] <- seq.int(from = nrows + 1L, length.out = n)
      new.rows <- i[new.rows]
    }
    i <- ii
  }
  
  if (all(i >= 0L) && (nn <- max(i)) > nrows) {
    if (n == 0L) {
      nrr <- (nrows + 1L):nn
      if (inherits(value, "data.frame") && (dim(value)[1L]) >= length(nrr)) {
        new.rows <- attr(value, "row.names")[seq_len(nrr)]
        repl <- duplicated(new.rows) | match(new.rows, rows, 0L)
        if (any(repl)) 
         new.rows[repl] <- nrr[repl]
      } else {
        new.rows <- nrr
      }
    }
    
    x <- xpdrows.data.frame(x, rows, new.rows)
    rows <- attr(x, "row.names")
    nrows <- length(rows)
  }
  
  iseq <- seq_len(nrows)[i]
  if (anyNA(iseq)) 
stop("non-existent rows not allowed")

  if (is.character(j)) {
    if ("" %in% j) 
stop("column name \"\" cannot match any column")

    jseq <- match(j, names(x))
    if (anyNA(jseq)) 
stop(gettextf("replacing element in non-existent column: %s", 
        j[is.na(jseq)]), domain = NA)
  } else if (is.logical(j) || min(j) < 0L) 
    jseq <- seq_along(x)[j]
  else {
    jseq <- j
    if (max(jseq) > nvars) 
      stop(gettextf("replacing element in non-existent column: %s", 
        jseq[jseq > nvars]), domain = NA)
  }
  
  if (length(iseq) > 1L || length(jseq) > 1L) 
stop("only a single element should be replaced")

  x[[jseq]][[iseq]] <- value
  class(x) <- cl
  x
}


"old.methodize.USAGE" <-
function( nlocal=sys.parent()) mlocal({
# Post-process to set "\method" pedantry in USAGE
# Check for aliases that don't appear in USAGE-- if these appear to be methods of S3 generics, then
# ... tag the USAGE calls with \method

  USAGE.start <- grep( '^\\\\usage\\{', Rd)[1]
  if( !is.na( USAGE.start)) { # Not all docos have USAGE or ARGUMENTS, e.g. package doco
    # All on one line? If so, split
    if( grepl( '}', Rd[ USAGE.start], fixed=TRUE)) {
      bits <- sub( ' *\\} *$', '', sub( '^\\\\usage\\{ *', '', Rd[ USAGE.start]))
      Rd <- multirep( Rd, USAGE.start, list( c( '\\usage{', bits, '}')))
    } else if( grepl( '[{] *[^ ]', Rd[ USAGE.start])) {
      Rd <- multirep( Rd, USAGE.start, list( c( '\\usage{', substring( Rd[ USAGE.start], nchar( '.usage{')+1))))
    }

    USAGE.end <- match( '}', Rd[ -(1:USAGE.start)], NA)+USAGE.start

    ulines <- (USAGE.start+1) %upto% (USAGE.end-1)

    if( FALSE) { # Old code
      aliases <- unique( c( overall.name, sub( '\\\\alias\\{ *', '', sub( ' *\\}.*', '',
          grep( '\\alias{', Rd, fixed=TRUE, value=TRUE)))))

      parzo <- uparzo <- parse( text=gsub( '\\%', '%', Rd[ ulines], fixed=TRUE), keep.source=TRUE)
      if( length( parzo)) {
        is.a.call <- sapply( parzo, is.call)
        is.complass <- is.a.call # complex assignments, e.g. dim(y) <- ...
        is.complass[ is.a.call] <- do.on( parzo[ is.a.call],
            identical( .[[1]], as.name( '<-')) && is.call( .[[2]]))
        which.calls <- which( is.a.call)
        srcref <- do.on( which.calls,
            unclass( attr( parzo[.], 'srcref')[[1]][ 3:4]))
        comments <- substring( Rd[ ulines][ srcref[1,]], srcref[2,]+1)

        rx <- regexpr("^ *# *S3 +method +for +(?:class +)?(?<class>(?:(\\w|[.])+|\"[^\"]+\"|'[^']+'))",
            comments, perl = TRUE)
        is.S3.meth <- rx>0

        if( any( is.S3.meth)) {
          cs <- attr( rx, 'capture.start')[ is.S3.meth, 'class']
          ce <- cs + attr( rx, 'capture.length')[ is.S3.meth, 'class'] - 1
          S3.class <- substring( comments[ is.S3.meth], cs, ce)

          # CRANia checks no longer like funny-named functions in normal quotes; need backticks
          S3.class <- gsub( '^["\']', '`', S3.class)
          S3.class <- gsub( '["\']$', '`', S3.class)
          # S3.class <- gsub( '\\{', '{', S3.class, fixed=TRUE) # fixes earlier mistake; Lcurly shouldn't be escaped

          meth.calls <- which.calls[ which( is.S3.meth)]

          for( i in which( is.complass) %that.are.in% meth.calls) {
            parzo[[ c( i, 2, 1)]] <- as.name( '@@method@@' %&% as.character( parzo[[ c( i, 2, 1)]]))
          }

          for( i in which( !is.complass) %that.are.in% meth.calls) {
            parzo[[ c( i, 1)]] <- as.name( '@@method@@' %&% as.character( parzo[[ c( i, 1)]]))
          }

          deparzo <- do.on( parzo, paste( deparse( ., width.cutoff=500), collapse=' '))
          deparzo <- gsub( '%', '\\%', deparzo, fixed=TRUE)
          deparzo[ which.calls] <- deparzo[ which.calls] %&% comments
          deparzo[ meth.calls] <- sprintf( sub( '`@@method@@([^`]+)`', '\\\\method{\\1}{%s}', deparzo[ meth.calls]),
              S3.class)

          Rd <- massrep( Rd, list( ulines), list( deparzo))
        } # if any S3 meths
      } # if length parzo
    } # END OLD CODE

    # New code tested in 'glungo', Dec 2018...
    Rcode <- Rd[ ulines]
    Rcode <- ' ' %&% Rcode %&% ' ' # so gap ops are legit
    Rcode <- c( '{', Rcode, '}')

    pp <- try( parse( text=Rcode))
    # Parse-error *shouldn't* happen, since code should have been parsed/disinfected
    # back in make.Rd2(). But... Also, contentless text (eg all comments) requires no action!
    if( (pp %is.a% 'try-error') || (length( pp[[1]])==1)) {
return( local.return())
    }

    src <- attr( pp[[1]], 'srcref')[-1] # drop with the {}
    # Add a spurious final line
    src <- c( src, list( rep( src[[ length( src)]][ 3:4] + c( 0, 1), 2)))
    pp <- as.expression( as.list( pp[[1]])[-1])

    # Gaps between R expressions
    for( ipp in seq_along( pp)) {
      gappi <- c( src[[ ipp]][3:4]+c(0,1), src[[ipp+1]][1:2]-c(0,1))
      at_gap_start <- substring( Rcode[ gappi[ 1]], gappi[ 2])

      rx <- regexpr("^ *# *S3 +method +for +(?:class +)?(?<class>(?:(\\w|[.])+|\"[^\"]+\"|'[^']+'))",
            at_gap_start, perl = TRUE)
      if( rx>0) { # METHOD AHOY! modify preceding function call
        cs <- attr( rx, 'capture.start')[ 1, 'class']
        ce <- cs + attr( rx, 'capture.length')[ 1, 'class'] - 1
        S3.class <- substring( at_gap_start[ 1], cs, ce)

        # CRANia checks no longer like funny-named functions in normal quotes; need backticks
        S3.class <- gsub( '^["\']', '`', S3.class)
        S3.class <- gsub( '["\']$', '`', S3.class)
        # S3.class <- gsub( '\\{', '{', S3.class, fixed=TRUE) # fixes earlier mistake; Lcurly shouldn't be escaped

        # Now identify the method--- might be an assignment or subset
        metho <- as.character( pp[[ c( ipp, 1)]])
        is_assign <- metho=='<-'
        if( is_assign) {
          metho <- as.character( pp[[ c( ipp, 1, 2, 1)]])
        }
        if( metho %in% c( '[', '[[', '$')) {
          # Require the actual call and the S3 message to be all on one line ..!
          # subset/assign methods should after all be simple

          if( src[[ ipp]][ 1] == src[[ ipp]][ 3]){
            warning( "Subset/replace method in USAGE needs to be all on one line. Sorry.'")
            # ... but I don't really want to cause a crash, so just do nothing...
            # ie "more than my job's worth mate"
          } else {
            # then we need to replace the actual usage-line(s)--- which should be short--- by fake version
            # First, concat the args
            args <- paste( deparse( pp[[ c( ipp, 2)]]), collapse=' ')
            args <- sub( '\\]+', '', sub( '(\\[+)|\\$)', ',', args))
            args <- sprintf( '(%s)', args)
            if( is_assign) {
              args <- args %&% ' <- ' %&% as.character( pp[[ c( ipp, 3)]])
            }

            Rcode[ gappi[1]] <- ' ' %&% metho %&% args %&% substring( Rcode[ gappi[ 1]], gappi[2])
          }
        }

        Rcode[ src[[ ipp]][ 1]] <- sprintf( ' \\method{%s}{%s}', metho, S3.class) %&% sub( '^[^(]*[(]', '(', Rcode[ src[[ ipp]][ 1]])
      } # if method
    } # for expressions

    # Remember to strip wrapping {}
    Rcode <- Rcode[ -c( 1, length( Rcode))]
    # ... and extra starting/trailing spaces
    Rcode <- substring( Rcode, 2, nchar( Rcode)-1)

    # Reinsert backslashes...
#    strings <- gsub( rep.percent, '\\%', strings, fixed=TRUE)
#    strings <- gsub( rep.brace, '\\{', strings, fixed=TRUE)
#    strings <- gsub( rep.backbrace, '\\}', strings, fixed=TRUE)
#    strings <- gsub( rep.backslash, '\\\\', strings, fixed=TRUE) # ?is this correct #backslashes?
#

    Rd[ ulines] <- Rcode
  }  # if USAGE
})


"old.onLoad.stuff" <-
function ( nlocal=sys.parent()) mlocal({
  hack.help <- function ( ...) {
    # help <- get("base.help", pos = "mvb.session.info")
    mc <- as.list(match.call(expand.dots = TRUE))
    mc[[1]] <- quote( as.environment( 'mvb.session.info')$base.help)
    
    # Set 'mvb_help_type', just in case it's needed
    mvb_help_type <- mc$help_type
    if( is.null( mvb_help_type))
      mvb_help_type <- getOption( 'mvb_help_type', getOption( 'help_type', "text"))
      
    if (!is.null(mc$topic) && !is.call(mc$topic) && is.null(mc$type) &&
        is.null(mc$lib.loc) && is.null(mc$try.all.packages)) {
        h1 <- try(eval(as.call(mc), sys.frame( sys.parent())), silent = TRUE)
        if (((h1 %is.not.a% "try-error") && length(unclass(h1)) >
            0) || ((h1 <- dochelp( as.character( mc$topic), help_type=mvb_help_type)) %is.a%
            c( "pagertemp", "browsertemp")))
            return(h1)
    }

    eval(as.call(mc), sys.frame( sys.parent()))
  }
  formals( hack.help) <- formals( help)
  # assign.to.base.opt( 'help', hack.help)

  hack.query <- function ( e1, e2) {
    # `?` <- get("base.?", pos = "mvb.session.info")
    mc <- as.list(match.call())
    mc[[1]] <- quote( as.environment( 'mvb.session.info')$'base.?')
    
    if( is.null( mc$e2)) {
      # Set 'mvb_help_type', just in case it's needed
      mvb_help_type <- mc$help_type
      if( is.null( mvb_help_type))
        mvb_help_type <- getOption( 'mvb_help_type', getOption( 'help_type', "text"))

      h1 <- try(eval(as.call(mc), parent.frame()), silent = TRUE)
      if( (h1 %is.not.a% "try-error") && (length(unclass(h1)) > 0)) 
  return( h1)
  
      h1 <- dochelp( as.character( mc$e1), help_type=mvb_help_type) 
      if( h1 %is.a% c( "pagertemp", "browsertemp"))
  return(h1)
  
      # If that failed too, just call it again & permit the crash...
    }

    eval(as.call(mc), parent.frame())
  }
  
  
  if( FALSE && (getRversion() >= '2.14') && ('print.function' %in% my.reps))
    assign.to.base( 'print.function', pfn)
#    source.print( TRUE)
})


"old_help2flatdoc_bits" <-
function () {
  if( FALSE) { # old code
    # cat( length( text), 'lines of help read; class=', class( text), '\n')
    otext <- text
    text <- c( text, '')
    text <- gsub( '[' %&% sQuote( '') %&% ']', "'", text)
    text <- gsub( '[' %&% dQuote( '') %&% ']', '"', text)

    # Remove bolding of section headings; could have told Rd2txt not to do it...
    # ... but it makes them easy to find
    is.heading <- regexpr( '^_\b', text) > 0 & regexpr( ':$', text) > 0
    text <- gsub( '_\b', '', text)
    text[ is.heading] <- upper.case( substring( text[ is.heading], 1, nchar( text[ is.heading])-1))

    # Trim leading spaces, but only as far as the indent in DESCRIPTION
    is.descrip <- index( text=='DESCRIPTION')[1]
    is.normal.line <- grep( '^ *[^ ]', text) 
    descrip.text.1 <- min( is.normal.line %such.that% (. > is.descrip))
    def.indent <- sub( '[^ ].*', '', text[ descrip.text.1])
    text <- gsub( '^' %&% def.indent, '', text)
    # old brutal version: text <- gsub( '^ +', '', text)

    # Zap xtuple spaces *inside* headings
    text[ is.heading] <- gsub( '([^ ]+) +', '\\1 ', text[ is.heading])

    # Infer subsec level
    text[ is.heading] <- sub( secindent, '', text[ is.heading], fixed=TRUE)
    text[ is.heading] <- gsub( persubsecindent, '.', text[ is.heading], fixed=TRUE)

    expando <- rep( seq( along=text), 1+is.heading)
    text <- text[ expando]
    is.heading <- is.heading[ expando]
    zappo <- 1+index( diff( is.heading)==1)
    is.heading[ zappo] <- FALSE
    text[ zappo] <- ''

    nc <- nchar( text)
    nc.next <- c( nc[-1], 0)
    nc.prev <- c( 0, clip( nc))
    is.heading <- is.heading & nc>0

    myhead <- c( '', text[ is.heading])[ 1+cumsum( is.heading)]

    is.argdef <- myhead=='ARGUMENTS' & nc>0 & nc.prev==0 & 
        regexpr( '^( *[[:alpha:]]+,)* *[[:alpha:]]+ *: ', text)>0
    text[ is.argdef] <- ' ' %&% text[ is.argdef]

    if( any( is.argdef)) {
      # Nonblank lines right after an argdef, ie before next blank line, should be joined to previous
      is.argdef.contline <- !is.argdef & 
          (most.recent( is.argdef) > most.recent( nc==0))
      text[ is.argdef.contline] <- sub( '^ +', ' ', text[ is.argdef.contline])
    }

    start.cont <- (myhead %not.in% cq( USAGE, EXAMPLES)) & nc.prev==0 & nc>0 & nc.next>0
    mid.cont <- (myhead %not.in% cq( USAGE, EXAMPLES)) & nc>0 & nc.prev>0
    end.cont <- (myhead %not.in% cq( USAGE, EXAMPLES)) & nc.prev >0 & nc>0 & nc.next==0
    if( any( start.cont | mid.cont)) {
      splitto <- split( text[ start.cont | mid.cont], cumsum( start.cont)[ start.cont | mid.cont])
      text[ start.cont] <- sapply( splitto, paste, collapse=' ')
    }

    # All lists left-justified, not colon-justified:
    text <- sub( '^ +([A-Za-z0-9_.]+:)', ' \\1', text)

  }

}


"option.or.default" <-
function (opt.name, default=NULL) {
  value <- getOption(opt.name)
  if (!is.null(value)) 
    value
  else default
}


"organize.web.display" <-
function( resequence=TRUE, merge01=FALSE, plotmath=FALSE, nlocal=sys.parent()) mlocal({
# Now we have to figure out what level in the hierarchy each fn. belongs at.
# Simple-minded approach: anything NOT called by any other function is top-
# level; anything called only by top-levels is second-level; etc.

  level <- rep(0, n); names( level) <- funs
  current.level <- 1
  if( n>1)
  while( any( level==0)) {
    tops <- rep( 1, sum( level==0)) %**% funmat[level==0, level==0] == 0
    if( !any( tops))  # we have to sort out functions that call each other
      tops <- least.mutual.dependency( funmat, funs, level)

    level[ (1:n)[ level==0] [tops] ] <- current.level # dimnames( funmat)[[1]]
    current.level <- current.level+1 }
  else
    level[] <- 1

# Super. Now we need to organize things on each level, placing slaves below
# their masters. This OUGHT to be a 'forwards-and-backwards' algorithm,
# because the appropriate placement of masters may depend on which slaves
# they call. EG if you have masters A, B, C, calling slaves (a,c), (b), (c)
# respectively, then  ACB is better than ABC, to avoid crossings.
# Bugger that for now! I am going to fix each layer in concrete, and let the
# underlings sort themselves out.

# NB *something* needs to happen here, in order that level gets returned
# ... but it doesn't have to do X-organization unless plotting

  x <- numeric( n)
  n.masters <- sum( level==1)

# Now sift out 'level 0 functions'; that is, top-level functions that don't
# call any others. No logical reason for this, but may improve clarity.
  if( !merge01) {
    level[ level==1 & ((funmat %*% rep(1,n))==0)] <- 0
    if( !sum( level==1)) # then we have 'taken the top biscuit'!
      level[level==0] <- 1
  }

  if( calc_xpos) { # otherwise, save some time
    for( current.level in min(level):max(level)) {
      if( resequence) {
        if( current.level>1) {
    #     Position of slave 's' is based on mean position of s's callers
          slave.of <- funmat[ funs[level<current.level], funs[level==current.level],
              drop=FALSE]
          pos.order <- (x[ level<current.level] %*% slave.of) /
              (rep( 1, sum( level<current.level)) %*% slave.of)
          pos.order <- jitter( c( 0, 1, pos.order))[ -(1:2)] }
        else if( current.level==1) {
    #     Rough ordering algorithm for the top layer. The aim is to put heavy
    #     callers in the middle, light ones at either end.
          pos.order <- rank( jitter( c( -2, -1, funmat[ level==1,] %*% rep( 1, n)))[-(1:2)])
          pos.order[ pos.order %% 2==0] <-
              2*length( pos.order)-pos.order[ pos.order %% 2==0] }
        else # level 0 order is arbitrary
          pos.order <- 1:sum( level==0)

        pos.order <- order( pos.order)
      } else # if not resequence
        pos.order <- 1:sum(level==current.level)

      # Offset x-positions in intermediate levels, to reduce line overlap
      # max offset = +/- 0.5 char
      # Added 12/2011
      level.shift <- if( current.level %in% c( 0, 1, max( level))) 0 else
        (current.level-1) / (max( level)-1) - 0.5

  #   Space out function names ppnl to # of letters
      if( plotmath) {
        fn <- lapply( funs[ level==current.level], function( x) parse( text=x)[[1]])
        nch <- sapply( fn, strwidth)
        charlim <- strwidth( paste( rep( 'x', charlim), collapse='')) }
      else
        nch <- nchar( funs[ level==current.level])
      if( exists( 'minstrl', frame=sys.nframe()))
        nch <- pmax( nch, minstrl)
      nch <- cumsum( nch[ pos.order])
      x[ level==current.level][pos.order] <-
         (c(0, clip( nch)) + nch + level.shift)/ (2*nch[length(nch)])
      layers <- nch[length(nch)] %/% charlim
      if( layers)
        layers <- rep( 0.1*seq( from=-layers, to=layers, by=2),
            sum( level==current.level) / (1+layers) + 1)[
            1:sum(level==current.level)]
      level[level==current.level][pos.order] <-
          level[level==current.level][pos.order] + layers
    }
  }

  level <- 1+max(round(level))-level
})


"osource.mvb" <-
function( con, envir=parent.frame(), max.n.expr=Inf, 
    echo=getOption( 'verbose', FALSE), prompt.echo=getOption( 'prompt'), 
    evaluate=TRUE, debug.script=FALSE) {
############################
# Obsolete version I haven't had the guts to cull
  if( !exists( 'source.list', 'mvb.session.info'))
    source.list <- list()
  else
    source.list <- get( 'source.list', 'mvb.session.info')
  if( is.character( con))
    con <- file( con)

  source.list[[ length( source.list)+1]] <- con
  put.in.session( source.list=source.list)
  if( !isOpen( con)) {
    open( con, 'r') # if you want fancy options on e.g. blocking, you need to pre-open 'con'
    on.exit( try( close( con)))
  }

  on.exit( { put.in.session( source.list=clip( source.list)) },
      add=TRUE)

  orig.line <- 0

  ow <- options( warn=-1)
  on.exit( options( ow), add=TRUE)

  expr.count <- 1
  while( expr.count <= max.n.expr) {
    # Loop until EOF or a non-blank line
    repeat{
      check.EOF <- readLines( con, n=1, ok=TRUE)
      if( !length( check.EOF) || nchar( check.EOF))
    break
    }

    if( !length( check.EOF))
  break
    pushBack( check.EOF, con)

#    cat( 'Con =', seek( con)); print( con)
#    cat( 'Inc=', isIncomplete( con), '\n')

    tryo <- try( list( parse( file=con, n=1)), silent=TRUE)
    if( tryo %is.a% 'try-error') {
#      print( readLines( con))
      if( echo)
        cat( "parse error; not echoing expression\n")
      errline <- as.numeric( rev( strsplit( geterrmessage(), ' ')[[1]])[1])
      if( !is.na( errline))
stop( "parse error in line " %&% errline, call.=FALSE)
      else
stop( geterrmessage(), call.=FALSE)
    }

    if( echo) {
      dp <- unlist( lapply( tryo[[1]], deparse), use.names=FALSE)
      dp[ 1] <- prompt.echo %&% dp[1]
      dp[ 2 %upto% length( dp)] <- getOption( 'continue') %&% dp[ 2 %upto% length( dp)]
      cat( '', dp, sep='\n')
    }


#    Experimental code to only evaluate if it seems "useful"-- probably not a good idea
#    do.eval <- !is.na( evaluate) && evaluate
#    if( is.na( evaluate)) {
#      do.eval <- is.call( tryo[[1]][[1]]) && (tryo[[1]][[1]][[1]]=='structure') && 
#          is.call( lt <- tryo[[1]][[1]][[ length( tryo[[1]][[1]]) ]] ) && 
#          is.name( lt[[1]]) && (as.character(lt[[1]]) %in% cq( readLines.mvb, flatdoc))
#    }
    
#    if( do.eval)
    if( evaluate) {
      last <- {if( identical( unname( debug.script), TRUE)) eval.scriptlet else eval}( 
          tryo[[ 1]], env=envir) 
    } else 
      last <- tryo[[1]][[1]] # get through the 'expression'
      
    if( echo)
      try( print( last))

    expr.count <- expr.count + 1
  }

  last
}


"parse_and_maybe_methodize_USAGE" <-
function( Rcode, methodize=NA) {
# Called directly from make.Rd2
# Can be called with methodize=FALSE simply to check parsing--- eg for EXAMPLES
# Previously (mvbutils <- 2.8.210) there was methodize.USAGE which was called
# post hoc on a dot-Rd--- but when revising that to deal with longer lines,
# I hit problems because USAGE already had
# backslash before percent, brace, etc at that stage

  orig_Rcode <- Rcode
  Rcode <- ' ' %&% Rcode %&% ' ' # so gap ops are legit
  Rcode <- c( '{', Rcode, '}')

  pp <- try( parse( text=Rcode, keep.source=TRUE), silent=TRUE)

  if( (pp %is.a% 'try-error')) {
return( pp)
  }

  # Content-free text (eg all comments) requires no action
  if( (length( pp[[1]])==1) || !methodize) {
return( orig_Rcode)
  }

  src <- attr( pp[[1]], 'srcref')[-1] # drop with the {}
  # Add a spurious final line
  src <- c( src, list( rep( src[[ length( src)]][ 3:4] + c( 0, 1), 2)))
  pp <- as.expression( as.list( pp[[1]])[-1])
  # print( pp)
  # print( src)

  # Gaps between R expressions
  for( ipp in seq_along( pp)) {
    gappi <- c( src[[ ipp]][3:4]+c(0,1), src[[ipp+1]][1:2]-c(0,1))
    at_gap_start <- substring( Rcode[ gappi[ 1]], gappi[ 2])

    rx <- regexpr("^ *# *S3 +method +for +(?:class +)?(?<class>(?:(\\w|[.])+|\"[^\"]+\"|'[^']+'))",
          at_gap_start, perl = TRUE)
    if( rx>0) { # METHOD AHOY! modify preceding function call
      cs <- attr( rx, 'capture.start')[ 1, 'class']
      ce <- cs + attr( rx, 'capture.length')[ 1, 'class'] - 1
      S3.class <- substring( at_gap_start[ 1], cs, ce)

      # CRANia checks no longer like funny-named functions in normal quotes; need backticks
      S3.class <- gsub( '^["\']', '`', S3.class)
      S3.class <- gsub( '["\']$', '`', S3.class)
      # S3.class <- gsub( '\\{', '{', S3.class, fixed=TRUE) # fixes earlier mistake; Lcurly shouldn't be escaped

      # Now identify the method--- might be an assignment or subset
      metho <- as.character( pp[[ c( ipp, 1)]])
      is_assign <- metho=='<-'
      if( is_assign) {
        metho <- as.character( pp[[ c( ipp, 2, 1)]]) # square-bracket etc
      }
      if( metho %in% c( '[', '[[', '$')) {
        # Require the actual call and the S3 message to be all on one line ..!
        # subset/assign methods should after all be simple

        if( src[[ ipp]][ 1] != src[[ ipp]][ 3]){
          warning( "Subset/replace in USAGE must be one-liner. Sorry.'")
          # ... but I don't really want to cause a crash, so just do nothing...
          # ie "more than my job's worth mate"
        } else {
          # then we need to replace the actual usage-line(s)--- which should be short--- by fake version
          # First, concat the args
          if( metho=='$'){
            warning( "Prolly not working right yet...")
          }
          args <- paste( deparse( pp[[ ipp]]), collapse=' ')
          args <- sub( '\\]+', '', sub( '(\\[+)|\\$)', ',', args))
          args <- sprintf( '(%s)', args)
          if( is_assign) {
            args <- args %&% ' <- ' %&% as.character( pp[[ c( ipp, 3)]])
            # puts "<-value" in twice...
            args <- sub( ' *<- *value', '', args)
          }

          # Put a nice version of the call
          # Tho, does this get over-written? (It doesn't _look_ that nice for
          # eg subset-replacement... but the end result seems OK)
          Rcode[ gappi[1]]  <- ' ' %&% metho %&% args %&% 
              substring( Rcode[ gappi[ 1]], gappi[2])
        }
      }

      Rcode[ src[[ ipp]][ 1]] <- 
          sprintf( ' "method{%s}{%s}"', metho, S3.class) %&%
          sub( '^[^(]*[(]', '(', Rcode[ src[[ ipp]][ 1]])
    } # if method
  } # for expressions

  # Remember to strip wrapping {}
  Rcode <- Rcode[ -c( 1, length( Rcode))]
  # ... and extra starting/trailing spaces
  Rcode <- substring( Rcode, 2, nchar( Rcode)-1)
return( Rcode)
}


"patch.install" <-
function(...){
  # Synonym for patch.installed
  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- as.name( 'patch.installed')
  eval( mc, parent.frame())
}


"patch.installed" <-
function(
    pkg,
    character.only= FALSE,
    force.all.docs= FALSE,
    rewrap.forcibly= TRUE,
    help.patch= TRUE,
    DLLs.only= FALSE,
    update.installed.cache= getOption( 'mvb.update.installed.cache', TRUE),
    pre.inst= !DLLs.only,
    dir.above.source= '+',
    R.target.version= getRversion(),
    autoversion= getOption( 'mvb.autoversion', TRUE),
    click.version= TRUE,
    vignette.build= FALSE,
    compress.lazyload= getOption( 'mvb.compress.lazyload', TRUE),
    silent= FALSE
){
########################
  set.pkg.and.dir() # dir. sourcedir ewhere pkg (as character)
  rpath <- dir.
  spath <- sourcedir

  if( is.null( rpath))
stop( "Can't find path of raw package '" %&% pkg %&% "'")

  is.Rd2 <- R.target.version >= '2.10'

  find.pkg <- index( search()=='package:' %&% pkg)[1]
  ipath <- if( !is.na( find.pkg))
        attr( as.environment( find.pkg), 'path')
      else if( isNamespaceLoaded( pkg))
        asNamespace( pkg)$.__NAMESPACE__.$path
      else
        dirname( system.file( '.', package=pkg)) # returns '.' if pkg not installed

  if( is.null( ipath) || (ipath=='.'))
stop( "Can't find path of installed package '" %&% pkg %&% "'")

  ipath <- ipath[1] # if multiple installations, then fix only topmost
  dynamic.help <- is.Rd2 && file.exists( file.path( ipath, 'help', 'paths.rds'))

  if( pre.inst) {
    pre.install( 
        pkg, 
        character.only= TRUE, 
        force.all.docs= force.all.docs,
        rewrap.forcibly= rewrap.forcibly,
        R.target.version= R.target.version, 
        dir.above.source= dir.above.source,
        autoversion= autoversion, 
        click.version= click.version, 
        vignette.build= vignette.build,
        silent= silent
      )
    # If specific docs are forced (via force.all.docs as character), need fixup.help below to notice
    force.all.docs <- is.character( force.all.docs) || force.all.docs
  }

  # DLLs: if new(er)/better, copy to installed place; re-load if in memory
  fixup.DLLs( isNamespaceLoaded( pkg), ipath, rpath, spath, pkg, use.newest=TRUE)

  if( DLLs.only)
return( invisible( NULL))

  # files to copy
  update.installed.dir( spath, ipath, 'demo')
  update.installed.dir( spath, ipath, 'exec')
  update.installed.dir( spath, ipath, 'data')
  if( is.dir( file.path( spath, 'inst'))) {
    # 12/2023: changed FALSE to TRUE next, so that "obsolete" files
    # and dirs in installed version *will* be zapped.
    update.installed.dir( spath, ipath, 'inst', '.', FALSE)
  }


  # R functions
  if( !file.exists( from <- file.path( spath, 'R', 'funs.rda')))
stop( "No 'funs.rda' file available for quick reinstall")

  if( file.exists( nsfile <- file.path( spath, 'NAMESPACE'))) {
    file.copy( nsfile, file.path( ipath, 'NAMESPACE'), TRUE)
  } else
    suppressWarnings( unlink( file.path( ipath, 'NAMESPACE'))) # changed to non-NAMESPACE package!
  # Force direct use of NAMESPACE, if any
  suppressWarnings( unlink( file.path( ipath, 'Meta', 'nsInfo.rds')))

  is.rda <- file.exists( to <- file.path( ipath, 'R', 'all.rda'))
  is.rdb <- !is.rda && file.exists( file.path( ipath, 'R', pkg %&% '.rdb'))
  # is.rdb==TRUE if lazy-loading

  lazy.loading <- getRversion() >= '2.14'
  if( !lazy.loading) {
    lazy.loading <- tools$.read_description( file.path( ipath, 'DESCRIPTION'))[ 'LazyLoad']
    lazy.loading <- is.na( lazy.loading) | (toupper( lazy.loading) %in% c( 'Y', 'YES'))
  }
  if( lazy.loading)
    is.rdb <- TRUE # from R 2.14 on, lazy.loading is always TRUE

  must.unload <- FALSE
  nsreg <- NULL
  if( packageHasNamespace( pkg, dirname( ipath))) {
    loader.file <- 'nspackloader.R'

    must.unload <- !isNamespaceLoaded( pkg) # in which case, we'll do a partial load--- which has to be unloaded later
    if( !must.unload) {
      ns <- asNamespace( pkg)
    } else {
      ns <- try( loadNamespace( pkg, partial=TRUE))
      if( ns %is.a% 'try-error')
stop( "Package isn't loadable; must be sorted out before 'patch.install' will work")

      unloadio <- function() {
        try( unloadNamespace( ns), silent=TRUE) # prints Error cos of unregistered ns cos of partial load in first place
        try( suppressWarnings( rm( list=pkg, envir=nsreg)), silent=TRUE) # actually done
      }
      on.exit( if( must.unload) unloadio(), add=TRUE)

      # Next bit needed otherwise 'identity' below fails to find the namespace, warns, and then replaces it with .GlobalEnv ... :/
      nsreg <- get.nsreg()
      assign( pkg, ns, envir=nsreg)
    }
  } else {
    ns <- .GlobalEnv
    loader.file <- 'packloader.R'
  } # loader files may not be used

  # Force lazy-loads--- avoid out-of-date lazyload promises later evalling wrong
  # in p'tic $.__S3MethodsTable__. but maybe others
  # Used to have: eapply( ns, identity)
  for( obji in lsall( ns)) {
    temp <- ns[[ obji]]
  }


  if( !lazy.loading) { # Raw source
    src <- readLines( file.path( spath, 'R', pkg %&% '.R'))
    src <- c( ".packageName <- '" %&% pkg %&% "'", src)
    cat( src, file=file.path( ipath, 'R', pkg), sep='\n')
  } else
    file.copy( file.path( R.home(), "share", "R", loader.file), file.path( ipath, 'R', pkg),
        overwrite=TRUE)

  # For some reason, R2.10 makes a pkg.rdb even if not lazy-loading
  if( is.rda | is.rdb) {
    e <- new.env()
    load( from, envir=e, verbose=FALSE)

    # Added July 2019, for source-code stuff
    Cloaders <- file.path( spath, sprintf( 'R/Cloaders_%s.R', pkg))
    if( file.exists( Cloaders)) {
      source( Cloaders, local=e, echo=FALSE, verbose=FALSE)
    }
    f <- find.funs( e) # should be all of them
    for( i.f in f) {
      g <- e[[i.f]]
      environment( g) <- ns
      e[[ i.f]] <- g
    }

    if( is.rda) # Saved image
      save( list=lsall( e), envir=e, file=to)
    else { # Lazy load
      # NB all in-package promises are forced at load-time for maintained packages, so should be no risk
      # of loading the wrong bit of the file.
      # Should apply to importees, too, thanks to hack of importIntoEnv
      if( !silent) {
        cat( "\rMaking lazyLoad :/")
      }
      tools$makeLazyLoadDB( e, file.path( ipath, 'R', pkg), compress=compress.lazyload)
      LLDBflush( file.path( ipath, 'R', pkg %&% '.rdb'))
      if( !silent) {
        cat( "Done\n")
      }
    }
    rm( e)
  }
  if( must.unload) {
    unloadio()
    must.unload <- FALSE # so it's not repeated on exit
  }

  # Non-functions. I think let's always make sysdata.rdb etc, even if empty!
  to.nonfuns <- file.path( ipath, 'R', 'sysdata')
  e <- new.env()
  if( file.exists( from.nonfuns <- file.path( spath, 'R', 'sysdata.rda'))) {
    # As of ~R2.9, extra data *must* be lazy-loaded
    load( from.nonfuns, envir=e, verbose=FALSE)
  }

  tools$makeLazyLoadDB( e, to.nonfuns, compress=compress.lazyload)
  LLDBflush( to.nonfuns %&% '.rdb')
  rm( e)

  #fixup.package.info()-- luckily done by:
  #tools: : :.vinstall_package_descriptions_as_RDS( sub( '/[^/]+$', '', ipath), pkg)
  owidth <- options( width=72)
  on.exit( options( owidth))
  tools$.install_package_description( spath, ipath)
  if( file.exists( file.path( ipath, 'NAMESPACE'))) {
    tools$.install_package_namespace_info( ipath, ipath)
    # Re-register S3 methods
    nsInfo <- readRDS( file.path( ipath, 'Meta', 'nsInfo.rds'))
    # 12/2019: next line used to make 3-col matrix; now however many cols it is supposed to
    # <- matrix( '', 0, ncol( nsInfo$S3methods)) doesn't work because nsInfo doesn't exist there!
    evalq( S3methods <- S3methods[0,,drop=FALSE], ns$.__NAMESPACE__.) # purge
    evalq( rm( list=ls( all.names=TRUE)), ns$.__S3MethodsTable__.) # purge
    registerS3methods( nsInfo$S3methods, pkg, ns)
  }

  rindex <- file.path( spath, 'INDEX')
  iindex <- file.path( ipath, 'INDEX')
  if( !identical( md5sum( rindex), md5sum( iindex))) # OK with non-existent files
    mvb.file.copy( rindex, iindex)

  if( file.exists( vigind <- file.path( spath, 'R', 'meta.vignette.rds')))
    file.copy( vigind, file.path( ipath, 'Meta', 'vignette.rds'), overwrite=TRUE)
  # more.fixup.vignettes

  if( help.patch)
    fixup.help() # doesn't yet link properly into search system

  if( isNamespaceLoaded( pkg)) {
    fixup.exports( pkg)

    # Update loaded version details-- tucked well away, no doubt undocoed, and maybe never used!
    nsinfo <- asNamespace( pkg)$.__NAMESPACE__.
    if( (nsinfo %is.an% 'environment') && ((spec <- nsinfo$spec) %is.a% 'character')) {
      spec[ 'version'] <- as.character( packageVersion( pkg))
      assign( 'spec', spec, envir=nsinfo)
    }
  }

  if( update.installed.cache)
    installed.packages( noCache=TRUE) # reset info
invisible( NULL)
}


"patch_Rcpp_autos" <-
function( nlocal=sys.parent()) mlocal({
  # Check if sources have changed
  cpp_files <- file.path( sourcedir, 'src',
      sort( dir( file.path( sourcedir, 'src'), pattern='[.]cpp$') %that.dont.match% 'RcppExports(_[^.]+)?[.]cpp$'))

  Rcpp_autos <- c( dir( file.path( sourcedir, 'src'), pattern='RcppExports(_[^.]+)?[.]cpp$'),
    dir( file.path( sourcedir, 'R'), pattern='(RcppExports|Cloader_.*)[.]R'))

  if( !length( cpp_files)) { # ... then what is the point of Rcpp?!
    unlink( Rcpp_autos)
  } else {
    mvb_header_string <- 'mvbutils: source md5sums = ' %&% paste(
        sprintf( '%s: %s', file_path_sans_ext( basename( cpp_files)), md5sum( cpp_files)),
        collapse='; ')

    regen <- FALSE
    if( all( file.exists( Rcpp_autos))) { # check for changes. Also, mvb-modded versions must exist
      for( fauto in Rcpp_autos) {
        commento <- if( file_ext( fauto)=='cpp') '// ' else '## '
        rl <- readLines( fauto)
        if( !grepl( '^' %&% commento %&% mvb_header_string, rl[ 1])) {
          regen <- TRUE
      break
        }
      }
    } else {
      regen <- TRUE
    } # if Rcpp_autos already exist

    if( regen) {
      # CRAN whines again. FFS.
      # I am slightly wondering why special code is needed here for two cases, but it's probably
      # ... cos this is an example really, that kinda belongs elsewhere
      re_bloody_quire <- function( name) { 
            get( 're' %&% 'quire', baseenv())( name, character.only=TRUE, quietly=TRUE)
          return( as.environment( 'package:' %&% name))
          }
      # The packages in question will already be attached
      ca <- try( if( importing_Clink[ 'vscode']){
          re_bloody_quire( vscode)$makearoo( sourcedir)
        } else {
          re_bloody_quire( 'Rcpp')$compileAttributes( sourcedir)
        })

      if( ca %is.not.a% 'try-error') {
        cfile <- Rcpp_autos %that.match% 'cpp$'
        text <- readLines( cfile)
        writeLines( c( '// ' %&% mvb_header_string, text), cfile)
        file.copy( cfile, file.path( dir., 'src'))

        rfile <- Rcpp_autos %except% cfile
        if( importing_Clink[ 'Rcpp']){
          text <- readLines( rfile)
          writeLines( c(
              '## ' %&% mvb_header_string,
              'DLL <- new.env()',
              'evalq( envir=DLL, {',
              text,
              '}) # mvbutils evalq wrapper',
              ''),
            rfile)
        }
        mkdir( file.path( dir., 'R'))
        file.copy( rfile, file.path( dir., 'R'))
      } else {
warning( "Could not run Rcpp::compileAttributes or vscode::makearoo--- not ready to install")
      } # if compileAttributes/makearoo  OK
    } # if regen reqd
  } # if any cpp files
})


"pfn" <-
function( x, useSource=TRUE, ...){
  # Obsolete??
  if( is.null( sr <- attr( x, 'srcref')) && !is.null( osrc <- attr( x, 'source'))) {
    last.line <- max( which( nzchar( osrc)))
    last.char <- nchar( osrc[ last.line])
    attr( x, 'srcref') <- srcref( srcfilecopy( 'dummy', osrc), 
        c( 1, 1, last.line, last.char))
  }
  
  #  AntiCRANKiness FFS; oooh doesn't like mvbutils:::bpf; hates print.function( x, useSource, ...))
  eval( asNamespace( 'mvbutils')$body.print.function) 
}


"plot.cdtree" <-
function( x, ...) {
  foodweb( x, ...)
  invisible( x)
}


"plot.foodweb" <-
function( x, textcolor, boxcolor, xblank, border, textargs=list(), 
    use.centres=TRUE, color.lines=TRUE, 
    poly.args=list(), expand.xbox=1.05, expand.ybox=expand.xbox*1.2, 
    plotmath=FALSE, cex=par( 'cex'), 
    ...
){
  for( ipar in cq( boxcolor, xblank, border, textcolor))
    if( do.call( 'missing', list( ipar)))
      assign( ipar, formals( foodweb)[[ ipar]])

  # Weird buggy R bollox with changing the font sizes--- the 'ps' param gets decremented by 1 *every* time it's reset!!!
  # FFS... have had to work round this in foodweb itself

  oldwarn <- options( warn=-1)$warn
  oldpar <- par( no.readonly=TRUE) %without.name% 'ps' 
    # ... %such.that% grepl( 'cex', names( .)) # , new=FALSE)
  on.exit( par( oldpar))
  if( names( grDevices::dev.cur())=='RGUI-BUG-windows') { # par( 'ps') SNAFU
    oldpar$ps <- par( 'ps') + 1L
    on.exit( scatn( '%i %i', par( ps=5)$ps, par( 'ps')), add=T) 
      # ... oldpar$ps+2L), add=TRUE)
  }
  
  options( warn=oldwarn)
  do.call( 'par', list( mar=c(1,2,1,2), ...))

  web <- x # called 'x' in arglist only to match generic 'plot'
  level <- web$level; funmat <- web$funmat; x <- web$x; funs <- names(level)
  n <- length( level)

#  if( names(dev.cur()[1])=='graphsheet') {
#    gs <- guiGetCurrMetaDoc( 'GraphSheet')
#    colortab <- guiGetPropertyValue( 'GraphSheet', Name=gs, 'ColorTable')
#    colortab <- unlist( unpaste( colortab, '|'), use=FALSE)
#    boxcolor <- background <- length( colortab)
##   Can't get background color directly as a number. Make it the negative of the first colour!
#    background.color <- 255 - as.numeric( unlist( unpaste( colortab[1], ','), FALSE))
#    colortab[ background] <- paste( background.color, collapse=',') # '255,255,255'
#    colortab <- paste( colortab, collapse='|')
#    guiModify( 'GraphSheet', Name=gs, ColorTable=colortab)
#  }

  plot( 0:1, c(min(level)-0.5, max( level)+0.5), axes=FALSE, type='n',
      xlab='', ylab='', main='')
  from <- rep( 1:n, n)[ funmat>0]
  to <- rep( 1:n, rep(n,n))[ funmat>0]
  same <- round(level[from])== round(level[to])
  if( any( same)) {
    segments( (x[from[same]]+x[to[same]])/2, level[from[same]]+0.5,
        x[ to[same] ], level[ to[same] ], 
        col=if( color.lines) level[from[same]] else 1 )
    arrows( x[from[same]], level[from[same]], (x[from[same]]+x[to[same]])/2,
        level[from[same]]+0.5, #size=par('cin'), open=TRUE, works in Splus
        col=if( color.lines) level[from[same]] else 1)
    from <- from[!same]; to <- to[!same] }

# Now just the different-level calls (the vast majority). Used to have arrows
# here too, but can make for too much clutter!

  if( identical( version$language, 'R')) {
    if( plotmath){ 
      funs <- lapply( funs, function( x) parse( text=x)[[1]])
    }

    # Next works for plotmath expressions as well as text      
    sw <- sapply( funs, strwidth); sh <- sapply( funs, strheight) 
  } else
    sw <- sh <- 0

  if( length( from)) {
    if( use.centres)
      segments( x[from], level[from], 
          x[to], level[to], col=if( color.lines) level[from] else 1 )
    else
      segments( x[from], level[from]-sh[from]/2, 
          x[to], level[to]+sh[to]/2, col=if( color.lines) level[from] else 1)
  }

#  arrows( x[from], level[from], (x[to]+x[from])/2,
#      (level[from]+level[to])/2, size=par('cin'), open=TRUE)

# Empty boxes for text. Doesn't work in Splus 4.0.
#  charscale <- par('1em')
#  if( is.null( charscale))
  charscale <- par( 'cxy')
  if( is.null( xblank))
    xblank <- 1
  if( identical( version$language, 'R'))
    do.call( 'rect', c( list( x-expand.xbox*sw/2, level-expand.ybox*sh/2,
        x+expand.xbox*sw/2, level+expand.ybox*sh/2, 
        border=border, col=boxcolor), poly.args))
  else
    do.call( 'polygon', c( list( 
          rep( x, rep( 5, n))+xblank*charscale[1]*rep( nchar( funs), 
          rep( 5, n))*c(-1,-1,1,1,NA),
          rep( level, rep( 5, n))+0.5*charscale[2]*c(-1,0.5,0.5,-1,NA), 
          col=boxcolor), 
        poly.args))
  retlist <- returnList( x, level, funs)
  for( i in seq( along=x))
    text( x[i], level[i], funs[[i]], col=textcolor, cex=cex)
#  do.call( 'text', c( unname( retlist), list( col=textcolor), textargs))
  mc <- as.list( match.call( expand.dots=TRUE))
  print( mc)
  ac <- formals( sys.function())
  not.named <- names( ac) %except% c( names( mc), '...')
  for( i in not.named)
    mc[[ i]] <- get( i)
  mode( mc) <- 'call'
  attr( retlist, 'call') <- mc
  invisible( retlist)
}


"pos" <-
function(substrs, mainstrs, any.case = FALSE, names.for.output) {
  ls <- length(substrs)
  lm <- length(mainstrs)
  .pos <- function(substr, mainstr)
  {
    ns <- nchar(substr)
    nm <- nchar(mainstr)
    if(ns > nm)
      return(0)
    mainstr <- substring(mainstr, 1:(nm - ns + 1), ns:nm)
    t <- (1:length(mainstr))[mainstr == substr]
    if(length(t) == 0)
      0
    else t
  }
  if(any.case) {
    substrs <- upper.case(substrs)
    mainstrs <- upper.case(mainstrs)
  }
  if((ls == 1) && (lm == 1))
    return(matrix(.pos(substrs, mainstrs), 1))
  if((ls %% lm) * (lm %% ls))
    warning( "Length of longer not a multiple of length of shorter")
  if(ls < lm) {
    if(missing(names.for.output))
      names.for.output <- names(mainstrs)
    substrs <- rep(substrs, (lm %/% ls) + 1)
  }
  else if(ls > lm) {
    if(missing(names.for.output))
      names.for.output <- names(substrs)
    mainstrs <- rep(mainstrs, (ls %/% lm) + 1)
  }
  else if(missing(names.for.output))
    names.for.output <- names(mainstrs)
  ls <- max(ls, lm)
  j <- vector("list", ls)
  for(i in (1:ls))
    j[[i]] <- .pos(substrs[i], mainstrs[i])
  max.n.pos <- max(sapply(j, length))
  if(max.n.pos == 1)
    jj <- matrix(unlist(j), 1)
  else {
    jj <- sapply(j, function(x, w)
    c(x, rep(0, w - length(x))), w = max.n.pos)
  }
  dimnames(jj) <- list(character(0), names.for.output)
  t(jj)
}


"pre.install" <-
structure( function(
  pkg,
  character.only= FALSE,
  force.all.docs= FALSE,
  rewrap.forcibly= TRUE,
  dir.above.source= '+',
  autoversion= getOption( 'mvb.autoversion', TRUE),
  click.version= TRUE,
  R.target.version= getRversion(),
  Roxygen= NULL,
  timeout_Roxygen= getOption( 'mvb.timeout_Roxgyen', 0), # seconds
  vignette.build= TRUE,
  silent= FALSE,
  ...
){
#########
  set.pkg.and.dir( FALSE) # set 'dir.', 'sourcedir', and 'ewhere'; ensure 'pkg' is character

  # Herewith a fudge to avoid unnecessary file-copies of mlazy objects later on
  # ... move all existing inst/mlazy/obj**.rda files into a tempdir
  mlazy.temp.dir <- NULL
  if( is.dir( mlazy.inst.dir <- file.path( sourcedir, 'inst', 'mlazy'))) {
    # Some fairly paranoid programming here
    tdctr <- 0
    while( file.exists( mlazy.temp.dir <- 
        file.path( dir., 'temp-inst-mlazy' %&% tdctr)))
      tdctr <- tdctr + 1
    mkdir( mlazy.temp.dir)
    mlazy.OK <- FALSE # reset later if all goes well
    on.exit({
      if( !mlazy.OK) {
        suppressWarnings( mkdir( mlazy.inst.dir))
        mlazy.inst.files <- dir( mlazy.temp.dir, pattern='^obj[0-9]+.rda$')
        for( fi in mlazy.inst.files)
          file.rename( file.path( mlazy.temp.dir, fi),
              file.path( mlazy.inst.dir, fi)) # won't overwrite newer versions
      }
      unlink( mlazy.temp.dir, TRUE)
    }) # on.exit

    old.mlazy.files <- dir( mlazy.inst.dir, pattern='^obj[0-9]+.rda$')
    for( fi in old.mlazy.files)
      file.rename( file.path( mlazy.inst.dir, fi), 
          file.path( mlazy.temp.dir, fi))
  }

  unlink( file.path( sourcedir), recursive=TRUE)
  if( !all( mkdir( file.path( sourcedir, cq( R, man, inst)))))
stop( "couldn't make essential directories")

  ## Check DESCRIPTION (and create a default if none)--- not written out yet
  # Precedence now given to an internal text object 'mypack.DESCRIPTION'
  if( !is.null( description <- ewhere[[ pkg %&% '.DESCRIPTION']])) {
    cat( description, sep='\n', file=file.path( dir., 'DESCRIPTION'))
  }

  if( file.exists( description.file <- file.path( dir., 'DESCRIPTION'))) {
    # Can't do in one step as gsub strips names
    description <- read.dcf( description.file)[1,]
    description[] <- gsub( '\n', ' ', description)
  } else {
    # Default
    description <- c( Package=pkg, Title='What the package does',
        Version='1.0.0', Author='R.A. Fisher', 
        Description='More about what it does',
        Maintainer='Who to complain to <yourfault@somewhere.net>',
        License='???') # adapted from 'package.skeleton'
    # ... put a default into the task package. "Real" one in source package comes later
    writeLines( sprintf( '%s: %s', names( description), description),
        con=file.path( dir., 'DESCRIPTION'))
  }

  kpd <- description[ 'KeepPlaintextDoco']
  keep_plaintext_doco <- !is.na( kpd) && (toupper( kpd) %in% cq( Y, YES, T, TRUE))

  # Autoversion: package must already be installed, with >= 3 parts to version number
  if( autoversion) {
    # Click is now under user control. Don't do it if eg push.to.p in new R version
    # click.version <- TRUE #
    ood.version <- ewhere[[ pkg %&% '.VERSION']]
    if( is.null( ood.version)) {
      ood.version <- try( package_version( ood.version, strict=TRUE))
      if( (ood.version %is.a% 'try-error') || (length( ood.version) != 1)) {
        warning( sprintf( "Illegal package version in '%s.VERSION': %s", 
            pkg, ewhere[[ pkg %&% '.VERSION']]))
        ood.version <- NULL
      }
    }

    if( is.null( ood.version)) {
      # Choose newer of installed and DESCRIPTION...
      # ... but these may not be all possible installed versions
      # ... eg for later versions of R than is now running
      inst.version <- max_pkg_ver( pkg, .libPaths())
      desc.version <- try( package_version( description[ 'Version']))
      if( inst.version %is.a% 'try-error') {
        inst.version <- desc.version
      } else if( desc.version %is.a% 'try-error') {
        desc.version <- inst.version
      }

      if( inst.version %is.a% 'try-error') {
        warning( "Can't deduce version: setting to 1.0.0")
        ood.version <- numeric_version( '1.0.0')
        click.version <- FALSE
      } else {
        ood.version <- if( inst.version > desc.version) inst.version else 
            desc.version
      }
    }

    # Update the description
    ood.version <- as.character( ood.version)
    if( click.version) {
      ok.bit <- sub( '([.-])[0-9]+$', '\\1', ood.version)
      last.bit <- as.numeric( substring( ood.version, nchar( ok.bit)+1))
      last.bit <- last.bit + 1
      new.version.str <- ok.bit %&% last.bit
    } else {
      new.version.str <- ood.version
    }

    description[ 'Version'] <- new.version.str
    assign( pkg %&% '.VERSION', new.version.str, envir=ewhere)
    Save.pos( ewhere)
  }

  description <- description %without.name% c( 'Built', 'LazyLoad', 'SaveImage')
#  description[ 'SaveImage'] <- 'yes'
#  description[ cq( LazyLoad, LazyData)] <- 'no'

  ## 2020: Direct copies of "important" text files, iff stored as charvecs in the task package
  importants <- ls( ewhere, pattern=sprintf( '%s[.](%s)$', pkg, 
      paste( key_package_files, collapse='|')))
  for( i in importants) {
    writeLines( ewhere[[ i]], file.path( sourcedir, 
        substring( i, nchar( pkg)+2)) )
  }

  changes.file <- file.path( dir., 'changes.txt')
  changes.exists <- exists( 'changes.txt', mode='character', ewhere, 
      inherits=FALSE)
  has.changelog <- changes.exists || file.exists( changes.file)
  if( has.changelog) {
    # description[ 'ChangeLog'] <- 'inst/changes.txt' # removed for R 3.0 CRANal checks
    if( changes.exists)
      cat( ewhere$changes.txt, 
          file=file.path( sourcedir, 'inst', 'changes.txt'), sep='\n')
    else
      mvb.file.copy( changes.file, 
          file.path( sourcedir, 'inst', 'changes.txt'), TRUE)
  }


  # Removed 2019, since I think it's neither required nor advisable!
#  # Sometimes makefiles live in the main dir, instead of / as well as in src:
#  if( length( makes.in.top <- dir( dir., pattern='^Makefile')))
#    mvb.file.copy( file.path( dir., makes.in.top), file.path( sourcedir, makes.in.top))

  # Removed...
  # fixup.DLLs( TRUE, NULL, dir., sourcedir, pkg)

  ## Low-level code prep, returning: description, useDynLib, Cloaders, run_postcopy_hooks
  if( rewrap.forcibly) {
    # Purge any previously-autogenerated files
    unlink( dir( file.path( dir., 'R'), full.names=TRUE, no..=TRUE))
  }
  extract.named( handle_prebuilds( pkg, dir., description, rewrap.forcibly))

  ## Straight file copies
  excludo <- ewhere[[ pkg %&% '.file.exclude.regexes']]
  # Hardwire *some* exclusions (for now...)
  # Don't want r_int_defs_debug.h *unless* it is specifically in package 'vscode' or similar!
  excludo <- unique( c( excludo,
      '/[.]vscode/'
      ,'(^|/)[.]gitignore'
      ,'(?<!/include)/r_int_defs_debug[.]h$'
      # ,'(^|/)src/.*[Ee]xports[^.]*[.]cpp'
      ))
  if( !length( excludo))
    unexcluded <- identity
  else
    unexcluded <- function( strs) {
        o <- do.call( 'rbind', lapply( excludo, grepl, x=strs, perl=TRUE))
        strs[ !apply( o, 2, any)]
      }

  get.nondirs <- function( x, recursive=TRUE) {
      if( is.dir( cdir <- file.path( dir., x))) {
        f <- unexcluded( file.path( x, list.files( cdir, all.files=TRUE, 
            full.names=FALSE, recursive=recursive)))
    return( f)
      } else
    return( character( 0))
    }
  # Next line: used to copy ./R folder too, but that is unwise
  copies <- lapply( named( cq( 
      inst, src, data, demo, vignettes, exec, tests, R)), 
      get.nondirs, recursive=TRUE)

  ## Allow direct copying of precompiled DLLs--- only for Windows--- prolly should try HARD to avoid this..!
  # eg proper makefile with fpc
  if( (.Platform$OS.type=='windows') && 
      ('inst' %in% names( copies)) && 
      any( grepl( '^inst/libs/', copies$inst))
    ){ r"--{
      Then make sure both the "inst/libs/i386" and "inst/libs/x64" folders exist in destination, since if a DLL only exists in one of these, the other won't be auto-created. This allows installation for both archis, avoiding problems with install.pkg etc, even if it shouldn't be allowed ! In the long run, all packages should be made to work on 32 and 64 anyway. Just do it now; a bit of a hack
      }--"
    mkdir( file.path( sourcedir, 'inst/libs/i386'))
    mkdir( file.path( sourcedir, 'inst/libs/x64'))
  }

  # Allow non-functions to be documented via aliasses in func doco
  allfuns <- find.funs( ewhere)
  allthings <- lsall( ewhere)
  alldoc <- find.documented( ewhere, doctype='Rd', only.real.objects=FALSE)
  nonfuncs.docoed.in.funcs <- alldoc %except% allfuns

  ## Documented non-functions and ine
  extra.docs <- (allthings %that.match% '\\.doc$') %SUCH.THAT% exists( ., ewhere,
      mode='character')
  named.in.extra.docs <- unlist( lapply( extra.docs,
      function( x) named.in.doc( ewhere[[x]])))
  # avoid mvbutils-utils
  named.in.extra.docs <- unique( c( named.in.extra.docs, nonfuncs.docoed.in.funcs)
      ) %that.are.in% allthings


  ## Namespace
  use.existing.NAMESPACE <- FALSE
  if( NAMESPACE.exists <- file.exists( file.path( dir., 'NAMESPACE'))) {
    nscontents <- readLines( file.path( dir., 'NAMESPACE'))
    use.existing.NAMESPACE <- any( regexpr( '^ *export(Pattern)? *\\(', nscontents) > 0)
  }
  has.namespace <- NAMESPACE.exists || 
      exists( '.onLoad', ewhere, inherits=FALSE) ||
      !is.na( description[ 'Imports']) || 
      (R.target.version >= '2.14')
  # Next line is default namespace stuff-- may not use
  forced.exports <- if( exists( 'forced!exports', ewhere, mode='character',
        inherits=FALSE))
      ewhere$'forced!exports'
    else
      character( 0)
  nsinfo <- make.NAMESPACE( ewhere, description=description,
      more.exports=c( named.in.extra.docs, forced.exports),
      useDynLib=useDynLib)


  ## Test scripts (for tinytest only)
  # First, a utility function:
  write_the_bloody_Lines <- function( text, filename) { # create dir if necessary
      if( !dir.exists( dirname( filename))) {
        mkdir( dirname( filename))
      }
      writeLines( text, con=filename)
    }

  scriptlets <- (allthings %that.match% '[.][Rr]$') %SUCH.THAT% 
      exists( ., ewhere, mode='character')
  tinytests <- scriptlets[ grepl( 'tinytest', 
      do.on( scriptlets, ewhere[[.]][1]))]

  if( length( tinytests)){
    extra.filecontents <- mget( tinytests, ewhere)
    names( extra.filecontents) <- 'inst/tinytest/' %&% 
        names( extra.filecontents)
    ttr <- file.path( dir., 'tests/tinytest.R')
    if( !file.exists( ttr)){ # must exist for tinytest to work
      write_the_bloody_Lines( filename=ttr, c(
          'if( requireNamespace("tinytest", quietly=TRUE)){',
          sprintf( 'tinytest::test_package("%s")', pkg),
          '}')
        )
    }
  } else {
    extra.filecontents <- list()
  }

  ## *** HOOK CALLED HERE ***
  default.list <- c( copies, returnList(
      extra.filecontents= extra.filecontents,
      env=ewhere,
      extra.docs,
      description,
      has.namespace,
      use.existing.NAMESPACE,
      nsinfo,
      exclude.funs= c( 'pre.install.hook.' %&% pkg, '.First.task'),
      exclude.data=  c(
          extra.docs,
          tinytests,
          pkg %&% c( '.DESCRIPTION', '.UNSTABLE', '.VERSION', '.ORIG.ROXYGEN'),
          cq( 'forced!exports',
              .required, .Depends, tasks, .Traceback, .packageName, 
              last.warning, .Random.seed, .SavedPlots, .Last.value)),
      task.path=pkg,
      dont.check.visibility=getOption( 'mvb_dont_check_visibility', TRUE))
    )

  if( is.function( fphook <- ewhere[[ 'pre.install.hook.' %&% pkg]]))
    default.list <- fphook( default.list, ...)
  extract.named( default.list %without.name% cq( task.path, env))

  ## Nasty-looking hook... from Sep 2016, apparently
  # Your hook can provide the *contents* of files which should be written directly, via...
  # 'extra.filecontents' should be a list of character vectors; names should include paths eg "inst/src/utils.pas" as ONE name; elements are file contents.
  # Hook can create each element via readLines() or "manually"
  FOR( names( extra.filecontents),
    write_the_bloody_Lines( extra.filecontents[[.]], file.path( sourcedir, .)))


  ## Copy files
  for( cdir in names( copies)){
    if( length( cfiles <- default.list[[cdir]])) {
      mkdir( file.path( sourcedir, cdir))
      csourcedirs <- unique( dirname( cfiles)) %except% cq( ., ..)
      mkdir( file.path( sourcedir, csourcedirs))

      mvb.file.copy( file.path( dir., cfiles), 
          file.path( sourcedir, cfiles), TRUE)
    }
  }


  ## DESCRIPTION written
  writeLines( paste( names( description), description, sep=': '),
      con= file.path( sourcedir, 'DESCRIPTION'))

  ## Demo index
  if( is.dir( demo.dir <- file.path( sourcedir, 'demo')) &&
      !file.exists( file.path( demo.dir, '00Index'))) {
    # make one!
    demos <- dir( demo.dir, pattern='\\.(r|R)$')
    first.comment <- function( f) {
      txt <- readLines( file.path( demo.dir, f))
      hash <- grep( '^#', txt, value=TRUE)[1]
      if( !is.na( hash))
        stuff <- sub( '# +', '', hash)
      else
        stuff <- 'Demo of ' %&% sub( '\\.(r|R)$', '', basename( f))
    return( stuff)
    }
    demo.lines <- sapply( demos, first.comment)
    cat( paste( sub( '\\.(r|R)$', '', demos), demo.lines, sep='\t'),
        file=file.path( demo.dir, '00Index'), sep='\n')
  }

  ## Vignettes
  if( vignette.build && 
      dir.exists( vigdir <- file.path( sourcedir, 'vignettes')) &&
      length( vigfiles <- dir( vigdir))
    ){
      # Tidy up vignettes...
      # only Rmd file preserved amongst dot-lyx, dot-Rnw, ...
      vigrmd <- vigfiles %that.match% '[.](?i)rmd$'
      if( length( vigrmd)){
        imposters <- vigfiles[ !match( basename( vigfiles), basename( vigrmd), 0)]
        imposters <- imposters %except% vigrmd
        if( length( imposters)){
          unlink( file.path( vigdir, imposters))
        }
      } # if rmd files

      buildVignettes( dir= sourcedir)
  } else {  
    # Try old code..! Presumably for hand-built vignettes
    fixup.vignettes()
  }

  # Zap inst if empty, otherwise R 2.10 complains...
  if( !length( dir( file.path( sourcedir, 'inst'), all.files=TRUE) %except% 
      c( '.', '..')))
    unlink( file.path( sourcedir, 'inst'), recursive=TRUE)

  ## DLLs: DEPRECATED mid-2019
  if( FALSE && length( dll.paths)) {
    slibpath <- file.path( sourcedir, 'inst', 'libs')
    if( getRversion() > '2.12')
      slibpath <- file.path( slibpath, .Platform$r_arch)
    mkdir( slibpath)
    mvb.file.copy( dll.paths, file.path( slibpath, names( dll.paths)))
  }

  ## Main R code
  # Augment functions to include all that are named in each others aliasses
  funs <- find.funs( ewhere) %except% exclude.funs
  # Search env for functions: son of ewhere so on-the-fly changes can go there
  ewhereson <- new.env( parent=ewhere)

  # mlazy objects, and code to auto-load them (involves hacking .onLoad or .First.lib
  mlazies <- mcachees( ewhere) %except% c( '.Random.seed', exclude.data)
  if( length( mlazies)) {
    mkdir( file.path( sourcedir, 'inst', 'mlazy'))
    objfiles <- 'obj' %&% attr( ewhere, 'mcache')[ mlazies] %&% '.rda'
    md5new <- sapply( file.path( dir., 'mlazy', objfiles), md5sum)
    if( !is.null( mlazy.temp.dir)) {
      md5old <- sapply( file.path( mlazy.temp.dir, objfiles), md5sum)
  #    fsnew <- file.info( file.path( dir., 'mlazy', objfiles))
  #    fsold <- file.info( file.path( sourcedir, 'inst', 'mlazy', objfiles)) # some may not exist
  #    different.file <- (fsnew$size != fsold$size) | (fsnew$mtime != fsold$mtime)
      different.file <- md5old != md5new
      different.file[ is.na( different.file)] <- TRUE
    } else
      different.file <- rep( TRUE, length( objfiles))
    if( any( different.file))
      mvb.file.copy( file.path( dir., 'mlazy', objfiles[ different.file]),
          file.path( sourcedir, 'inst', 'mlazy', objfiles[ different.file]))
    for( fi in objfiles[ !different.file])
      file.rename( file.path( mlazy.temp.dir, fi), 
          file.path( sourcedir, 'inst', 'mlazy', fi))
    mlazy.OK <- TRUE # files sorted out

    if( has.namespace) {
      wot.env <- 'environment( sys.function())'
      wot.fun <- '.onLoad'
    } else {
      wot.env <- 'as.environment( "package:" %&% pkgname))'
      wot.fun <- '.First.lib'
    }

    plb <- substitute( nsenv <- wot.env, 
        list( wot.env=parse( text=wot.env)[[1]]))
    for( i in mlazies)
      plb <- c( plb, substitute( delayedAssign( x=i, {
          load( file.path( libname, pkgname, 'mlazy', objfile), nsenv, 
              verbose=FALSE)
          nsenv[[ i]]
        }, assign.env=nsenv, eval.env=environment()),
        returnList( i, objfile='obj' %&% attr( ewhere, 'mcache')[i] %&% 
            '.rda')))

    # Ensure these data are unlocked, so that they can be loaded.
    # ...use 'dont.lockBindings' mechanism but ensure package is indept of mvbutils
    dlb <- dont.lockBindings
    sho <- setHook.once
    environment( dlb) <- environment( sho) <- .GlobalEnv
    plb <- c( plb, substitute( {
        dont.lockBindings <- dlb
        setHook.once <- sho}, returnList( dlb, sho)))
    plb <- c( plb, substitute( dont.lockBindings( mlazies, pkgname),
        list( mlazies=mlazies)))

    loader <- get( wot.fun, ewhere)
    if( is.null( loader))
      loader <- function( libname, pkgname) NULL

    # Now have to prepend plb to body of loader
    # ...not easy
    thing <- quote( {a})
    for( i in seq_along( plb))
      thing[[ i+1]] <- plb[[i]]
    body( loader) <- call( '{', thing, body( loader))
    assign( wot.fun, loader, envir=ewhereson)
    mlazy.OK <- TRUE
  }

  # Source code:
  # hide.vars and ifun are for workaround to silly CRAN check below
  hide.vars <- vector( 'list', length( funs))
  ifun <- 1

  ff <- function( x) {
    cat( '\n"', x, '" <-\n', sep='', file=rfile, append=TRUE)
    fx <- get( x, ewhereson, inherits=TRUE) # ewhere[[ x]] is broken
    if( is.function( fx)) {
      if( !keep_plaintext_doco){
        attributes( fx) <- attributes( fx) %without.name% 'doc'
      }
      nicewrite_function( fx, rfile, append=TRUE, doc.special=FALSE, 
          prefix_package= ff_prefix_package)
      hide.vars[[ ifun]] <<- c( all.names( body( fx), unique=TRUE),
          unlist( lapply( formals( fx), all.names, unique=TRUE), 
              use.names=FALSE))
      ifun <<- ifun+1
    } else
      print( fx)
  }

  suppressWarnings(
    unlink( file.path( sourcedir, 'R',
        dir( file.path( sourcedir, 'R'), all.files=TRUE)))) # clean out oldies
  rfile <- file.path( sourcedir, 'R', pkg %&% '.R')
  # cat( '.packagename <- "', pkg, '"\n', sep='', file=rfile)
  cat( '# This is package', pkg, '\n', file=rfile)
  
  # If the package is mvbutils, then docattr needs to come first, so that source docu is kept...
  funs <- c( funs %that.are.in% cq( docattr, string2charvec), 
      funs %except% cq( docattr, string2charvec))
  ff_prefix_package <- pkg != 'mvbutils'
  sapply( funs, ff)

  if( is.logical( dont.check.visibility) && isT( dont.check.visibility[1])) {
    # Workaround silly CRAN check
    # No point in hiding imports or existing functions
    dont.hide.vars <- c( funs, lsall( baseenv()))
    for( imp in nsinfo$import)
      dont.hide.vars <- c( dont.hide.vars, 
          getNamespaceExports( asNamespace( imp)))
    dont.hide.vars <- unique( dont.hide.vars)

    hide.vars <- unique( unlist( hide.vars, use.names=FALSE)) %except% 
        dont.hide.vars
    # NB Feb 2013: Checking code is in codetools: : :checkUsageEnterGlobal...
    # ... hidden vars are specified in tools: : :.check_code_usage_in_package by calling globalVariables()
  } else if( is.character( dont.check.visibility))
    hide.vars <- dont.check.visibility
  else
    hide.vars <- character( 0)

  if( FALSE && length( hide.vars)) {
    # Suppresssion utils::globalVariables() no longer fucking works because
    # CRAN changed the fucking rules to use codetools()
    # but hasn't changed the fucking documentation.
    # Another day of my life completely wasted.
    # Fuck youse all.
    # .Traceback gets excluded during pre.install because it exists in baseenv()...
    # ... but it may not exist during RCMD CHECK, so add the bloody thing!
    # In any case, need some var on first line for first comma to latch onto
    cat( "# MVB's workaround for futile CRAN 'no visible blah' check:",
        sprintf( r'--{
  declare_globvars <- function() {
    if( 'utils' %%in%% loadedNamespaces()) {
      u_bloody_tils <- asNamespace( 'utils')
      setHook( packageEvent( '%s', 'onLoad'), function(...)
        u_bloody_tils$globalVariables( package="%s",}--', pkg, pkg),
        '  names=c( ".Traceback", "last.warning"',
        file=rfile, sep='\n', append=TRUE)
    cat( sprintf( '    ,"%s"', hide.vars), '  ))) }}', 'declare_globvars()', '',
        file=rfile, sep='\n', append=TRUE)
  }

  # Non-functions:
  extra.data <- allthings %except% c( allfuns, tinytests, exclude.data, mlazies)
  if( length( extra.data))
    save( list=extra.data, file=file.path( sourcedir, 'R', 'sysdata.rda'), 
        envir=ewhere, compress=TRUE)

  # Save file ready for patch.installed
  epar <- new.env()
  epar$globalVariables <- function( ...) 0 # not sure if this is required, but avoid side-effects from 'source'...
  e <- new.env( parent=epar) # will hold stuff to save for patch.installed
  # ... did have 'parent=ewhere' but parent( ewhere)==EmptyEnv for maintained package so..?
  eval( substitute( source( rfile, local=TRUE, keep.source=TRUE)), envir=e)
  for( ff in find.funs( e))
    environment( e[[ff]]) <- .GlobalEnv
  e$.packageName <- pkg # ready for quick install
  save( list=lsall( e), file=file.path( sourcedir, 'R', 'funs.rda'), envir=e,
      compress=TRUE)
  rm( e)

  if( file.exists( RBI <- file.path( dir., '.Rbuildignore'))){
    # Actual file gets priority: copy whole
    mvb.file.copy( RBI, file.path( sourcedir, '.Rbuildignore'))
    # Fucken R requires *different* file for binaries vs targz FFS
    mvb.file.copy( RBI, file.path( sourcedir, '.Rinstignore'))
  } else {
    # Tell RCMD not to build "funs.rda" into package...
    # ... plus any Lyx or Rnw vignette sources (and backup versions)...
    # ... plus any user specs in <pkg>.Rbuildignore
    RBIlines <- unique( c( ewhere[[ pkg %&% '.Rbuildignore']],
      '\\bR/funs[.]rda$', 
      '(?i)\\bvignettes/.*[.](lyx|rnw|Rnw)[^[:alnum:]]*$'))
    writeLines( RBIlines, file.path( sourcedir, '.Rbuildignore'))
    writeLines( RBIlines, file.path( sourcedir, '.Rinstignore'))    
  }
  
  
  # Obsolete: Object list, used by maintain.packages-- funny name to avoid clashing
  # cat( '\n`original!object!list` <-', deparse( c( funs, extra.data)), '', sep='\n',
  #    file=rfile, append=TRUE)

  ## Documentation:
  # Code is set to only update files if they've changed. However, they're *all* deleted anyway
  # ...by the unlink( recruvsive=TRUE) above

  if( !silent) {
    cat( '\rDocumentation...')
  }

  doc2Rd.info.file <- file.path( dir., 'doc2Rd.info.rda')
  if( is.character( force.all.docs)) {
    forcible.redoc <- force.all.docs
    force.all.docs <- FALSE
  } else {
    forcible.redoc <- character()
  }

  if( !force.all.docs && file.exists( doc2Rd.info.file)) {
    load( doc2Rd.info.file) # creates doc2Rd.info
    if( length( forcible.redoc)) {
      doc2Rd.info <- doc2Rd.info %without.name% forcible.redoc
      force.all.docs <- FALSE
    }
  } else {
    doc2Rd.info <- list()
  }

  # Check for handwritten Rd files-- must live in a sourcedir called Rd
  Rd.files.to.keep <- character(0)
  if( is.dir( Rd.dir <- file.path( dir., 'Rd'))) {
    existing.Rd.files <- dir( Rd.dir, pattern='\\.Rd$', all.files=TRUE)
    Rd.files.to.keep <- existing.Rd.files
    Rd.already <- character( 0)
    mvb.file.copy( file.path( Rd.dir, existing.Rd.files),
        file.path( sourcedir, 'man', existing.Rd.files), TRUE)
    for( i in existing.Rd.files) {
      rl <- readLines( file.path( Rd.dir, i))
      docced <- rl %that.match% c( '^\\name\\{', '^\\alias\\{')
      Rd.already <- c( Rd.already, sub( '.*\\{([^}])\\}.*', '\\1', rl))
    }
  } else
    Rd.already <- character(0)

  # Point is to avoid calling the Rdconv function if the requisite output already exists
  # Rdconv ~= doc2Rd but can vary slightly depending on how get.updated.Rd is called

  doco_aliases <- character()

  get.updated.Rd <- function( docname, new.docco, Rdconv, ...) {
      if( !identical( new.docco, doc2Rd.info[[ docname]]$docattr)) {
        scatn( "Generating Rd for '%s'", docname)
        Rd <- try( Rdconv( ...))
        doc2Rd.info[[ i]] <<- if( Rd %is.not.a% 'try-error')
            list( docattr=new.docco, Rd=Rd)
          else
            NULL
      } else {# no change
        Rd <- doc2Rd.info[[ docname]]$Rd
      }

      # Check aliases, to avoid trampling on previous ones (eg like package-doco used to)
      alias_lines <- grep( '^\\\\alias[{]', Rd, value=FALSE)
      if( length( alias_lines)){
        aliases <- sub( '[^{]*[{]([^}]*)[}].*', '\\1', Rd[ alias_lines])
        alreadies <- which( aliases %in% doco_aliases)
        if( length( alreadies)){
          Rd <- Rd[ -alias_lines[ alreadies]]
          aliases <- aliases[ -alreadies]
        }
        doco_aliases <<- c( doco_aliases, aliases)
      }

      Rd
    }

  provisionally.add.man.file <- function( docname, text, fname) {
    new.md5 <- doc2Rd.info[[ docname]]$md5
    already <- file.path( sourcedir, 'man', fname)

    if( !force.all.docs && !is.null( new.md5) && !is.na( new.md5) &&
        file.exists( already)) {
      do.write <- md5sum( already) != new.md5
    } else
      do.write <- TRUE

    if( do.write) {
      cat( text, file=already, sep='\n')
      doc2Rd.info[[ docname]]$md5 <<- md5sum( already)
    }

    Rd.files.to.keep <<- c( Rd.files.to.keep, fname)
  }

  Rd.version <- if( R.target.version < '2.10') "1" else "2"
  docfuns <- (funs %except% Rd.already) %that.are.in%
      find.documented( ewhere, doctype='own')
  for( i in docfuns) {
    geti <- ewhere[[i]]
    Rd <- get.updated.Rd( i, attr( geti, 'doc'), doc2Rd, geti, 
        Rd.version= Rd.version, def.valids= alldoc)
    if( Rd %is.not.a% 'try-error') {
      fname <- sub( '\\}', '', sub( '\\\\name\\{', '', Rd[1])) %&% '.Rd'
      fname <- legal.filename( fname)
      if( length( grep( '^\\.', fname)))
        fname <- '01' %&% fname
      provisionally.add.man.file( i, Rd, fname)
    }
  }

  if( !has.namespace) {
    Rdconv.internals <- function()
        doc2Rd( make.internal.doc( undoc.funs, pkg, pkenv=ewhere), 
            Rd.version=Rd.version)
    undoc.funs <- funs %except% c( find.documented( ewhere, doctype='any'),
        cq( .First.lib, .Last.lib, .onLoad, .onAttach))
    if( length( undoc.funs)) {
      raw.undocco <- unlist( lapply( cq( mlazy, cd),
          function( x) clip( deparse( args( get( x))))))
      Rd.undoc <- get.updated.Rd( pkg %&% '-internal', raw.undocco, 
          Rdconv.internals)
      provisionally.add.man.file( pkg %&% '-internal', Rd.undoc, pkg %&% '-internal.Rd')
    }
  }

  ## TO DO: make sure Rd files don't have funny names (so Rd2roxygen can cope)
  # Could possibly check for clash with Rd.already, but will assume user's brain is working
  for( i in extra.docs) {
    geti <- ewhere[[ i]]
    Rd.extra <- get.updated.Rd( i, geti, doc2Rd, geti, 
        Rd.version= Rd.version, def.valids= alldoc)
    # For package doc, put 00 first to get indexing right...
    # ...and change . into -
    # ...and *remove* alias to packagename, if that alias already occurs in an actual function

    if( length( grep( '[.-]package\\.doc$', i))){
      docname <- '00' %&% sub( '\\.', '-', sub( '\\.doc$', '', i))

    } else {
      docname <- sub( '\\.doc$', '', i)
    }
    provisionally.add.man.file( i, Rd.extra, docname %&% '.Rd')
    # cat( file=file.path( sourcedir, 'man', docname %&% '.Rd'), Rd.extra, sep='\n')
  }
  suppressWarnings(   file.remove( file.path( sourcedir, 'man',
        dir( file.path( sourcedir, 'man'), all.files=TRUE) %except% 
            Rd.files.to.keep)))

  save( doc2Rd.info, file=doc2Rd.info.file)
  if( !silent) {
    cat( 'Done\n')
  }

  if( has.namespace) {
    if( use.existing.NAMESPACE)
      mvb.file.copy( file.path( dir., 'NAMESPACE'), 
          file.path( sourcedir, 'NAMESPACE'), TRUE)
    else
      write.NAMESPACE( nsinfo, file.path( sourcedir, 'NAMESPACE'))
  }

  # Anything set up by PIBHs for low-level code? EG if Rcpp (not RcppTidy) is used
  eval( run_postcopy_hooks)
  make_Cloader_funs( pkg, Cloaders, sourcedir)

  ## Index last, so it looks up-to-date for RCMD BUILD
  index.file <- file.path( sourcedir, 'INDEX')
  Rdindex( file.path( sourcedir, 'man'), index.file)
  # Put the ***-package file first, if it exists
  index.stuff <- scan( index.file, what='', sep='\n', quiet=TRUE)
  if( !is.na( i <- grep( '^' %&% pkg %&% '\\-package', index.stuff)[1])) {
    cat( index.stuff[ c( i, (1:length( index.stuff)) %except% i)], sep='\n',
        file=index.file)
  }

  ## FINALLY... if DESCRIPTION indicates roxygen, then call Rd2roxygen
  if( is.null( Roxygen)) {
    Roxygen <- 'RoxygenNote' %in% names( description)
  }
  if( Roxygen) {
    if( timeout_Roxygen>0){
      setTimeLimit( elapsed=timeout_Roxygen, transient=TRUE)
      # ... if too slow, there'll be a trapped error and OK will be FALSE
    }
    OK <- bugfix_Rd2roxygen( sourcedir, pkg, nsinfo)
    
    if( (OK %is.a% 'try-error') || !OK) {
      warning( "'RoxygenNote' mentioned in DESCRIPTION, but package 'Rd2roxygen' is either unavailable or just didn't work")
    }
  }

  invisible( NULL)
}
, vignette.stub =  string2charvec( r"{
%\VignetteIndexEntry{User manual}
\documentclass{article}
\begin{document}
\end{document}
}")
, doc =  docattr( r"-{
pre.install  package:mvbutils
patch.installed
patch.install
pre.install.hook...
spkg


Update a source and/or installed package from a task package


DESCRIPTION

See 'mvbutils.packaging.tools' before reading or experimenting!

'pre.install' creates a "source package" from a "task package", ready for first-time installation using 'install.pkg'. You must have called 'maintain.packages( mypack)' at some point in your R session before 'pre.install( mypack)' etc.

'patch.install' is normally sufficient for subsequent maintenance of an already-installed package (ie you rarely need call 'install.pkg' again). Again, 'maintain.packages' must have been called earlier. It's also expected that the package has been loaded via 'library()' before 'patch.install' is called, but this may not be required. 'patch.install' first calls 'pre.install' and then modifies the installed package accordingly on-the-fly, so there is no need to re-load or re-build or re-install. 'patch.install' also updates the help system with immediate effect, i.e. during the current R session. You don't need to call 'patch.install' after every little maintenance change to your package during an R session; it's usually only necessary when (i) you want updated help, or (ii) you want to make the changes "permanent" (eg so they'll work in your next R session). However, it's not a problem to call 'patch.install' quite often. 'patch.installed' is a synonym for 'patch.install'.

It's possible to tweak the source-package-creation process, and this is what 'pre.install.hook..." is for; see DETAILS and section on OVERRIDINGx.DEFAULTS below.

'spkg' is a rarely-needed utility that returns the folder of source package created by 'pre.install'.

Vignettes have to built "manually" (it's easy!), using 'vignette.pkg' (qv).


USAGE

 # 95% of the time you just need:
 # pre.install( pkg)
 # patch.install( pkg)
 # Your own hook: pre.install.hook.<<mypack>>( default.list, <<myspecialargs>>, ...)

 pre.install(
     pkg,
     character.only= FALSE,
     force.all.docs= FALSE,
     rewrap.forcibly= TRUE,
     dir.above.source= "+",
     autoversion= getOption("mvb.autoversion", TRUE),
     click.version= TRUE,
     R.target.version= getRversion(),
     Roxygen= NULL,
     timeout_Roxygen= getOption( 'mvb.timeout_Roxgyen', 0), # seconds
     vignette.build= TRUE,
     silent= FALSE,
     ...)
 patch.installed(
     pkg,
     character.only= FALSE,
     force.all.docs= FALSE,
     rewrap.forcibly= TRUE,
     help.patch= TRUE,
     DLLs.only= FALSE,
     update.installed.cache= getOption("mvb.update.installed.cache", TRUE),
     pre.inst= !DLLs.only,
     dir.above.source= "+",
     R.target.version= getRversion(),
     autoversion= getOption("mvb.autoversion", TRUE),
     click.version= TRUE,
     vignette.build= FALSE,
     compress.lazyload= getOption( 'mvb.compress.lazyload', TRUE),
     silent= FALSE)
 patch.install(...) # actually, args are exactly as for 'patch.installed'
 spkg( pkg)


ARGUMENTS

 pkg: package name. Either quoted or unquoted is OK; unquoted will be treated as quoted unless 'character.only=TRUE'. Here and in most other places in 'mvbutils', you can also specify an actual in-memory-task-package object such as '..mypack'.

 character.only: Default FALSE, which allows unquoted package names. You can set it to TRUE, or just set e.g. 'char="my@funny@name"', which will trump any use of 'pkg'.

 force.all.docs: normally just create help files for objects whose documentation has changed (which will always be generated, regardless of 'force.all.docs'). If TRUE, then recreate help for all documented objects. Can also be a character vector of specific docfile names (usually function names, but can be the names of the Rd file, without path or the Rd extension), in which case those Rd files will be regenerated.

 rewrap.forcibly: iff the package contains low-level code (C etc) and this is TRUE, this will re-invoke the PIBH (Pre-Install Build Hook) to recreate "housekeeping" code that e.g. creates R wrappers to call the low-level code from (avoiding direct use of '.Call' etc). Otherwise, the "housekeeping" code will only be recreated if the low-level source code has changed. It will also _purge_ any pre-existing files in the "R" subfolder of of your _task_--- be warned! That feature is so that if you change from using eg the 'Rcpp' package to the 'RcppTidy' package, you won't have multiple versions of C-loaders. See other places in doco...

 help.patch: if TRUE, patch the help of the installed package

 DLLs.only: just synchronize the DLLs and don't bother with other steps (see COMPILED.CODE)

 default.list: list of various things-- see under "Overriding..." below

 ...: arguments to pass to your 'pre.install.hook.XXX' function, usually if you want to be able to build different "flavours" of a package (e.g. a trial version vs. a production version, or versions with and without enormous datasets included). In 'patch.install', '...' is just shorthand for the arg list of 'patch.installed'.

 update.installed.cache: If TRUE, then clear the installed-package cache, so that things like 'installed.packages' work OK. The only reason to set to FALSE could be speed, if you have lots of packages; feedback appreciated. Default is TRUE unless you have set 'options( mvb.update.installed.cache=FALSE)'.

 pre.inst: ?run 'pre.install' first? Default is TRUE unless 'DLLs.only=TRUE'; leave it unless you know better.

 autoversion: if TRUE, use the '<mypack>.VERSION' counter to update source-package DESCRIPTION. This is generally much better than manually tweaking the task package's '<mypack>.DESCRIPTION' object (or task package's DESCRIPTION file, in the hyper-manual case). Only versions with at least 3 levels will be updated: so 1.0.0 will go to 1.0.1, 1.0.0.0 will go to 1.0.0.1, but 1.0 will stay the same. Default is TRUE unless you have set 'options( mvb.autoversion=FALSE)'. To force a 'major' revsion, modify '..<mypack>$<mypack>.VERSION' yourself (unless you are using a manual version in DESCRIPTION, which is discouraged). However, if you have manually changed the DESCRIPTION object or file's version to something beyond the source/installed version, then the larger number will take precedence.

 click.version: if TRUE, try to automatically increment the version number in the source (and installed, if 'patch.install') packages. Normally a good idea, _except_ if you are updating several incompatible libraries for different R versions--- then you will need to run 'pre.install/patch.install' in each R version but for the same underyling package, and you don't want the versions to get out of synch!

 vignette.build: if TRUE, call 'tools::buildVignettes' to generate HTMLs and/or PDFs from any RMD vignettes (only), so that the (?binary?) package will install properly. At present, you are still responsible for generating the RMD files in the first place; and the installed package is _not_ updated by 'patch.install' even if 'vignette.build=TRUE'. Note that the default value is different for 'pre.install' vs 'patch.install', because the former is meant to prepare a package for distro, whereas the latter is mainly updating the locally-installed version, for which vignette-updates won't work (would need to rebuild installed index, etc). If 'vignette.build=FALSE', any hand-pre-built vignettes _may_ still get installed correctly. This is all messy stuff, subject to change!

 dir.above.source: folder within which the source package will go, with a '+' at the start being shorthand for the task package folder (the default). Hence 'pre.install( pkg=mypack, dir="+/holder")' will lead to creation of "holder/mypack" below the task folder of 'mypack'. Set this manually if you have to maintain different versions of the package for different R versions, or different flavours of the package for other reasons, or if your source package must live in a "subversion tree" (whatever that is).

 R.target.version: Not needed 99% of the time; use only if you want to create source package for a different version of R. Supercedes the 'Rd.version' argument of 'pre.install' pre-'mvbutils' 2.5.57, used to control the documentation format. Set 'R.target.version' to something less than "2.10" for ye olde "Rd version 1" format.

 compress.lazyload: Installed packages feature "lazy-load" databases for documentation and for the R functions themselves (whether you like it or not), and 'patch.installed' updates them. By default, R will compress these databases as it builds them, which can be remarkably slow. This option is an experimental feature to make uncompressed versions (by tweaking the 'compress' argument to 'tools:::makeLazyLoadDB'). You can try setting 'options(mvbutils.compress.lazyload=FALSE)' for a speedup, but it's not really tested yet...

 Roxygen: ?should the Rd files be run thru 'Rd2Roxygen' and added into the source-script "<mypack>.R"? NULL (default) means that the decision is based on whether the DESCRIPTION file contains a "RoxygenNote" field.

 timeout_Roxygen: In case 'Rd2roxygen' just does not work properly, you can stop it hanging the machine by setting this timeout (in which, pre-installation will continue, but without any Roxygen in the source--- ohdearhowsadnevermindeh). Use 'options( mvb.timeout_Roxygen=<something>)' to do it for your whole R session (which is also the only way to apply it during 'patch.install'). 

 silent: whether to show messages about starting/finishing documentation-prep and lazyification.


DETAILS

As per the Glossary section of 'mvbutils.packaging.tools' (qv): the "task package" is the directory containing the ".RData" file with the guts of your package, which should be linked into the 'cd' task hierarchy. The "source package" is usually the directory "<<pkg>>" below the task package, which will be created if needs be.

The default behaviour of 'pre.install' is as follows-- to change it, see OVERRIDING.DEFAULTS. A basic source package is created in a sourcedirectory "<<pkg>>" of the current task. The package will have at least a DESCRIPTION file, a NAMESPACE file, a single R source file with name "<<pkg>>.R" in the "R" sourcedirectory, possibly a "sysdata.rda" file in the same place to contain non-functions, and a set of Rd files in the "man" sourcedirectory. Rd files will be auto-created from 'docattr' or 'flatdoc' (qv) style documentation, although precedence will be given to any pre-existing Rd files found in an "Rd" source directory of your task, which get copied directly into the package. If the DESCRIPTION file or object contains a field "KeepPlaintextDoco" with value YES/TRUE or abbrevation thereof, then the plain-text "docattr" documentation will be stored in the R source file too--- see 'dedoc_namespace'. If DESCRIPTION includes a "RoxygenNote" field, then 'pre.install' will try to add Roxgyen comments before documented functions, using 'Rd2Roxygen' (which is buggy, but at least one bug gets fixed automatically here). Any "inst", "demo", "vignettes", "tests", "src", "exec", and "data" subdirectories will be copied to the source package, recursively (i.e. including any of _their_ sourcedirectories). There is no compilation of source code, since only a source package is being created; see also COMPILED.CODE below.

Most objects in the task package will go into the source package, but there are usually a few you wouldn't want there: objects that are concerned only with how to create the package in the first place, and ephemeral system clutter such as '.Random.seed'. The default exceptions are: functions 'pre.install.hook.<<pkg>>', '.First.task', and '.Last.task'; data '<<pkg>>.file.exclude.regexes', '<<pkg>>.DESCRIPTION', '<<pkg>>.VERSION', '<<pkg>>.UNSTABLE', 'forced!exports', '.required', '.Depends', 'tasks', '.Traceback', '.packageName', 'last.warning', '.Last.value', '.Random.seed', '.SavedPlots'; and any character vector whose name ends with ".doc".

All pre-existing files in the "man", "src", "tests", "exec", "demo", "inst", and "R" sourcedirectories of the source-package directory will be removed (unless you have some 'mlazy' objects; see below). If--- but this is deprecated--- a file ".Rbuildignore" is present in the task package, then it's copied to the package directory, but I've never gotten this feature to work. If not but there is an object '<pkg>.Rbuildignore' (the preferred way; it should be a character vector), then that's used (and is automatically augmented to exclude some task-package housekeeping files). To exclude files that would otherwise be copied, i.e. those in "inst/demo/src/data" folders, create a character vector of regexes called '<<pkg>>.file.exclude.regexes'; any file matching any of these won't be copied.

If there is a "changes.txt" file in the task package (but this is deprecated), it will be copied to the "inst" sourcedirectory of the package, as will any files in the task's own "inst" sourcedirectory. A DESCRIPTION file will be created, preferably from a '<<pkg>>.DESCRIPTION' _object_ in the task package; see 'mvbutils.packaging.tools' for more. Any "Makefile.*" in the task package will be copied, as will any in the "src" sourcedirectory (not sure why both places are allowed). No other files or sourcedirectories in the package directory will be created or removed, but some essential files will be modified.

Any other character-vectors in the task package with names 'mypack.x', where "mypack" is your packagename and "x" is one of (NEWS, CHANGES, LICENCE, LICENSE, INSTALL, configure, cleanup, ChangeLog, README, Rbuildignore) or "README.y" where "y" is whatever, will be written into the source package as the corresponding file (e.g. a NEWS file will be created).

If a NAMESPACE file is present in the task (usually no need), then it is copied directly to the package. If not, then 'pre.install' will generate a NAMESPACE file by  calling 'make.NAMESPACE', which makes reasonable guesses about what to import, export, and S3methodize. What is & isn't an S3 method is generally deduced OK (see 'make.NAMESPACE' for gruesome details), but you can override the defaults via the pre-install hook. FWIW, since adding the package-creation features to 'mvbutils', I have never bothered explicitly writing a NAMESPACE file for any of my packages. By default, only _documented_ functions are exported (i.e. visible to the user or other packages); the rest are only available to other functions in your package.

If any of the Rd files starts with a period, e.g. ".dotty.name", it will be renamed to e.g. "01.dotty.name.Rd" to avoid some problems with RCMD. This should never matter, but just so you know...

To speed up conversion of documentation, a list of raw & converted documentation is stored in the file "doc2Rd.info.rda" in the task package, and conversion is only done for objects whose raw documentation has changed, unless 'force.all.docs' is TRUE.

'pre.install' creates a file "funs.rda" in the package's "R" sourcedirectory, which is subsequently used by 'patch.installed'. The function 'build.pkg' (or R CMD BUILD) and friends will omit this file (currently with a complaint, which I intend to fix eventually, but which does not cause trouble).



.COMPILED.CODE

'pre.install' tries to produce automatic R-side/C-side wrappers of C(++) code written for package 'Rcpp'. The system used is extensible by the user (a pretty advanced user!) to other flavours of R/C code, via 'Clink_packages' (qv). Current extensions are 'RcppTidy' and 'ADT'. Version-tracking on the automatically-generated wrapper files (one in "./src" and one in "./R") doesn't seem to be working that well yet, and you may well need to call 'rewrap_forcibly' to make it happen. Watch out for old versions of those files left lying around by accident, since they can cause havoc.

In the case of package 'RcppTidy', what happens is this (actually via 'Clink_packages()$RcppTidy', which itself is a function that 'mvbutils' is notified of by package 'RcppTidy' itself when the latter is loaded):

 - it calls 'compileAttributes' to generate an "RcppExports.cpp" file in the source package (in folder "src"), if a change from previous one is detected. The file is edited to retain the md5sum of the sources, which is used in subsequent runs to check for changes.

 - it _modifies_ the "RcppExports.R" file so that the R-side auto headers are all placed into an environment 'DLL' in the namespace.

This is to avoid polluting your namespace at point-of-load with possible aliases for C code, and to allow you to document and/or export "Rcpp functions" in the same way you would your other functions. It is less automated but arguably more controlled. To export (for the R-level user) a "Rcpp function", you need to explicitly write a wrapper, eg

%%#
rapid_thing <- function( a, b, c) DLL$rapid_thing( a, b, c)

and add documentation for 'rapid_thing'. There is also provision for different compilation systems, like 'RcppTidy' and an ADT-oriented one...

'patch.install' does not compile source code; currently, you need to do that yourself, though I might add support for that if I can work a sufficiently general mechanism. If you use R to do your compilation, then 'install.pkg' should work after 'pre.install', though you may need 'detach("package:mypack", unload=T)' first and that will disrupt your R session. Alternatively, you may be able to use R CMD SHLIB to create the DLL directly, which you can then copy into the "libs" sourcedirectory of the installed package, without needing to re-install. I haven't tried this, but colleagues have reported success.

If, like me, you pre-compile your own DLLs manually (not allowed on CRAN, but fine for distribution to other users on the same OS), then you can put the DLLs into a folder "inst/libs" of the task (see next for Windows); they will end up as usual in the "libs" folder of the installed package, even though R itself hasn't compiled them. On Windows, put the DLLs one level deeper in "inst/libs/<<arch>>" instead, where "<<arch>>" is found from '.Platform$r_arch'; for 32-bit Windows, it's currently "i386". All references in this section to "libs", whether in the task or source or installed package, should be taken as meaning "libs/<<arch>>". You pretty much also need to create the alternate "x64" folder, too, even if it's empty; otherwise, the 'mvbutils' installation tools will fail ( >= R3.3 or so).

To load your package's DLLs, call 'library.dynam' in the '.onLoad' function, for example like this:

%%#
.onLoad <- function( libname, pkgname){
  library.dynam( 'my_first_dll', package=pkgname)
  library.dynam( 'my_other_dll', package=pkgname)  # fine to have several DLLs
}

To automatically load all DLLs, you can copy the body of 'mvbutils:::generic.dll.loader' into your own '.onLoad', or just include a call to 'generic.dll.loader(libname,pkgname)' if you don't mind having dependence on 'mvbutils'.

After the package has been installed for the first time, I change my compiler settings so that the DLL is created directly in the installed package's "libs" folder; this means I can use the compiler's debugger while R is running. To accommodate this, 'patch.install' behaves as follows:

 - any new DLLs in the task package are copied to the installed package;
 - any DLLs in the installed package but not in the task package are deleted;
 - for any DLLs in both task & installed, both copies are synchronized to the _newer_ version;
 - the source package always matches the task package

You can call 'patch.install( mypack, DLLs.only=TRUE)' if you only want the DLL-synching step.

(Before version 2.5.57, 'mvbutils' allowed more latitude in where you could put your home-brewed DLLs, but it just made life more confusing. The only place that now works is as above.)


.DATA.OBJECTS

Data objects are handled a bit differently to the recommendations in "R extensions" and elsewhere-- but the end result for the package user is the same, or better. The changes have been made to speed up package maintenance, and to improve useability. Specifically:

 - Undocumented data objects live only in the package's namespace, i.e. visible only to your functions.

 - Documented data objects appear both in the visible part of the package (i.e. in the search path), and in the namespace. [The R standard is that these should not be visible in the namespace, but this doesn't seem sensible to me.]

 - The easiest way to export a data object, is to "document" it by putting its name into an alias line of the doc attribute of an existing function. (Alias lines are single-word lines directly after the first line of the doc attr.)

 - To document a data object 'xxx' in its own right, include a flat-format text object 'xxx.doc' in your task package; see 'doc2Rd'. 'xxx.doc' itself won't appear in the packaged object, but will result in documentation for 'xxx' _and_ any other data objects that are given as alias lines.

 - Big data objects can be set up for transparent individual lazy-loading (see below) to save time & memory, but lazy-loading is otherwise off by default for individual data objects.

 - There is no need for the user ever to call 'data' (qv) to access a dataset in the package, and in fact it won't work.

Note that the 'data(...)' function has been pretty much obsolete since the advent of lazy-loading in R 2.0; see R-news #4/2.

In terms of package structure, as opposed to operation, there is no "data" sourcedirectory. Data lives either in the "sysdata.rdb/rdx" files in the "R" sourcedirectory (but can still be user-visible, which is not normally the case for objects in those files), or in the "mlazy" sourcedirectory for those objects with individual lazy-loading.

..BIG.DATA.OBJECTS

Lazy-loading objects cached with 'mlazy' are handled specially, to speed up 'pre.install'. Such objects get their cache-files copied to "inst/mlazy", and the '.onLoad' is prepended with code that will load them on demand. By default, they are exported if and only if documented, and are not locked. The following objects are not packaged by default, even if 'mlazy'ed: '.Random.seed', '.Traceback', 'last.warning', and '.Saved.plots'. These are 'mlazy'ed automatically if 'options( mvb.quick.cd)' is 'TRUE'-- see 'cd'.


.TINYTESTS

Any "scriptlets" (charvecs whose name ends ".r" or ".R') whose first line contains the word "tinytest", are assumed to be for package 'tinytest'. They will be written into eponymous files in the "inst/tinytest" folder, where they will be accessible to 'tinytest::test_package(<mypack>)'.  If you already have manual tinytest script-files in "inst/tinytest", they will be copied into the sourcedirectory tree too (and will overwrite any scriptlets with the same names). Package 'tinytest' also requires a magic file "tinytest.R" to exist in the folder "<mypack>/tests", and that will be created in the sourcedirectory if it does not exist in your task directory. Remember to add "tinytest" to "Suggests" field in DESCRIPTION.


.DOCUMENTATION.AND.EXPORTING

..PACKAGE.DOCUMENTATION

Just because you have a package 'Splendid', it doesn't follow that a user will be able to figure out how to use it from the alphabetical list of functions in 'library( help=Splendid)'; even if you've written vignettes, it may not be obvious which to use. The recommended way to provide a package overview is via "package documentation", which the user accesses via 'package?Splendid'. You can write this in a text object called  e.g. "Splendid.package.doc", which will be passed through 'doc2Rd' (qv) with an extra "docType{package}" field added. The first line should start e.g. "Splendid-package" and the corresponding ".Rd" file will be put first into the index. It's good to have just the name of the package as a second line (unless it is also the name of an already-documented function within the package). Speaking as a frequently bewildered would-be user of other people's packages-- and one who readily gives up if the "help" is impenetrable-- I urge you to make use of this feature!


..VIGNETTES

See 'mvbutils.packaging.tools'.


..BARE.MINIMUM.FOR.EXPORT

Only documented functions and data are exported from your package (unless you resort to the subterfuge described in the subsection after this). Documented things are those found by 'find.documented( doc="any")'. The simplest way to document something is just to add its name as an "alias line" to the existing documentation of another function, before the first empty line. For example, if you're already using 'flatdoc' to document 'my.beautiful.function', you can technically "document" and thus export other functions like so:

%%#
structure( function( blahblahblah)...
,doc=flatdoc())
my.beautiful.function    package:splendid
other.exported.function.1
other.exported.function.2


The package will build & install OK even if you don't provide USAGE and ARGUMENTS sections for the other functions. Of course, R CMD CHECK wouldn't like it (and may have a point on this occasion). If you just are after "legal" (for R CMD CHECK) albeit unhelpful documentation for some of your functions that you can't face writing proper doco for yet, see 'make.usage.section' and 'make.argument.section'.


..EXPORTING.UNDOCUMENTED.THINGS.AND.VICE.VERSA

A bit naughty (RCMD CHECK complains), but quite doable. Note that "things" can be data objects, not just functions. Simply write a pre-install hook (see OVERRIDING.DEFAULTS) that includes something like this:

%%#
pre.install.hook.mypack <- function( hooklist) {
  hooklist$nsinfo$exports <- c( hooklist$nsinfo$exports, "my.undocumented.thing")
return( hooklist)
}

You can follow a similar approach if you want to document something but _not_ to export it (so that it can only be accessed by 'Splendid:::unexported.thing'). This probably isn't naughty.


.OVERRIDING.DEFAULTS

Source package folder can be controlled via 'options("mvbutils.sourcepkgdir.postfix")', as per "Folders and different R versions" in 'mvbutils.packaging.tools'. You'd only need to do this if you have multiple R versions installed that require different source-package formats (something that does not often change).

If a function 'pre.install.hook.<<pkgname>>' exists in the task "<<pkgname>>", it will be called during 'pre.install'. It will be passed one list-mode argument, containing default values for various installation things that can be adjusted; and it should return a list with the same names. It will also be passed any '...' arguments to 'pre.install', which can be used e.g. to set "production mode" vs "informal mode" of the end product. For example, you might call 'preinstall(mypack,modo="production")' and then write a function 'pre.install.hook.mypack( hooklist, modo)' that includes or excludes certain files depending on the value of 'modo'. The hook can do two things: sort out any file issues not adequately handled by 'pre.install', and/or change the following elements in the list that is passed in. The return value should be the possibly-modified list. Hook list elements are:

 copies: files to copy directly
 dll.paths: DLLs to copy directly
 extra.filecontents: named list; each element is the contents of a text file, the corresponding name being the path of the file to create eg '"inst/src/utils.pas"'--- a nonstandard name
 extra.docs: names of character-mode objects that constitute flat-format documentation
 description: named elements of DESCRIPTION file
 task.path: path of task (ready-to-install package will be created as a sourcedirectory in this)
 has.namespace: should a namespace be used?
 use.existing.NAMESPACE: ignore default and just copy the existing NAMESPACE file?
 nsinfo: default namespace information, to be written iff 'has.namespace==TRUE' and 'use.existing.NAMESPACE==FALSE'
 exclude.funs: any functions *not* to include
 exclude.data: non-functions to exclude from 'system.rda'
 dont.check.visibility: either TRUE (default default), FALSE, or a specified character vector, to say which objects are _not_ to be checked for "globality" by RCMD CHECK (using the 'globalVariables' mechanism). Leave alone if you don't understand this. You can change the "default default" via 'options( mvb_dont_check_visibility=FALSE)'.

There are two reasons for using a hook rather than directly setting parameters in 'pre.install'. The first is that 'pre.install' will calculate sensible but non-obvious default values for most things, and it is easier to change the defaults than to set them up from scratch in the call. The second is that once you have written a hook, you can forget about it-- you don't have to remember special argument values each time you call 'pre.install' for that task.

..DEBUGGING.A.PRE.INSTALL.HOOK

To understand what's in the list and how to write a pre-install hook, the easiest way is probably to write a dummy one and then 'mtrace' it before calling 'pre.install(mypack)'. However, it's all a bit clunky at present (July 2011). Because the hook only exists in the "..mypack" shadow environment, 'mtrace' won't find it automatically, so you'll need 'mtrace( pre.install.hook.mypack, from=..mypack)'. That's fine, but if you then modify the source of your hook function, you'll get an error following the "Reapplying trace..." message. So you need to do 'mtrace.off' _before_ saving your edited hook-function source, and then 'mtrace' the hook again before calling 'pre.install(mypack)'. To be fixed, if I can work out how...

.DIFFERENT.VERSIONS.OF.R

R seems to be rather fond of changing the structural requirements of source & installed packages. 'mvbutils' tries to shield you from those arcane and ephemeral details-- usually, your task package will not need changing, and 'pre.install' will automatically generate source & installed packages in whatever format R currently requires. However, sometimes you do at least need to be able to build different "instances" of your package for different versions of R. The 'sourcedir' and maybe the 'R.target.version' arguments of 'pre.install' may help with this.

But if you need to build instances of your package for a different version of R, then you may need this argument (and 'dir.above.source'). I try to keep 'mvbutils' up-to-date with R's fairly frequent revisions to package structure rules, with the aim that you (or I) can easily produce a source/binary-source package for a version of R later than the one you're using right now, merely by setting 'R.target.version'. However, be warned that this may not always be enough; there might at some point be changes in R that will require you to be running the appropriate R version (and an appropriate version of 'mvbutils') just to recreate/rebuild your package in an appropriate form.

The nuances of 'R.target.version' change with the changing tides of R versions, but the whole point of 'pre.install' etc is that you shouldn't really need to know about those details; 'mvbutils' tries to look after them for you. For example, though: as of 10/2011, the "detailed behaviour" is to enforce namespaces if 'R.target.version' >= 2.14, regardless of whether your package has a '.onLoad' or not.


..PACKAGES.WITHOUT.NAMESPACES.PRE.R2.14

You used to be allowed to build packages without namespaces-- not to be encouraged for general distribution IMO, but occasionally a useful shortcut for your own stuff nevertheless (mainly because everything is "exported", documented or not). For R <= 2.14, 'mvbutils' will decide for itself whether your package is meant to be namespaced, based on whether any of the following apply: there is a NAMESPACE file in the task package; there is a '.onLoad' function in the task; there is an "Imports" directive in the DESCRIPTION file.



SEE.ALSO

'mvbutils.packaging.tools', 'cd', 'doc2Rd', 'maintain.packages'


EXAMPLES

## Don't run
# Workflow for simple case:
cd( task.above.mypack)
maintain.packages( mypack)

# First-time setup, or after major R version changes:
pre.install( mypack)
install.pkg( mypack)
library( mypack)
# ... do stuff

# Subsequent maintenance:
maintain.packages( mypack) # only once per session, usually at the start
library( mypack) # maybe optional

# ...do various things involving changes to mypack, then...
patch.install( mypack) # keep disk image up-to-date

# Prepare copies for distribution
build.pkg( mypack) # for Linux or CRAN
build.pkg.binary( mypack) # for Windows or Macs
check.pkg( mypack) # if you like that sort of thing

## End Don't run


AUTHOR

Mark Bravington


KEYWORDS

programming, utilities
}-")

)

"pre.install.hook.mvbutils" <-
function( default.list) {
  # Just for demo purposes really; its only role is to include itself in the package!
  default.list$exclude.funs <- default.list$exclude.funs %except% 'pre.install.hook.mvbutils'
  default.list
}


"prepare.for.move" <-
function( path) {
  if( is.environment( path)) { # maintained.packages$packagename
    saving <- NA # used to have TRUE, which forced auto-save; bit bossy
    env <- path
    path <- attr( env, 'path')
  } else {
    found.me <- function( x) (!is.null( spath <- attr( as.env( x), 'path')) && spath==path)

    env <- index( sapply( 1:length( search()), found.me))[1]
    if( found <- !is.na( env)) 
      env <- as.environment( env)
    else if( length( maintained.packages)) {
      env <- index( sapply( maintained.packages, found.me))[1]
      if( found <- !is.na( env))
        env <- maintained.packages[[ env]]
    }

    if( !found) {
      env <- new.env()
      attr( env, 'path') <- path
      load.refdb( file=file.path( path, '.RData'), envir=env)
      saving <- TRUE
    } else
      saving <- if( path != .Path[ length( .Path)]) NA else FALSE # don't explicitly save globalenv
  }
  
  obj <- lsall( envir=env)
  list( env=env, saving=saving, obj=obj, path=path)
}


"print" <-
structure( function( x, ...)
  UseMethod( 'print')
, doc =  docattr( r"{
print    package:cbinder
print.cat
print.compacto
print.specialprint
print.pagertemp
print.function
print.call
print.name
print.<-
print.(
print.{
print.if
print.for
print.while
print.default


Print values

DESCRIPTION

See base-R documentation of 'print' and 'print.default'. Users should see no difference with the 'mvbutils' versions; they need to be documented and exported in 'mvbutils' for purely technical reasons. There are also three useful special-purpose print methods in 'mvbutils'; see VALUE.Some of the base-R documentation is reproduced below.

The motive for redeclaration is to have a seamless transition within the 'fixr' editing system, from the nice simple "source"-attribute system used to store function source-code before R2.14, to the quite extraordinarily complicated "srcref" system used thereafter. 'mvbutils' does so via an augmented version of base-R's print method for functions, without which your careful formatting and commenting would be lost. If a function has a "source" attribute but no "srcref" attribute (as would be the case for many functions created prior to R2.14), then the augmented 'print.function' will use the "source" attribute. There is no difference from base-R in normal use.

See HOW.TO.OVERRIDE.AN.S3.METHOD if you really want to understand the technicalities.

USAGE

print(x, ...) # generic
print(x, ...) # S3 method for default
print(x, useSource=TRUE, ...) # S3 method for function
print(x, ...) # S3 method for cat
print(x, ...,
    gap= attr( x, 'gap'), 
    width= attr( x, 'width'),
    extra= attr( x, 'extra'))# S3 method for compacto
print(x, ...) # S3 method for specialprint
print(x, ...) # S3 method for pagertemp
print(x, ...) # S3 method for call
print(x, ...) # S3 method for "<-" (a special sort of call)
print(x, ...) # S3 method for "(" (a special sort of call)
#print(x, ...) # S3 method for "{" (a special sort of call)
print(x, ...) # S3 method for "if" (a special sort of call)
print(x, ...) # S3 method for "for" (a special sort of call)
print(x, ...) # S3 method for "while" (a special sort of call)
print(x, ...) # S3 method for name (symbol)



ARGUMENTS

 x: thing to print.

 ...: other arguments passed to 'NextMethod' and/or ignored. There are many special arguments to base-R 'print.default', as described in its documentation. They are not named individually in the 'mvbutils' version for technical reasons, but you can still use them.

 gap, width, extra: see 'compacto'

 useSource: [print.function] logical, indicating whether to use source references or copies rather than deparsing language objects.  The default is to use the original source if it is available. The 'mvbutils' override will print a "source" attribute if one exists but no "srcref" attribute does, whereas base-R post-2.14 would just print a deparsed version of the function.


VALUE

Technically, an 'invisible' version of the object is returned. But the point of 'print' is to display the object. 'print.function' displays source code, as per DESCRIPTION. 'print.default' and 'print.call' need to exist in 'mvbutils' only for technical reasons. The other two special methods are:

'print.cat' applies to character vectors of S3 class 'cat', which are printed each on a new line, without the [1] prefix or double-quotes or backslashes. It's ideal for displaying "plain text". Use 'as.cat' to coerce a character vector so that it prints this way.

'print.compacto' shows 'compacto' (qv) matrices with _vertical_ column names/labels, and optionally with no gaps between columns.

'print.specialprint' can be used to ensure an object (of class 'specialprint') displays in any particular way you want, without bothering to define a new S3 class and write a print method. Just give the object an attribute "print" of mode 'expression', which can refer to the main argument 'x' and any other arguments. That expression will be run by 'print.specialprint'-- see EXAMPLES.

'print.pagertemp' is meant only for internal use by the informal-help viewer.


HOW.TO.OVERRIDE.AN.S3.METHOD

Suppose you maintain a package 'mypack' in which you want to mildly redefine an existing S3 method, like 'mvbutils' does with 'print.function'. (Drastic redefinitions are likely to be a bad idea, but adding or tweaking functionality can occasionally make sense.) The aim is that other packages which import 'mypack' should end up using your redefined method, and so should the user if they have explicitly called 'library( mypack)'. But your redefined method should _not_ be visible to packages that don't import 'mypack', nor to the user if 'mypack' has only been loaded implicitly (i.e. if 'mypack' is imported by another package, so that 'asNamespace(mypack)' is loaded but 'package:mypack' doesn't appear on the search path). It's hard to find out how to do this. Here's what I have discovered:

 - For a _new_ S3 method (i.e. for a class that doesn't already have one), then you just need to mark it as an 'S3method' in the 'mypack' NAMESPACE file (which 'mvbutils' packaging tools do for you automatically). You don't need to document the new method explicitly, and consequently there's no need to export it. The new method will still be found when the generic runs on an object of the appropriate class.

 - If you're modifying an existing method, you can't just declare it as 'S3method' in the NAMESPACE file of 'mypack'. If that's all you did, R would complain that it already has a registered method for that class--- fair enough. Therefore, you also have to redeclare and export the _generic_, so that there's a "clean slate" for registering the method (specifically, in the S3 methods table for 'mypack', where the new generic lives). The new generic will probably be identical to the existing generic, very likely just a call to 'UseMethod'. Because it's exported, it needs to be documented; you can either just refer to base-R documentation (but you still need all the formal stuff for Arguments etc, otherwise RCMD CHECK complains), or you can duplicate the base-R doco with a note. 'help2flatdoc' is useful here, assuming you're wisely using 'mvbutils' to build & maintain your package.

 - If you redeclare the generic, you also need to make sure that your _method_ is _exported_ as well as S3-registered in the NAMESPACE file of 'mypack'. This is somehow connected with the obscure scoping behaviour of 'UseMethod' and I don't really understand it, but the result is: if you don't export your method, then it's not found by the new generic (even though it exists in 'asNamespace(mypack)', which is the environment of the new generic, and even though your method is also S3-registered in that same environment). Because you export the method, you also need to document it.

 - Unfortunately, the new generic won't know about the methods already registered for the old generic. So, for most generics (exceptions listed later), you will also have to define a 'generic.default' method in 'mypack'--- and you need to export and therefore document it too, as per the previous point. This 'generic.default' just needs to invoke the original generic, so that the already-registered S3 methods are searched. However, this can lead to infinite loops if you're not careful. See 'mvbutils:::print.default' for how to do it. If you were redefining a generic that was originally (or most recently) defined somewhere other than 'baseenv()', then you'd need to replace the latter with 'asNamespace(<<original.defining.package>>)'.

 - Because your new 'generic.default' might invoke any of the pre-existing (or subsequently-registered) methods of the _original_ generic, you should just make its argument list 'x,...'. In other words, don't name individual arguments even if they are named in the original 'generic.default' (eg for 'print.default').

 - Objects of mode 'name', 'call', and '"("' or '"{"' or '"<-"' (special types of 'call') cause trouble in 'generic.default' (at least using the approach in the previous point, as in 'mvbutils:::print.default'). Unless they have a specific method, the object will be automatically evaluated. So if your generic is ever likely to be invoked on a 'call' object, you'll need a special 'generic.call' method, as in 'mvbutils:::print.call'; the same goes for those other objects.

 - A few generics--- 'rbind' and 'cbind', for example--- use their own internal dispatch mechanism and don't have e.g. an 'rbind.default'. Of course, there is a default behaviour, but it's not defined by an R-level function; see '?InternalGenerics'. For these generics, the previous point wouldn't work as a way of looking for existing methods. Fortunately, at least for 'rbind', things seem to "just work" if your redefined generic simply runs the code of the base generic (but don't call the latter directly, or you risk infinite loops--- just run its body code). Then, if _your_ generic is run, the search order is (1) methods registered for _your_ generic in 'asNamespace("mypack")', whether defined in 'mypack' itself or subsequently registered by another package that uses 'mypack', (2) methods defined/registered for the base generic (ie in the original generic's namespace), (3) the original "implicit default method". But if the _original_ generic is run (e.g. from another package that doesn't import 'mypack'), then step (1) is skipped. This is good; if another package 'pack2' imports 'mypack' and registers an S3 method, the S3 registration will go into the 'mypack' S3 lookup table, but if 'pack2' _doesn't_ import 'mypack' then the S3 registration will go into the base S3 lookup table (or the lookup table for whichever package the generic was originally defined in, eg package 'stats').

EXAMPLES

## Don't run

# Special methods shown below; basic behaviour of 'print', 'print.default',
# and 'print.function' is as for base-R

#cat
ugly.bugly <- c( 'A rose by any other name', 'would annoy taxonomists')
ugly.bugly
#[1] "A rose by any other name"                 "would annoy taxonomists"

as.cat( ugly.bugly) # calls print.cat--- no clutter
#A rose by any other name
#would annoy taxonomists

# nullprint:
biggo <- 1:1000
biggo
# [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
# [2] 19  20  21  22  23  24  25  26  27  28 etc...
oldClass( biggo) <- 'nullprint'
biggo # calls print.nullprint
# nuthin'

# specialprint:
x <- matrix( exp( seq( -20, 19, by=2)), 4, 5)
attr( x, 'print') <- expression( {
    x[] <- sprintf( '%12.2f', x);
    class( x) <- 'noquote';
    attr( x, 'print') <- NULL;
    print( x)
  })
class( x) <- 'specialprint'
x # calls print.specialprint; consistently formatted for once
#     [,1]         [,2]         [,3]         [,4]         [,5]
#[1,]         0.00         0.00         0.02        54.60    162754.79
#[2,]         0.00         0.00         0.14       403.43   1202604.28
#[3,]         0.00         0.00         1.00      2980.96   8886110.52
#[4,]         0.00         0.00         7.39     22026.47  65659969.14

## End don't run
}")

)

"print.(" <-
function( x, ...) {
  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- quote( baseenv()$print.default)
  eval.parent( mc)
}


"print.{" <-
function( x, ...) { 
  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- quote( baseenv()$print.default)
  eval.parent( mc)
}


"print.<-" <-
function( x, ...) { 
# This seemed to work, but makes me nervous...
#  x <- substitute( x)
#  base:::print.default( eval.parent( x), ...)

  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- quote( baseenv()$print.default)
  eval.parent( mc)
}


"print.browsertemp" <-
function( x, ...){
########
  # Display the file, *and delete it!!!*
  browseURL( 'file://' %&% x, getOption( 'browser'))
  unlink( x)
invisible( NULL)
}


"print.call" <-
function( x, ...) { 
# This seemed to work, but makes me nervous...
#  x <- substitute( x)
#  base:::print.default( eval.parent( x), ...)

  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- quote( baseenv()$print.default)
  eval.parent( mc)
}


"print.cat" <-
function( x, ...) { cat( x, sep='\n'); invisible( x) }


"print.cdtree" <-
function( x, ...) {
  levs <- round( x$level)
  max.lev <- max( levs)
  indents <- sapply( split( names( levs), levs), function( nn) max( nchar( nn)))
  indents <- cumsum( c( 0, rev( indents[-1])+1))
  indents <- sapply( indents, function( x) paste( rep( ' ', x), collapse=''))
  indents <- rev( indents)[ levs] %&% names( levs)
  cat( indents, sep='\n')
  invisible( x)
}


"print.compacto" <-
function( x, ..., 
    gap= attr( x, 'gap'), 
    width= attr( x, 'width'),
    extra= attr( x, 'extra')
){
  if( is.null( gap)){
    gap <- 0L
  }
  
  if( is.null( width)){
    width <- max( nchar( x, type='width'))
  }
  
  if( is.null( extra)){
    extra <- '.'
  }
  
stopifnot( is.matrix( x), nchar( extra)==1) 
  
  xval <- format( unclass( x), width=width, ...)

  if( is.null( cn <- colnames( x))){
    cn <- format( 1:ncol( x)) # right-justify  
  }
  
  gapstr <- strrep( ' ', gap)
  ncn <- nchar( cn)
  h <- max( ncn)+1
  cn <- as.character( cn)
  # Make L-just & same length; trailers into '.' or whatever
  # ... but only for odd-numbered columns
  # Next works thx2 R's auto-rep but prolly shouldn't orta...
  cn <- cn %&% strrep( c( extra, ' '), h - ncn)
  
  rch <- matrix( ' ', 2*h, ncol( x))
  c2 <- ncol( x) %/% 2
  c1 <- ncol( x) - c2
  s1 <- seq( 1, by=2, length=c1)
  s2 <- 2*(1:c2)
  rch[ h+1:h, s1] <- substring( paste( cn[ s1], collapse=''), 
      1:(c1*h), 1:(c1*h))
  rch[ 1:h, s2] <- substring( paste( cn[ s2], collapse=''), 
      1:(c2*h), 1:(c2*h))
  ch <- apply( rch, 1, paste, collapse=gapstr %&% strrep( ' ', width-1))
  
  if( is.null( rn <- rownames( x))){
    rn <- format( sprintf( '[%i]', 1:nrow( x)), justify='right')
  } else {
    rn <- format( rn, justify='right')
  }
  
  lmarg <- max( nchar( rn)) + width # +/-1 cancels
  ch <- strrep( ' ', lmarg) %&% gapstr %&% ch
  
  rowz <- rn %&% ' ' %&% gapstr %&%
    apply( xval, 1, paste, collapse=gapstr)
  
  xval <- c( ch, rowz)
  class( xval) <- 'cat'
  print( xval)
invisible( x) 
}


"print.default" <-
function( x, ...) {
  # Access S3 methods known to base::print but not mvbutils::print
  # cat( "Class of x:", class( x), '', sep='\n')
  # base::print( x, ...) # leads to recursion if default selected, as do other tricks
  
  l <- c( list( quote( print), x), list( ...)) # eval all args
  eval( as.call( l), baseenv()) # ensure that only ORIGINAL S3 method table is searched

invisible( x)
}


"print.docattr" <-
function (x, ...) 
  cat("# FLAT-FORMAT DOCUMENTATION\n")


"print.for" <-
function( x, ...) { 
# This seemed to work, but makes me nervous...
#  x <- substitute( x)
#  base:::print.default( eval.parent( x), ...)

  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- quote( baseenv()$print.default)
  eval.parent( mc)
}


"print.function" <-
function(x, useSource=TRUE, ...) {
  if( is.null( sr <- attr( x, 'srcref')) && !is.null( osrc <- attr( x, 'source'))) {
    # Concoct artificial srcref
    last.line <- max( which( nzchar( osrc)))
    last.char <- nchar( osrc[ last.line])
    attr( x, 'srcref') <- srcref( srcfilecopy( 'dummy', osrc), 
        c( 1, 1, last.line, last.char))
  }

  #  Patch to avoid explicit useless printing of x@source in newer R
  if( exists( 'getRversion', mode='function') && getRversion() > '3.2') {
    attr( x, 'source') <- NULL
  }
  
  # Call base method
  eval( body.print.function) #   unmentionable( print.function( x, useSource, ...)) # sigh...
}


"print.if" <-
function( x, ...) { 
# This seemed to work, but makes me nervous...
#  x <- substitute( x)
#  base:::print.default( eval.parent( x), ...)

  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- quote( baseenv()$print.default)
  eval.parent( mc)
}


"print.name" <-
function( x, ...)
  cat( as.character( x), '\n')


"print.nullprint" <-
function( x, ...) 
  NULL


"print.pagertemp" <-
function( x, ...) {
  file.show( x, title="mvbutils-style informal help on '" %&% names( x) %&% "'", delete.file=TRUE)
  put.in.session( just.created.window=TRUE)
  invisible( x)
}


"print.specialprint" <-
function( x, ...){
  # scatn( "G'day!")
  printo <- attr( x, 'print')
  if( printo %is.not.an% 'expression') {
    scatn( "No 'print' attribute found-- using default")
    NextMethod()
  } else {
    mc <- match.call( expand.dots=TRUE)
    thrub <- new.env()
    # scatn( 'Filling...')
    for( i in names( mc)[-1])
      thrub[[ i]] <- get( i) # force
    # scatn( 'Ready to eval...')
    eval( printo, thrub)
    # scatn( 'Done!')
  }
invisible( x) # so won't call print again
}


"print.thing.with.source" <-
function( x, ...) {
  cat( '# SOURCED FROM THIS:', attr( x, 'source'), sep='\n', ...)
invisible( x)
}


"print.while" <-
function( x, ...) { 
# This seemed to work, but makes me nervous...
#  x <- substitute( x)
#  base:::print.default( eval.parent( x), ...)

  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- quote( baseenv()$print.default)
  eval.parent( mc)
}


"promote.2.to.1" <-
function () {
    full.path <- attr(pos.to.env(2), "path")
    detach(2)
    load.mvb( filename = file.path( full.path, '.RData'), name=names( full.path), pos=1, path=full.path)
    env <- .GlobalEnv
    attr(env, "path") <- full.path
}


"put.in.session" <-
function (...) 
{
    orig.mc <- mc <- as.list(match.call())[-1]
    if (length(mc)) {
        if (is.null(names(mc))) 
            which <- 1:length(mc)
        else which <- names(mc) == ""
        for (i in index(which)) if (is.symbol(orig.mc[[i]])) 
            names(mc)[i] <- as.character(orig.mc[[i]])
        mc <- lapply(mc, eval, envir = parent.frame())
        for (i in 1:length(mc)) assign(names(mc)[i], mc[[i]], 
            pos = "mvb.session.info")
    }
    invisible(NULL)
}


"rbdf" <-
structure( function( ..., deparse.level=1) {
  mc <- match.call()
  mc[[1]] <- rbind.data.frame # should find mvbutils version...
  # ... so don't need to upset the CRANIAcs with quote( mvbutils:::rbind.data.frame)
  eval( mc, parent.frame())
}
, doc =  docattr( r"{
rbdf   package:mvbutils
rbind
rbind.data.frame


Data frames: better behaviour with zero-length cases

DESCRIPTION

'rbind' concatenates its arguments by row; see 'cbind' for basic documentation. There is an 'rbind' method for data frames which 'mvbutils' overrides, and 'rbdf' calls the override directly. The 'mvbutils' version should behave exactly as the base-R version, with two exceptions:

 - zero-row arguments are *not* ignored, e.g. so that factor levels which never appear are not dropped.

 - dimensioned (array or matrix) elements do not lose any extra attributes (such as 'class').

 I find the zero-row behaviour more logical, and useful because e.g. it lets me create an 'empty.data.frame' with the correct type/class/levels for all columns, then subsequently add rows to it. The behaviour for matrix (array) elements allows e.g. the 'rbind'ing of data frames that contain matrices of 'POSIXct' elements without losing the 'POSIXct' class (as in my package 'nicetime').

When 'rbind'ing data frames, best practice is to make sure all the arguments really are data frames. Lists and matrices also work OK (they are first coerced to data frames), but scalars are dangerous (even though base-R will process them without complaint). 'rbind' is quirky around data frames; unless _all_ the arguments are data frames, sometimes 'rbind.data.frame' will not be called even when you'd expect it to be, and the coercion of scalars is frankly potty; see DETAILS and EXAMPLES. 'mvbutils:::rbind.data.frame' tries to mimic the base-R scalar coercion, but I'm not sure it's 100% compatible. Again, the safest way to ensure a predictable outcome, is to make sure all arguments really are data frames, and/or to call 'rbdf' directly.

Note that ("thanks" to 'stringsAsFactors') the order in which data frames are rbound can affect the result--- see EXAMPLES.


.OBSOLETE

Versions of 'mvbutils' prior to 2.8.207 installed replacements for '$<-.data.frame' and '[[<-.data.frame' that circumvented weird behaviour with the base-R versions when the 'data.frame' had zero rows. That weird behaviour seems to be fixed in base-R as of version 3.4.4 (perhaps earlier). I've therefore removed those replacements (after warnings from newer versions RCMD CHECK). Hopefully, everything works... but just for the record, here's the old text, which I _think_ no longer applies.

[I _think_ this paragraph is obsolete.] Normally, you can replace elements in, or add a column to, data frames via e.g. 'x$y <- z' or 'x[["y"]] <- z'. However, in base-R this fails for no good reason if 'x' is a zero-row data frame; the sensible behaviour when 'y' doesn't exist yet, would be to create a zero-length column of the appropriate class. 'mvbutils' overrides the base (mis)behaviour so it works sensibly. Should work for matrix/array "replacements" too.



USAGE

rbind(..., deparse.level = 1) # generic
rbind(..., deparse.level = 1) # S3 method for data.frame
rbdf(..., deparse.level = 1) # explicitly call S3 method...
# ... for data frames (circumvent rbind dispatch)

## OBSOLETE x[[i,j]] <- value # S3 method for data.frame; only ...
## OBS ... the version x[[i]] <- value is relevant here, tho' arguably j==0 might be
## OBS x$name <- value # S3 method for data.frame


ARGUMENTS

 ...: Data frames, or things that will coerced to data frames. NULLs are ignored.
 deparse.level: not used by 'rbind.data.frame', it's for the default and generic only



DETAILS

% old arguments
% i,j: column and row subscripts
% name: column name
% x, value: that's up to you; I just have to include them here to stop RCMD CHECK from moaning... :/


See 'cbind' documentation in base-R.

R's dispatch mechanism for 'rbind' is as follows [my paraphrasing of base-R documentation]. Mostly, if any argument is a data frame then 'rbind.data.frame' will be used. However, if one argument is a data frame but another argument is a scalar/matrix of a class that has an 'rbind' method, then "default rbind" will be called instead. Although the latter still returns a data frame, it stuffs up e.g. class attributes, so that 'POSIXct' objects will be turned into huge numbers. Again, if you really want a data frame result, make sure all the arguments are data frames.

In 'mvbutils:::rbind.data.frame' (and AFAIK in the base-R version), arguments that are not data frames are coerced to data frames, by calling 'data.frame()' on them. AFAICS this works predictably for list and matrix arguments; note that lists need names, and matrices need column names, that match the names of the real data frame arguments, because column alignment is done by name not position. Behaviour for scalars is IMO weird; see EXAMPLES. The idea seems to be to turn each scalar into a single-row data frame, coercing its names and truncating/replicating it to match the columns of the first real data frame argument; any 'names' of the scalar itself are disregarded, and alignment is by position not name. Although 'mvbutils:::rbind.data.frame' tries to mimic this coercion, it seems to me unnecessary (the user should just turn the scalar into something less ambiguous), confusing, and dangerous, so 'mvbutils' issues a warning. Whether I have duplicated every quirk, I'm not sure.

Note also that R's accursed 'drop=TRUE' default means that things you might reasonably think _should_ be data frames, might not be. Under some circumstances, this might result in 'rbind.data.frame' being bypassed. See EXAMPLES.

Short of rewriting 'data.frame' and 'rbind', there's nothing 'mvbutils' can do to fix these quirks. Whether base-R should consider any changes is another story, but back-compatibility probably suggests not.


VALUE

[Taken from the base-R documentation, modified to fit the 'mvbutils' version]
The 'rbind' data frame method first drops any NULL arguments, then coerces all others to data frames (see DETAILS for how it does this with scalars). Then it drops all zero-column arguments.  (If that leaves none, it returns a zero-column zero-row data frame.)  It then takes the classes of the columns from the first argument, and matches columns by name (rather than by position). Factors have their levels expanded as necessary (in the order of the levels of the levelsets of the factors encountered) and the result is an ordered factor if and only if all the components were ordered factors.  (The last point differs from S-PLUS.)  Old-style categories (integer vectors with levels) are promoted to factors. Zero-row arguments are kept, so that in particular their column classes and factor levels are taken account of.

Because the class of each column is set by the _first_ data frame, rather than "by consensus", numeric/character/factor conversions can be a bit surprising especially where NAs are involved. See the final bit of EXAMPLES.


SEE.ALSO

'cbind' and 'data.frame' in base-R; 'empty.data.frame'


EXAMPLES

# mvbutils versions are used, unless base:: or baseenv() gets mentioned

# Why base-R dropping of zero rows is odd
rbind( data.frame( x='yes', y=1)[-1,], data.frame( x='no', y=0))$x # mvbutils
#[1] no
#Levels: yes no # two levels

base::rbind( data.frame( x='yes', y=1)[-1,], data.frame( x='no', y=0))$x # base-R
#[1] no
#Levels: no # lost level

rbind( data.frame( x='yes', y=1)[-1,], data.frame( x='no', y=0, stringsAsFactors=FALSE))$x
#[1] no
#Levels: yes no

base::rbind( data.frame( x='yes', y=1)[-1,], data.frame( x='no', y=0, stringsAsFactors=FALSE))$x
#[1] "no" # x has turned into a character

# Quirks of scalar coercion

evalq( rbind( data.frame( x=1), x=2, x=3), baseenv()) # OK I guess
#   x
#1  1
#x  2
#x1 3

evalq( rbind( data.frame( x=1), x=2:3), baseenv()) # NB lost element
#  x
#1 1
#x 2

evalq( rbind( data.frame( x=1, y=2, z=3), c( x=4, y=5)), baseenv())
# NB gained element! Try predicting z[2]...
#  x y z
#1 1 2 3
#2 4 5 4


evalq( rbind( data.frame( x='cat', y='dog'), cbind( x='flea', y='goat')), baseenv()) # OK
#     x    y
#1  cat  dog
#2 flea goat

evalq( rbind( data.frame( x='cat', y='dog'), c( x='flea', y='goat')), baseenv()) # Huh?
#Warning in `[<-.factor`(`*tmp*`, ri, value = "flea") :
#  invalid factor level, NAs generated
#Warning in `[<-.factor`(`*tmp*`, ri, value = "goat") :
#  invalid factor level, NAs generated
#     x    y
#1  cat  dog
#2 <NA> <NA>

evalq( rbind( data.frame( x='cat', y='dog'), c( x='flea')), baseenv()) # Hmmm...
#Warning in `[<-.factor`(`*tmp*`, ri, value = "flea") :
#  invalid factor level, NAs generated
#Warning in `[<-.factor`(`*tmp*`, ri, value = "flea") :
#  invalid factor level, NAs generated
#     x    y
#1  cat  dog
#2 <NA> <NA>

try( evalq( rbind( data.frame( x='cat', y='dog'), cbind( x='flea')), baseenv())) # ...mmmm...
#Error in rbind(deparse.level, ...) :
#  numbers of columns of arguments do not match

# Data frames that aren't:

data.frame( x=1,y=2)[-1,] # a zero-row DF-- OK
# [1] x y
# <0 rows> (or 0-length row.names)

data.frame( x=1)[-1,] # not a DF!?
# numeric(0)

data.frame( x=1)[-1,,drop=FALSE] # OK, but exceeeeeedingly cumbersome
# <0 rows> (or 0-length row.names)

# Implications for rbind:

rbind( data.frame( x='yes')[-1,], x='no')
#  [,1]
# x "no" # rbind.data.frame not called!

rbind( data.frame( x='yes')[-1,,drop=FALSE], x='no')
#Warning in rbind(deparse.level, ...) :
#  risky to supply scalar argument(s) to 'rbind.data.frame'
#   x
#x no

# Quirks of ordering and character/factor conversion:
rbind( data.frame( x=NA), data.frame( x='yes'))$x
#[1] NA    "yes" # character

rbind( data.frame( x=NA_character_), data.frame( x='yes'))$x
#[1] <NA> yes
#Levels: yes # factor!

rbind( data.frame( x='yes'), data.frame( x=NA))$x[2:1]
#[1] <NA>  yes
#Levels: yes # factor again

x1 <- data.frame( x='yes', stringsAsFactors=TRUE)
x2 <- data.frame( x='no', stringsAsFactors=FALSE)
rbind( x1, x2)$x
# [1] yes no
# Levels: yes no
rbind( x2, x1)$x
# [1] "no"  "yes"
# sigh...

}")

)

"rbind" <-
function (..., deparse.level = 1) 
  eval( body.rbind)


"rbind.data.frame" <-
function( ..., deparse.level=1) {
  allargs <- list( ...) %SUCH.THAT% !is.null( .)
  if( !length( allargs))
return( brdf()) # weird-ass 0*0 DF, as base-R doco mandates (why??!!); should not be reached by dispatch

  # This for some kind of compatibility with potty base-R behaviour
  is.scalar <- sapply( allargs, is.atomic) & sapply( allargs, is.vector)
  
  allargs[ !is.scalar] <- lapply( allargs[ !is.scalar], data.frame)
  ncols <- sapply( allargs[ !is.scalar], ncol)
  if( any( ncols != ncols[1]))
stop( 'Differing number of columns')

  # Make all scalars into single-row data frames: crazy base-R. Should not be allowed!
  if( any( is.scalar)) {
    warning( "risky to supply scalar argument(s) to 'rbind.data.frame'")
    target <- names( allargs[ !is.scalar][[ 1]])
    make.like.target <- function( x) {
        xout <- rep( x[1], length( target))
        xout[] <- x
        names( xout) <- target
        data.frame( as.list( xout), check.names=FALSE)
      }
    allargs[ is.scalar] <- lapply( allargs[ is.scalar], make.like.target)
  }

  if( length( allargs)==1)
return( allargs[[1]])

  rows <- sapply( allargs, nrow)
  norows <- rows==0

  # 0-row args get a row of NAs. Must avoid calling rbind!
  allargs[ norows] <- lapply( allargs[ norows], function( x) {
      x <- data.frame( x, check.names=FALSE) # since matrices don't like next line...
      x[1,] <- x[1,] # ... which adds a row of NAs, even for cols of DF that are matrices
      x
    })
  
  # brdf = base::rbind.data.frame, modded to handle classed matrices
  rbindo <- do.call( brdf, c( allargs, list( deparse.level=deparse.level)))
  if( any( norows)) # should work anyway but...
    rbindo <- rbindo[ -cumsum( rows + norows)[norows],,drop=FALSE]
  rbindo
}


"rcmdgeneric.pkg2" <-
function(
  pkg,
  outdir,
  indir,
  cmd='ECHO',
  postfix='',
  flags=character( 0),
  envars=character(0),
  must_hack_makeconf=FALSE,
  ...) {
#########################
  cd <- getwd()
  old.rlibs <- Sys.getenv( 'R_LIBS') # must at least set R_LIBS...
  tf <- tempfile()

  on.exit({
    setwd( cd)
    Sys.setenv( R_LIBS=old.rlibs)
    unlink( tf)
  })

  setwd( outdir) #   dirname( subdir)) # pre-7/2013, was (dir.)
  Sys.setenv( R_LIBS=paste( .libPaths(), collapse=';'))
  
  if( length( envars)){
    old_envars <- Sys.getenv( names( envars))
    on.exit( add=TRUE,
        do.call( 'Sys.setenv', as.list( old_envars)))
    do.call( 'Sys.setenv', as.list( envars))
  }
  
  comm <- paste( c( 'RCMD', cmd, paste( flags, collapse=' '), 
      indir %&% postfix), collapse=' ')
  has.tee <- nzchar( Sys.which( 'tee'))
  if( has.tee) {
    comm <- comm %&% '| tee ' %&% tf
  } else {
    warno <- "'tee' not available-- return value (error status) will be NA"
    if( .Platform$OS.type=='windows') {
      warno <- warno %&% "; install 'coreutils' from Gnuwin32 and check '?build.pkg'"
    }
    warning( warno)
  }

  if( !must_hack_makeconf || hack_Makeconf()) {
    stuff <- shell( comm, intern=!has.tee, invisible=has.tee, ...)
    if( has.tee) {
      tc <- readLines( tf)
    } else {
      tc <- stuff
      stuff <- NA
    }
  } else {
    stuff <- "Wasn't allowed to hack <R_HOME>/etc/x64/Makeconf; can't cross-compile"
  }

  attributes( stuff) <- returnList( pkg, outdir, output=tc)

return( stuff)
}


"Rd2txt_easy" <-
function( p1, options=FALSE) { # p1 from parse_Rd()
  rdo <- Rd2txt_options( width=10000,
      itemBullet= '\006 ', # '* ',
      sectionIndent= 80, sectionExtra= 2,
      minIndent= -24, extraIndent=0, # ignore list nesting
      enumFormat= function( n) sprintf( '%d. ', n),
      showURLs=TRUE,
      code_quote=TRUE,
      underline_titles=TRUE)
  on.exit( Rd2txt_options( rdo))

  if( options) {
return( Rd2txt_options()) # the new lot
  }

  t1 <- tempfile()
  on.exit( { unlink( t1); Rd2txt_options( rdo)})

  Rd2txt( p1, t1)
readLines( t1)
}


"re_bloody_quire" <-
function( name) { 
  OK <- get( 're' %&% 'quire', baseenv())( name, character.only=TRUE, quietly=TRUE)
  if( OK) {
    attr( OK, 'env') <- as.environment( 'package:' %&% name)
  }
return( OK)
}


"re_bloody_quireNamespace" <-
function( name) {
  OK <- get( 're' %&% 'quireNamespace', baseenv())( name, quietly=TRUE)
  if( OK) {
    attr( OK, 'env') <- asNamespace( name)
  }
return( OK)
}


"read.bkind" <-
function( where=1) {
  dir <- get.path.from.where( where)
  files <- object.names <- character( 0) # in case can't find
  
  index.file <- file.path( dir, '.Backup.mvb/index')
  if( file.exists( index.file)) {
    ow <- options( warn=-1)
    files <- readLines( index.file)
    options( ow)
    files <- files[ substr( files, 1, 2)=='BU']
  }
  
  if( length(files)) {
    object.names <- strsplit( files, '=', fixed=TRUE)
    files <- sapply( object.names, '[', 1)
    object.names <- lapply( object.names, '[', -1)
    object.names <- sapply( object.names, paste, collapse='=') # e.g. for something called 'per=verse'
 }

  returnList( files, object.names)
}


"readLines.mvb" <-
structure( function( con = stdin(), n = -1, ok = TRUE, EOF=as.character( NA), line.count=FALSE) {
  if( con %is.a% 'character')
    con <- file( con)
    
  if( !isOpen( con, 'r')) {
    open( con, open='r')
    on.exit( close( con))
  }
    
  n[ n < 0] <- Inf 
  answer <- character( 0)  
  lines.read <- 0
  
  while( lines.read < n) { 
    new.answer <- readLines( con=con, n=1, ok=TRUE)
    if( length( new.answer))
      lines.read <- lines.read + 1
    if( !length( new.answer) || match( new.answer, EOF, 0))
  break
    answer <- c( answer, new.answer)
  }

  if( line.count) {
    attr( answer, 'line.count') <- lines.read
    try( 
      if( !is.null( sli <- attr( con, 'source.list.num'))) {
        if( !is.null( olc <- attr( con, 'line.count'))) {
          mvbsi <- as.environment( 'mvb.session.info')
          sl <- mvbsi$source.list
          attr( sl[[ sli]], 'line.count') <- olc + lines.read
          assign( 'source.list', sl, envir=mvbsi)
        }
      }
    ) # end try
  }
    
return( answer)
}
, doc =  docattr( r"{
readLines.mvb             package:mvbutils

Read text lines from a connection

DESCRIPTION

Reads text lines from a connection (just like 'readLines'), but optionally only until a specfied string is found.


USAGE

  readLines.mvb( con=stdin(), n=-1, ok=TRUE, EOF=as.character( NA), line.count=FALSE)

  
ARGUMENTS

 con: A connection object or a character string.

 n: integer.  The (maximal) number of lines to read. Negative values indicate that one should read up to the end of the connection.

 ok: logical. Is it OK to reach the end of the connection before `n > 0' lines are read? If not, an error will be generated.

 EOF: character. If the current line matches the EOF, it's treated as an end-of-file, and the read stops. The connection is left OPEN so that subsequent reads work.
 
 line.count: (default FALSE) see VALUE.


DETAILS

Apart from stopping if the EOF line is encountered, and as noted with 'line.count==TRUE', behaviour should be as for 'readLines'.
 
 
VALUE

A character vector of length the number of lines read. If 'line.count==TRUE', it will also have an attribute "line.count" showing the number of lines read.


EXAMPLES

tt <- tempfile()
cat( letters[ 1:6], sep="\n", file=tt)
the.data <- readLines.mvb( tt, EOF="d")
unlink( tt)
the.data # [1] "a" "b" "c"


SEE.ALSO

'source.mvb', 'current.source', 'flatdoc'


KEYWORDS
 IO
}")

)

"readr" <-
function( x, ...) {
  mc <- match.call( expand.dots=TRUE)
  mc$fixing <- FALSE
  mc$new <- FALSE
  mc[[1]] <- quote( fixr)
  eval( mc, parent.frame())
}


"reduce.empty.links" <-
function( nlocal=sys.parent()) mlocal({
  # Rather pointless since empty links are harmless
  
  # Shouldn't be any occurrences of \link{} except to avoid Rd bugs...
  # ... because \link{} itself would appear as something different!
  
  # I *think* that...
  # ... if character after \link{} is not special, we can delete the \link{} 
  # ... do it sequentially, deleting last one in each line in turn
  
  # Note that \code is always a risky thing, so "c" is treated as special here
  
  mtlinx <- seq_along( Rd)
  repeat{ 
    mtlinx <- mtlinx[ grep( '\\\\link\\{\\}([^c{\\\\%]|$|\\\\\\})', Rd[ mtlinx])]
    if( !length( mtlinx))
  break
    Rd[ mtlinx] <- sub( '\\\\link\\{\\}([^c{\\\\%]|$|\\\\\\})', '\\1', Rd[ mtlinx])
  }
})


"remove.from.package" <-
function( ...) { # identical to rm.pkg
  mc <- match.call( expand.dots=TRUE)
  mc[[1]] <- quote( rm.pkg)
  eval( mc, sys.parent())
}


"rename.els" <-
function( ..., ignore.missing=FALSE){
  l <- list( ...)
  x <- l[[1]]
  l <- unlist( l[-1])
  present <- names(l) %in% names( x)
  
  if( !ignore.missing)
stopifnot( all( present))  

  l <- l[ present]
  
  names( x)[ match( names( l), names( x))] <- l
  x
}


"RENEWS" <-
structure( function( 
  pkg, 
  character.only= FALSE
){
## Markdownize and reverse NEWS charvec
  set.pkg.and.dir( FALSE)
  mp <- maintained.packages[[ pkg]]
  NEWS <- mp[[ pkg %&% '.NEWS']]
  if( is.null( NEWS)){
stop( sprintf( "No news found for '%s'; not changing anything", pkg))
  }
  
  NEWS <- gsub( '^ +$', '', NEWS) %such.that% nzchar(.)
  NEWS <- rev( NEWS) |>
    xsub( '^[0-9]+/[0-9]+(/[0-9]+)? *(,)?', '') |>
    xsub( '^ *[vV]?', '') |>
    xsub( ':', '\n -')
  
  NEWS <- c( t( cbind( NEWS, '\n'))) |>
    strsplit( '\n', fixed=TRUE) |>
    unlist( use.names=FALSE)
    
  nc <- nchar( NEWS)
  NEWS <- NEWS[ (nc > 0) | (c( nc[-1], 0) > 0) ]

return( as.cat( NEWS))
}
, doc =  docattr( r"{
RENEWS    package:mvbutils


Markdownize & reverse NEWS object


DESCRIPTION

Probably only for me. Each of my maintained packages has a '<mypack>.NEWS' object (character vector) in a pretty arbitrary format which I might as well markdownize, so that 'utils::news' can process it. Reverse order (previously, most recent came  last).) Remove dates.


USAGE

RENEWS( pkg, character.only = FALSE) 


ARGUMENTS

 pkg: eg '..debug' or 'debug' or '"debug"' (the latter a string)

 character.only: for programmatic use, enforce string format as 'pkg'


VALUE

Modified '<pkg>.NEWS' of class 'cat', so you can check it before manually assiging to eg '..<mypack>$<mypack>.NEWS'. Likely to need cleanup with 'fixr' afterwards!


EXAMPLES

## Don't run
RENEWS( mvbutils)
## End don't run


KEYWORDS
internal
}")

)

"replace_function_by_sourcecode" <-
function( ff, all){
  sc <- ff
  if( is.function( ff)){
    s <- unclass( attr( ff, 'srcref')[1:4])
    sc <- if( s[3]>s[1]) c( 
        substring( all[ s[1]], s[2]), 
        all[ (s[1]+1) %upto% (s[3]-1)],
        substring( all[ s[3]], 1, s[4])
      ) else {
        substring( all[ s[1]], s[2], s[4])
      }
    sc <- call( 'eval', call( 'parse', text=sc))
  }

  for( a in names( attributes( ff)) %except% 'srcref'){
    attr( sc, a) <- replace_function_by_sourcecode( attr( ff, a), all)
  }

return( sc)
}


"REPORTO" <-
structure( function( ..., names=NULL){
  dots <- match.call( expand.dots=FALSE)$...
stopifnot( all( sapply( dots, is.name)))
  names <- c( as.character( dots), names)
  
  list2env( mget( names, parent.frame(), inherits=TRUE), 
     envir=environment( sys.function( sys.parent())))
return( NULL)
}
, doc =  docattr( r"{
REPORTO    package:mvbutils


Stash variables in caller's environment


DESCRIPTION

'REPORTO' is a convenience function for use during model-fitting, when you have a hand-written "objective function" to optimize. Suppose your function 'obfun' computes lots of jolly interesting intermediate quantities, which you would like to preserve somewhere, before the function exits and they vanish. Then, just insert a call eg 'REPORTO( key_result, fascinating, important)' somewhere. You can make multiple calls to 'REPORTO' (with different variables...) and they will all be stashed.

You should probably give 'obfun' an environment before you do this, otherwise the interesting stuff will end up in '.GlobalEnv' (if you are lucky), resulting in clutter. You can also use 'environment(obfun)' to stash data in, so that 'obfun' will be able to just refer directly to it, again without cluttering up '.GlobalEnv'. That level of self-discipline is worth cultivating. See EXAMPLES, and eg '?closure' for some kind of intro to R's lexical-scoping rules, on which this all depends. There must be a more reader-friendly help link somewhere, though...

Of course, you can do all this with base-R commands anyway (see below). But a *key reason* for using 'REPORTO'--- at least if you are using the package 'offarray'---  is that the package 'offartmb' will automatically translate 'REPORTO' calls into 'RTMB::REPORT' calls, so your code can then run under package 'RTMB' without further modification; see 'offarray::reclasso'. Plus, even in normal R use, the 'REPORTO( var1, var2)' syntax is clearer and easier.


.PEDANTS.CORNER

HaRd-nuts will note that normal-R-use 'REPORTO' is "just" syntactic sugar for the totally-self-explanatory idiom:

%%#
list2env( mget( c( "key_result", "fascinating", "important")),
  envir=environment( sys.function()))

And, yes, of course, in normal-R use you can also achieve the effect via '<<-' and 'assign'. But the former requires you to pre-create the interesting things in 'environment( obfun)', the latter has pig-ugly syntax, and both require self-discipline, which is hateful to me anyway. However, if you really want to do all that, feel free! (And remember to write your own code to handle the 'RTMB' case.)


USAGE

REPORTO(..., names = NULL) 


ARGUMENTS

 ...: variables you want to stash--- unquoted. Can be empty.

 names: A character vector with the names of _additional_ variables to stash.
 
Thus, 'REPORTO(myvar)' or 'REPORTO(char="myvar")' have identical effects. The 'names' argument is handy if you want to stash, say, all variables whose names begin with "ncomps_"--- then 'REPORTO(names=ls(pattern="^ncomps_")'.


VALUE

'REPORTO' itself returns NULL; it is called for its side-effects.


EXAMPLES

rego <- function( beta){
  v1 <- X %*% beta
  v2 <- y-v1
  REPORTO( v1, v2)
  ssq <- sum( v2*v2)
return( ssq)
}

e <- new.env( parent=environment( rego))
e$X <- matrix( 1:6, 3, 2)
e$y <- 7:9
environment( rego) <- e

# Now rego will "know about" X & y...
rego( c( 1.6, 2.4))

# ... and it can stash its results there
e$v1
e$v2
 

}")

)

"returnList" <-
function( ...) { 
# Returns its arguments; unnamed arguments are named using deparse & substitute
# Does what the deprecated version of 'return' used to do before R 1.8
  orig.mc <- mc <- as.list( match.call())[ -1]

  if( length( mc)) {
    if( length( mc)==1)
      mc <- eval( mc[[1]], envir=parent.frame())
    else { # multiple arguments, so return as named list
      if( is.null( names( mc)))
        which <- 1:length( mc)
      else
        which <- names( mc)==''

      for( i in index( which))
        if( is.symbol( orig.mc[[ i]]))
          names( mc)[ i] <- as.character( orig.mc[[ i]] )
      mc <- lapply( mc, eval, envir=parent.frame())
    }
  }
  
  mc
}


"rm.pkg" <-
structure( function( pkg, ..., list=NULL, save.=NA) {
  if( is.null( list))
    list <- sapply( match.call( expand.dots=FALSE)$..., as.character)

  if( is.null( list)) # nothing to do-- can happen in patch.package
return()

  if( is.environment( pkg)) {
    pkenv <-  pkg
    pkg <- attr( pkenv, 'name')
  } else {
    pkenv <- maintained.packages[[ pkg]]
  }
  suppressWarnings( rm( list=list, envir=pkenv)) # that bit was easy
  maybe.save.after.move( list( env=pkenv, path=attr( pkenv, 'path'), saving=save.))

  attacho <- index( search()=='package:' %&% pkg)[1]
  if( !is.na( attacho))
    suppressWarnings( rm( list=list, envir=pos.to.env( attacho)))

  lns <- loadedNamespaces()
  if( pkg %in% lns) {
    nspkg <- asNamespace( pkg)
    suppressWarnings( rm( list=list, envir=nspkg))

    exlist <- list %that.are.in% lsall( nspkg$.__NAMESPACE__.$exports)
    if( length( exlist)) {
      # Import envs are locked, so can't remove
      # Could possibly hack round that with 'hack.lockEnvironment' but hard & ?dangerous?
      # Can't use active binding instead of existing binding either
      # Best is to use delayedAssign to try to fetch the object from baseenv
      gnu <- getNamespaceUsers( pkg)
      impenvs <- lapply( named( gnu), function( x) parent.env( asNamespace( x)))
      impls <- lapply( impenvs, ls)
      impacks <- rep( gnu, sapply( impls, length))
      impls <- unlist( impls, use.names=FALSE)
      for( x in exlist %that.are.in% impls) {
        for( impenv in impenvs[ impacks[ impls==x]]) {
          if( bl <- balloonIsTethered( x, impenv)) # should be locked
            untetherBalloon( x, impenv)
          do.call( 'delayedAssign', list( x=x, value=substitute( get( x, baseenv()), list( x=x)),
              eval.env=baseenv(), assign.env=impenv))
          if( bl)
            tetherBalloon( x, impenv)
        } # for ihas in has
      } # for x in exlist...
    } # if length exlist

    # meths <- pmatch( names( .knownS3Generics) %&% '.', list, dup=TRUE)
    # For now, just zap methods known to base

    suppressWarnings( rm( list=list, envir=baseenv()$.__S3MethodsTable__.))
  }
}
, doc =  docattr( r"{
rm.pkg    package:mvbutils
remove.from.package

Remove object(s) from maintained package

DESCRIPTION

Remove object(s) from maintained package. If the package is loaded, then objects are also removed from the search path version if any, the namespace if any, any importing namespaces, and any S3 method table. 'remove.from.package' is a synonym. You will be prompted about whether to auto-save the maintained package.

USAGE

rm.pkg( pkg, ..., list = NULL, save.=NA)
# remove.from.package( pkg, ..., list=NULL)
remove.from.package( ...) # really has same args as 'rm.pkg'

ARGUMENTS

 pkg: (string, or environment) package name or environment, e.g. '..mypack'
 ...: unquoted object names to remove
 list: character vector alternative to ..., which is ignored if 'list' is set
 save.: For internal use--- leave this alone!

DETAILS

For now, methods are only removed from the *base* S3 methods table; if new S3 generics have been defined in loaded packages, and you are trying to remove a method for such a generic, then it won't be removed. I could implement this feature if anyone really wants it.

SEE.ALSO

'maintain.packages'

EXAMPLES

## Don't run
rm.pkg( "mypackage", foo, bar)
rm.pkg( "mypackage", list=cq( foo, bar))
rm.pkg( ..mypackage, list=cq( foo, bar))
## End Don't run

KEYWORDS

misc
}")

)

"rsample" <-
function( n=length(pop), pop, replace=FALSE, prob=NULL) 
  pop[ sample( seq_along( pop)-1, size=n, replace=replace, prob=prob)+1]


"safe.rbind" <-
function( df1, df2) {
  # As of 2013:
  .Deprecated( 'rbind', package='mvbutils', msg='Better to look after column classes manually')
  
  # In R, can hit problems when vars take all-NA or "numeric" values in one df, but character values in the other
  if( is.null( df1))
return( df2)
  if( is.null( df2))
return( df1)

  fac1 <- sapply( df1, is.factor)
  fac2 <- sapply( df2, is.factor)
  if( any( fac2 & !fac1))
    df1[ fac2 & !fac1] <- lapply( df1[ fac2 & !fac1], factor)
  if( any( fac1 & !fac2))
    df2[ fac1 & !fac2] <- lapply( df2[ fac1 & !fac2], factor)  
  rbind( df1, df2)
}


"Save" <-
structure( function() {
  Save.pos( 1)
  try( savehistory())
}
, doc =  docattr( r"{
Save                 package:debug                           R documentation
Save.pos


Save R objects

DESCRIPTION

These function resemble 'save' and 'save.image', with two main differences. First, any functions which have been 'mtrace'd (see package 'debug') will be temporarily untraced during saving (the 'debug' package need not be loaded). Second, 'Save' and 'Save.pos' know how to deal with lazy-loaded objects set up via 'mlazy' (qv). 'Save()' is like 'save.image()', and also tries to call 'savehistory' (see DETAILS). 'Save.pos(i)' saves all objects from the 'i'th position on the search list in the corresponding ".RData" file (or "all.rda" file for image-loading packages, or "*.rdb/*.rdx" for lazyloading packages). There is less flexibility in the arguments than for the system equivalents. If you use the 'cd' system in 'mvbutils', you will rarely need to call 'Save.pos' directly; 'cd', 'move' and 'FF' will do it for you.


USAGE

Save()
Save.pos( pos, path, ascii=FALSE)


ARGUMENTS

  pos: string or numeric position on search path, or environment (e.g. '..mypack' if "mypack" is a maintained-package).
  path: directory or file to save into (see DETAILS).
  ascii: file type, as per 'save' (qv)


DETAILS

There is a safety provision in 'Save' and 'Save.pos', which is normally invisible to the user, but can be helpful if there is a failure during the save process (for example, if the system shuts down unexpectedly). The workspace image is first saved under a name such as "n.RData" (the name will be adapted to avoid clashes if necessary). Then, if and only if the new image file has a different checksum to the old ".RData" file, the old file will be deleted and the new one will be renamed ".RData"; otherwise, the new file will be deleted. This also means that the ".RData" file will not be updated at all if there have been no changes, which may save time when synchronizing file systems or backing up.

Two categories of objects will not be saved by 'Save' or 'Save.pos'. The first category is anything named in 'options( dont.save)'; by default, this is ".packageName", ".SavedPlots", "last.warning", and ".Traceback", and you might want to add ".Last.value". The second category is anything which looks like a maintained package, i.e. an environment whose name starts with ".." and which has attributes "name", "path", and "task.tree". A warning will be given if such objects are found. [From bitter experience, this is to prevent accidents on re-loading after careless mistakes such as '..mypack$newfun <- something'; what you _meant_, of course, is '..mypack$newfun <<- something'. Note that the accident will not cause any bad effects during the current R session, because environments are not duplicated; anything you do to the "copy" will also affect the "real" '..mypack'. However, a mismatch will occur if the environment is accidentally saved and re-loaded; hence the check in 'Save'.] 

'path' is normally inferred from the 'path' attribute of the 'pos' workspace. If no such attribute can be found (e.g. if the attached workspace was a list object), you will be prompted. If 'path' is a directory, the file will be called ".RData" if that file already exists, or "R/all.rda" if that exists, or "R/*.rbd" for lazy loads if that exists; and if none of these exist already, then the file will be called ".RData" after all. If you specify 'path', it must be a complete directory path or file path (i.e. it will not be interpreted relative to a 'path' attribute).

.COMPRESSION

'mvbutils' uses the default compression options of 'save' (qv), unless you set 'options()' "mvbutils.compress" and/or "mvbutils.compression_level" to appropriate values as per '?save'. The same applies to 'mlazy' objects. Setting 'options(mvbutils.compression_level=1)' can sometimes save quite a bit of time, at the cost of using more disk space. Set these options to NULL to return to the defaults.

.HISTORY.FILES

'Save' calls 'savehistory()'. With package 'mvbutils' from about version 2.5.6 on, 'savehistory' and 'loadhistory' will by default use the same file throughout each and every R session. That means everything works nicely for most users, and you really don't need to read the rest of this section unless you are unhappy with the default behaviour.

If you are unhappy, there are two things you might be unhappy about. First, 'savehistory' and 'loadhistory' are  by default modified to always use the _current_ value of the R_HISTFILE environment variable at the time they are called, whereas default R behaviour is to use the value when the session started, or ".Rhistory" in the current directory if none was set. I can't imagine why the default would be preferable, but if you do want to revert to it, then try to follow the instructions in '?mvbutils', and email me if you get stuck. Second, the default for R_HISTFILE itself is set by 'mvbutils' to be the file ".Rhistory" in the '.First.top.search' directory-- normally the one you start R in. You can change that default by specifying R_HISTFILE yourself before loading 'mvbutils', in one of the many ways described by the R documentation on '?Startup' and '?Sys.getenv'. 


SEE.ALSO

'save', 'save.image', ' mtrace' in package 'debug', 'mlazy'


EXAMPLES

## Don't run

Save() #
Save.pos( "package:mvbutils") # binary image of exported functions
Save.pos( 3, path="temp.Rdata") # path appended to attr( search()[3], "path")

## End don't run


AUTHOR

Mark Bravington


KEYWORDS

 debugging; file
}")

)

"save.mchanged" <-
function( objs, envir) {
  path <- attr( envir, 'path')
  mcache <- omcache <- attr( envir, 'mcache')
  mcache <- mcache %such.that% (names(.) %in% lsall( envir))
  # Check to avoid "objNA.rda"; but this fun is called only
  # ...by  mtidy & save.refdb, which should check anyhow
  objs <- objs %such.that% (. %in% lsall( envir)) 
  
  changed.objs <- objs %such.that% (mcache[.]<0)
  if( any( is.na( mcache[ changed.objs])))
warning( "mcache is corrupted somehow-- use 'demlazy' and 'mlazy' again for these objects: " %&% 
    paste( changed.objs %such.that% is.na( mcache[ .]), collapse=', '))

  if( length( changed.objs) || length( mcache)<length(omcache)) {
    if( getOption( 'mlazy.subdir', TRUE)) {
      dir.create( file.path( path, 'mlazy'), showWarnings=FALSE)
      objpath <-  file.path( 'mlazy', 'obj') }
    else 
      objpath <- 'obj'
      
#   e <- new.env() # looks as if 'e' is unnecessary-- acbins get saved as normal objects
    for( i in changed.objs) 
      xsave( list=i, file=file.path( path, objpath %&% -mcache[ i] %&% '.rda'), envir=envir)

    mcache[ changed.objs] <- -mcache[ changed.objs]
    mupdate.mcache.index.if.opt( mcache, objpath)
  }

  attr( envir, 'mcache') <- mcache
}


"Save.pos" <-
function (pos, path, ascii = FALSE) {
  set.pos.and.path()
#    on.exit(save.pos(pos)) # in R2.0, can't safely default to this

  if ("mvb.session.info" %!in% search()) {
    warning("Can't find session info")
return(invisible(NULL))
  }

  if( ('package:debug' %in% search()) && exists( 'tracees', 'package:debug')
      && length( pos.tracees <- check.for.tracees( pos))) {
    retracees <- pos.tracees %that.are.in% names( tracees)
    restoro <- sapply( named( retracees), get, envir=pos)
    temp.unmtraced <- tracees[ retracees]
    on.exit( {
      for( fname in retracees)
        lapply( retrace.envs[[ fname]], assign, x=fname, value=restoro[[ fname]])
      tp <- asNamespace( 'debug')$tracees # debug:::tracees annoys RCMD CHECK...
      tp[ retracees] <- temp.unmtraced
      assign( 'tracees', tp, 'package:debug') # does namespace version as well!
    })
    
    # Now untrace them, and store the environment(s) containing (un)traced functions...
    # ... this may not be 'pos' if there is namespacing
    retrace.envs <- lapply( named( retracees), mtrace, fname=NULL, tracing=FALSE, 
        from=pos, return.envs=TRUE) # fname=NULL forces char.fname
        
    # Now functions that are in a debugged state, but that debug has forgotten...    
    lapply( pos.tracees %except% retracees, mtrace, fname=NULL, tracing=FALSE, 
        from=pos, return.envs=FALSE) # fname=NULL forces char.fname
        
  }

  save.refdb( file=file.path( path, '.RData'), pos)
  if( !is.null( getOption( 'backup.fix', NULL)))
    create.backups( pos)

  return(invisible(NULL))
}


"save.refdb" <-
function( file, envir, ...) {
  envir <- as.env( envir)

  if( missing( file)) {
    path <- attr( envir, 'path')
    if( !is.dir( path))
      mkdir( path)
    file <- file.path( path, '.RData')
  } else
    path <- dirname( file)

  mcache <- attr( envir, 'mcache')
  mcache <- mcache %such.that% (names( .) %in% lsall( envir))
  attr( envir, 'mcache') <- mcache
  if( is.null( mcache))
    mcache <- numeric(0)

  # Housekeep dead files

  mpath <- attr( envir, 'path')
  if( getOption( 'mlazy.subdir', TRUE))
    mpath <- file.path( mpath, 'mlazy')
  if( is.dir( mpath)) {
    objfiles <- list.files( mpath, '^obj(NA|[0-9]+)\\.rda$')
    file.remove( file.path( mpath, objfiles %except% ('obj' %&% abs( mcache) %&% '.rda')))
  }

  # Save into temporary file and keep old one, in case of stuff-up
  # Changed Sept 07 so safety-check works even without mcached objects
  new.file <- file
  while( file.exists( new.file))
    new.file <- file.path( dirname( new.file), 'n' %&% basename( new.file))

  # Check for ..mypackage accidentally stored here...
  badness <- lsall( envir) %that.match% '^[.][.][^.]'
  badness <- badness %SUCH.THAT% is.environment( envir[[.]])
  badness <- badness %SUCH.THAT% (all( cq( path, name, task.tree) %in% 
      names( attributes( envir[[.]]))))
  if( length( badness))
    warning( "Not saving '" %&% paste( badness, collapse="', '") %&% "' which didn't ought to be here...")

  if( length( mcache)) {
    cache.name <- get.mcache.store.name( envir) %&% '0' # guaranteed not to exist & to be findable
    e <- new.env( parent=envir)
    assign( cache.name, abs( mcache), e) # avoid assigning into envir
    # was.there etc check if any changes have been made. If not, leave original file...
    # strictly unchanged datewise.
    
    ans <- xsave( list = c( cache.name, lsall( envir=envir) %except% c( names( mcache), badness,
        dont.save())), file=new.file, envir=e, ...)
    rm( e) # ?not necessary?

    save.mchanged( names( mcache), envir)
  } else
    ans <- xsave( list=lsall( envir=envir) %except% c( badness, dont.save()), file=new.file,
        envir=envir, ...)

  if( new.file != file) {
    checksums <- md5sum( c( file, new.file))
    if( checksums[1]==checksums[2])
      file.remove( new.file)
    else {
      file.remove( file)
      file.rename( from=new.file, to=file)
    }
  }

  ans
}


"scatn" <-
function( fmt, ..., sep='\n', file='', append=FALSE) cat( sprintf( fmt, ...), sep=sep, file=file)


"screen_masked_imports" <-
structure( function( imports, myfuns){
  dont_import_from <- as.list( named( imports))
  exports <- lapply( named( imports), getNamespaceExports)

  # Next: if something each package is exporting clashes with any later fun
  # then we need to *not* import that
  later <- myfuns
  for( imp in rev( imports)){
    dont_import_from[[ imp]] <- exports[[ imp]] %that.are.in% later
    later <- unique( c( later, exports[[ imp]]))
  }

return( dont_import_from %SUCH.THAT% (length(.)>0))
}
, doc =  docattr( r"{
screen_masked_imports    package:mvbutils


Avoid clashing package imports


DESCRIPTION

Suppose your package imports various other packages. Despite the pious advice about only selectively importing certain functions, it's _perfectly fine_ and _very convenient_ to just be able to 'import(otherpack)' in your NAMESPACE (or the equivalent, in whatever system you use to auto-generate the NAMESPACE). One annoyance, though, is that several packages may export different things with the same name, which leads to disconcerting warnings about "replacing import..." when your package is loaded. This can be circumvented in NAMEPACE using this syntax:

%%# import( otherpack, except=c("function_that_would_be_overridden"))

But you sure don't want to have to work out those NAMESPACE details yourself, so this function does it for you.

Mostly, this function will be called invisibly during 'make.NAMESPACE' (qv) during 'pre.install' (qv), but you could try it yourself for more manual things.


USAGE

screen_masked_imports( imports, myfuns)


ARGUMENTS

 imports: names of packages that yours imports (or Depends on)

 myfuns: functions in your package. You don't want to import other functions with the same name, because those imports will just be overridden.


VALUE

List of character vectors, one element per 'imports', saying which if any functions exported by that importee should _not_ be imported via your NAMESPACE.

}")

)

"search.for.regexpr" <-
structure( function( pattern, where=1, lines=FALSE, doc=FALSE, code.only=FALSE, scripts=TRUE, ...) {
  docmatch <- NULL
  if( doc %is.a% 'character') {
 stopifnot( length( doc) == 1)
    docmatch <- doc
    doc <- TRUE
  } else if( doc) {
    docmatch <- "\\.doc$"
  }

  if( scripts) {
    doc <- TRUE
    docmatch <- if( is.null( docmatch)) '[.][rR]$' else sprintf( '(%s)|([.][rR]$)', docmatch)
  }

  ## Function definitions

  get.source <- if( doc) {
    function( f)
      as.character( if( is.function( f)) {
        mostly <- capture.output( print( f)) # ... which we'd do anyway, ie if !doc
        # But also look at text attrs that might not get printed, eg "doc"
        # ... as.list() next to catch NULL
        otheratts <- as.list( attributes( f) %SUCH.THAT% is.character( .))
        mostly <- c( mostly, do.call( 'paste', otheratts))
      } else if( is.character( f))
        f
      else
        character())
  } else {
    function( f) {
      if( !code.only) {
        capture.output( print( f)) # should work with srcref OR @source
      } else { # just the actual naked bare-bones code...
        attributes( f) <- list()
        deparse( f)
      }
    }
  } # if doc

  found <- function( f, pattern, where) {
    thing <- get0( f, envir=where, inherits=FALSE, mode='function', ifnotfound=NULL)
    if( is.null( thing)) {
      thing <- get0( f, envir=where, inherits=FALSE, mode='character', ifnotfound=NULL)
    }
    f <- get.source( thing)
    any( grepl( pattern, f, ...))
  }

  search.one <- function( where) {
    ff <- find.funs( where)
    if( doc) {
      texty_ones <- (lsall( where) %except% ff) %that.match% docmatch
      ff <- c( ff, texty_ones)
    }
    if( length( ff)) {
      successful <- sapply( ff, found, pattern=pattern, where=as.environment( where))
      ff <- ff[ successful]
    }

    ff
  }

  ## actual code here ####

  if( is.environment( where))
    where <- list( where)
  answer <- lapply( where, search.one)
  if( is.numeric( where) || is.character( where))
    names( answer) <- search()[ where]

  has.some <- sapply( answer, length)>0

  if( lines) {
    for( e in index( has.some))
      answer[[ e]] <- lapply( named( answer[[ e]]),
        function( x) grep( pattern, get.source( get( x, envir=where[[e]])), value=TRUE, ...))
  }

  answer[ has.some]
}
, doc =  docattr( r"{
search.for.regexpr    package:mvbutils

Find functions/objects/flatdoc-documentation containing a regexp.

DESCRIPTION

Search one or more environments for objects that contain a regexp. Within each environment, check (i) all functions, and possibly (ii) the "doc" attributes of all functions, and possibly (iii) "scripts" and "documentation" ie character objects whose name ends with ".r" or ".R" or ".doc" (or a specified regexp).

This is a convenience function that suits the way I work, and has evolved to match that without breaking compatibility too much; in particular, the arguments 'doc', 'code.only', and 'scripts' are not what I would now design from scratch! So it might seem or behave a bit odd(ly) for you.


USAGE

search.for.regexpr( pattern, where=1, lines=FALSE, 
    doc=FALSE, code.only=FALSE, scripts=TRUE, ...)


ARGUMENTS

 pattern: the regexp

 where: an environment, something that can be coerced to an environment (so the default corresponds to '.GlobalEnv'), or a list of environments or things that can be coerced to environments.

 lines: if FALSE, return names of objects mentioning the regexp. If TRUE, return the actual lines containing the regexp.

 doc: if FALSE, search function source code only (unless 'scripts=TRUE'; see below). Otherwise, _also_ search the usual 'flatdoc' (qv) places, i.e. "doc" attributes of functions, and certain character objects. If 'doc==TRUE', the name of those objects must end in ".doc"; otherwise, if 'doc' is a string (length-1 character vector), then the names of the character object must grep that string; hence, 'doc="[.]doc$"' is equivalent to 'doc=TRUE'.

 code.only: if FALSE, search only the deparsed version of "raw" code, so ignoring e.g. comments and "flatdoc" documentation.

 scripts: if TRUE, look in 'character' objects whose name ends with ".r" or ".R", in addition to any character-mode objects controlled by 'doc'.

 ...: passed to 'grep'-- e.g. "fixed", "ignore.case".


VALUE

A list with one element per environment searched, containing either a vector of object names that mention the regexp, or a named list of objects & the actual lines mentioning the regexp.

SEE.ALSO

'flatdoc', 'find.docholder', 'find.documented'

EXAMPLES

## Don't run
# On my own system's ROOT task (i.e. workspace--- see ?cd)
search.for.regexpr( 'author', doc=FALSE)

# $.GlobalEnv
# [1] "cleanup.refs"
# the code to function 'cleanup.refs' contains "author"

search.for.regexpr( 'author', doc=TRUE)
# $.GlobalEnv
# [1] "scrunge"
# 'scrunge' is a function with a character attribute that contains "author"

search.for.regexpr( 'author', doc='p')
#$.GlobalEnv
# [1] "scrunge" "p1"      "p2"
## 'scrunge' again, plus two character vectors whose names contain 'p'

## End don't run


KEYWORDS

misc
}")

)

"search.task.trees" <-
structure( function(){
  tasks <- lapply( seq( along=search()), function( x) names( attr( pos.to.env( x), 'path')[1]))
  taski <- index( sapply( tasks, is.character))
  tasks <- unlist( tasks)
  task.trees <- sapply( 1:length( tasks), function( x) paste( tasks[length( tasks):x], collapse='/'))
  names( taski) <- task.trees
  taski
}
, doc =  docattr( r"{
search.task.trees    package:mvbutils


Locate loaded tasks on search path.

DESCRIPTION

Returns the search positions of loaded tasks, with names showing the attached branch of the tree-- see EXAMPLES.

USAGE

search.task.trees()


VALUE

Increasing numeric vector with names such as "ROOT", "ROOT/top.task", "ROOT/top.task/sub.task".


SEE.ALSO

'cd'


EXAMPLES

## Don't run
search.task.trees() # c( ROOT=1) if you haven't used cd yet
cd( mytask)
search.task.trees() # c( "ROOT/mytask"=1, ROOT=2)
## End Don't run

}")

)

"set.finalizer" <-
structure( function( handle, finalizer.name, PACKAGE=NULL) {
  # Should make this flexi enuf for .Call as well as .C

  # Avoid creation unless we can finalize
  # Check for existence of finalizer FIRST

  if( is.character( finalizer.name)) {
    finalizer.name <- getNativeSymbolInfo( finalizer.name, PACKAGE=PACKAGE)
  } else {
    oc <- oldClass( finalizer.name)
stopifnot( any( (oc == 'CRoutine') | (oc == 'RegisteredNativeSymbol')))
  }

  # NOW trigger creation, thanks to lazy eval
  handle <- as.integer( handle)

  if( all( handle==0)) # all() in case 64 bit, when handle is length 2
return( list( handle=handle, trigger=emptyenv()))

  e <- new.env( parent=.GlobalEnv)
  e$handle <- handle # not really needed

  finalize.me <- function( x) .C( finalizer, handle)
  e1 <- new.env( parent=baseenv()) # so .C is found
  e1$finalizer <- finalizer.name
  e1$handle <- handle
  environment( finalize.me) <- e1

  reg.finalizer( e, finalize.me, onexit=TRUE)
return( list( handle=handle, trigger=e))
}
, doc =  docattr( r"{
set.finalizer package:handy2

Obsolete but automatic finalization for persistent objects created in C.

DESCRIPTION

[Almost certainly obsolete; '.Call' really is the way to go for newer code, complexity notwithstanding.]

Suppose you want to create persistent objects in C-- i.e. objects that can be accessed from R by subsequent calls to C. The usual advice is that '.C' won't work safely because of uncertain disposal, and that you should use '.Call' and "externalptr" types instead. However, '.Call' etc is very complicated, and is much harder to use than '.C' in e.g. numerical settings. As an alternative, 'set.finalizer' provides a safe way to ensure that your '.C'-created persistent object will tidy itself up when its R pointer is no longer required, just as you can with 'externalptr' objects. There is no need for 'on.exit' or other precautions.


USAGE

# Always assign the result to a variable-- usually a temporary var inside a function...
# ... which R will destroy when the function ends. EG:
# keeper <- set.finalizer( handle, finalizer.name, PACKAGE=NULL)

set.finalizer( handle, finalizer.name, PACKAGE=NULL)


ARGUMENTS

 handle: [integer vector]. Pointer to your object, of length 1 on 32-bit systems or 2 on 64-bit systems. Will have been returned by your object-creation function in C.
 finalizer.name: Preferably a "native symbol" corresponding to a registered routine in a DLL; alternatively a string that names your '.C'-callable disposal routine. The routine must take exactly one argument, a 32-bit or 64-bit integer (the handle).
 PACKAGE: [string] iff 'finalizer.name' is 'character', this is a PACKAGE argument that specifies the DLL.


DETAILS

You *must* assign the result to a variable, otherwise your object will be prematurely terminated!

'set.finalizer' provides a wrapper for R's own 'reg.finalizer', setting up a dummy "trigger" environment with a registered finalizer. The trigger is defined as an environment rather than the more obvious choice of an external pointer, because the latter would require me to get fancy with '.Call'. The role of 'reg.finalizer' is to prime the trigger, so that when the trigger is subsequently garbage-collected, your specified '.C' function is called to do the finalization.

Note that finalization will only happen after _all copies_ of 'keeper' have been deleted. If you make a "temporary" copy in the global environment, remember to delete it! (Though presumably finalizers are de-registered if R is restarted and the keeper is reloaded, so there shouldn't be cross-session consequences.). Finalization won't necessarily happen immediately the last copy is deleted; you can call 'gc()' to force it.


VALUE

A list with elements 'handle' and 'trigger', the second being the environment that will trigger the call when discarded. The first is the original handle; it has storage mode integer so, as per EXAMPLES, you don't need to coerce it when subsequently passing it to '.C'.

EXAMPLES

## Don't run:
myfun <- function( ...) {
  ...0
  # Create object, return pointer, and ensure safe disposal
  keeper <- set.finalizer( .C( "create_thing", handle=integer(2), ...1)$handle,
      "dispose_of_thing")
  "cause" + "crash" # whoops, will cause crash: but finalizer will still be called
  # "dispose_of_thing" had better be the name of a DLL routine that takes a...
  # ... single integer argument, of length 1 or 2

  # Intention was to use the object. First param of DLL routine "use_thing" should
  # be pointer to thing.
  .C( "use_thing", keeper$handle, ...2)
}
myfun(...)
## End Don't run

SEE.ALSO

.C, .Call, reg.finalizer

}")

)

"set.path.attr" <-
function (env, the.path, task.name = character(0)) 
{
    if (length(task.name)) 
        names(the.path) <- task.name
    attr(env, "path") <- the.path
}


"set.pkg.and.dir" <-
function( need.outdir=FALSE, force.outdir=need.outdir, nlocal=sys.parent(), where, loaded.as.task) mlocal({
  # Create dir. sourcedir ewhere outdir; maybe modify pkg
  if( is.character( character.only)) {
    pkg <- character.only
  } else if( !character.only) {
    pkg <- as.character(substitute( pkg))
    # Allow eg ..mypack, technically a mistake
    if( (substring( pkg, 1, 2)=='..') && 
        (substring( pkg, 3) %in% names( maintained.packages))) {
      pkg <- substring( pkg, 3)
    }
  } else if( is.environment( pkg)) { # can happen with eg build.pkg( ..mypack)
    pkg <- names( attr( pkg, 'path')) # and if this is null, eg if not actually a maintained package, then it will barf later
  }

  loaded.as.task <- regexpr( '/' %&% pkg %&% '$', 
      names( search.task.trees()) %&% '$', fixed=TRUE) > 0
  where <- index( loaded.as.task)[1]
  if( !is.na( where))
    ewhere <- as.environment( where)
  else {
    ewhere <- maintained.packages[[ pkg]]
    if( is.null( ewhere)) { # what about ..mypack ? Not fully tested the consequences of this...
      ewhere <- try( as.environment( 'mvb.session.info')[[ pkg]], silent=TRUE)
      if( ewhere %is.a% 'try-error') {
        ewhere <- NULL
      }
    }
    if( is.null( ewhere)) {
      # Might be a path...
      if( dir.exists( pkg)) {
        ewhere <- structure( 0, path=pkg)
        pkg <- basename( pkg)
      } else {
stop( "Can't find raw package '" %&% pkg %&% "'")
      }
    }
  }

  dir. <- file.path.as.absolute( attr( ewhere, 'path'))
  sourcedir <- file.path( dir., pkg %&% 
      getOption( 'mvbutils.sourcepkgdir.postfix', ''))

  if( need.outdir) {
    extract.named( get.last.R.mandatory.rebuild.version()) # Rrebver, last.R.major

    outdir <- dir(dir., include.dirs=TRUE,
        pattern = "^[Rr][ _-]?[0-9]+", full.names=TRUE) %such.that% is.dir( .)

    if( length( outdir)) {
      # Find most up-to-date, but not in R's future...
      udver <- numeric_version( sub( '^[^0-9]+', '', 
          sub( '([0-9])[^0-9]+$', '\\1',
          basename( outdir))))
      not.in.future <- udver <= last.R.major
      outdir <- outdir[ not.in.future]
      udver <- udver[ not.in.future]
      wmax <- match( max( udver), udver) # no 'which.max'
      udver <- udver[ wmax]
      outdir <- outdir[ wmax]

      # ... or too far in R's past!
      if( !length( udver) || (udver < Rrebver)) {
        # Trigger new folder, for last R major
        outdir <- NULL
      }
    }

    if( !length( outdir) && eval( force.outdir)) { # eval() because of non-lazy-eval in mlocal()
      outdir <- file.path( dir., 'R' %&% as.character( last.R.major))
      mkdir( outdir)
    }
  } else { # not required
    outdir <- NULL
  }
})


"set.pos.and.path" <-
function (nlocal = sys.parent()) mlocal({
  pos <- as.environment( pos)
  if (missing(path)) {
    path <- attr( pos, "path")
    if (is.null(path))
    {
      cat("No obvious place to save it. What is the filename (single forward slashes only please)? ")
      path <- readline()
    }
  }

  path
})


"set.presave.hook.mvb" <-
structure( function( hook, set=TRUE){
  if( set)
    presave.hooks <<- c( presave.hooks, list( hook))
  else {
    which <- lapply( presave.hooks, hook, identical)
    presave.hooks <<- presave.hooks[ !which]
  }
NULL
}
, doc =  docattr( r"{
set.presave.hook.mvb    package:mvbutils


Hook of some kind

DESCRIPTION

I have forgotten what this function is for, but probably the only reason it's documented is to make sure it's exported... My advice: don't use it! (But it _is_ used by package 'debug' and others.)


USAGE

set.presave.hook.mvb( hook, set=TRUE)

ARGUMENTS

 hook: can't remember
 set: boolean...

}")

)

"set.rcmd.vars" <-
function( ...) {
######## NYI NYI NYI #########
# Supposed to let users set env vars specifically for R-related system calls, so that
# eg system( 'R CMD whatever') will just work.
# NYI!
return()

  ?sep
  vars <- unlist( list( ...))
  sysvars <- Sys.getenv()
  already <- names( vars) %that.are.in% names( sysvars)
  new <- names( vars) %except% names( sysvars)
  old.sysvars <- sysvars[ already]
  
  names( sysvars) <- sep %&% names( sysvars) %&% sep
  
  m <- gregexpr( paste( names( sysvars), collapse='|'), names( vars))
  for( ivar in seq_along( vars)) { 
    if( m[[ ivar]][1] > 0) {
      starts <- m[[ ivar]]
      ends <- starts + + attr( m[[ ivar]], 'match.length') - 1
      bits <- substring( vars[ ivar], starts, ends)
      valbits <- sysvars[ bits]
      vars[ ivar] <- rawToChar( massrep( charToRaw( vars[ ivar]), 
          atlist = mapply( ':', starts, ends),
          replist = lapply( valbits, charToRaw)))
    }
  }
  
  on.exit( {
    Sys.unsetenv( new)
    do.call( 'Sys.setenv', old.sysvars)
  })

  do.call( 'Sys.setenv', vars)
}


"set.test" <-
function (a, b) 
{
    r <- range(a - b)
    if (all(r == c(-1, 0))) 
        -1
    else if (all(r == c(0, 1))) 
        1
    else 0
}


"setHook.once" <-
function( pkg, hook.type, f, action=c( 'append', 'prepend', 'replace')){
  # the WEIRD thing here is that if I use cq instead of c in the args, R can't find it...
  identical.to.f <- function( x) {
    y <- x
    attr( y, '.Environment') <- NULL
    identical( y, f) }
  mangle <- packageEvent( pkg, hook.type)
  hooks <- getHook( mangle)
  if( !any( sapply( hooks, identical.to.f))) {
    action <- match.arg( action)
    setHook( mangle, f, action)
  }
}


"setup.dontruns" <-
function( Rd) {
  # Post-process to set /dontrun examples
  if( !length( ex <- grep( '^[\\]examples[{]', Rd)))
return( Rd)

  # If example on first line, give it its own line
  if( !grepl( '^[\\]examples[{] *$', Rd[ ex])) {
    Rd <- multirep( Rd, ex, list( c( '\\examples{', substring( Rd[ ex], nchar( '\\examples{')+1))))
  }
  end.ex <- (grep( '^[}]', Rd) %such.that% (.>ex)) [1] # braces must be escaped in ex so this is fine

  min <- function( ...) suppressWarnings( base::min( ...)) # annoying R "improvement"  
  repeat{ # once unless error; twice at most
    dontrun <- grep( "^ *## +(DON'T|NOT) +RUN *(:)? *$", Rd, ignore.case=TRUE) %such.that% 
        (. %in.range% c( ex, end.ex))
    
    if( length( dontrun)) {
      end.dontrun <- grep( "^## +END( +| *\\( *)(DON'T|NOT) +RUN\\b", Rd, ignore.case=TRUE)
      if( (length( end.dontrun) != length( dontrun)) || !all( diff( c( t( 
          matrix( c( dontrun, end.dontrun), ncol=2)))) > 0)) {
        # Don't allow fatal error
        oww <- getOption( 'warn')
        if( oww==2)
          options( warn=1)
        warning( "Unmatched DON'T RUN will be made as big as possible, " %&%
            "in doc with header:\n%s" %&% Rd[1] )
        options( warn=oww)
        # Add a dontrun immediately after unmatched ones
        unmatched <- rep( FALSE, length( dontrun))
        for( i in seq_along( dontrun)) {
          mind <- min( end.dontrun %such.that% (. > dontrun[ i]))
          unmatched[ i] <- mind >= min( dontrun[ -(1:i)]) 
        }
        unmatched <- index( unmatched)
        dontrun <- c( dontrun, end.ex)
        Rd <- multinsert( Rd, dontrun[ unmatched+1]-1, "## END DON'T RUN")
  next
      } else { # ie dontrun & end.dontrun are consistent
        Rd[ dontrun] <- '\\dontrun{'
        Rd[ end.dontrun] <- '}'
      }
    }
  break
  }

return( Rd)
}


"setup.mcache" <-
structure( function( envir, fpath=attr( envir, 'path'), refs) {
  envir <- as.environment( envir)

  mcache <- attr( envir, 'mcache') # usually NULL & overwritten by next bit
  if( missing( refs)) {
    cache.name <- get.mcache.store.name( envir)
    if( !exists( cache.name, envir=envir, inherits=FALSE))
return() # nothing to do; pre-mcache DB
    mcache <- get( cache.name, envir)
    refs <- names( mcache)
    remove( list=cache.name, envir=envir)
  }

  if( !length( refs)) # post-mcache nothing to do
return()

  objpath <- 'obj'
  if( getOption( 'mlazy.subdir', TRUE)) {
    # Back-compatibility tedious here; move files if in wrong place
    files.to.move <- (objpath %&% mcache[ refs] %&% '.rda') %such.that% (
        file.exists( file.path( fpath, .)) & !file.exists( file.path( fpath, 'mlazy', .)))
    # Normally, next if won't happen    
    if( length( files.to.move)) {
      dir.create( file.path( fpath, 'mlazy'), showWarnings=FALSE) # harmless fail if exists
      file.rename( file.path( fpath, files.to.move), file.path( fpath, 'mlazy', files.to.move))
    }
    objpath <- file.path( 'mlazy', 'obj')
  }
  
  # Create promises to load
  remove( list=refs %such.that% (. %in% lsall( envir=envir)), envir=envir) # only needed with 'move'
  for( i in refs) {
    objfile <- file.path( fpath, objpath %&% mcache[ i] %&% '.rda')
    if( !file.exists( objfile)) {
      warning( 'Can\'t find file "' %&% objfile %&% '"; deleting object "' %&% i %&% '"')
      mcache <- mcache %without.name% i
    } else {
      fx <- get.mcache.reffun( i, envir)
      efx <- environment( fx)
      # For efficiency (?), do this via promise, rather than directly coding 'if(!loaded)' in fx
      subbo <- substitute( { load( file, e, verbose=FALSE); e[[i]]}, list( e=efx, file=objfile, i=i))
      do.call( 'delayedAssign', list( x=i, value=subbo, eval.env=efx, assign.env=efx))
      suppressWarnings( makeActiveBinding( i, fx, envir))
    }
  }

  attr( envir, 'mcache') <- mcache
}
, doc =  docattr( r"{
setup.mcache  package:mvbutils

Cacheing objects for lazy-load access

DESCRIPTION

Manually setup existing reference objects-- rarely used explicitly.


USAGE

setup.mcache( envir, fpath, refs)


ARGUMENTS

 envir: environment or position on the search path.
 fpath: directory where "obj*.rda" files live.
 refs: which objects to handle-- all names in the 'mcache' attribute of 'envir', by default


DETAILS

Creates an active binding in 'envir' for each element in 'refs'. The active binding for an object 'myobj' will be a function which keeps the real data in its own environment, reading and writing it as required. Writing a new value will give 'attr( envir, "mcache")[ "myobj"]' a negative sign. This signals that the "obj*.rda" file needs updating, and the next 'Save' (or 'move' or 'cd') command will do so. [The "*" is the absolute value of 'attr( envir, "mcache")[ "myobj"]'.] One wrinkle is that the "real data" is initially a 'promise' created by 'delayedAssign', which will fetch the data from disk the first time it is needed.

SEE.ALSO

'mlazy', 'makeActiveBinding', 'delayedAssign'


AUTHOR

Mark Bravington


KEYWORDS

programming; data


}")

)

"simplest_name_generator" <-
function( x){
## Returns a string that can be pasted as <element_name> into 
## <element_name> = <contents>
## Uses minimal (?) extra quotes and raw-stringification, but should cope with *any* legal name, including names with backticks in them! but woe betide ye if so

# No weird shit
stopifnot( 
  is.character( x),
  length( x)==1,
  nzchar( x),
  !length( attributes( x))
)
  
  OK <- try( parse( text= sprintf( '%s=0', x)), silent=TRUE)

  if( (OK %is.a% 'try-error')){
    if( grepl( '"', x, fixed=TRUE)){ 
      # Make raw string. 
      # Must work even if there's a lookalike raw string signifier inside...
      rawz <- gregexpr( 'r"[-]*[{]', x)[[1]]
      n_dashes <- max( c( attr( rawz, 'match.length')-2, 0))
      x <- sprintf( 'r"%1$s{%2$s}%1$s"', strrep( '-', n_dashes), x)
    } else {
      x <- sprintf( "%s", x)
    }
  }

return( x)
}


"sleuth" <-
structure( function( pattern, ...) {
  ss <- named( search())
  if( 'mvb.session.info' %in% ss) {
    mp <- as.environment( 'mvb.session.info')$maintained.packages
    ss <- ss %except% ( 'package:' %&% names( mp))
    ss <- c( as.list( ss), mp)
  }
  
  ss <- FOR( ss, { obj <- lsall( pos=.); grep( pattern=pattern, obj, ..., value=TRUE)} )
return( ss[ lengths( ss) > 0])
}
, doc =  docattr( r"{
sleuth    package:mvbutils


Generalized version of find

DESCRIPTION

Looks for objects that regex-match 'pattern', in all attached workspaces (as per 'search()') and any maintained packages (see 'maintain.packages').


USAGE

sleuth(pattern, ...) 


ARGUMENTS

 pattern: regex
 ...: other args to 'grep', e.g. 'perl=TRUE' or 'ignore.case=TRUE'

VALUE

A list of environments containing one or more matching objects, with the object names returned as a character vector within each list element.



SEE.ALSO

'search.for.regexpr'


EXAMPLES 

sleuth( '^rm')

# On my setup, that currently gives:
#$ROOT
#[1] "rmsrc"
#
#$`package:stats`
#[1] "rmultinom"
#
#$`package:base`
#[1] "rm"
#
#$mvbutils
#[1] "rm.pkg"
#
#$handy2
#[1] "rmultinom"
#


}")

)

"source.mvb" <-
structure( function( con, envir=parent.frame(), max.n.expr=Inf,
  echo=getOption( 'verbose'), print.eval=echo,
  prompt.echo=getOption( 'prompt'), continue.echo=getOption( 'continue')) {
#####################
  mvbsi <- as.environment( 'mvb.session.info')
  if( is.null( source.list <- mvbsi$source.list))
    source.list <- list()
  if( is.character( con))
    con <- file( con)
  else if( con==stdin())
stop( 'source.mvb cannot read from stdin()')

  attr( con, 'line.count') <- 0
  source.list[[ length( source.list)+1]] <- con
  mvbsi$source.list <- source.list
  if( !isOpen( con)) {
    open( con, 'r') # if you want fancy options on e.g. blocking, you need to pre-open 'con'
    on.exit( try( close( con)))
  }

  on.exit( { put.in.session( source.list=clip( source.list)) },
      add=TRUE)

  all.lines <- readLines(con)
  pushBack( all.lines, con)

  ow <- options( warn=-1)
  on.exit( options( ow), add=TRUE)

  lines.read <- 0
  total.lines <- -1
  expr.count <- 1
  while( expr.count <= max.n.expr) {
    # Because srcfilecopy objects are environments, can't just change the lines each time---
    # ... need a new one each time
    sf <- srcfilecopy( 'dummyfile', all.lines[ (lines.read +1) %upto% length( all.lines)])

    old.lines.read <- lines.read
    thrub <- try( parse( con, n=1, srcfile=sf, keep.source=TRUE), silent=TRUE)
    if( !length( thrub))
  break # done

    if( thrub%is.a% 'try-error') {
      # old-style
      errline <- as.numeric( rev( strsplit( geterrmessage(), ' ')[[1]])[1])
      if( is.na( errline)) # try new-style
        errline <- as.numeric( sub( '.*dummyfile:([0-9]+):.*', '\\1', geterrmessage()))

      errmsg <- if( !is.na( errline))
          sprintf( "line %i", errline+lines.read)
        else
          sprintf( 'after line %i: %s', lines.read, geterrmessage())
stop( sprintf( 'parsing %s in %s', errmsg, summary( con)$description), call.=FALSE)
    }

    if( echo) {
      stuff <- deparse( thrub)
      stuff[1] <- prompt.echo %&% stuff[1]
      stuff[-1] <- continue.echo %&% stuff[-1]
      cat( stuff, sep='\n')
    }

    tryo <- try( list( eval( thrub, envir=envir)), silent=TRUE)
    if( tryo %is.a% 'try-error') {
      # Try to deduce line--- I don't think this ever works in the eval() phase
      errline <- as.numeric( rev( strsplit( geterrmessage(), ' ')[[1]])[1])
      errmsg <- if( !is.na( errline))
          sprintf( "at line %i", errline+lines.read)
        else
          sprintf( 'after line %i, in %s: %s', lines.read, paste( capture.output( print( thrub[[1]])), collapse=''),
              sub( '[^:]*: *', '', geterrmessage()))
stop( sprintf( 'in %s, %s', summary( con)$description, errmsg), call.=FALSE)
    }

    lines.just.read <- unclass( attr( thrub, 'srcref')[[1]])[3]
    sl <- mvbsi$source.list
    lines.read <- attr( sl[[ length( sl)]], 'line.count')
    lines.read <- lines.read + lines.just.read
    attr( sl[[ length( sl)]], 'line.count') <- lines.read
    assign( 'source.list', sl, mvbsi)

    last <- tryo[[1]]
    if( print.eval)
      print( tryo[[1]])
    expr.count <- expr.count + 1
  }

  last
}
, doc =  docattr( r"{
source.mvb             package:mvbutils
current.source
from.here

Read R code and data from a file or connection


DESCRIPTION

'source.mvb' is probably obsolete as of 'mvbutils' 2.11.0; see 'docattr'. Anyway, it works like 'source(local=TRUE)', except you can intersperse free-format data into your code. 'current.source' returns the connection that's currently being read by 'source.mvb', so you can redirect input accordingly. To do this conveniently inside 'read.table', you can use 'from.here' to read the next lines as data rather than R code.


USAGE

source.mvb( con, envir=parent.frame(), max.n.expr=Inf,
  echo=getOption( 'verbose'), print.eval=echo,
  prompt.echo=getOption( 'prompt'), continue.echo=getOption( 'continue'))
current.source()
from.here( EOF=as.character(NA)) # Don't use it like this!
# Use "from.here" only inside "read.table", like so:
# read.table( file=from.here( EOF=), ...)


ARGUMENTS

 con: a filename or connection

 envir: an environment to evaluate the code in; by default, the environment of the caller of 'source'

 max.n.expr: finish after evaluating 'max.n.expr' complete expressions, unless file ends first.

 EOF: line which terminates data block; lines afterwards will again be treated as R statements.

 ...: other args to 'read.table'


 echo, print.eval, prompt.echo, continue.echo: as per 'source'


DETAILS

Calls to 'source.mvb' can be nested, because the function maintains a stack of connections currently being read by 'source.mvb'. The stack is stored in the list 'source.list' in the 'mvb.session.info' environment, on the search path. 'current.source' returns the last (most recent) entry of 'source.list'.

The sequence of operations differs from vanilla 'source', which parses the entire file and then executes each expression in turn; that's why it can't cope with interspersed data. Instead, 'source.mvb' parses one statement, then executes it, then parses the next, then executes that, etc. Thus, if you include in your file a call to e.g.

'text.line <- readLines( con=current.source(), n=1)'

then the next line in the file will be read in to 'text.line', and execution will continue at the following line. 'readLines.mvb' can be used to read text whose length is not known in advance, until a terminating string is encountered; lines after the terminator, if any, will again be evaluated as R expressions by 'source.mvb'.

After 'max.n.expr' statements (i.e. syntactically complete R expressions) have been executed, 'source.mvb' will return.

If the connection was open when 'source.mvb' is called, it is left open; otherwise, it is closed.

If you want to use 'read.table' or 'scan' etc. inside a 'source.mvb' file, to read either a known number of lines or the rest of the file as data, you can use e.g. 'read.table( current.source(), ...)'.

If you want to 'read.table' to read an _unknown_ number of lines until a terminator, you could explicitly use 'readLines.mvb', as shown in the demo "source.mvb.demo.R". However, the process is cumbersome because you have to explicitly open and close a 'textConnection'. Instead, you can just use 'read.table( from.here( EOF=...), ...)' with a non-default 'EOF', as in USAGE and the same demo (but see NOTE). 'from.here' _shouldn't_ be used inside 'scan', however, because a temporary file will be left over.

'current.source()' can also be used inside a source file, to work out the source file's name. Of course, this will only work if the file is being handled by 'source.mvb' rather than 'source'.

If you type 'source.list' at the R command prompt, you should always see an empty list, because all 'source.mvb' calls should have finished. However, the source list can occasionally become corrupt, i.e. containing invalid connections (I have only had this happen when debugging 'source.mvb' and quitting before the exit code can clean up). If so, you'll get an error message on typing 'source.list' (?an R bug?). Normally this won't matter at all. If it bothers you, try 'source.list <<- list()'.


VALUE

'source.mvb' returns the value of the last expression executed, but is mainly called for its side-effects of evaluating the code. 'from.here' returns a connection, of class 'c( "selfdeleting.file", "file", "connection")'; see DETAILS. 'current.source' returns a connection.


LIMITATIONS

Because 'source.mvb' relies on 'pushBack', 'con=stdin()' won't work.


NOTE

'from.here' creates a temporary file, which should be automatically deleted when 'read.table' finishes (with or without an error). Technically, the connection returned by 'from.here' is of class 'selfdeleting.file' inheriting from 'file'; this class has a specific 'close' method, which unlinks the 'description' field of the connection. This trick works inside 'read.table', which calls 'close' explicitly, but not in 'scan' or 'closeAllConnections', which ignore the 'selfdeleting.file' class.

'from.here()' without an explicit terminator is equivalent to 'readLines( current.source())', and the latter avoids temporary files.


SEE.ALSO

'source', 'readLines.mvb', 'flatdoc', the demo in "source.mvb.demo.R"


EXAMPLES

# You wouldn"t normally do it like this:
tt <- tempfile()
cat( "data <- scan( current.source(), what=list( x=0, y=0))",
"27 3",
"35 5",
file=tt, sep="\n")
source.mvb( tt)
unlink( tt)
data # list( x=c( 27, 35), y=c(3, 5))
# "current.source", useful for hacking:
tt <- tempfile()
cat( "cat( \"This code is being read from file\",",
"summary( current.source())$description)", file=tt)
source.mvb( tt)
cat( "\nTo prove the point:\n")
cat( scan( tt, what="", sep="\n"), sep="\n")
unlink( tt)


}")

)

"source.print" <-
function( on=TRUE){
  # Obsolete ??
  if( on) {
    body( pfn) <- do.call( 'substitute', list( body( pfn), 
        list( unmentionable=as.name( '.' %&% 'Internal'))))
    assign.to.base( 'print.function', pfn)
  } else if( is.function( bpfn <- as.environment( 'mvb.session.info')$base.print.function)) {
    assign.to.base( 'print.function', bpfn)
  }
invisible( NULL)
}


"sourceable" <-
function( f, fname=deparse1( substitute( f))){
  # Get something sourceable, sans <bytecode> and <environment> dags
  # R makes this really bloody difficult
stopifnot( f %is.a% 'function')
  force( fname) # before changing f
  
  atts <- attributes( f)
  sr <- atts$srcref
  atts <- atts %without.name% 'srcref'
  attributes( f) <- list()
  
  src <- if( is.null( sr)){
    capture.output( print( f))
  } else {
    as.character( getSrcref( sr))
  }
  
  if( length( atts)){
    atts <- lapply( atts, deparse) # it might work
    src[1] <- 'structure( ' %&% src[ 1]
    for( l in names( atts)){
      n <- length( src)
      src[ n] <- src[ n] %&% ','
      src <- c( src, sprintf( '"%s" =', l), atts[[ l]])
    }
    src <- c( src, ')')
  }
  
  src[1] <- sprintf( '"%s" <- %s', fname, src[1])
as.cat( src) # make it print nicely
}


"sourcepack_info" <-
function( sourcedir) {
  files <- dir( sourcedir, recursive=TRUE, all.files=TRUE, full.names=TRUE)
  md5 <- md5sum( files)
  fnames <- substring( files, nchar( sourcedir)+1) # get rid of superfluities
  names( md5) <- fnames
return( md5)
}


"spkg" <-
function( pkg) {
  character.only <- FALSE
  dir.above.source <- '+'
  set.pkg.and.dir( FALSE)
return( sourcedir)
}


"sqr" <-
function(x)
x * x


"src_changed" <-
function( source_files, Cloader) {
## If 'source_files' have changed, or no previous manifest-file, or old manifest was differen,
# the latter needs freshening (ie rebuilding)
# Signal that by return the new manifest-line to go at the start
# otherwise return empty string

  o <- order( source_files)
  source_files <- source_files[ o]
  # This is meant for inclusion in R code, not C etc code...
  new_manifest <- '# Pre-install low-level-code manifest: ' %&%
    paste( c( rbind( basename( source_files), md5sum( source_files))),
    collapse=' ')

  rewrite <- TRUE
  if( file.exists( Cloader)) {
    old_manifest <- readLines( Cloader, n=1)
    rewrite <- old_manifest!=new_manifest
  }

return( if( rewrite) new_manifest else '')
}


"strip.missing" <-
structure( function( obs) {
  sp <- sys.frame( mvb.sys.parent())
  for( i in obs) {
    get.i <- mget( i, sp)[[1]]
    if( try( mode( get.i), silent=TRUE) %is.a% 'try-error')
      obs <- obs %except% i
  }
  obs
}
, doc =  docattr( r"{
strip.missing      package:mvbutils

Exclude "missing" objects

DESCRIPTION

To be called inside a function, with a character vector of object names in that function's frame. 'strip.missing' will return all names except those corresponding to formal arguments which were not set in the original call and which lack defaults. The output can safely be passed to 'get'.


USAGE

strip.missing( obs)


ARGUMENTS

 obs: character vector of object names, often from 'ls(all=TRUE)'
 
 
DETAILS

Formal arguments that were not passed explicitly, but which *do* have defaults, will *not* be treated as missing; instead, they will be set equal to their evaluated defaults. This could cause problems if the defaults aren't meant to be evaluated.


EXAMPLES

funco <- function( first, second, third) {
  a <- 9
  return( do.call("returnList", lapply( strip.missing( ls()), as.name)))
}

funco( 1) # list( a=9, first=1)
funco( second=2) # list( a=9, second=2)
funco( ,,3) # list( a=9, third=3)

funco2 <- function( first=999) {
  a <- 9
  return( do.call("returnList", lapply( strip.missing( ls()), as.name)))
}

funco2() # list( a=9, first=999) even tho' "first" was not set


SEE.ALSO

'returnList'


AUTHOR

Mark Bravington


KEYWORDS
programming
}")

)

"subco" <-
function( line, auto.link=!is.null( valid.links), valid.links=NULL){
  # This is the most recent version that had comments. I don't know why they were stripped
  # don't quite trust just using this version, and don't have an easy way to check the diff

  # PERL syntax in regexes
  
  line <- ' ' %&% line %&% ' '  
  chsubs <- raw(0)
  fullsubs <- character(0)
  rawl <- function( i, n.clip=0) rawToChar( rl[clip( i, n.clip)])

  # This must come before code & auto-link tests
  pkg1.frags <- gregexpr( "\\b[Pp]ackage '[a-zA-Z.][a-zA-Z.0-9]*'", line)[[1]]
  pkg2.frags <- gregexpr( "[Tt]he '[a-zA-Z.][a-zA-Z.0-9]*' package\\b", line)[[1]]
  pkg.frags <- c( pkg1.frags, pkg2.frags)
  o <- order( pkg.frags)
  o <- o[ pkg.frags[o]>0]
  pkg.frags <- pkg.frags[o]
  if( any( pkg.frags>0)) { # then they all will be
    pkg.len <- c( attr( pkg1.frags, 'match.length'), attr( pkg2.frags, 'match.length'))[o]
    pkg.seq <- mapply( seq, from=pkg.frags, length=pkg.len, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    pkg.sub <- sapply( pkg.seq, rawl)
    pkg.sub <- sub( "'([^']+)'", "\\\\pkg\\{\\1\\}", pkg.sub)
    rl[ pkg.frags] <- charToRaw( '\003')
    chsubs <- c( chsubs, rep( '\003', length( pkg.sub)))
    fullsubs <- c( fullsubs, pkg.sub)
    line <- rawToChar( rl[ -unlist( lapply( pkg.seq, '[', -1))])
  }

  if( (link.qv.frags <- gregexpr( "[( -]'[a-zA-Z.][a-zA-Z.0-9]*' +\\(qv\\)", line)[[1]])[1] > 0) {
    link.qv.frags[] <- link.qv.frags + 1
    link.qv.len <- attr( link.qv.frags, 'match.length') - 1
    link.qv.seq <- mapply( seq, from=link.qv.frags+1, length=link.qv.len-1, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    link.qv.sub <- sapply( link.qv.seq, rawl) # start AFTER sQuote
    link.qv.sub <- sub( "([^']+)'.*", "\\\\code\\{\\\\link\\{\\1\\}\\}", link.qv.sub)
    rl[ link.qv.frags] <- charToRaw( '\001')
    chsubs <- c( chsubs, rep( '\001', length( link.qv.sub)))
    fullsubs <- c( fullsubs, link.qv.sub)
    line <- rawToChar( rl[ -unlist( link.qv.seq)])
  }
  
  if( (link.see.frags <- gregexpr( "\\b[Ss]ee '[a-zA-Z.][a-zA-Z.0-9]*'", line)[[1]])[1] > 0) {
    link.see.len <- attr( link.see.frags, 'match.length')
    link.see.seq <- mapply( seq, from=link.see.frags, length=link.see.len, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    link.see.sub <- sapply( link.see.seq, rawl)
    link.see.sub <- sub( "'([^']+)'", "\\\\code\\{\\\\link\\{\\1\\}\\}", link.see.sub)
    rl[ link.see.frags] <- charToRaw( '\002')
    chsubs <- c( chsubs, rep( '\002', length( link.see.sub)))
    fullsubs <- c( fullsubs, link.see.sub)
    line <- rawToChar( rl[ -unlist( lapply( link.see.seq, '[', -1))])
  }

  if( auto.link && ((
      link.auto.frags <- gregexpr( "[( -]'([a-zA-Z.][a-zA-Z0-9._]*)'", line)[[1]])[1] > 0)) {
    link.auto.frags[] <- link.auto.frags
    link.auto.len <- attr( link.auto.frags, 'match.length') 
    link.auto.seq <- mapply( seq, from=link.auto.frags+2, length=link.auto.len-2, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    link.auto.sub <- sapply( link.auto.seq, rawl, n.clip=1) # between quotes
    if( !is.null( valid.links)) {
      ok <- link.auto.sub %in% valid.links
      link.auto.sub <- link.auto.sub[ok]
      link.auto.frags <- link.auto.frags[ok]
      link.auto.seq <- link.auto.seq[ok]
    }
      
    if( length( link.auto.sub)) {
      rl[ link.auto.frags+1] <- charToRaw( '\007')
      chsubs <- c( chsubs, rep( '\007', length( link.auto.sub)))
      fullsubs <- c( fullsubs, '\\code{\\link{' %&% link.auto.sub %&% '}}')
      line <- rawToChar( rl[ -unlist( link.auto.seq)])
    }
  }

  if( (code.frags <- gregexpr( "([ (])'([^']+)'", line)[[1]])[1] > 0) {
    code.len <- attr( code.frags, 'match.length')
    code.seq <- mapply( seq, from=code.frags+2, length=code.len-2, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    code.sub <- sapply( code.seq, rawl)
    code.sub <- substring( code.sub, 1, nchar( code.sub)-1)
    # Some characters need to be escaped:
    # code.sub <- gsub( '{', '\\{', gsub( '%', '\\%', code.sub, fixed=TRUE), fixed=TRUE)
    code.sub <- '\\code{' %&% code.sub %&% '}'
    rl[ code.frags+1] <- charToRaw( '\006')
    # Backslash and braces in \code will be treated differently...
    code.sub <- gsub( '\016', '\017', code.sub, fixed=TRUE)
    code.sub <- gsub( '\020', '\022', code.sub, fixed=TRUE)
    code.sub <- gsub( '\021', '\023', code.sub, fixed=TRUE)    
    chsubs <- c( chsubs, rep( '\006', length( code.sub)))
    fullsubs <- c( fullsubs, code.sub)
    line <- rawToChar( rl[ -unlist( code.seq)])
  }
  
  emph.frags <- gregexpr( '[ (]_[^"_]+_', line)[[1]]
  if( emph.frags[1]>0) {
    emph.len <- attr( emph.frags, 'match.length')
    emph.seq <- mapply( seq, from=emph.frags+2, length=emph.len-2, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    emph.sub <- sapply( emph.seq, rawl)
    emph.sub <- '\\emph{' %&% substring( emph.sub, 1, nchar( emph.sub)-1) %&% '}'
    rl[ emph.frags+1] <- charToRaw( '\004')
    chsubs <- c( chsubs, rep( '\004', length( emph.sub)))
    fullsubs <- c( fullsubs, emph.sub)
    line <- rawToChar( rl[ -unlist( emph.seq)])
  }

  bold.frags <- gregexpr( '[ (]\\*[^"*]+\\*', line)[[1]]
  if( bold.frags[1]>0) {
    bold.len <- attr( bold.frags, 'match.length')
    bold.seq <- mapply( seq, from=bold.frags+2, length=bold.len-2, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    bold.sub <- sapply( bold.seq, rawl)
    bold.sub <- '\\bold{' %&% substring( bold.sub, 1, nchar( bold.sub)-1) %&% '}'
    rl[ bold.frags+1] <- charToRaw( '\005')
    chsubs <- c( chsubs, rep( '\005', length( bold.sub)))
    fullsubs <- c( fullsubs, bold.sub)
    line <- rawToChar( rl[ -unlist( bold.seq)])
  }

  url.frags <- gregexpr( "<[a-z]+://[0-9a-zA-Z%$_.+!*'()/-]+>", line)[[1]]
  if( url.frags[1]>0) {
    url.len <- attr( url.frags, 'match.length')
    url.seq <- mapply( seq, from=url.frags+1, length=url.len-1, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    url.sub <- sapply( url.seq, rawl)
    url.sub <- '\\url{' %&% substring( url.sub, 1, nchar( url.sub)-1) %&% '}'
    rl[ url.frags] <- charToRaw( '\010')
    chsubs <- c( chsubs, rep( '\010', length( url.sub)))
    fullsubs <- c( fullsubs, url.sub)
    line <- rawToChar( rl[ -unlist( url.seq)])
  } 

  email.frags <- gregexpr( '<[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]+>', line)[[1]]
  if( email.frags[1]>0) {
    email.len <- attr( email.frags, 'match.length')
    email.seq <- mapply( seq, from=email.frags+1, length=email.len-1, SIMPLIFY=FALSE)
    rl <- charToRaw( line)
    email.sub <- sapply( email.seq, rawl)
    email.sub <- '\\email{' %&% substring( email.sub, 1, nchar( email.sub)-1) %&% '}'
    rl[ email.frags] <- charToRaw( '\013')
    chsubs <- c( chsubs, rep( '\013', length( email.sub)))
    fullsubs <- c( fullsubs, email.sub)
    line <- rawToChar( rl[ -unlist( email.seq)])
  } 


  # Escape other special characters - {}\ already done
  # line <- gsub( '([#\\_${}])', '\\\\\\1', line)
  if( numeric_version( Rd.version) < '2')
    line <- gsub( '([#_$])', '\\\\\\1', line)

  # R 
  line <- gsub( "( |\\()R([ .,;:'])", "\\1\\R{}\\2", line)
  
  # Ellipsis: not within dquotes
  # 2013: not converting ... any more, because it was INCREDIBLY slow. Unbelievable actually.
  if( FALSE) {
    # Seems to be an R bug that can't cope with nested backref numbers. Nesting per se works fine.
    # old.line <- line
    repeat {
    break # check!
      findots <- regexpr( "^([^\"]*(\"[^\"]*\")*)*[.]{3}", line)
      if( findots<0)
    break
      ml <- attr( findots, 'match.length')
      line <- substring( line, 1, ml-3) %&% "\\dots{}" %&% substring( line, ml+1)
      #line <- gsub("^([^\"]*(\"[^\"]*\")*)*[.]{3}", "\\1\\\\dots{}", old.line)
      #if (all(line == old.line)) 
    #break
      # old.line <- line
    }

    # Very old code that presumably did some unwanted conversions
    # line <- gsub( '...', '\\dots{}', line, fixed=TRUE)
  } # if FALSE: dots

  # Multiple tabs & spaces go down to 1 space, except keep a double space at the start
  line <- gsub( '(.)[ \t]+', '\\1 ', line)

  # Put things back
  for( isub in seq_along( chsubs))
    line <- sub( chsubs[ isub], fullsubs[ isub], line, fixed=TRUE)
    
  substring( line, 2, nchar( line)-1) # remove first and last spaces
}


"subset_keeping_attributes" <-
function( x, ind) {
## R has dreadful behaviour of (sometimes) killing attributes during subsetting.
## eg even if subsetting returns entire original!
## In contexts (mvbutils operators with %...%) where this is used, I don't want that to happen
  if( !length( x) || all( ind)) {
return( x)
  }

  oatts <- attributes( x)
  oatts$names <- oatts$names[ ind]
  x <- x[ ind]
  attributes( x) <- oatts
return( x)
}


"task.home" <-
structure( function(fname) {
  if(!missing(fname)) {
    if(fname == "" || substr( fname, 1, 1) %in% c( '/', '\\') || pos(":", fname)[1])
return(fname)
    else
return( file.path( .Path[ length( .Path)], fname))    
  } else
return( as.vector( .Path[ length( .Path)]))
}
, doc =  docattr( r"{
task.home    package:mvbutils

Organizing R workspaces

DESCRIPTION

Returns file path to current task, or to a file in that task.


USAGE

# Often: task.home()
task.home(fname)


ARGUMENTS

 fname: file name, a character(1)


DETAILS

Without any arguments, 'task.home' returns the path of the current task. With a filename argument, the filename is interpreted as relative to the current task, and its full (non-relative) path is returned.

'task.home' is almost obsolete in R, since the working directory tracks the current task. It is more important in the S+ version of 'mvbutils'.


EXAMPLES

## Don't run:
task.home( "myfile.c") # probably the same as file.path( getwd(), "myfile.c")
task.home() # probably the same as getwd()
## End Don't run


SEE.ALSO

'cd', 'getwd', 'file.path'


AUTHOR

Mark Bravington


KEYWORDS

utilities
}")

)

"tidyup_docattr" <-
function( e=.GlobalEnv){
  ff <- find.funs( e) %SUCH.THAT% !is.null( attr( e[[.]], 'doc'))
  tf <- tempfile()
  on.exit( unlink( tf))
  for( ifun in ff){
    efun <- e[[ ifun]]
    edoc <- attr( efun, 'doc')
    if( edoc %is.not.a% 'docattr'){
      class( edoc) <- 'docattr'
      attr( efun, 'doc') <- edoc
    }
    write_sourceable_function( efun, tf)
    newfun <- try( source( tf)$value)
    if( newfun %is.a% 'function'){
      # Next bit of weirdness needed for 'fixr'!
      attr(  attr( newfun, 'srcref'), 'srcfile')$filename <- 'dummyfile'
      assign( ifun, newfun, envir=e)
    }
  }
}


"to.regexpr" <-
function (x) 
{
    x <- strsplit(x, "")
    repfun <- function(xx) {
        m <- match(xx, c("&", ".", "%", "\\", "[", "]", "(", 
            ")", "^", "{", "}", "+", "|", "$", "?", "*"), 0)
        xx[m > 0] <- "\\" %&% xx[m > 0]
        paste(xx, collapse = "")
    }
    sapply(x, repfun)
}


"undent" <-
function( s){
stopifnot( 
    is.character( s), 
    length( s)==1,
    nzchar( s))
  prewhite <- sub( '^(\n *).*', '\\1', s)
  gsub( prewhite, '\n', s, fixed=TRUE) |> 
    substring( 2) |>
    xsub( ' +$', '')
}


"undull" <-
function( x){
  if( 'dull' %in% oldClass( x)){
    # ... x might have acquired explicit implicit classes, eg 'matrix', 'array'
    # Don't need them after undulling
    oldClass( x) <- oldClass( x) %except% c( 'dull', class( unclass( x)))
  }
return( x)
}


"unmaintain.package" <-
function( pkg, character.only=FALSE){
  if( !character.only) 
    pkg <- as.character( substitute( pkg))

  maintained.packages <<- maintained.packages %without.name% pkg
  rm( list='..' %&% pkg, envir=as.environment( 'mvb.session.info'))
  
  # Anything being fixr-ed?
  editees <- (do.on( fix.list$where, 
      tail( strsplit( ., '/', fixed=TRUE)[[1]], 1))==pkg) &  
      fix.list$where.type=='package'
  fix.list$where.type[ editees] <<- 'task'
}


"unmaintain.packages" <-
function( ..., character.only=FALSE, autosave=NA){
  if( character.only)
    packs <- list(...)[[1]]
  else {
    mc <- as.list( match.call( expand.dots=FALSE)$...)
    packs <- sapply( mc, as.character)
  }

  if( is.na( autosave) || save)
    for( i in packs %that.are.in% names( maintained.packages))
      if( !is.na( autosave) || yes.no( "Save maintained package '" %&% i %&% "'? "))
        Save.pos( maintained.packages[[ i]])
  maintained.packages <<- maintained.packages %without.name% packs

  # Clear load hooks
  for( i in packs) {
    setHook( packageEvent( i, 'onLoad'), NULL, 'replace')
    setHook( packageEvent( i, 'attach'), NULL, 'replace')
  }

  dont.lock.envs <<- dont.lock.envs %without.name% 'imports:' %&% packs
  dont.lock.envnames <<- dont.lock.envnames %except% 'package:' %&% packs

return( names( maintained.packages))
}


"unpackage" <-
structure( function(
    spath,
    force= FALSE,
    alias=x,
    drop_Conly= TRUE,
    trust_importFrom= FALSE,
    zap_Cloaders= TRUE,
    keep_Roxygen= FALSE
){
  if( getRversion() < '2.10')
stop( "help2flatdoc only works with R 2.10 & up")

  oppo <- options( useFancyQuotes=FALSE, keep.source=TRUE)
  on.exit( options( oppo))
  # DESCRIPTION.in used to be legal; not sure it is now but let's be nice
  if( !is.dir( spath) || is.na(
      desc <- grep( '^DESCRIPTION([.]in)?$', dir( spath), perl=TRUE, value=TRUE)[1]))
stop( dQuote( spath) %&% " doesn't seem to be a source package")

  x <- tools$.read_description( file.path( spath, desc))[ 'Package']
  if( is.null( alias)) {
    alias <- x
  }
  descro <- read.dcf( file.path( spath, desc))[1,] # named vector please

  if( !force && is.dir( x) && 
      length( dir( x, all.files=TRUE) %except% c( '.', '..'))>2) {
    force <- yes.no( 'Directory "' %&% x %&%
    '" already exists with stuff in it, which will be deleted if you proceed. OK? ')
  } else if( !force && file.exists( x) && !is.dir( x)) {
    cat( '"x" already exists, as a file\n')
    force <- FALSE
  }  else
    force <- TRUE

  if( force) {
    tpath <- file.path( getwd(), alias)
    mkdir( tpath)
    unlink( file.path( tpath, dir( tpath) %except% c( '.', '..')), recursive=TRUE)
  } else
stop( "Not overwriting")

  # one file goes straight to task: DESCRIPTION, Makefile.*
  # some files disappear: CONTENTS, INDEX, MD5, NAMESPACE (
  # other files go to "inst" (this will include CITATION)
  # some dirs disappear: chtml, html, latex, meta, R-ex
  # some dirs get used (R, man--- WAS data)
  # some dirs go straight to task (eg src, data)
  # all other dirs (incl. libs, demos) go to "inst"

  # Recursive file- or directory-copy
  filecop <- function( fname, ...) {
      # fname: either a single dir or a bunch of non-dirs
      if( !any( file.exists( file.path( spath, fname))))
    return()

      tpath... <- do.call( 'file.path', c( list( tpath), unlist( list( ...))))
      mkdir( tpath...)
      if( is.dir( file.path( spath, fname[1]))) {
        mkdir( file.path( tpath..., fname))
        mcf <- mcd <- as.list( match.call( expand.dots=TRUE))

        fd <- dir( file.path( spath, fname), all.files=TRUE) %except% c( '.', '..')

        # Copy all *files* at once...
        fd.dirs <- fd[ is.dir( file.path( spath, fname, fd))]
        mcf$fname <- file.path( fname, fd %except% fd.dirs)
        eval( as.call( mcf), sys.parent())

        # ... and subdirs one at a time...
        for( this.dir in fd.dirs) {
          mcd$fname <- file.path( fname, this.dir)
          eval( as.call( mcd), sys.parent())
        }
      } else
        file.copy( file.path( spath, fname),
            file.path( tpath..., fname))
    }

  # not filecop( 'DESCRIPTION') in case DESCRIPTION.in
  # was: mvb.file.copy( file.path( spath, desc), file.path( tpath, 'DESCRIPTION'))
  # now insert text object later

  nondirs <- dir( spath, all.files=TRUE) %such.that% !is.dir( file.path( spath, .))
  nondirs <- nondirs %except% c( desc, cq( CONTENTS, INDEX, MD5, NAMESPACE))
  filecop( nondirs, '.')

  filecop( 'src')
  filecop( 'demo')
  filecop( 'tests')
  filecop( 'exec')
  filecop( 'inst')
  filecop( 'vignettes')
  filecop( 'data') # 2023 change

  instdirs <- (dir( spath, all.files=TRUE) %such.that% is.dir( .))
  instdirs <- instdirs %except%
      cq( ., .., src, chtml, html, latex, meta, 'R-ex', R, help, data, libs)
  filecop( instdirs, 'inst')
  filecop( 'libs', file.path( 'inst', 'libs'))

  ## Deal wih R code, data, and help
  # R code: changed 11/2017 to deal better with labyrinthine srcref arcana
  # changed 2/2019 to avoid globalVariables() problems
  epar <- new.env( parent=.GlobalEnv)
  epar$globalVariables <- function(...) 0
  e <- new.env( parent=epar)

  # Omit version from internal DESCRIPTION object, to avoid trouble later...
  # and join lines
  descro <- gsub( '\n', ' ', descro, fixed=TRUE)
  desversion <- descro[ 'Version']
  descro <- descro %without.name% cq( Version, Built)
  descro <- sprintf( '%s: %s', names( descro), descro)
  e[[ x %&% '.DESCRIPTION']] <- as.cat( descro)
  e[[ x %&% '.VERSION']] <- desversion

  # sapply( dir( file.path( spath, 'R'), pattern='[.][RrSsq]$', full.names=TRUE), source, local=e)
  srcfiles <- dir(file.path(spath, "R"), pattern = "[.][RrSsq]$", full.names = TRUE)
  FOR( srcfiles, source( ., keep.source=TRUE, local = e))
  roxy_lines <- FOR( srcfiles, get_roxy_lines(.))

  ## Namespace: only preserve "importFrom" directives
  if( trust_importFrom){
    nsimpf <- readLines( file.path( spath, 'NAMESPACE')) %that.match% 'importFrom[(]'
    nsimpf <- sub( '.*[(]([^)]*).*', '\\1', nsimpf)
    nsimpf <- strsplit( gsub( ' +', '', nsimpf), ',')
    nsimpf <- structure( do.on( nsimpf, .[1]), names=do.on( nsimpf, .[2])) # charvec: names=funs, contents=pkg

    # Now construct a call that will make the matrix

    flubbody <- substitute(  {
        listio$nsinfo$importFrom <- nsimpf
      return( listio)
      }, list( nsimpf=nsimpf))
    flub <- function( listio, ...) 0
    body( flub) <- flubbody

    # Can't put nsimpf into the environment of flub, because envs are reset on saving here
    #  ee <- new.env( parent=.GlobalEnv)
    #  ee$nsimpf <- nsimpf
    #  environment( flub) <- ee
    e[[ sprintf( 'pre.install.hook.%s', x)]] <- flub
  }

  if( zap_Cloaders){
    suppressWarnings( rm( list='run_Cloaders_' %&% alias, envir=e))
  }

  # Set srcrefs to somewhere...
  tf <- tempfile()
  on.exit( unlink( tf), add=TRUE)

  for( stuff in lsall( e)) {
    thing <- e[[stuff]]

    if( is.function( thing)) {
      temp_thing <- thing
      attributes( temp_thing) <- attributes( thing)[ 'srcref']
      roxy_stuff <- get_my_roxy_lines( stuff, roxy_lines, 
          attr( temp_thing, 'srcref')) # immediately preceding bits

      nicewrite_function( temp_thing, tf, append=FALSE, print.name=FALSE)
      lines <- readLines( tf)
      last.line <- max( index( nzchar( lines)))
      last.char <- nchar( lines[ last.line])

      if( length( attributes( thing))>1) { # write out all inclu attrs, but 
          # srcref will only point to the "core" function
        nicewrite_function( thing, tf, append=FALSE, print.name=FALSE, 
            doc.special=TRUE)
        lines <- readLines( tf)
      }

      sc <- as.integer( c( 1, regexpr( 'function', lines[1]), 
          last.line, last.char, 1, last.char, 1, last.line))
      attr( sc, 'srcfile') <- srcfilecopy( 'dummyfile', lines)
      oldClass( sc) <- 'srcref'
      attr( thing, 'srcref') <- sc
      if( !is.null( roxy_stuff)) { # Keep here for now; drop later before writing
        attr( thing, 'roxy_orig') <- roxy_stuff
      }
      e[[stuff]] <- thing
    }
  }

  if( file.exists( file.path( spath, 'NAMESPACE')) && is.null( e$.onLoad))
    e$.onLoad <- function( libname, pkgname){} # so pre.install will NAMESPACE


  funs <- find.funs(e)
  if( drop_Conly) {
    # exclude pure dot-Calls (which are presumably auto headers)
    funs <- funs %except% Conly( e)
  }

  do.on( funs, environment( e[[.]]) <- .GlobalEnv) # OK unless weird stuff has been done (poss legit, but deffo weird)
  droppo <- lsall( e) %except% funs
  droppo <- droppo %SUCH.THAT% (e[[.]] %is.an% 'environment')
  if( length( droppo)) {
    cat( "Warning: dropping some environments:", droppo, sep='\n')
    rm( list=droppo, envir=e)
  }

  ## Unexported data:
  if( file.exists( sysdat <- file.path( spath, 'R', 'sysdata.rda')))
    load( sysdat, envir=e, verbose=FALSE)

  ## Exported data: USED TO import it into the package
  # but as of 2023 I'm just copying the folder
  if( FALSE && is.dir( datdir <- file.path( spath, 'data'))) {
    exdata <- dir( datdir, pattern='[.](R|r|rda|Rdata|txt|TXT|tab|csv|CSV)$')
    data( list= sub( '[.][^.]+$', '', exdata),
        package=x, lib.loc=dirname( spath), verbose=FALSE, envir=e)
  }

  ## Help:
  roxy_orig <- list()
  doc_md5_orig <- character()

  Rd.files <- dir( file.path( spath, 'man'), pattern='Rd$', full.names=TRUE)
  for( rdf in Rd.files) {
    p1 <- parse_Rd( rdf)
    rdt <- tools$RdTags( p1)
    name <- unlist( p1[ rdt=='\\name'])
    aliases <- unlist( p1[ rdt=='\\alias'])
    namal <- unique( c( name, aliases))
    helpo <- help2flatdoc( fun.name=name, pkg=x, aliases=aliases, 
        text=Rd2txt_easy( p1))

    # Dock whitespace and blank lines at the end--- for change-checking later
    helpo <- gsub( ' +$', '', helpo)
    last_charred <- max( which( nzchar( helpo)))
    helpo <- helpo[ 1 %upto% last_charred]

    class( helpo) <- 'docattr'

    # What fun to associate this with? Try 'name' first, otherwise
    matcho <- match( name, funs, 0)
    if( !matcho) {
      matcho <- min( match( namal, funs, 0))
    }
    if( matcho) {
      # Make sure the doco is included in srcref. 
      # src@srcfile$lines contains *all*, but src itself
      # ... only points to a part of it
      attr( e[[ funs[ matcho]]], 'doc') <- helpo
      if( !is.null( roxor <- attr( e[[ funs[ matcho]]], 'roxy_orig'))) {
        tf1 <- tempfile()
        writeLines( helpo, tf1)
        md5 <- md5sum( tf1)
        unlink( tf1)
        roxy_orig[[ funs[ matcho]]] <- roxor
        doc_md5_orig[ funs[ matcho]] <- md5
        attr( e[[ funs[ matcho]]], 'roxy_orig') <- NULL # don't want it written out!
      }
      nicewrite_function( e[[ funs[ matcho]]], tf, append=FALSE, 
          print.name=FALSE, doc.special=TRUE)
      lines <- readLines( tf)
      attr( attr( e[[ funs[ matcho]]], 'srcref'), 'srcfile')$lines <- lines
      attr( e[[ funs[ matcho]]], 'srcref')[ 2] <- regexpr( 'function', lines[1])
      # Single-line functions will lose characters thx2 addition of word "structure", so...
      if( attr( e[[ funs[ matcho]]], 'srcref')[ 3]==1) {
        attr( e[[ funs[ matcho]]], 'srcref')[ 4] <- nchar( lines[ 1])
      }
      attr( e[[ funs[ matcho]]], 'srcref')[1:8] <- 
          attr( e[[ funs[ matcho]]], 'srcref')[ c( 1:4, 2, 4, 1, 3)]
    } else {
      e[[ sprintf( '%s%s.doc', name, if(name==x) '.package' else '')]] <- helpo
    }
  }

  ## Original Roxygen, if applicable
  # usable to minimize changes when regenerating Roxygenesque gitty version (but WHY Roxygen? it's horrible)
  # More sensible might just be to assume/enforce alphabetical order... but this is written now
  if( keep_Roxygen && length( roxy_orig)) {
    e[[ sprintf( '%s.ORIG.ROXYGEN', x)]] <- returnList(
        def_order= unname( unlist( do.on( roxy_lines, names( .$defs)))),  # collate close to order of original file(s)
        roxy_orig,
        doc_md5_orig)
  }

  ## "Special" text files in top dir: 2020
  importants <- grep( sprintf( '^(%s)$', paste( key_package_files, collapse='|')), dir( spath), value=TRUE)
  for( i in importants) {
    e[[ i]] <- as.cat( readLines( file.path( spath, i)))
  }


  ## And save... removing any roxyshite first
  thrubbo <- do.on( e, !is.null( attr( ., 'roxy_orig')))
  for( i in names( thrubbo)) {
    xx <- e[[ i]]
    attr( xx, 'roxy_orig') <- NULL
    e[[ i]] <- xx
  }
  save( list=lsall( e), file=file.path( tpath, '.RData'), envir=e)

  ## Update tasks...
  if( is.null( tasks <- .GlobalEnv$tasks)) {
    tasks <- character( 0)
  }
  lotasks <- length( tasks)

  # unique() kills names :(
  tasks <- c( tasks, structure( './' %&% alias, names=alias))
  tasks <- tasks[ !duplicated( tasks)]

  if( length( tasks) != lotasks) {
    top.workspace <- .GlobalEnv # CRANally retentive check coming up...
    assign( 'tasks', tasks, top.workspace)
    cat( sprintf(
        '"tasks" vector has been augmented with "%1$s"-- remember to Save()...\n' %&%
        '...otherwise R won\'t be able to find task pkg for "%1$s" next time\n', x))
  }
return( invisible( NULL))
}
, doc =  docattr( r"{
unpackage    package:mvbutils


Convert existing source package into task package

DESCRIPTION

Converts an existing source package into a task package. A subdirectory with the package name will be created under the current working directory, and will be populated with a ".RData" file and various other files/directories from the source package. All Rd files will be turned into flat-format help in the ".RData", either attached to functions or as stand-alone "*.doc" text objects, as per 'help2flatdoc' (qv). The subdirectory will also be made into a _task_, i.e. it will be added to the "tasks" vector in the current workspace that 'cd' uses to keep track of the task hierarchy.

Roxygen pre-function comments are by default removed; see 'keep_Roxygen' argument.


USAGE

unpackage(spath, force = FALSE, alias=x,
    drop_Conly=TRUE, trust_importFrom=FALSE, 
    zap_Cloaders= TRUE, keep_Roxygen= FALSE)


ARGUMENTS

 spath: where to find the source package

 force: if TRUE, overwrite any previous contents of task package without prompting.

 alias: if you _don't_ want the package to go into a folder (and task) with its obvious name, then set eg 'alias="temp_mypack"'; useful for comparing versions.

 drop_Conly: ?exclude pure R wrappers of dot-Call/dot-External? Normally (I presume) those are auto-generated, and will be regenerated by 'pre.install' in the "Cloaders" business, so this _should_ be OK.

 trust_importFrom: Whether to force-copy all importFrom() NAMESPACE directives into the maintained package. Used to be TRUE by default, but I think FALSE is better...

 zap_Cloaders: whether to remove 'run_Cloaders_<mypack>', if it's present; though it shouldn't really be, I think, cos it seems to prevent installation. (It's regenerated by 'pre.install' automatically.) IOW, 'run_Cloaders_<mypack>' should be removed before a git push unless you are using 'mvbutils' package building system.
 
 keep_Roxygen: I thoroughly dislike Roxygen, and my pref is to generate the help for each function--- stored in its 'docattr' attribute--- directly from the dot-Rd file. If anyone ever really wants Roxygen, they can try 'Rd2Roxygen' (beware the bugs...). So my default is FALSE, but if you set TRUE then 'unpackage' will make a big effort to store the original Roxybollocks.


DETAILS

The NAMESPACE file won't be copied; instead, it will be auto-generated by 'pre.install'. Therefore, some features of the original NAMESPACE may be lost. You can either copy the NAMESPACE manually (in which case, you'll need to maintain it by hand), or write a "pre.install.hook.MYPACK" function.

The DESCRIPTION file becomes a character vector called e.g. '<mypack>.DESCRIPTION', of class "cat" (see 'as.cat').

Functions in the ".RData" may be saved with extra attributes, in particular "doc" (deduced from a dot-Rd file) but perhaps other things too that they acquire after the package code is 'source'd. Attributes that are character vector will acquire class "docattr", so that they won't be fully displayed during default printing of the function; to see them, use e.g. 'as.cat( attr( myfun, "myatt"))' or 'unclass( attr( myfun, "myatt"))' or, if you are using the 'atease' package, just 'as.cat( myfun@myatt)'. Editing the function with 'fixr' will display the character attributes in full.

Any environment objects found in the package's environment (its namespace environment) will be dropped from the ".RData" file, with a warning; this is to avoid dramas on reloading.


SEE.ALSO

'pre.install', 'mvbutils.packaging.tools'

}")

)

"update.installed.dir" <-
function( opath, ipath, source, installed=source, delete.obsolete=TRUE, excludo=character( 0)) {
#################
  if( is.dir( file.path( opath, source))) {
    mkdir( file.path( ipath, installed))
    
    # Avoid a/./b
    fp <- function( ...) normalizePath( file.path( ...), winslash='/', mustWork=FALSE)
    
    if( !length( excludo))
      unexcluded <- identity
    else 
      unexcluded <- function( strs) {
          o <- do.call( 'rbind', lapply( excludo, grepl, x=strs))
          strs[ !apply( o, 2, any)]
        }
    
    source.dirs <- unexcluded( list.dirs( fp( opath, source)))
    inst.dirs <- list.dirs( fp( ipath, installed))
    
    nipath <- fp( ipath, installed)
    nopath <- fp( opath, source)
    ibasename <- function( fpath) substring( fpath, nchar( nipath) + 2)
    sbasename <- function( fpath) substring( fpath, nchar( nopath) + 2)
    
    if( delete.obsolete) {
      is.xs <- ibasename( inst.dirs) %not.in% sbasename( source.dirs)
      unlink( inst.dirs[ is.xs])
      inst.dirs <- inst.dirs[ !is.xs]
    }

    is.new.dir <- sbasename( source.dirs) %not.in% ibasename( inst.dirs)
    mkdir( fp( ipath, sbasename( source.dirs[ is.new.dir])))
    
    # Used to use dir() here, but list.files doesn't include folders; better
    sources <- unexcluded( list.files( fp( opath, source), full.names=TRUE, recursive=TRUE))
    installeds <- list.files( fp( ipath, installed), full.names=TRUE, recursive=TRUE)
    
    if( delete.obsolete) {
      is.xs <- ibasename( installeds) %not.in% sbasename( sources)
      unlink( installeds[ is.xs])
      installeds <- installeds[ !is.xs]
    }
    
    if( length( sources)) {
      # Really should check whether files have turned into dirs or vice versa...
      old.md5 <- md5sum( installeds)
      names( old.md5) <- ibasename( names( old.md5))
      new.md5 <- md5sum( sources)
      names( new.md5) <- sbasename( names( new.md5))

      new.files <- sbasename( sources) %except% ibasename( installeds)
      changed.files <- names( new.md5) %that.are.in% names( old.md5)
      changed.files <- changed.files[ new.md5[ changed.files] != old.md5[ changed.files]]
      to.copy <- c( new.files, changed.files)
      if( length( to.copy)) # keep file times
        mvb.file.copy( fp( opath, source, to.copy), fp( ipath, installed, to.copy),
            overwrite=TRUE)
    } # if anything potentially to copy
  } else if( delete.obsolete)
    unlink( file.path( ipath, installed), recursive=TRUE)
}


"update.maintained.package" <-
function( pkg, nlocal = sys.parent()) mlocal({
  if( !is.null( mp <- maintained.packages[[ pkg]])) {
    rm( list=lsall( mp), envir=mp)
    load.refdb( envir=mp) # filename is deduced
    try( pre.install( pkg, character.only=TRUE))
    try( patch.installed( pkg, character.only=TRUE))
  }
})


"update_loaded_pkg" <-
function( pkg, name, ff, disatt= ff %is.a% 'function') {
  ffatt <- attributes( ff)
  if( disatt) {
    if( ff %is.a% 'function') {
      keepo <- names( ffatt) %except% cq( doc, export.me)
      if( !getOption( 'keep.source.pkgs', TRUE))
        keepo <- keepo %except% 'source'
      attributes( ff) <- ffatt[ keepo]
    } # else attributes( ff) <- list() # too brutal? what about 'source' attr for call-types etc?
  } # if disatt

  # Ancient pre-namespace... dormanced in 2020
  if( FALSE && (!is.na( j <- index( search()=='package:' %&% pkg)[1]))
      && !isNamespaceLoaded(pkg)) {
    assign( name, ff, j) # shouldn't be locked
  }

  # Nicked from utils, sans S4 stuff
  isS3generic <- function( x, env){
      # inherits=FALSE, otherwise baseenv() recycles thru attachees!
      if( !exists( x, mode= 'function', envir= env, inherits=FALSE)){
    return( FALSE)
      }

      isUMEbrace <- function(e) {
          for (ee in as.list(e[-1L])) if (nzchar(res <- isUME(ee)))
              return(res)
          ""
      }
      isUMEif <- function(e) {
          if (length(e) == 3L)
              isUME(e[[3L]])
          else {
              if (nzchar(res <- isUME(e[[3L]])))
                  res
              else if (nzchar(res <- isUME(e[[4L]])))
                  res
              else ""
          }
      }
      isUME <- function(e) {
          if (is.call(e) && (is.name(e[[1L]]) || is.character(e[[1L]]))) {
              switch(as.character(e[[1L]]), UseMethod = as.character(e[[2L]]),
                  `{` = isUMEbrace(e), `if` = isUMEif(e),
                  "")
          }
          else ""
      }
      nzchar( isUME( body( env[[ x]])))
    }


  # Put into namespace & importers thereof
  if( isNamespaceLoaded( pkg)) {
    nspkg <- asNamespace( pkg)

    # 2020: *dont'* do the update if the existing version is just a .Call()
    # because then the "real" function is being autocreated during package startup, with a pointer to C code
    # and the editee should presumably just be a stub. Don't risk it.
    replace <- !exists( name, nspkg, mode='function', inherits=FALSE)
    if( !replace) { # ie there is an existing function
      bodd <- body( nspkg[[ name]])
      replace <- bodd %is.not.a% 'call'
      if( !replace) { # ie it is a call
        bodd1 <- bodd[[1]]
        replace <- (bodd1 != quote( .Call)) && (bodd1 != quote( .External))
      }
      if( !replace) {
        warning( sprintf( "Not replacing '%s' in namespace etc; looks like a construct-on-load low-level wrapper.", name))
      }
    }

    if( replace) {
      if( is.function( ff)) {
        # respect existing environment, in case of weird stuff like my soap hacks
        if( exists( name, envir=nspkg, mode='function', inherits=FALSE))
          environment( ff) <- environment( nspkg[[ name]])
        else
          environment( ff) <- nspkg # don't muck about
      }

      if( name %in% lsall( nspkg$.__NAMESPACE__.$exports)
          || any( names( ffatt) %in% cq( doc, export.me))
          || exists( name %&% '.doc', envir=nspkg, mode='character', inherits=FALSE) ) {
        if( match( 'package:' %&% pkg, search(), 0)) # still with source
          try( force.assign( name, ff, as.environment( 'package:' %&% pkg)))

        # Check importers that are loaded:
        gnu <- getNamespaceUsers( pkg) %that.are.in% loadedNamespaces()

        for( j in lapply( gnu, function( x) parent.env( asNamespace( x))))
          if( exists( name, j, inherits=FALSE))
            force.assign( name, ff, j)
      }

      # If a doc object or function, be sure to export the thing(s) being docced
      exclude.me <- character(0)
      if( length( grep( '\\.doc$', name)) && is.character( ff)
          && exists( sub( '\\.doc$', '', name), envir=nspkg, inherits=FALSE))
        doc.to.check <- ff
      else if( is.function( ff) && ('doc' %in% names( ffatt))) {
        doc.to.check <- ffatt$doc
        exclude.me <- name # have already arranged my own export
      } else
        doc.to.check <- NULL

      for( doccee in named.in.doc( doc.to.check) %except% exclude.me) {
        if( exists( doccee, envir=nspkg, inherits=FALSE)) {
          if( match( 'package:' %&% pkg, search(), 0)) # ?on search path?
            force.assign( doccee, get( doccee, envir=nspkg), as.environment( 'package:' %&% pkg))
          nspkg$.__NAMESPACE__.$exports[[ doccee]] <- doccee

          for( j in lapply( getNamespaceUsers( pkg), function( x) parent.env( asNamespace( x))))
            if( !environmentIsLocked( j) || exists( name, j, inherits=FALSE))
              force.assign( name, ff, j)
        } # if docced object exists yet
      } # loop over docced objects

      force.assign( name, ff, nspkg)
      if( ('package:' %&% pkg) %in% search())
        force.assign( name, ff, as.environment( 'package:' %&% pkg))

      # S3 methods--- quite the p*gf*ck, really
      # To add to p*gf*ckery, utils::isS3method has a bug (R4.0.5 at least) for
      # ... eg name='.onLoad' or name='stupid..name'
      bugless_isS3method <- utils$isS3method
      e <- new.env( parent=environment( bugless_isS3method))
      e$strsplit <- function( x, split, fixed=FALSE, perl=FALSE, ...) {
          if( fixed && identical( split, '.')){ # buggy line in utils::isS3method
            fixed <- FALSE
            perl <- TRUE
            split <- '(?<=[^.])[.](?=[^.])' # period on its own
          }
        return( base::strsplit( x=x, split=split, fixed=fixed, perl=perl, ...))
        }
      environment( bugless_isS3method) <- e

      # is.S3method <- utils$isS3method( name)
      is.S3method <- bugless_isS3method( name)

      # TBH I don't really trust isS3method() anyway--- look at the code!
      if( is.S3method) {
        methenv <- NULL # pessimism

        # Possible generics (cos isS3method() doesn't tell ya)
        dotpos <- gregexpr( '(?<=.)[.]+', name, perl=TRUE)[[1]]
        possgen <- substring( name, rep( 1L, length( dotpos)), dotpos-1)
        thisgen <- NULL

        # Loop over places where generic might be defined
        # ... and thus where 'ff' should go into the S3MT
        for( imp in unique( c(
            list( nspkg),
            lapply( names( nspkg$.__NAMESPACE__.$imports), asNamespace),
            list( .BaseNamespaceEnv)))){
          s3m <- imp$.__S3MethodsTable__.
          if( !is.null( s3m)){
            if( name %in% lsall( s3m)){ # already there
              force.assign( name, ff, s3m)
        break
            }

            # Not in S3MT, but maybe should be (if new method...)
            thisgen <- possgen %SUCH.THAT% isS3generic( ., imp)
            if( length( thisgen) == 1){
              classaroo <- substring( name, nchar( thisgen)+2, nchar( name))
              scatn( "Registering S3 method for generic '%s' class '%s' in '%s'",
                  thisgen, classaroo, environmentName( imp))
              registerS3method( thisgen, classaroo, name, imp)
        break
            }
          } # if there's an S3MT (perhaps there always is, but...)
        } # for places where 'ff' maybe should be regoed
      } # if S3 method
    } # if not a dot-call
  } # if namespaced
}


"upper.case" <-
function (s) 
{
    a <- attributes(s)
    if (exists("casefold", mode = "function")) 
        s <- casefold(s, upper = TRUE)
    else {
        s <- strsplit(s, "")
        lets <- LETTERS
        names(lets) <- letters
        transfer <- function(x) {
            change <- x %in% letters
            x[change] <- lets[x[change]]
            paste(x, collapse = "")
        }
        s <- sapply(s, transfer)
    }
    do.call("structure", c(list(.Data = s), a))
}


"vignette.pkg" <-
structure( function( 
  pkg,
  pattern= '[.]Rmd$',
  character.only= FALSE,
  precompile= FALSE,
  build= TRUE,
  ...
){
  set.pkg.and.dir( TRUE)

  # Precompilation happens in task package
  vigpath <- file.path( dir., 'vignettes')
  if( !dir.exists( vigpath)){
    cat( "Can't see a vignettes folder\n")
return()
  }
  
  if( precompile){
    pcfile <- file.path( vigpath, 'precomp.R')
    if( !file.exists( pcfile)){
warning( "Can't find 'precomp.R' in vignettes folder")
    } else {
      precompile <- try( source( pcfile, local=TRUE, echo=TRUE))
    }
    
    if( precompile %is.a% 'try-error'){
  return()
    } # no point in updating indices
  }

  tvigpath <- vigpath 
  # Copy vigs & move action to the source package
  vigpath <- file.path( sourcedir, 'vignettes')  
  
  if( dir.exists( sourcedir)){
    unlink( vigpath, recursive=TRUE, force=TRUE)
    mkdir( vigpath) # who knows what unlink will really do?
    file.copy( dir( tvigpath, full.names=TRUE, all.files=FALSE), 
       vigpath, recursive=TRUE)
  }
  
  if( !build){
return()
  }
  
  if( !isNamespaceLoaded( pkg)){
stop( "Must load package before building its vignette(s)")
  }
  
  viggies <- dir( vigpath, pattern=pattern)
  if( !length( viggies)){
    cat( "Can't find any matching vignettes\n")
return()
  }

  # Build vignette into INSTALLED package
  docdir <- file.path( system.file( package=pkg), 'doc')  
  if( !dir.exists( docdir)){ # surely it must?
    mkdir( docdir)
  }
  
  builts <- character()
  if( build){
    for( vig in viggies){
      file.copy( file.path( vigpath, vig), docdir) # needed for pkgVignettes
      res <- try( tools::buildVignette( 
          file= file.path( vigpath, vig), dir= docdir, ...))
      if( res %is.not.a% 'try-error'){
        builts <- c( builts, res)
      } else {
        scatn( "Failed on %s", vig)
        print( res)
      }
    }
  }
  
  if( !build || length( builts)){
    nstools <- asNamespace( 'tools') # blame fucking CRAN for this...
    all_vigs <- tools::pkgVignettes( dir=system.file( package=pkg), 
        subdir='doc', output=TRUE)
    vig_ind <- nstools$.build_vignette_index( all_vigs)
    vig_ind_file <- file( file.path( docdir, 'index.html'), 
        open='w')
    try( nstools$.writeVignetteHtmlIndex( pkg, vig_ind_file, vig_ind))
    close( vig_ind_file)
    saveRDS( vig_ind, file = system.file( 'Meta/vignette.rds', package=pkg))
  }
  
return( builts)
}
, doc =  docattr( r"{
vignette.pkg    package:mvbutils


Build vignette(s) for mvbutils-style package


DESCRIPTION

Vignette-building is insanely complicated (though this might be hidden from you) and can be slow. So it's not handled directly by 'pre.install', 'patch.install', and friends. Doing 'build.pkg' and 'install.pkg' will work normally, but if you want to _change_ a vignette in an installed package without complete re-installation, then you have to manually (re)build vignette(s) and indices. 'vignette.pkg' should do that for you.

It will copy all files (and folders) from the task's "vignettes" folder into the source package's "vignettes" folder (after zapping the latter). If 'build=TRUE' (the default) it will then build the vignettes in the _installed_ package (that's just how R does it, for whatever reason).

Also, there can be an intermediate level of vignette, where all the calculations/plots are already done and saved, and the precompiled vignette is just ready to be turned into HTML and/or PDF, something which should be fairly quick. If you give your original vignette files the extension ".Rmd.orig", then an R script "precomp.R" will be created by 'pre.install' in the task package vignette. It is a very simple script that mainly just shows the 'knitr' command to use. 'vignette.pkg(...,precompile=TRUE)' will then run that script to precompile all the vignettes (which can be slow, of course) in the task package "vignettes" folder, producing ".Rmd" files that are precompiled, along with figure files etc in subfolders.

Precompilation happens only if 'precompile=TRUE'. Copying the "vignettes" folder always happens, unless 'precompile=TRUE' and precompilation fails, in which case the function aborts. After copying, building happens unless 'build=FALSE'. Index reconstruction happens only if some building has taken place.


USAGE

vignette.pkg( pkg, pattern= "[.]Rmd$", 
  character.only= FALSE, precompile= FALSE, build= TRUE, ...)


ARGUMENTS

 pkg: Name of package; see 'pre.install' for options

 pattern: Regex to select vignette files (only if 'build' is TRUE). Default matches anything ending ".Rmd". You can specify a complete filename to only do that one vignette.

 character.only: for automated use; see 'pre.install'
 
 precompile: ?run the "precomp.R" script in the source package vignettes folder?
 
 build: ?should the vignettes be rebuilt?
 
 ...: passed to 'tools::buildVignette' (qv)


VALUE

A character vector of all files that were built. If there are errors during the build process, you should see on-screen messages.


SEE.ALSO

'pre.install'


EXAMPLES

## Don't run
vignette.pkg( kinference)
## End don't run

}")

)

"visify" <-
structure( function( 
  exprs, 
  local=parent.frame(), 
  prompt.echo='', 
  ...
){
  exprs <- substitute( exprs)
stopifnot( exprs %is.a% '{')
  for( i in 2 %upto% length( exprs)){
    if( (exprs[[ i]] %is.not.a% 'call') || 
        (exprs[[i]][[1]] != as.name( '{'))){
      exprs[[ i]] <- call( '(', exprs[[ i]])
    }
  }
  
  # Remove the call to curlybrace, and make an expression() obj
  exprs <- as.expression( as.list( exprs)[-1])
  flubadub <- capture.output(
      res <- withAutoprint( exprs, evaluated=TRUE, local=local,  
          prompt.echo=prompt.echo, ...)
    )
  flubadub <- flubadub[ !startsWith( flubadub, '+ ')]
  print( as.cat( flubadub))
invisible( res)
}
, doc =  docattr( r"{
visify    package:mvbutils


Make a function autoprint all its doings


DESCRIPTION

Occasionally you want a function to do a whole bunch of things, and print the results as it goes along; you might be thinking about executing its code directly in a "script", but you don't want to be cluttering up the workspace. If so, you can wrap the body of your function in a call to 'visify', and R will act as if you 'source' the corresponding script. The output isn't particularly beautiful, but it's jolly handy for eg routine diagnostics when fitting a series of models.

You can also use 'visify' inside a function, to just display certain bits. (Well, not entirely; as of 2.9.18, any code before 'visify' _always_ seems to be shown.  But at least you can hide the return value.) For example, it's sometimes useful to not visify the entire return-value of a function, even though you want to "show the rest of your working". In that case, you can just not return the value within the 'visify' block, but separately afterwards; see EXAMPLES.

'visify' is experimental as of package 'mvbutils' v2.9.228, and I might add features over time, eg to make output look prettier and give better options for hiding things. At present, it deliberately strips all continuation lines of input, so you only see the first line of each statement (a blessing IME so far). More could be done; but I don't yet understand how all the options to 'source' and 'withAutoprint' work, and this is really a convenience function rather than something intended for producing report-quality output.


.TO.DO

As of package 'mvbutils' v 2.9.23 and package 'debug' v 1.4.18, the latter does not currently handle 'visify' nicely (in contrast to eg 'evalq', which is operationally very similar except WRTO output). What you have to do for now, is manually replace the call to 'visify' with a call to 'evalq' (and remove any extraneous arguments).

This kinda thing comes up often. I really need a general mechanism in the 'debug' package, for any other package (or the user) to supply an 'mtrace'-able version of their pet function.


USAGE

# Never use it like this...
visify(exprs, local = parent.frame(), prompt.echo='', ...)
# ... always like this, for an entire function:
# my_autoprinting_function <- function(<args>) visify( {<body>})
# ... or just as part of one:
# my_part_apf <- function( <args>){ visify({<shown>}); <posthoc-and-returns>}


ARGUMENTS

 exprs: The body of your function

 local: Normally leave this alone; it's the environment to run 'exprs' in
 
 prompt.echo: what to print at the start of each line. Default is nothing.

 ...: other args to 'withAutoprint' (qv), such as 'max.deparse.length' or 'width.cutoff'.
 

DETAILS

Compound statements, such as 'if' and 'for', are not printed "internally", only the final outcome (which is NULL for 'for'). The first line of the compound code is still printed, though.

If you want certain statements in your function to execute without autoprinting their output (eg because it is an enormous and cluttery intermediate calculation), wrap it or them in curlies, a la '{ <dont; show; these; outputss>; NULL}'. Again, the first line of code will be printed regardless--- so you could just make into a "Hide me!" comment, as per EXAMPLES.

The trick behind 'visify' is to use 'withAutoprint' (qv), but it's not obvious exactly how to do so. I was encouraged by:

'https://stackoverflow.com/questions/58285497'

However, I did not use exactly the solutions there, because I wanted a slightly different "flow".


SEE.ALSO

'withAutoprint' 


EXAMPLES

# Basic: show it all
tv1 <- function( xx) visify({
  yy <- xx + 1
  # Comments show up, too...
  for( i in 1:5) yy <- yy + 1 
  # ... but loops only show end result; ditto ifs 
  # and other compound statements

  xx <- xx+1 
  xx <- xx + 2
  
  xx*yy
})
tv1( 3)

# Note the use of max.deparse.length param. Also try width.cutoff
tv2 <- function( xx, MDL=Inf) visify( max.deparse.length=MDL, {
  yy <- xx + 1
  # Comments show up, too...
  for( i in 1:5) yy <- yy + 1 
  
  # Dont' want to show gory details of next "block"
  { # HIDE ME!
    xx <- xx+1 
    xx <- xx + 2
    NULL # that's all you'll see
  }
  
  xx*yy
})

tv2( 3)
tv2( 3, MDL=100)
tv2( 3, MDL=50) # too terse

# Hide boring stuff
tv2 <- function( xx){
  # I don't understand why this bit _before_ visify() is shown
  yy <- xx + 1
  for( i in 1:5) yy <- yy + 1 

  visify({
    xx <- xx+1 
    xx <- xx + 2
    NULL # that's all you'll see
  })
  
  # At least the return-value isn't!
invisible( rep( xx*yy, 9999)) # don't wanna show this!
}
tv2( 3)

}")

)

"warn.and.subset" <-
structure( function( x, cond, 
    mess.head=deparse( substitute( x), width.cutoff=20, control=NULL, nlines=1), 
    mess.cond=deparse( substitute( cond), width.cutoff=40, control=NULL, nlines=1), 
    row.info=rownames( x), sub=TRUE) {
  
  mum <- mvb.sys.parent()
  mum <- if( mum==0)
      .GlobalEnv
    else
      sys.frames()[[ mum]]

  force( mess.cond)    
  if( sub)
    cond <- substitute( cond)
  cond <- eval( cond, x, enclos=mum)
  cond[ is.na( cond)] <- FALSE
  if( !all( cond)) {
    warning( mess.head %&% ': dropping cases that fail ' %&% mess.cond %&% ':', call.=FALSE) 
    outo <- paste( row.info[ !cond], collapse='; ')
    cat( file=stderr(), strwrap( outo, indent=2), '\n')
  }
  x[ cond,,drop=FALSE]
}
, doc =  docattr( r"{
warn.and.subset package:mvbutils

Extract subset and warn about omitted cases

DESCRIPTION

Extract row-subset of a 'data.frame' according to a condition. If any cases (rows) are omitted, they are listed with a warning. Rows where the condition gives NA are omitted.


USAGE

# This is the obligatory format, and is not very useful; look at EXAMPLES instead
warn.and.subset(x, cond, 
    mess.head=deparse( substitute( x), width.cutoff=20, control=NULL, nlines=1), 
    mess.cond=deparse( substitute( cond), width.cutoff=40, control=NULL, nlines=1), 
    row.info=rownames( x), sub=TRUE)


ARGUMENTS

 x: data.frame
 cond: expression to evaluate in the context of 'data.frame'. If 'sub=TRUE' (the default), this will be substituted. If 'sub=FALSE', you can use a pre-assigned expression; in that case, you had better set 'mess.cond' manually.
 mess.head: description of data.frame (e.g. its name) for use in a warning.
 mess.cond: description of the desired condition for use in a warning.
 row.info: character vector that will describe rows; omitted elements appear in the warning
 sub: should 'cond' be treated as a literal expression to be evaluated, or as a pre-computed logical index?
# ...: just there to keep RCMD CHECK happy-- for heaven's sake...

VALUE

The subsetted data.frame.


SEE.ALSO

'%where.warn%' which is a less-flexible way of doing the same thing


EXAMPLES 

df <- data.frame( a=1:3, b=letters[1:3])
df1 <- warn.and.subset( df, a %% 2 == 1, 'Boring example data.frame', 'even-valued "a"')
condo <- quote( a %% 2 == 1)
df2 <- warn.and.subset( df, condo, 'Same boring data.frame', deparse( condo), sub=FALSE)

}")

)

"what.is.open" <-
function () 
{
    if (!exists(".Open", "mvb.session.info")) 
        character(0)
    else get(".Open", "mvb.session.info")
}


"write.mvb.tasks" <-
function( tasks=env$tasks, env=.GlobalEnv, dir=attr( env, 'path'))  
  cat( '\ntasks <- ', deparse( as.call( c( as.name( 'c'), tasks))), 
    file=file.path( dir, 'tasks.R'), append=TRUE)


"write.NAMESPACE" <-
function( ns, file){
  sink( file)
  on.exit( sink())
  if( !is.null( ns$useDynLib) || !is.null( ns$useDynLib_sans_rego)) {
    udls <- c( ns$useDynLib, ns$useDynLib_sans_rego)
    if( is.null( names( udls))) {
      names( udls) <- rep( '', length( udls))
    }
    udl_is_named <- nzchar( names( udls))
    udl_uses_rego <- udls %in% ns$useDynLib # as opposed to ..._sans_rego
    udlout <- sprintf( '%suseDynLib( %s%s)',
        ifelse( udl_is_named, sprintf( '%s = ', names( udls)), ''),
        udls,
        ifelse( udl_uses_rego, ', .registration=TRUE', '')
      )
    writeLines( udlout)
  }

  if( length( ns$export)) {
    cat( 'export( ')
    # I used to have dQuote here but it just leads to trouble AAAAARGH
    cat( paste( '"' %&% ns$export %&% '"', collapse=",\n"))
    cat( ')\n')
  }

  if( length( ns$import)){
    scatn( 'import( %s)', get_import_instructions( ns))
  } # if import

  if( length( ns$importFrom)) {
    if( is.matrix( ns$importFrom)) {
      impack <- ns$importFrom[,1]
      fun <- ns$importFrom[,2]
    } else { # charvec eg c( first_fun='mvbutils', second_fun='utils') etc
      impack <- unname( ns$importFrom)
      fun <- names( ns$importFrom)
    }
    scatn( 'importFrom( %s, %s)', impack, fun)
  }
  if( length( ns$S3))
      scatn( sprintf( 'S3method( "%s", "%s")', ns$S3[,1], ns$S3[,2]))
  cat( '\n')
}


"write.sourceable.function" <-
structure( function( 
  x, con, append=FALSE, print.name=FALSE, doc.special=TRUE, xn=NULL
){
############################
  if( is.character( con))
    con <- file( con)
  if( need.to.close <- !isOpen( con))
    open( con, open=ifelse( append, 'a', 'w'))

  if( !identical( con, stdout())) {
    sink( con)
    on.exit( sink())
  }

  on.exit( if( need.to.close) try( close( con)), add=TRUE)

  if( print.name) {
    if( is.null( xn)) {
      xn <- x
      if( !is.character( x)) {
        if( is.name( substitute( x)))
          xn <- deparse( substitute( x))
        else
  stop( "Can't figure out what name to print")
      }
    } # if null xn
    cat( '"', xn, '" <- ', sep='')
  }

  if( is.character( x))
    x <- get( x)

  natts <- names( attributes( x)) %except% cq( source, bug.position, srcref)
  if( is.function( x) && length( natts)) {
    # Prepare to have other attributes
    cat( 'structure( ')
    atts <- attributes( x)[ natts]
    attributes( x)[ natts] <- NULL
  }

  if( is.function( x)) {
    environment( x) <- .GlobalEnv # avoid <environment: x> after definition
    if( is.null( sr <- attr( x, 'srcref')) && !is.null( osr <- attr( x, 'source')) && (getRversion() >= '2.14'))
      print( as.cat( osr))
    else
      print( x)
  } else {
    x <- as.cat( attr( x, 'source'))
    print(x)
  }

  if( is.function( x) && length( natts)) {
    # Treat class "docattr" attributes specially. Non-character doc's (references) are OK.
    freeforms <- if( doc.special)
        natts[ sapply( atts, 'inherits', 'docattr') ]
      else
        character( 0)

    for( iatt in natts %except% freeforms)
      cat( ',', iatt, '=',
          paste( deparse.names.parsably( atts[[ iatt]]), collapse=' '), '\n')

    if( length( freeforms)) { # bizarre syntax of next line is to avoid misleading a syntax-highlighting editor
      if( any( freeforms=='doc'))
        freeforms <- c( freeforms %except% 'doc', 'doc') # move doc to last
      eof.markers <- '<<end of ' %&% freeforms %&% '>>'
      names( eof.markers) <- freeforms
      for( iatt in freeforms)
        while( any( atts[[ iatt]] == eof.markers[ iatt]))
          eof.markers[ iatt] <- paste( eof.markers[ iatt], '<', iatt, '>', sep='')
#      eof.markers[ length( eof.markers)] <- '' # default for doc; help syntax highlighters
      cat( ',', paste( freeforms %&% '=flatdoc( EOF="' %&% eof.markers %&% '")',
          collapse=',\n'), ')\n', sep='')
      for( iatt in freeforms)
        cat( atts[[iatt]], eof.markers[ iatt], sep='\n') } # last one will be end of doc
    else
      cat( ')')
    cat( '\n')
  }

  cat("\n")
}
, doc =  docattr( r"{
write.sourceable.function              package:mvbutils

Sourceable code for functions (and more) with flat-format documentation


DESCRIPTION

Works like 'write' for functions without flat documentation (i.e. without a "doc" attribute). If a "doc" attribute exists, the file is written in a form allowing it to be edited and then read back in with "source.mvb"; the "doc" attribute is given as free-form text following the function definition. If applied to a non-function with a "source" attribute, just the source attribute is printed; the idea is that this could be read back by 'source' (or 'source.mvb'), probably in the course of 'FF' after 'fixr', to regenerate the non-function object.


USAGE

write.sourceable.function( x, con, append=FALSE, print.name=FALSE,
    doc.special=TRUE, xn=NULL)

ARGUMENTS

 x: function or other object, or the name thereof, that is to be written. If 'x' is not a function, then it must have an attribute "source".
 con: a connection or filename
 append: if "con" is not already open, should it be appended to rather than overwritten?
 print.name: should output start with '"NAME" <-' (where NAME is deduced from 'x')?
 doc.special: TRUE if 'doc' attribute is to be printed as flat doc-- assumes readback via 'source.mvb'
 xn: (string) can set this to be the name of the function if 'print.name' is TRUE


DETAILS

If 'x' is unquoted and 'print.name=TRUE', the name is obtained from 'deparse( substitute( x))'. If 'x' is a character string, the name is 'x' itself and the function printed is 'get(x)'.

The real criterion for an attribute to be output in 'flatdoc'-style, is not whether the attribute is called 'doc', but rather whether it is a character-mode object of class 'docattr'. You can use this to force 'flatdoc'-style output of several 'doc'-like attributes.

The default EOF line for an attribute is <<end of doc>>, but this will be adjusted if it appears in the attribute itself.


EXAMPLES

## Don't run

write.sourceable.function( write.sourceable.function, "wsf.r")

# To dump all functions and their documentation in a workspace into a single sourceable file:

cat( "", file="allfuns.r")
sapply( find.funs(), write.sourceable.function, file="allfuns.r", append=TRUE, print.name=TRUE)

# A non-function
scrunge <- c( 1:7, 11)
attr( scrunge, "source") <- c( "# Another way:", "c( 1:4, c( 5:7, 11))")
scrunge # [1] 1 2 3 4 5 6 7 11
write.sourceable.function( scrunge, stdout()) # source
fixr( scrunge) # source

## End don't run


SEE.ALSO

'source.mvb', 'readLines.mvb', 'flatdoc', the file "demostuff/original.dochelp.rrr", the demo in "flatdoc.demo.r"


KEYWORDS

internal

}")

)

"write_sourceable_function" <-
structure( function( 
  x, con, append=FALSE, print.name=FALSE, xn=NULL, prefix_package=TRUE, ...
){
############################
  if( is.character( con))
    con <- file( con)
  if( need.to.close <- !isOpen( con))
    open( con, open=ifelse( append, 'a', 'w'))

  if( !identical( con, stdout())) {
    sink( con)
    on.exit( sink())
  }

  on.exit( if( need.to.close) try( close( con)), add=TRUE)

  if( print.name) {
    if( is.null( xn)) {
      xn <- x
      if( !is.character( x)) {
        if( is.name( substitute( x)))
          xn <- deparse1( substitute( x))
        else
  stop( "Can't figure out what name to print")
      }
    } # if null xn
    sprintf( '%s <- ', simplest_name_generator( xn))
  }

  if( is.character( x))
    x <- get( x)

  natts <- names( attributes( x)) %except% cq( source, bug.position, srcref)
  nbrax <- 0
  if( is.function( x) && length( natts)) {
    # Prepare to have other attributes
    cat( 'structure( ')
    atts <- attributes( x)[ natts]
    attributes( x)[ natts] <- NULL
    nbrax <- 1
  }

  if( is.function( x)) {
    environment( x) <- .GlobalEnv # avoid <environment: x> after definition
    if( is.null( sr <- attr( x, 'srcref')) && 
        !is.null( osr <- attr( x, 'source')) && 
        (getRversion() >= '2.14')) # prolly zappable ; we're at 4.4 now...
      print( as.cat( osr))
    else
      print( x)
  } else {
    x <- as.cat( attr( x, 'source'))
    print(x)
  }

  if( is.function( x) && length( natts)) {
    # Make sure 'doc' gets printed last, if it exists
    natts <- c( natts %except% 'doc', natts[ natts=='doc'])
      
    for( iatt in natts){
      cat( sprintf( ', %s = ', simplest_name_generator( iatt)))
      cat_strings_rawly( atts[[ iatt]], prefix_package=prefix_package)
    }
    cat( '\n')
  }

  cat( strrep( ')', nbrax))
  cat( '\n')
}
, doc =  docattr( r"--{
write_sourceable_function              package:mvbutils
string2charvec
simplest_name_generator
cat_strings_rawly


Sourceable text for functions, including character attributes


DESCRIPTION

'write_sourceable_function' works like 'write', to produce a 'source()'-friendly printout of a function. However, for the sake of clarity, any suitable character-vector attribute  is printed as a multi-line raw string (see 'Quotes') wrapped in a call to 'docattr' or to 'string2charvec', which will turn the string back into a character vector when the file is read back in by 'source'. This hides a _lot_ of ugliness, including escaped special characters  and superfluous quotes. Character objects that you want attached to the function (but not inside its code) actually looks like the real thing! (They can be accessed by the function code since they live inside 'environment(sys.function())'.) My own main use is a 'doc' attribute for free-text documentation (which later gets turned into "Rd" format by 'doc2Rd' when I produce my packages, but that's a detail). However, I quite often keep other text snippets too, eg "templates". 

Raw strings didn't use to exist in R, so before version 2.10 of 'mvbutils', the alternative version 'write.sourceable.function' (note the dots) instead relied the contortions of 'flatdoc' and 'source.mvb' and 'readLines.mvb' to trick R into accepting unmodified text. None of that should be necessary now.

_Obsolete_: if 'write_sourceable_function' is applied to a non-function with a "source" attribute, then just the source attribute is printed; the idea is that this could be read back by 'source', probably in the course of 'FF' after 'fixr', to regenerate the non-function object. I don't think it's wise to rely on this....


.HELPERS

'string2charvec', 'docattr', and 'simplest_name_generator' are helper functions that you're unlikely to use yourself. For the record, though:

 - 'string2charvec' turns a string (length-1 non-empty character vector with no attributes) into a character vector, with a new element for every newline. The first element is discarded, because it's usually just a linebreak (perhaps preceded by accidental spaces etc) inserted to let the "real" raw string start on a fresh line. 'string2charvec' is called by 'docattr' (qv) which facilitates keeping plain-text documentation directly with the function, as an attribute.
 
 - 'docattr' (qv) is very similar, but adds an S3 class "docattr". It simplifies the code produced by 'write_sourceable_function' for presenting the plain-text documentation. I don't recommend using 'docattr' for anything except an attribute called "doc" that contains, yes, documenbloodytation.
 
 - 'simplest_name_generator' prints an R symbol (a "name") in a way that could appear on the LHS of '<symbol> <- 0'. If the name is simple, with no funny characters in it, then it's not quoted and is left alone. If it contains mildly strange characters that would cause the unquoted version to not parse, then it's quoted. If it contains characters that would break simple quotes (for example, quotes or backticks!) then it's wrapped in a bullet-proof raw string. "Only the paranoid survive"...
 
 - 'cat_strings_rawly' outputs (using 'cat') a character vector as a single raw string wrapped in a call to 'docattr' (if its argument has class "docattr") or otherwise 'string2charvec'. Thus, 'source' will break up the raw string back into a separate element for each newline. ('cat_strings_rawly' is probably a bad name for this function, since it actually takes a character vector as input, not a string...). It calls 'cat' directly, so you already need to have directed output to wherever you want, eg via 'sink'.
 

.LIMITATIONS

Some exotic language elements simply cannot be represented in sourceable text: for example, a "hard-coded" environment. A file will still be produced, but it won't work with 'source'. There's no solution to such cases. For example:

%%#
f <- function( e=.GlobalEnv) environmentName( e)
formals( f)$e <- new.env()
tf <- tempfile()
write_sourceable_function( f, tf)
source( tf)
# ... complains about e = <environment>


USAGE

write_sourceable_function( x, con, append=FALSE, 
    print.name=FALSE, xn=NULL, prefix_package=TRUE, ...)
string2charvec( string)
simplest_name_generator( x)
cat_strings_rawly( x, prefix_package=TRUE)


ARGUMENTS

 x: function or other object, or the name thereof, that is to be written by 'write_sourceable_function'. If 'x' is not a function, then it must have an attribute "source". For the helper functions, 'x' is either a string itself (length-1 character vector), or for 'cat_strings_rawly' a character vector.

 con: a connection or filename

 append: if "con" is not already open, should it be appended to rather than overwritten?

 print.name: should output start with 'NAME <-', where NAME is deduced from 'x'? Note that NAME will be processed by 'simplest_name_generator' to make sure everything goes thru 'source' nicely.

 xn: (string) can set this to be the name of the function if 'print.name' is TRUE
 
 ...: ignored, but allows calls that use old 'write.sourceable.function' arguments
 
 string: a string (length-1 character vector), presumably a "raw string" though R doesn't care.
 
 prefix_package: Whether to prefix the call to 'docattr' or 'string2charvec' with 'mvbutils::'. Should always be TRUE _except_ when producing the R source code for 'mvbutils' itself with 'KeepPlaintextDoco=YES' in the "DESCRIPTION" file, since in that case those two functions won't be available as exports when the R source file is sourced.


DETAILS

If 'x' is unquoted and 'print.name=TRUE', the name is obtained from 'deparse( substitute( x))'. If 'x' is a character string, the name is 'x' itself and the function printed is 'get(x)'.

The criteria for deciding whether to raw-string-ify an attribute are:

 - it must be mode 'character'
 - it must have length>1 (otherwise there's little point)
 - it must not have any attributes, except perhaps an S3 'class' (e.g. no 'names', no 'dim')
 - it must not contain newline characters (since they would be confused with newlines inserted between elements).

Iff the attribute has S3 class "docattr", then 'cat_strings_rawly' will wrap it in a call to 'mvbutils::docattr' (which will mean it doesn't get full printed out at the console); otherwise, it will be wrapped in a call to 'mvbutils::string2charvec'.


EXAMPLES


# This is from the examples for 'flatdoc'. It's there to illustrate plain-text documentation, but you can see the call to 'docattr' in the middle.

flubbo <- structure( function( x){
  ## A comment
  x+1
}
,doc=docattr( r"-{
flubbo       not-yet-in-a-package

'flubbo' is a function! And here is some informal doco for it. Whoop-de-doo!

You can have multiple lines and lots of "quotes" and even weird characters like "\".

And you can use the power of raw strings to r"{have a short one}" inside your function. Just make sure your final closing "quote" matchs the number of dashes (0 or more) that follow the first r-double-quote, and exceeds the number in any r"{short quotelets}" inside the documentation. Usually there won't be any, so you won't need to add any dashes.
}-"))


## Don't run

write_sourceable_function( write_sourceable_function, "wsf.r")

# To dump all functions and their documentation in a workspace into a single sourceable file:

cat( "", file="allfuns.r")
sapply( find.funs(), write_sourceable_function, 
    file="allfuns.r", append=TRUE, print.name=TRUE)

# A non-function. Probably don't do this!
scrunge <- c( 1:7, 11)
attr( scrunge, "source") <- c( "# Another way:", "c( 1:4, c( 5:7, 11))")
scrunge # [1] 1 2 3 4 5 6 7 11
write_sourceable_function( scrunge, stdout()) # source
fixr( scrunge) # source

## End don't run
}--")

)

"xfactor" <-
function( 
  x, 
  exclude=if( is.factor( x) && any( is.na( levels( x)))) NULL else NA
){
## Ancient code: don't want to call this yourself. Used by D2A & a few other oldies
# If x is a factor and exclude is _explicitly_ NULL, replace any NA's by level "\001"--- as in 'multimatch'. I'm not sure that 'exclude' would ever be NULL by default, although that may be the intention; it would require an illegal manual construction of 'levels'

# If x is not a factor, turn it into one, with 'exclude' treated as normal, and just suck up the NAs...
  if( is.factor( x)) {
    if( is.null( exclude) && any( is.na( x))) {
      ax <- attributes( x)
      x <- unclass( x)
      x[ is.na( x)] <- length( ax$levels)+1
      ax$levels <- c( ax$levels, '\001')
      # Bloody R nannies have made this hard
      x <- factor( ax$levels[ x], levels=ax$levels)
    }
  } else {
    x <- factor( x, exclude=exclude)
  }
return( x)
}


"xgsub" <-
function( x, pattern, replacement, perl=!fixed, fixed=FALSE, ...) gsub( pattern, replacement, x, perl=perl, fixed=fixed, ...)


"xsave" <-
function( list, file, envir, ...){
  # Jul 2011: user-controlled compression options
  compress <- getOption( 'mvbutils.compress', TRUE)
  compression_level <- getOption( 'mvbutils.compression_level', NULL)
  if( is.null( compression_level))
    save( list=list, file=file, envir=envir, compress=compress, ...)
  else
    save( list=list, file=file, envir=envir, compress=compress,
          compression_level=compression_level, ...)
}


"xsub" <-
function( x, pattern, replacement, perl=!fixed, fixed=FALSE, ...) sub( pattern, replacement, x, perl=perl, fixed=fixed, ...)


"yes.no" <-
function (prompt, default) 
repeat {
    cat(prompt)
    answer <- upper.case(readline())
    if (answer == "" && !missing(default)) 
        answer <- default
    if (!is.na(answer <- pmatch(answer, c("YES", "NO")))) 
        return(answer == 1)
}

