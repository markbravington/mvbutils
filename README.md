## Package 'mvbutils': installation instructions

Hello! I now live in the R-universe. You can install me, plus my dependencies, like this:

```
# Tell R where I live
options(repos = unique( c(
    https://github.com/r-universe/markbravington= 'https://https://github.com/r-universe/markbravington.r-universe.dev',
    getOption( 'repos')[ 'CRAN'],
    'https://cloud.r-project.org'
)))
install.packages( "mvbutils")
```
