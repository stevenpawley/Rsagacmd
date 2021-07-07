## Test Environments

### winbuilder (old release)

devtools::check_win_oldrelease()
Status: OK
R version 4.0.5 (2021-03-31)

### winbuilder (release)

devtools::check_win_oldrelease()
Status: OK
R version 4.1.0 (2021-05-18)

### local (MacOS 11.4)

R CMD check Rsagacmd_0.1.1.tar.gz --as-cran
* using log directory ‘/Users/steven/GitHub/Rsagacmd.Rcheck’
* using R version 4.1.0 (2021-05-18)
* using platform: aarch64-apple-darwin20 (64-bit)
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘Rsagacmd/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘Rsagacmd’ version ‘0.1.1’
* package encoding: UTF-8
* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: ‘Steven Pawley <dr.stevenpawley@gmail.com>’
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking serialization versions ... OK
* checking whether package ‘Rsagacmd’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
  Running ‘testthat.R’
 OK
* checking PDF version of manual ... OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE

Status: OK

### R-hub builder

Build ID:	Rsagacmd_0.1.1.tar.gz-965f3976a5844600a11a4d9bfa36785f
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	34 minutes 57.3 seconds ago
Build time:	34 minutes 37.1 seconds
Rsagacmd 0.1.1: OK

Build ID:	Rsagacmd_0.1.1.tar.gz-5c69b0bca2314973867695a6c6a1fc84
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	35 minutes 39.2 seconds ago
Build time:	35 minutes 11.2 seconds
Rsagacmd 0.1.1: OK
