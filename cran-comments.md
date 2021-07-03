## Test environments
* local R installation, R 4.1.0

## Results
ℹ Updating Rsagacmd documentation
ℹ Loading Rsagacmd
Writing NAMESPACE
Writing NAMESPACE
── Building ────────────────────────────────────────────────────────────────────────────── Rsagacmd ──
Setting env vars:
• CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
──────────────────────────────────────────────────────────────────────────────────────────────────────
✓  checking for file ‘/Users/steven/GitHub/Rsagacmd/DESCRIPTION’ ...
─  preparing ‘Rsagacmd’:
✓  checking DESCRIPTION meta-information ...
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
─  building ‘Rsagacmd_0.1.1.tar.gz’
   
── Checking ────────────────────────────────────────────────────────────────────────────── Rsagacmd ──
Setting env vars:
• _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
• _R_CHECK_CRAN_INCOMING_       : FALSE
• _R_CHECK_FORCE_SUGGESTS_      : FALSE
• NOT_CRAN                      : true
── R CMD check ───────────────────────────────────────────────────────────────────────────────────────
─  using log directory ‘/private/var/folders/5l/3j1jdrfj4cz6m2qgwg96lz6w0000gn/T/RtmpJyOCaw/Rsagacmd.Rcheck’
─  using R version 4.1.0 (2021-05-18)
─  using platform: aarch64-apple-darwin20 (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’
✓  checking for file ‘Rsagacmd/DESCRIPTION’
─  checking extension type ... Package
─  this is package ‘Rsagacmd’ version ‘0.1.1’
─  package encoding: UTF-8
✓  checking package namespace information ...
✓  checking package dependencies (1.4s)
✓  checking if this is a source package
✓  checking if there is a namespace
✓  checking for executable files ...
✓  checking for hidden files and directories
✓  checking for portable file names ...
✓  checking for sufficient/correct file permissions
✓  checking serialization versions
✓  checking whether package ‘Rsagacmd’ can be installed (2.6s)
✓  checking installed package size ...
✓  checking package directory ...
✓  checking for future file timestamps (367ms)
✓  checking DESCRIPTION meta-information ...
✓  checking top-level files ...
✓  checking for left-over files
✓  checking index information
✓  checking package subdirectories ...
✓  checking R files for non-ASCII characters ...
✓  checking R files for syntax errors ...
✓  checking whether the package can be loaded (531ms)
✓  checking whether the package can be loaded with stated dependencies (529ms)
✓  checking whether the package can be unloaded cleanly (524ms)
✓  checking whether the namespace can be loaded with stated dependencies (534ms)
✓  checking whether the namespace can be unloaded cleanly (540ms)
✓  checking dependencies in R code (1.8s)
✓  checking S3 generic/method consistency (857ms)
✓  checking replacement functions (509ms)
✓  checking foreign function calls (539ms)
✓  checking R code for possible problems (3.1s)
✓  checking Rd files ...
✓  checking Rd metadata ...
✓  checking Rd line widths ...
✓  checking Rd cross-references ...
✓  checking for missing documentation entries (512ms)
✓  checking for code/documentation mismatches (1.6s)
✓  checking Rd \usage sections (971ms)
✓  checking Rd contents ...
✓  checking for unstated dependencies in examples ...
✓  checking examples (1.2s)
✓  checking for unstated dependencies in ‘tests’ ...
─  checking tests ...
✓  Running ‘testthat.R’ [132s/133s] (2m 12.8s)
✓  checking for non-standard things in the check directory (2m 12.8s)
✓  checking for detritus in the temp directory
   
   
── R CMD check results ─────────────────────────────────────────────────────────── Rsagacmd 0.1.1 ────
Duration: 2m 32s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓