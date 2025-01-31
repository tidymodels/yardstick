# ldmppr

<details>

* Version: 1.0.3
* GitHub: https://github.com/lanedrew/ldmppr
* Source code: https://github.com/cran/ldmppr
* Date/Publication: 2024-12-02 12:41:19 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::revdep_details(, "ldmppr")` for more info

</details>

## In both

*   checking whether package ‘ldmppr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/emilhvitfeldt/Github/yardstick/revdep/checks.noindex/ldmppr/new/ldmppr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ldmppr’ ...
** package ‘ldmppr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using C++17
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/ldmppr/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/ldmppr/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/ldmppr/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/ldmppr/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c self_correcting_model.cpp -o self_correcting_model.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o ldmppr.so RcppExports.o self_correcting_model.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [ldmppr.so] Error 1
ERROR: compilation failed for package ‘ldmppr’
* removing ‘/Users/emilhvitfeldt/Github/yardstick/revdep/checks.noindex/ldmppr/new/ldmppr.Rcheck/ldmppr’


```
### CRAN

```
* installing *source* package ‘ldmppr’ ...
** package ‘ldmppr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using C++17
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/ldmppr/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/ldmppr/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/ldmppr/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/ldmppr/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c self_correcting_model.cpp -o self_correcting_model.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o ldmppr.so RcppExports.o self_correcting_model.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [ldmppr.so] Error 1
ERROR: compilation failed for package ‘ldmppr’
* removing ‘/Users/emilhvitfeldt/Github/yardstick/revdep/checks.noindex/ldmppr/old/ldmppr.Rcheck/ldmppr’


```
# rTwig

<details>

* Version: 1.3.0
* GitHub: https://github.com/aidanmorales/rTwig
* Source code: https://github.com/cran/rTwig
* Date/Publication: 2024-11-21 21:30:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::revdep_details(, "rTwig")` for more info

</details>

## In both

*   checking whether package ‘rTwig’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/emilhvitfeldt/Github/yardstick/revdep/checks.noindex/rTwig/new/rTwig.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rTwig’ ...
** package ‘rTwig’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c box_counting.cpp -o box_counting.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cluster_cloud.cpp -o cluster_cloud.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c colors.cpp -o colors.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c pan_plot.cpp -o pan_plot.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c smoothing.cpp -o smoothing.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o rTwig.so RcppExports.o box_counting.o cluster_cloud.o colors.o convex_hull.o correct_radii.o cylinder_mesh.o generate_cloud.o helper_functions.o pan_plot.o smoothing.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [rTwig.so] Error 1
ERROR: compilation failed for package ‘rTwig’
* removing ‘/Users/emilhvitfeldt/Github/yardstick/revdep/checks.noindex/rTwig/new/rTwig.Rcheck/rTwig’


```
### CRAN

```
* installing *source* package ‘rTwig’ ...
** package ‘rTwig’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c box_counting.cpp -o box_counting.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cluster_cloud.cpp -o cluster_cloud.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c colors.cpp -o colors.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c pan_plot.cpp -o pan_plot.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG -I../inst/include -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/Rcpp/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/rTwig/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c smoothing.cpp -o smoothing.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o rTwig.so RcppExports.o box_counting.o cluster_cloud.o colors.o convex_hull.o correct_radii.o cylinder_mesh.o generate_cloud.o helper_functions.o pan_plot.o smoothing.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [rTwig.so] Error 1
ERROR: compilation failed for package ‘rTwig’
* removing ‘/Users/emilhvitfeldt/Github/yardstick/revdep/checks.noindex/rTwig/old/rTwig.Rcheck/rTwig’


```
# shapr

<details>

* Version: 1.0.1
* GitHub: https://github.com/NorskRegnesentral/shapr
* Source code: https://github.com/cran/shapr
* Date/Publication: 2025-01-16 13:00:05 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::revdep_details(, "shapr")` for more info

</details>

## In both

*   checking whether package ‘shapr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/emilhvitfeldt/Github/yardstick/revdep/checks.noindex/shapr/new/shapr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘shapr’ ...
** package ‘shapr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c AICc.cpp -o AICc.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c Copula.cpp -o Copula.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c Gaussian.cpp -o Gaussian.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c impute_data.cpp -o impute_data.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c weighted_matrix.cpp -o weighted_matrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o shapr.so AICc.o Copula.o Gaussian.o RcppExports.o distance.o features.o impute_data.o weighted_matrix.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [shapr.so] Error 1
ERROR: compilation failed for package ‘shapr’
* removing ‘/Users/emilhvitfeldt/Github/yardstick/revdep/checks.noindex/shapr/new/shapr.Rcheck/shapr’


```
### CRAN

```
* installing *source* package ‘shapr’ ...
** package ‘shapr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.6)’
using SDK: ‘MacOSX15.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c AICc.cpp -o AICc.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c Copula.cpp -o Copula.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c Gaussian.cpp -o Gaussian.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c impute_data.cpp -o impute_data.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/include" -DNDEBUG  -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/RcppArmadillo/include' -I'/Users/emilhvitfeldt/Github/yardstick/revdep/library.noindex/shapr/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c weighted_matrix.cpp -o weighted_matrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -L/opt/R/arm64/lib -o shapr.so AICc.o Copula.o Gaussian.o RcppExports.o distance.o features.o impute_data.o weighted_matrix.o -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/Versions/4.4-arm64 -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [shapr.so] Error 1
ERROR: compilation failed for package ‘shapr’
* removing ‘/Users/emilhvitfeldt/Github/yardstick/revdep/checks.noindex/shapr/old/shapr.Rcheck/shapr’


```
