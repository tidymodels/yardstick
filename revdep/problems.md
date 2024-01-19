# modeltime

<details>

* Version: 1.2.8
* GitHub: https://github.com/business-science/modeltime
* Source code: https://github.com/cran/modeltime
* Date/Publication: 2023-09-02 15:10:02 UTC
* Number of recursive dependencies: 249

Run `revdepcheck::revdep_details(, "modeltime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘modeltime-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: parallel_start
    > ### Title: Start parallel clusters using 'parallel' package
    > ### Aliases: parallel_start parallel_stop
    > 
    > ### ** Examples
    > 
    > 
    > # Starts 2 clusters
    > parallel_start(2)
    Error in serverSocket(port = port) : 
      creation of server socket failed: port 11904 cannot be opened
    Calls: parallel_start -> <Anonymous> -> makePSOCKcluster -> serverSocket
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘tidymodels’ ‘tidyverse’
      All declared Imports should be used.
    ```

