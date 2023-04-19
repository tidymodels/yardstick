# sknifedatar

<details>

* Version: 0.1.2
* GitHub: https://github.com/rafzamb/sknifedatar
* Source code: https://github.com/cran/sknifedatar
* Date/Publication: 2021-06-01 08:00:02 UTC
* Number of recursive dependencies: 213

Run `revdep_details(, "sknifedatar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sknifedatar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: multieval
    > ### Title: Evaluation of multiple metrics and predictions
    > ### Aliases: multieval
    > 
    > ### ** Examples
    > 
    > set.seed(123)
    ...
     20. │                   └─tidyselect:::vars_select_eval(...)
     21. │                     └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
     22. │                       └─tidyselect:::as_indices_sel_impl(...)
     23. │                         └─tidyselect:::as_indices_impl(...)
     24. │                           └─vctrs::vec_as_subscript(x, logical = "error", call = call, arg = arg)
     25. ├─dplyr::bind_rows(.)
     26. │ └─rlang::list2(...)
     27. ├─dplyr::mutate(., modelo = y)
     28. └─rlang::cnd_signal(x)
    Execution halted
    ```

