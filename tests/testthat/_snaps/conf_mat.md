# `...` is deprecated with a warning

    Code
      conf_mat(two_class_example, truth, predicted, foo = 1)
    Condition
      Warning:
      The `...` argument of `conf_mat()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored.
    Output
                Truth
      Prediction Class1 Class2
          Class1    227     50
          Class2     31    192

---

    Code
      conf_mat(hpc_cv, obs, pred, foo = 1)
    Condition
      Warning:
      The `...` argument of `conf_mat()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored.
    Output
      # A tibble: 10 x 2
         Resample conf_mat  
         <chr>    <list>    
       1 Fold01   <conf_mat>
       2 Fold02   <conf_mat>
       3 Fold03   <conf_mat>
       4 Fold04   <conf_mat>
       5 Fold05   <conf_mat>
       6 Fold06   <conf_mat>
       7 Fold07   <conf_mat>
       8 Fold08   <conf_mat>
       9 Fold09   <conf_mat>
      10 Fold10   <conf_mat>

