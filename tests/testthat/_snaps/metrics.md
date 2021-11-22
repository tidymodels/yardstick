# print metric_set works

    Code
      metric_set(rmse, rsq, ccc)
    Output
      # A tibble: 3 x 3
        metric class          direction
        <chr>  <chr>          <chr>    
      1 rmse   numeric_metric minimize 
      2 rsq    numeric_metric maximize 
      3 ccc    numeric_metric maximize 

