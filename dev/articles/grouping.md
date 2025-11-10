# Grouping behavior in yardstick

The 1.3.0 release of yardstick introduced an implementation for
*groupwise metrics*. The use case motivating the implementation of this
functionality is *fairness metrics*, though groupwise metrics have
applications beyond that domain. Fairness metrics quantify the degree of
disparity in a metric value across groups. To learn more about carrying
out fairness-oriented analyses with tidymodels, see the blog post on the
tidymodels website. This vignette will instead focus on groupwise
metrics generally, clarifying the meaning of “groupwise” and
demonstrating functionality with an example dataset.

``` r
library(yardstick)
library(dplyr)

data("hpc_cv")
```

## Group-awareness

Even before the implementation of groupwise metrics, *all* yardstick
metrics had been *group-aware*. When grouped data is passed to a
group-aware metric, it will return metric values calculated for each
group.

To demonstrate, we’ll make use of the `hpc_cv` data set, containing
class probabilities and class predictions for a linear discriminant
analysis fit to the HPC data set of Kuhn and Johnson (2013). The model
is evaluated via 10-fold cross-validation, and the predictions for all
folds are included.

``` r
tibble(hpc_cv)
#> # A tibble: 3,467 × 7
#>    obs   pred     VF      F       M          L Resample
#>    <fct> <fct> <dbl>  <dbl>   <dbl>      <dbl> <chr>   
#>  1 VF    VF    0.914 0.0779 0.00848 0.0000199  Fold01  
#>  2 VF    VF    0.938 0.0571 0.00482 0.0000101  Fold01  
#>  3 VF    VF    0.947 0.0495 0.00316 0.00000500 Fold01  
#>  4 VF    VF    0.929 0.0653 0.00579 0.0000156  Fold01  
#>  5 VF    VF    0.942 0.0543 0.00381 0.00000729 Fold01  
#>  6 VF    VF    0.951 0.0462 0.00272 0.00000384 Fold01  
#>  7 VF    VF    0.914 0.0782 0.00767 0.0000354  Fold01  
#>  8 VF    VF    0.918 0.0744 0.00726 0.0000157  Fold01  
#>  9 VF    VF    0.843 0.128  0.0296  0.000192   Fold01  
#> 10 VF    VF    0.920 0.0728 0.00703 0.0000147  Fold01  
#> # ℹ 3,457 more rows
```

For the purposes of this vignette, we’ll also add a column `batch` to
the data and select off the columns for the class probabilities, which
we don’t need.

``` r
set.seed(1)

hpc <-
  tibble(hpc_cv) |>
  mutate(batch = sample(c("a", "b"), nrow(hpc_cv), replace = TRUE)) |>
  select(-c(VF, F, M, L))

hpc
#> # A tibble: 3,467 × 4
#>    obs   pred  Resample batch
#>    <fct> <fct> <chr>    <chr>
#>  1 VF    VF    Fold01   a    
#>  2 VF    VF    Fold01   b    
#>  3 VF    VF    Fold01   a    
#>  4 VF    VF    Fold01   a    
#>  5 VF    VF    Fold01   b    
#>  6 VF    VF    Fold01   a    
#>  7 VF    VF    Fold01   a    
#>  8 VF    VF    Fold01   a    
#>  9 VF    VF    Fold01   b    
#> 10 VF    VF    Fold01   b    
#> # ℹ 3,457 more rows
```

If we wanted to compute the accuracy of the first resampled model, we
could write:

``` r
hpc |> 
  filter(Resample == "Fold01") |>
  accuracy(obs, pred)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy multiclass     0.726
```

The metric function returns one row, giving the `.metric`, `.estimator`,
and `.estimate` for the whole data set it is passed.

If we instead group the data by fold, metric functions like `accuracy`
will know to compute values for each group; in the output, each row will
correspond to a Resample.

``` r
hpc |> 
  group_by(Resample) |>
  accuracy(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric  .estimator .estimate
#>    <chr>    <chr>    <chr>          <dbl>
#>  1 Fold01   accuracy multiclass     0.726
#>  2 Fold02   accuracy multiclass     0.712
#>  3 Fold03   accuracy multiclass     0.758
#>  4 Fold04   accuracy multiclass     0.712
#>  5 Fold05   accuracy multiclass     0.712
#>  6 Fold06   accuracy multiclass     0.697
#>  7 Fold07   accuracy multiclass     0.675
#>  8 Fold08   accuracy multiclass     0.721
#>  9 Fold09   accuracy multiclass     0.673
#> 10 Fold10   accuracy multiclass     0.699
```

Note that the first row, corresponding to `Fold01`, gives the same value
as manually filtering for the observations corresponding to the first
resample and then computing the accuracy.

This behavior is what we mean by group-awareness. When grouped data is
passed to group-aware metric functions, they will return values for each
group.

## Groupwise metrics

Groupwise metrics are associated with a data-column such that, when
passed data with that column, the metric will temporarily group by that
column, compute values for each of the groups defined by the column, and
then aggregate the values computed for the temporary grouping back to
the level of the input data’s grouping.

More concretely, let’s turn to an example where there is no pre-existing
grouping in the data. Consider the portion of the HPC data pertaining to
the first resample:

``` r
hpc |> 
  filter(Resample == "Fold01")
#> # A tibble: 347 × 4
#>    obs   pred  Resample batch
#>    <fct> <fct> <chr>    <chr>
#>  1 VF    VF    Fold01   a    
#>  2 VF    VF    Fold01   b    
#>  3 VF    VF    Fold01   a    
#>  4 VF    VF    Fold01   a    
#>  5 VF    VF    Fold01   b    
#>  6 VF    VF    Fold01   a    
#>  7 VF    VF    Fold01   a    
#>  8 VF    VF    Fold01   a    
#>  9 VF    VF    Fold01   b    
#> 10 VF    VF    Fold01   b    
#> # ℹ 337 more rows
```

Suppose that the `batch`es in the data represent two groups for which
model performance ought not to differ. To quantify the degree to which
model performance differs for these two groups, we could compute
accuracy values for either group separately, and then take their
difference. First, computing accuracies:

``` r
acc_by_group <- 
  hpc |> 
  filter(Resample == "Fold01") |>
  group_by(batch) |>
  accuracy(obs, pred)

acc_by_group
#> # A tibble: 2 × 4
#>   batch .metric  .estimator .estimate
#>   <chr> <chr>    <chr>          <dbl>
#> 1 a     accuracy multiclass     0.713
#> 2 b     accuracy multiclass     0.739
```

Now, taking the difference:

``` r
diff(c(acc_by_group$.estimate[2], acc_by_group$.estimate[1]))
#> [1] -0.02518607
```

Groupwise metrics encode the
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) and
aggregation step (in this case, subtraction) shown above into a
yardstick metric. We can define a new groupwise metric with the
[`new_groupwise_metric()`](https://yardstick.tidymodels.org/dev/reference/new_groupwise_metric.md)
function:

``` r
accuracy_diff <-
  new_groupwise_metric(
    fn = accuracy,
    name = "accuracy_diff",
    aggregate = function(acc_by_group) {
      diff(c(acc_by_group$.estimate[2], acc_by_group$.estimate[1]))
    }
  )
```

- The `fn` argument is the yardstick metric that will be computed for
  each data group.
- The `name` argument gives the name of the new metric we’ve created;
  we’ll call ours “accuracy difference.”
- The `aggregate` argument is a function defining how to go from `fn`
  output by group to a single numeric value.

The output, `accuracy_diff`, is a function subclass called a
`metric_factory`:

``` r
class(accuracy_diff)
#> [1] "metric_factory" "function"
```

`accuracy_diff` now knows to take accuracy values for each group and
then return the difference between the accuracy for the first and second
result as output. The last thing we need to associate with the object is
the name of the grouping variable to pass to
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html); we
can pass that variable name to `accuracy_diff` to do so:

``` r
accuracy_diff_by_batch <- accuracy_diff(batch)
```

The output, `accuracy_diff_by_batch`, is a yardstick metric function
like any other:

``` r
class(accuracy)
#> [1] "class_metric" "metric"       "function"

class(accuracy_diff_by_batch)
#> [1] "class_metric" "metric"       "function"
```

We can use the `accuracy_diff_by_batch()` metric in the same way that we
would use
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md).
On its own:

``` r
hpc |> 
  filter(Resample == "Fold01") |>
  accuracy_diff_by_batch(obs, pred)
#> # A tibble: 1 × 4
#>   .metric       .by   .estimator .estimate
#>   <chr>         <chr> <chr>          <dbl>
#> 1 accuracy_diff batch multiclass   -0.0252
```

We can also add `accuracy_diff_by_batch()` to metric sets:

``` r
acc_ms <- metric_set(accuracy, accuracy_diff_by_batch)

hpc |> 
  filter(Resample == "Fold01") |>
  acc_ms(truth = obs, estimate = pred)
#> # A tibble: 2 × 4
#>   .metric       .estimator .estimate .by  
#>   <chr>         <chr>          <dbl> <chr>
#> 1 accuracy      multiclass    0.726  NA   
#> 2 accuracy_diff multiclass   -0.0252 batch
```

*Groupwise metrics are group-aware.* When passed data with any grouping
variables other than the column passed as the first argument to
`accuracy_diff()`—in this case, `group`—`accuracy_diff_by_batch()` will
behave like any other yardstick metric. For example:

``` r
hpc |> 
  group_by(Resample) |>
  accuracy_diff_by_batch(obs, pred)
#> # A tibble: 10 × 5
#>    Resample .metric       .by   .estimator .estimate
#>    <chr>    <chr>         <chr> <chr>          <dbl>
#>  1 Fold01   accuracy_diff batch multiclass -0.0252  
#>  2 Fold02   accuracy_diff batch multiclass  0.106   
#>  3 Fold03   accuracy_diff batch multiclass  0.0220  
#>  4 Fold04   accuracy_diff batch multiclass -0.000300
#>  5 Fold05   accuracy_diff batch multiclass -0.0361  
#>  6 Fold06   accuracy_diff batch multiclass  0.0153  
#>  7 Fold07   accuracy_diff batch multiclass -0.0323  
#>  8 Fold08   accuracy_diff batch multiclass -0.0159  
#>  9 Fold09   accuracy_diff batch multiclass -0.0131  
#> 10 Fold10   accuracy_diff batch multiclass -0.0255
```

Groupwise metrics form the backend of fairness metrics in tidymodels. To
learn more about groupwise metrics and their applications in fairness
problems, see
[`new_groupwise_metric()`](https://yardstick.tidymodels.org/dev/reference/new_groupwise_metric.md).
