# Survival Analysis Results

Survival Analysis Results

## Value

- lung_surv:

  a data frame

## Details

These data contain plausible results from applying predictive survival
models to the [lung](https://rdrr.io/pkg/survival/man/lung.html) data
set using the censored package.

## Examples

``` r
data(lung_surv)
str(lung_surv)
#> tibble [228 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ .pred            :List of 228
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.819 0.597 0.407 0.264 0.164
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 306 306
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.735 0.735
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.36 1.36
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.888 0.737 0.588 0.455 0.344
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 455
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.627
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.59
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.901 0.763 0.625 0.498 0.389
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.845 0.648 0.47 0.326 0.219
#>   .. ..$ .weight_time    : num [1:5] 100 200 210 210 210
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.915 0.915 0.915
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.09 1.09 1.09
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.897 0.755 0.613 0.484 0.374
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.819 0.597 0.407 0.264 0.164
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.841 0.64 0.46 0.316 0.209
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 310 310
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.735 0.735
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.36 1.36
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.837 0.631 0.449 0.305 0.2
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 361 361
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.707 0.707
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.41 1.41
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.851 0.659 0.484 0.341 0.233
#>   .. ..$ .weight_time    : num [1:5] 100 200 218 218 218
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.909 0.909 0.909
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.1 1.1 1.1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7568 0.4871 0.2858 0.1563 0.0806
#>   .. ..$ .weight_time    : num [1:5] 100 166 166 166 166
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.845 0.648 0.47 0.326 0.219
#>   .. ..$ .weight_time    : num [1:5] 100 170 170 170 170
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.841 0.64 0.46 0.316 0.209
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.897 0.755 0.614 0.485 0.374
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] NA NA NA NA NA
#>   .. ..$ .weight_time    : num [1:5] 71 71 71 71 71
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.845 0.648 0.47 0.326 0.219
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.83 0.618 0.433 0.289 0.186
#>   .. ..$ .weight_time    : num [1:5] 100 144 144 144 144
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.825 0.609 0.422 0.278 0.176
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7524 0.4799 0.2785 0.1504 0.0765
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.858 0.674 0.503 0.361 0.251
#>   .. ..$ .weight_time    : num [1:5] 61 61 61 61 61
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.845 0.648 0.47 0.326 0.219
#>   .. ..$ .weight_time    : num [1:5] 88 88 88 88 88
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.83 0.618 0.433 0.289 0.186
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 301 301
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.752 0.752
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.33 1.33
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.945 0.865 0.777 0.688 0.602
#>   .. ..$ .weight_time    : num [1:5] 81 81 81 81 81
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.855 0.668 0.495 0.353 0.243
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.899 0.759 0.619 0.491 0.381
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 371 371
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.697 0.697
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.44 1.44
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.884 0.728 0.575 0.44 0.329
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 394 394
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.663
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.51
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.895 0.751 0.607 0.478 0.367
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.897 0.755 0.613 0.484 0.374
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.6149 0.2851 0.1125 0.0392 0.0123
#>   .. ..$ .weight_time    : num [1:5] 100 118 118 118 118
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.851 0.659 0.484 0.341 0.233
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 390 390
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.663
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.51
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7274 0.4398 0.2392 0.12 0.0563
#>   .. ..$ .weight_time    : num [1:5] 12 12 12 12 12
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.896 0.753 0.611 0.481 0.371
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 473
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.73 0.443 0.243 0.123 0.058
#>   .. ..$ .weight_time    : num [1:5] 26 26 26 26 26
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.783 0.533 0.334 0.197 0.11
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.853 0.663 0.489 0.346 0.237
#>   .. ..$ .weight_time    : num [1:5] 100 107 107 107 107
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7568 0.4871 0.2858 0.1563 0.0806
#>   .. ..$ .weight_time    : num [1:5] 53 53 53 53 53
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.85 0.657 0.481 0.338 0.23
#>   .. ..$ .weight_time    : num [1:5] 100 122 122 122 122
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.748 0.4727 0.2713 0.1446 0.0725
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.899 0.76 0.62 0.492 0.382
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7274 0.4398 0.2392 0.12 0.0563
#>   .. ..$ .weight_time    : num [1:5] 93 93 93 93 93
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.995 0.995 0.995 0.995
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.901 0.764 0.626 0.499 0.389
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.825 0.609 0.422 0.278 0.176
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 460
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.834 0.625 0.441 0.297 0.193
#>   .. ..$ .weight_time    : num [1:5] 100 153 153 153 153
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.94 0.851 0.756 0.66 0.569
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 433
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.64
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.56
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.853 0.663 0.489 0.346 0.237
#>   .. ..$ .weight_time    : num [1:5] 100 145 145 145 145
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.829 0.615 0.429 0.286 0.183
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.829 0.616 0.43 0.286 0.183
#>   .. ..$ .weight_time    : num [1:5] 95 95 95 95 95
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.995 0.995 0.995 0.995
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.882 0.723 0.568 0.433 0.321
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 303 303
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.744 0.744
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.34 1.34
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.836 0.63 0.448 0.304 0.199
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.882 0.723 0.568 0.433 0.321
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.914 0.792 0.666 0.548 0.442
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.893 0.747 0.601 0.47 0.359
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.894 0.748 0.604 0.473 0.362
#>   .. ..$ .weight_time    : num [1:5] 100 189 189 189 189
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.954 0.954 0.954 0.954
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.05 1.05 1.05 1.05
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.888 0.737 0.588 0.455 0.344
#>   .. ..$ .weight_time    : num [1:5] 53 53 53 53 53
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.899 0.759 0.619 0.491 0.381
#>   .. ..$ .weight_time    : num [1:5] 100 200 246 246 246
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.845 0.845 0.845
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.18 1.18 1.18
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.842 0.642 0.463 0.319 0.212
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.895 0.751 0.607 0.477 0.366
#>   .. ..$ .weight_time    : num [1:5] 65 65 65 65 65
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.936 0.843 0.743 0.643 0.55
#>   .. ..$ .weight_time    : num [1:5] 5 5 5 5 5
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7653 0.5014 0.3005 0.1683 0.0891
#>   .. ..$ .weight_time    : num [1:5] 100 132 132 132 132
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.907 0.776 0.643 0.52 0.412
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.901 0.764 0.626 0.499 0.389
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 345 345
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.717 0.717
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.39 1.39
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.83 0.619 0.434 0.29 0.187
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 444
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.64
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.56
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.858 0.673 0.502 0.36 0.25
#>   .. ..$ .weight_time    : num [1:5] 100 200 223 223 223
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.895 0.895 0.895
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.12 1.12 1.12
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.82 0.6 0.411 0.267 0.167
#>   .. ..$ .weight_time    : num [1:5] 100 175 175 175 175
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.978 0.978 0.978 0.978
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.02 1.02 1.02 1.02
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.9 0.762 0.623 0.496 0.386
#>   .. ..$ .weight_time    : num [1:5] 60 60 60 60 60
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.827 0.612 0.426 0.282 0.179
#>   .. ..$ .weight_time    : num [1:5] 100 163 163 163 163
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7413 0.4618 0.2605 0.1362 0.0668
#>   .. ..$ .weight_time    : num [1:5] 65 65 65 65 65
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.843 0.643 0.463 0.32 0.213
#>   .. ..$ .weight_time    : num [1:5] 100 200 208 208 208
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.915 0.915 0.915
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.09 1.09 1.09
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.937 0.844 0.745 0.646 0.553
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.888 0.737 0.588 0.455 0.344
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 428
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.64
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.56
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.83 0.618 0.433 0.289 0.186
#>   .. ..$ .weight_time    : num [1:5] 100 200 230 230 230
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.874 0.874 0.874
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.14 1.14 1.14
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.894 0.748 0.604 0.473 0.362
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.915 0.796 0.672 0.554 0.449
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 305 305
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.735 0.735
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.36 1.36
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7274 0.4398 0.2392 0.12 0.0563
#>   .. ..$ .weight_time    : num [1:5] 11 11 11 11 11
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.868 0.694 0.53 0.39 0.279
#>   .. ..$ .weight_time    : num [1:5] 100 132 132 132 132
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.911 0.786 0.658 0.537 0.431
#>   .. ..$ .weight_time    : num [1:5] 100 200 226 226 226
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.874 0.874 0.874
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.14 1.14 1.14
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.894 0.749 0.604 0.474 0.363
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 426
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.64
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.56
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.944 0.862 0.773 0.682 0.595
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.908 0.78 0.649 0.527 0.419
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 363 363
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.707 0.707
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.41 1.41
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.874 0.706 0.545 0.407 0.295
#>   .. ..$ .weight_time    : num [1:5] 11 11 11 11 11
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.883 0.725 0.572 0.437 0.325
#>   .. ..$ .weight_time    : num [1:5] 100 176 176 176 176
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.972 0.972 0.972 0.972
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.03 1.03 1.03 1.03
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.898 0.757 0.616 0.488 0.377
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.848 0.654 0.477 0.334 0.226
#>   .. ..$ .weight_time    : num [1:5] 95 95 95 95 95
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.995 0.995 0.995 0.995
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.866 0.689 0.523 0.383 0.272
#>   .. ..$ .weight_time    : num [1:5] 100 NA NA NA NA
#>   .. ..$ .pred_censored  : num [1:5] 0.995 NA NA NA NA
#>   .. ..$ .weight_censored: num [1:5] 1.01 NA NA NA NA
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.918 0.803 0.682 0.568 0.464
#>   .. ..$ .weight_time    : num [1:5] 100 167 167 167 167
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.863 0.684 0.516 0.375 0.264
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.824 0.606 0.418 0.275 0.173
#>   .. ..$ .weight_time    : num [1:5] 100 200 284 284 284
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.793 0.793 0.793
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.26 1.26 1.26
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.903 0.768 0.632 0.506 0.397
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.896 0.753 0.61 0.48 0.37
#>   .. ..$ .weight_time    : num [1:5] 100 147 147 147 147
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.918 0.803 0.682 0.568 0.464
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7321 0.4471 0.2462 0.1253 0.0597
#>   .. ..$ .weight_time    : num [1:5] 100 163 163 163 163
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.99 0.99 0.99 0.99
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.01 1.01 1.01 1.01
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.894 0.748 0.604 0.473 0.362
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.825 0.609 0.422 0.278 0.176
#>   .. ..$ .weight_time    : num [1:5] 100 200 239 239 239
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.86 0.86 0.86
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.16 1.16 1.16
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.832 0.621 0.437 0.293 0.189
#>   .. ..$ .weight_time    : num [1:5] 88 88 88 88 88
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.907 0.778 0.646 0.524 0.416
#>   .. ..$ .weight_time    : num [1:5] 100 200 245 245 245
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.845 0.845 0.845
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.18 1.18 1.18
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.933 0.837 0.733 0.632 0.536
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.7321 0.4471 0.2462 0.1253 0.0597
#>   .. ..$ .weight_time    : num [1:5] 30 30 30 30 30
#>   .. ..$ .pred_censored  : num [1:5] 1 1 1 1 1
#>   .. ..$ .weight_censored: num [1:5] 1 1 1 1 1
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.827 0.612 0.426 0.282 0.179
#>   .. ..$ .weight_time    : num [1:5] 100 179 179 179 179
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.966 0.966 0.966 0.966
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.03 1.03 1.03 1.03
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.824 0.606 0.418 0.275 0.173
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 310 310
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.735 0.735
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.36 1.36
#>   ..$ : tibble [5 × 5] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ .eval_time      : num [1:5] 100 200 300 400 500
#>   .. ..$ .pred_survival  : num [1:5] 0.835 0.627 0.444 0.3 0.196
#>   .. ..$ .weight_time    : num [1:5] 100 200 300 400 477
#>   .. ..$ .pred_censored  : num [1:5] 0.995 0.928 0.76 0.663 0.613
#>   .. ..$ .weight_censored: num [1:5] 1.01 1.08 1.31 1.51 1.63
#>   .. [list output truncated]
#>  $ .pred_time       : num [1:228] 324 476 521 368 506 ...
#>  $ surv_obj         : 'Surv' num [1:228, 1:2]  306   455  1010+  210   883  1022+  310   361   218   166  ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:2] "time" "status"
#>   ..- attr(*, "type")= chr "right"
#>  $ .pred_linear_pred: num [1:228] 5.78 6.17 6.26 5.91 6.23 ...

# `surv_obj` is a `Surv()` object
```
