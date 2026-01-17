---
name: detect-metrics
description: Detect and list all metric functions in the yardstick package. Use when a user asks to find, list, or identify all metrics in the package.
---

# Detect metrics

Use this skill to find and list all metric functions defined in the yardstick package.

## Overview

Yardstick metrics follow consistent patterns that make them detectable:

1. File naming conventions by metric type.
2. Constructor functions that wrap metrics with class and direction.
3. Standard structure with generic, data.frame method, and vector implementation.

## Metric types and file patterns

Metrics are organized by type with filename prefixes:

| Type | File pattern | Constructor |
|------|--------------|-------------|
| Classification | `R/class-*.R` | `new_class_metric()` |
| Numeric/regression | `R/num-*.R` | `new_numeric_metric()` |
| Probability | `R/prob-*.R` | `new_prob_metric()` |
| Survival (dynamic) | `R/surv-*.R` | `new_dynamic_survival_metric()` |
| Survival (static) | `R/surv-*.R` | `new_static_survival_metric()` |
| Survival (integrated) | `R/surv-*.R` | `new_integrated_survival_metric()` |
| Fairness | `R/fair-*.R` | `new_groupwise_metric()` |
| Quantile | `R/quant-*.R` | `new_quantile_metric()` |
| Ordered probability | `R/orderedprob-*.R` | `new_ordered_prob_metric()` |
| Probability curves | `R/probcurve-*.R` | `new_prob_metric()` |

## Detection methods

### Method 1: Search by file naming convention

List all metric files by prefix:

```bash
ls R/class-*.R R/num-*.R R/prob-*.R R/surv-*.R R/fair-*.R R/quant-*.R R/orderedprob-*.R R/probcurve-*.R 2>/dev/null
```

### Method 2: Search for metric constructors (simple regex)

Use this regex to find all metric definitions:

```
.* <- new_.*_metric\(
```

Example with grep:

```bash
grep -E ".* <- new_.*_metric\(" R/*.R
```

### Method 3: Search for specific metric constructors

Find all uses of specific metric constructor functions:

```bash
grep -l "new_class_metric\|new_numeric_metric\|new_prob_metric\|new_quantile_metric\|new_ordered_prob_metric\|new_dynamic_survival_metric\|new_static_survival_metric\|new_integrated_survival_metric\|new_linear_pred_survival_metric\|new_groupwise_metric" R/*.R
```

### Method 4: Extract metric names from constructor calls

Find the actual metric names being defined:

```bash
grep -E "^[a-z_]+ <- new_(class|numeric|prob|quantile|ordered_prob|dynamic_survival|static_survival|integrated_survival|linear_pred_survival)_metric" R/*.R
```

### Method 5: Check NAMESPACE for exported metrics

Exported metrics appear in the NAMESPACE file:

```bash
grep "^export(" NAMESPACE | sed 's/export(//' | sed 's/)//'
```

## Metric structure reference

Each metric typically has these components:

```r
# 1. Generic function
metric_name <- function(data, ...) {

  UseMethod("metric_name")
}

# 2. Wrap with constructor (defines class and optimization direction)
metric_name <- new_*_metric(
  metric_name,
  direction = "minimize"  # or "maximize" or "zero"
)

# 3. Data frame method
metric_name.data.frame <- function(data, truth, estimate, ...) {
  *_metric_summarizer(
    name = "metric_name",
    fn = metric_name_vec,
    ...
  )
}

# 4. Vector implementation
metric_name_vec <- function(truth, estimate, ...) {
  # Implementation
}
```

## Key files

- `R/aaa-new.R` - Metric constructor definitions
- `R/template.R` - Metric summarizer functions
- `R/aaa-metrics.R` - Default metric sets via `metrics()`
- `R/aaa-metric_set.R` - `metric_set()` for combining metrics

## Checklist

When detecting metrics:

- [ ] Search files by naming convention (`class-`, `num-`, `prob-`, etc.).
- [ ] Verify with constructor pattern search.
- [ ] Cross-reference with NAMESPACE exports.
- [ ] Note the metric direction (minimize/maximize/zero).
