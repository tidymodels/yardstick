---
name: detect-metrics
description: Detect and list all metric functions in the yardstick package. Use when a user asks to find, list, or identify all metrics in the package.
---

# Detect metrics

## Detection commands

Find all metric definitions (recommended):

```bash
grep -E ".* <- new_.*_metric\(" R/*.R
```

Find available metric constructors:

```bash
grep -E "^new_.*_metric <- function" R/*.R
```

Check exported metrics:

```bash
grep "^export(" NAMESPACE | sed 's/export(//' | sed 's/)//'
```

## Metric structure

Each metric has:

1. Generic function calling `UseMethod()`
2. Constructor wrap: `metric_name <- new_*_metric(metric_name, direction = "minimize"|"maximize"|"zero")`
3. Data frame method using `*_metric_summarizer()`
4. Vector implementation: `metric_name_vec()`

## Key files

- `R/aaa-new.R` - Metric constructor definitions
- `R/fair-aaa.R` - Groupwise metric constructor
- `R/template.R` - Metric summarizer functions
