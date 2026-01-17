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

## Metric patterns

Metrics follow consistent naming patterns:

1. **Constructor functions**: All metric constructors follow the pattern `new_*_metric()` defined in `R/aaa-new.R` (and `R/fair-aaa.R` for groupwise metrics).

2. **File naming**: Metric files use prefixes like `class-`, `num-`, `prob-`, `surv-`, `fair-`, `quant-`, etc. Discover current prefixes dynamically rather than relying on a fixed list.

3. **Metric definition**: Metrics are defined with `<name> <- new_*_metric(...)`.

To discover all current metric constructors:

```bash
grep -E "^new_.*_metric <- function" R/*.R
```

## Detection methods

### Method 1: Search for metric constructors (recommended)

Use this regex to find all metric definitions:

```
.* <- new_.*_metric\(
```

Example with grep:

```bash
grep -E ".* <- new_.*_metric\(" R/*.R
```

### Method 2: Find files containing metric definitions

Find all files that contain metric constructor calls:

```bash
grep -l "new_.*_metric(" R/*.R
```

### Method 3: Discover metric file prefixes

Find all unique file prefixes used for metrics:

```bash
ls R/*.R | grep -E "R/[a-z]+-" | sed 's/R\/\([a-z]*-\).*/\1/' | sort -u
```

### Method 4: Check NAMESPACE for exported metrics

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

- [ ] Search for constructor pattern `<name> <- new_*_metric(`.
- [ ] Cross-reference with NAMESPACE exports.
- [ ] Note the metric direction (minimize/maximize/zero).
