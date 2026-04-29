# gmov

`gmov` implements trajectory-level generative movement validation diagnostics
for simulations from fitted step selection function (SSF) and integrated step
selection function (iSSF) models.

The package is designed as a companion to `amt`, not as a replacement. A
typical workflow is:

1. prepare tracks and steps with `amt`;
2. fit an SSF or iSSF model with `amt`;
3. simulate trajectories from the fitted model;
4. validate whether the simulated trajectories reproduce trajectory-level
   patterns seen in the observed track.

The first package version focuses on four validation pillars:

- emergent utilization distributions;
- mean squared displacement;
- path sinuosity;
- barrier crossing across known linear features.

These diagnostics follow the framework described in:

Nicosia, A. (2026). Beyond the next step: A multi-criteria generative
validation framework for step selection functions. *Methods in Ecology and
Evolution*. <https://doi.org/10.1111/2041-210x.70313>

## Installation

Install from GitHub using the repository SSH URL:

```r
remotes::install_git("git@github.com:AurelienNicosiaULaval/gmov.git")
```

## Minimal example

```r
library(gmov)

set.seed(1)

make_track <- function(n = 40, step_sd = 1) {
  data.frame(
    x = cumsum(stats::rnorm(n, sd = step_sd)),
    y = cumsum(stats::rnorm(n, sd = step_sd))
  )
}

observed_track <- make_track(n = 40, step_sd = 1)
simulated_tracks <- replicate(
  n = 19,
  expr = make_track(n = 40, step_sd = 1),
  simplify = FALSE
)

res <- validate_ssf_generative(
  observed = observed_track,
  simulated = simulated_tracks,
  metrics = c("ud", "msd", "sinuosity"),
  ud_args = list(grid_size = 15)
)

summary(res)
plot(res, metric = "msd")
```

## Relationship to amt

`gmov` is designed as a companion package for generative validation of SSF and
iSSF workflows, not as a replacement for `amt`. Users should fit and simulate
movement models with `amt` or another modeling workflow, then pass the observed
and simulated tracks to `gmov`. The current interface accepts `amt`-style
tracks with `x_` and `y_` coordinate columns, and direct `amt` integration tests
are planned. Coordinates are assumed to be in a common planar coordinate system;
`gmov` does not transform coordinates.

```r
library(amt)
library(gmov)

# Sketch only:
# observed_track <- ...
# fitted_model <- ...
# simulated_tracks <- ...

res <- validate_ssf_generative(
  observed = observed_track,
  simulated = simulated_tracks,
  metrics = c("ud", "msd", "sinuosity")
)
```

If a known linear barrier is available as an `sf` LINESTRING or
MULTILINESTRING object, include the barrier pillar:

```r
res <- validate_ssf_generative(
  observed = observed_track,
  simulated = simulated_tracks,
  metrics = c("ud", "msd", "sinuosity", "barrier"),
  barrier = barrier_sf
)
```

## Current limitations

- `validate_ud()` uses empirical grid utilization distributions and the
  1-Wasserstein distance through the `transport` package. This is a
  Wasserstein distance between discretized empirical grid distributions, not an
  exact distance between continuous utilization distributions.
- `validate_ssf_generative()` validates supplied simulations. It does not yet
  provide wrappers around `amt` simulation internals.
- Barrier validation requires a known barrier geometry supplied before running
  the diagnostic. The implementation counts movement segments that intersect
  the barrier; touching or overlapping the barrier also counts as an
  intersection.
- Monte Carlo p-values should be interpreted as conditional diagnostics for the
  supplied fitted model and simulation procedure.

## Roadmap

- Add an integration test using real `amt` track objects.
- Consider an optional wrapper for common `amt` simulation outputs.
- Improve computational performance for UD validation with many simulations or
  fine grids.
- Add richer barrier-crossing diagnostics once the basic segment-intersection
  diagnostic has been validated against realistic workflows.

## License

`gmov` is released under the MIT license.
