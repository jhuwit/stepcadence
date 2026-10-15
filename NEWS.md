# stepcadence 0.2.0

* Initial CRAN submission.
* Package renamed from `cadence` to `stepcadence`.
* `summarize_cadence()` computes all metrics in one row per group:
  peak 1-, 30-, and 60-minute cadence (`compute_cadence_peak()`), maximum
  5-, 20-, 30-, and 60-minute consecutive cadence (`compute_cadence_max()`),
  uncensored and censored mean cadence (`compute_cadence_mean()`), time in
  cadence levels (`compute_cadence_level()`), and time in cadence bands
  (`compute_cadence_bands()`, `summarize_cadence_bands()`).
* Cadence bands follow Tudor-Locke et al. (2018): `zero`, `incidental`,
  `sporadic`, `purposeful`, `slow_walk`, `medium_walk`, `brisk_walk`, and
  `faster_ambulation` (120+ steps/min).  Only exactly 0 steps is `zero`.
* `compute_daily_steps()`: total steps, stepping time, and mean cadence
  while stepping; included in `summarize_cadence()`.
* `compute_cadence_minutes_above()`: cumulative minutes at or above
  thresholds (default 40 and 100 steps/min).
* `compute_cadence_bouts()`: sustained cadence bouts (default 10+ minutes
  at 100+ steps/min), with an interruption tolerance.
* `compute_cadence_extended_bout()`: extended bout cadence
  (Saint-Maurice et al., 2020).
* `cadence_thresholds()`: age-specific CADENCE moderate and vigorous
  thresholds (ages 6-85).
* `step_volume_category()`: steps/day categories (Tudor-Locke & Bassett,
  2004).
* The default `step_thresholds` for cadence levels is now `c(100, 130)`,
  the CADENCE-Adults moderate and vigorous thresholds (was `c(60, 120)`).
  The upper threshold may be `NA`, giving an `NA` `high_cadence`.
* `mask_wear = TRUE` excludes non-wear minutes using a `wear` column.
* `nhanes_group_cols()` gives the NHANES participant-day columns.
* Requires R >= 4.1.0.
