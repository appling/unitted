# unreleased (dev)

* Fixed `select()` and `rename()` on `unitted_data.frame` and `unitted_tbl_df`
  objects. dplyr 1.0.0 removed `dplyr::select_vars()` and `rename_vars()`,
  causing a runtime error in any code path that calls these methods (e.g.,
  `streamMetabolizer::calc_light_merged()`). Replaced both calls with
  `tidyselect::vars_select()` and `tidyselect::vars_rename()` respectively,
  and added `tidyselect` and `rlang` to `Imports`.

* Removed dead test assertions for `dplyr::select_()` and `dplyr::rename_()`
  (the old standard-evaluation underscore-suffix variants made defunct in
  dplyr 1.0.0). No `select_.`/`rename_.` S3 method was ever implemented for
  `unitted_data.frame` or `unitted_tbl_df`, so these tests exercised only the
  defunct dplyr generics themselves — not any unitted code path.

# 0.2.9

* new implementations of `select`, `rename`, and `mutate` (for unitted data) to
keep up with changes to the tidyverse

# 0.2.8 and previous

* we weren't tracking news until 0.2.9, sorry!
