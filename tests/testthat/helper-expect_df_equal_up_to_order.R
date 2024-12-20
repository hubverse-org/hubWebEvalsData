#' Check that two data frames are equal up to row order
#'
#' @param df_act The actual data frame
#' @param df_exp The expected data frame
expect_df_equal_up_to_order <- function(df_act, df_exp) {
  cols <- colnames(df_act)
  testthat::expect_equal(cols, colnames(df_exp))
  testthat::expect_true(isTRUE(all.equal(
    dplyr::arrange(df_act, dplyr::across(dplyr::all_of(cols))),
    dplyr::arrange(df_exp, dplyr::across(dplyr::all_of(cols))),
    check.attributes = FALSE
  )))
}
