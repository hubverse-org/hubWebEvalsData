#' Helper function to check that the output files were created and have the expected contents
#' for one evaluation window.
#' @param out_path The path to the output directory where scores were saved.
#' @param window_name The name of the evaluation window.
#' @param model_out_tbl The model output table, filtered to data for the evaluation window.
#' @param oracle_output The oracle output.
check_exp_scores_for_window <- function(out_path, window_name, model_out_tbl, oracle_output) {
  # check that the output files were created and have the expected contents
  # no disaggregation
  scores_path <- file.path(out_path, "wk inc flu hosp", window_name, "scores.csv")
  testthat::expect_true(file.exists(scores_path))

  actual_scores <- read.csv(scores_path)
  expected_mean_scores <- hubEvals::score_model_out(
    model_out_tbl = model_out_tbl |> dplyr::filter(output_type == "mean"),
    oracle_output = oracle_output,
    metrics = "se_point",
    by = "model_id"
  )
  expected_median_scores <- hubEvals::score_model_out(
    model_out_tbl = model_out_tbl |> dplyr::filter(output_type == "median"),
    oracle_output = oracle_output,
    metrics = "ae_point",
    by = "model_id"
  )
  expected_quantile_scores <- hubEvals::score_model_out(
    model_out_tbl = model_out_tbl |> dplyr::filter(output_type == "quantile"),
    oracle_output = oracle_output,
    metrics = c("wis", "ae_median", "interval_coverage_50", "interval_coverage_95"),
    by = "model_id"
  )
  expected_scores <- expected_mean_scores |>
    dplyr::left_join(expected_median_scores, by = "model_id") |>
    dplyr::left_join(expected_quantile_scores, by = "model_id")
  expect_df_equal_up_to_order(actual_scores, expected_scores, ignore_attr = TRUE) # nolint: object_usage_linter

  for (by in c("location", "reference_date", "horizon", "target_end_date")) {
    # check that the output files were created and have the expected contents
    # disaggregated by `by`
    scores_path <- file.path(out_path, "wk inc flu hosp", window_name, by, "scores.csv")
    testthat::expect_true(file.exists(scores_path))

    actual_scores <- read.csv(scores_path)
    if (by %in% c("reference_date", "target_end_date")) {
      actual_scores[[by]] <- as.Date(actual_scores[[by]])
    }

    expected_mean_scores <- hubEvals::score_model_out(
      model_out_tbl = model_out_tbl |> dplyr::filter(.data[["output_type"]] == "mean"),
      oracle_output = oracle_output,
      metrics = "se_point",
      by = c("model_id", by)
    )
    expected_median_scores <- hubEvals::score_model_out(
      model_out_tbl = model_out_tbl |> dplyr::filter(.data[["output_type"]] == "median"),
      oracle_output = oracle_output,
      metrics = "ae_point",
      by = c("model_id", by)
    )
    expected_quantile_scores <- hubEvals::score_model_out(
      model_out_tbl = model_out_tbl |> dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = oracle_output,
      metrics = c("wis", "ae_median", "interval_coverage_50", "interval_coverage_95"),
      by = c("model_id", by)
    )
    expected_scores <- expected_mean_scores |>
      dplyr::left_join(expected_median_scores, by = c("model_id", by)) |>
      dplyr::left_join(expected_quantile_scores, by = c("model_id", by))
    expect_df_equal_up_to_order(actual_scores, expected_scores, ignore_attr = TRUE) # nolint: object_usage_linter
  }
}

test_that(
  "generate_eval_data works, integration test",
  {
    out_path <- withr::local_tempdir()
    hub_path <- test_path("testdata", "ecfh")
    model_out_tbl <- hubData::connect_hub(hub_path) |>
      dplyr::collect()
    oracle_output <- read.csv(
      test_path("testdata", "ecfh", "target-data", "oracle-output.csv")
    )
    oracle_output[["target_end_date"]] <- as.Date(oracle_output[["target_end_date"]])

    generate_eval_data(
      hub_path = hub_path,
      config_path = test_path("testdata", "test_configs", "config_valid_mean_median_quantile.yaml"),
      out_path = out_path,
      oracle_output = oracle_output
    )

    check_exp_scores_for_window(out_path,
                                "Full season",
                                model_out_tbl,
                                oracle_output)
    check_exp_scores_for_window(out_path,
                                "Last 5 weeks",
                                model_out_tbl |> dplyr::filter(reference_date >= "2022-12-17"),
                                oracle_output)
  }
)
