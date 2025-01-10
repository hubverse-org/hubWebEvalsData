library(rlang)

hub_path <- testthat::test_path("testdata", "ecfh")
model_out_tbl <- hubData::connect_hub(hub_path) |>
  dplyr::collect()
oracle_output <- read.csv(
  testthat::test_path("testdata", "ecfh", "target-data", "oracle-output.csv")
)
oracle_output[["target_end_date"]] <- as.Date(oracle_output[["target_end_date"]])

make_score_fixtures_one_window <- function(window_name, model_out_tbl) {
  for (by in list(NULL, "location", "reference_date", "horizon", "target_end_date")) {
    expected_mean_scores <- hubEvals::score_model_out(
      model_out_tbl = model_out_tbl |> dplyr::filter(.data[["output_type"]] == "mean"),
      oracle_output = oracle_output,
      metrics = "se_point",
      relative_metrics = "se_point",
      baseline = "FS-base",
      by = c("model_id", by)
    )
    expected_median_scores <- hubEvals::score_model_out(
      model_out_tbl = model_out_tbl |> dplyr::filter(.data[["output_type"]] == "median"),
      oracle_output = oracle_output,
      metrics = "ae_point",
      relative_metrics = "ae_point",
      baseline = "FS-base",
      by = c("model_id", by)
    )
    expected_quantile_scores <- hubEvals::score_model_out(
      model_out_tbl = model_out_tbl |> dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = oracle_output,
      metrics = c("wis", "ae_median", "interval_coverage_50", "interval_coverage_95"),
      relative_metrics = c("wis", "ae_median"),
      baseline = "FS-base",
      by = c("model_id", by)
    )
    expected_scores <- expected_mean_scores |>
      dplyr::left_join(expected_median_scores, by = c("model_id", by)) |>
      dplyr::left_join(expected_quantile_scores, by = c("model_id", by))

    # drop relative_skill metrics
    expected_scores <- expected_scores |>
      dplyr::select(!dplyr::all_of(
        c("se_point_relative_skill", "ae_point_relative_skill",
          "wis_relative_skill", "ae_median_relative_skill")
      ))

    save_path <- testthat::test_path("testdata", "expected-scores")
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }

    file_name <- paste0("scores_", window_name, ifelse(is.null(by), "", paste0("_by_", by)), ".csv")
    file_name <- gsub(" ", "_", file_name)
    write.csv(expected_scores, file = file.path(save_path, file_name), row.names = FALSE)
  }
}

make_score_fixtures_one_window("Full season", model_out_tbl)
make_score_fixtures_one_window("Last 5 weeks", model_out_tbl |> dplyr::filter(reference_date >= "2022-12-17"))
