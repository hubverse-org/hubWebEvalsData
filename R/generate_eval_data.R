#' Generate evaluation data for a hub
#'
#' @param hub_path A path to the hub.
#' @param config_path A path to a yaml file that specifies the configuration
#' options for the evaluation.
#' @param out_path The directory to write the evaluation data to.
#' @param oracle_output A data frame of oracle output to use for the evaluation.
#'
#' @export
generate_eval_data <- function(hub_path,
                               config_path,
                               out_path,
                               oracle_output) {
  config <- read_config(hub_path, config_path)
  for (target in config$targets) {
    generate_target_eval_data(hub_path, config, out_path, oracle_output, target)
  }
}


#' Generate evaluation data for a target
#'
#' @inheritParams generate_eval_data
#' @param config The configuration options for the evaluation.
#' @param target The target to generate evaluation data for. This is one object
#' from the list of targets in the config, with properties "target_id",
#' "metrics", and "disaggregate_by".
#'
#' @noRd
generate_target_eval_data <- function(hub_path,
                                      config,
                                      out_path,
                                      oracle_output,
                                      target) {
  target_id <- target$target_id
  metrics <- target$metrics
  disaggregate_by <- target$disaggregate_by
  eval_windows <- config$eval_windows

  task_groups_w_target <- get_task_groups_w_target(hub_path, target_id)
  metric_name_to_output_type <- get_metric_name_to_output_type(task_groups_w_target, metrics)

  for (eval_window in eval_windows) {
    model_out_tbl <- load_model_out_in_window(hub_path, target$target_id, eval_window)

    # calculate overall scores followed by scores disaggregated by a task ID variable.
    for (by in c(list(NULL), disaggregate_by)) {
      get_and_save_scores(
        model_out_tbl = model_out_tbl,
        oracle_output = oracle_output,
        metric_name_to_output_type = metric_name_to_output_type,
        target_id = target_id,
        window_name = eval_window$window_name,
        by = by,
        out_path = out_path
      )
    }
  }
}


#' Get and save scores for a target in a given evaluation window,
#' collecting across different output types as necessary.
#' Scores are saved in .csv files in subdirectorys under out_path with one of
#' two structures:
#' - If by is NULL, the scores are saved in
#' out_path/target_id/window_name/scores.csv
#' - If by is not NULL, the scores are saved in
#' out_path/target_id/window_name/by/scores.csv
#' @noRd
get_and_save_scores <- function(model_out_tbl, oracle_output, metric_name_to_output_type,
                                target_id, window_name, by,
                                out_path) {
  # Iterate over the output types and calculate scores for each
  scores <- purrr::map(
    unique(metric_name_to_output_type$output_type),
    ~ hubEvals::score_model_out(
      model_out_tbl = model_out_tbl |> dplyr::filter(output_type == !!.x),
      oracle_output = oracle_output,
      metrics = metric_name_to_output_type$metric[
        metric_name_to_output_type$output_type == .x
      ],
      by = c("model_id", by)
    )
  ) |>
    purrr::reduce(dplyr::left_join, by = c("model_id", by))

  target_window_by_out_path <- file.path(out_path, target_id, window_name)
  if (!is.null(by)) {
    target_window_by_out_path <- file.path(target_window_by_out_path, by)
  }
  if (!dir.exists(target_window_by_out_path)) {
    dir.create(target_window_by_out_path, recursive = TRUE)
  }
  utils::write.csv(scores,
                   file = file.path(target_window_by_out_path, "scores.csv"),
                   row.names = FALSE)
}
