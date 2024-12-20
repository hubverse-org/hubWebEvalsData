#' Load model output data from a hub, filtering to a specified target and
#' evaluation window.
#'
#' @param hub_path A path to the hub.
#' @param target_id The target_id to filter to.
#' @param eval_window A list specifying the evaluation window, derived from the
#' eval_windows field of the predeval config.
#'
#' @return A data frame containing the model output data.
#' @noRd
load_model_out_in_window <- function(hub_path, target_id, eval_window) {
  conn <- hubData::connect_hub(hub_path)

  # filter to the requested target_id
  hub_tasks_config <- hubUtils::read_config(hub_path, config = "tasks")
  round_ids <- hubUtils::get_round_ids(hub_tasks_config)
  task_groups <- hubUtils::get_round_model_tasks(hub_tasks_config, round_ids[1])
  target_ids_by_task_group <- get_target_ids_by_task_group(task_groups)
  task_group_idxs_w_target <- get_task_group_idxs_w_target(target_id, target_ids_by_task_group)

  target_meta <- purrr::keep(
    task_groups[[task_group_idxs_w_target[[1]]]]$target_metadata,
    function(x) x$target_id == target_id
  )
  target_task_id_var_name <- names(target_meta[[1]]$target_keys)
  target_task_id_value <- target_meta[[1]]$target_keys[[target_task_id_var_name]]

  conn <- conn |>
    dplyr::filter(!!rlang::sym(target_task_id_var_name) == target_task_id_value)

  # if eval_window doesn't specify any subsetting by rounds, return the full data
  no_limits <- identical(names(eval_window), "window_name")
  if (no_limits) {
    return(conn |> dplyr::collect())
  }

  # if eval_window specifies a minimum round id, filter to that
  round_id_var_name <- hub_tasks_config[["rounds"]][[1]][["round_id"]]
  if ("min_round_id" %in% names(eval_window)) {
    conn <- conn |>
      dplyr::filter(!!rlang::sym(round_id_var_name) >= eval_window$min_round_id)
  }

  # load the data
  model_out_tbl <- conn |> dplyr::collect()

  if ("n_last_round_ids" %in% names(eval_window)) {
    # filter to the last n rounds
    max_present_round_id <- max(model_out_tbl[[round_id_var_name]])
    round_ids <- round_ids[round_ids <= max_present_round_id]
    round_ids <- utils::tail(round_ids, eval_window$n_last_round_ids)
    model_out_tbl <- model_out_tbl |>
      dplyr::filter(!!rlang::sym(round_id_var_name) %in% round_ids)
  }

  return(model_out_tbl)
}
