#' Filter task groups from a hub's tasks config to those that contain a target_id.
#' Additionally, subset the target_metadata to just the entry for the target_id.
#'
#' @noRd
filter_task_groups_to_target <- function(task_groups, target_id) {
  # For each task group, subset the target_metadata to just the entry for the target_id
  # If the target_id is not in the task group, the target_metadata will be empty
  task_groups <- purrr::map(
    task_groups,
    function(task_group) {
      task_group$target_metadata <- purrr::keep(task_group$target_metadata,
                                                ~ .x$target_id == target_id)
      return(task_group)
    }
  )

  # Remove task groups that don't contain the target_id
  task_groups <- purrr::keep(task_groups, ~ length(.x$target_metadata) > 0)

  return(task_groups)
}
