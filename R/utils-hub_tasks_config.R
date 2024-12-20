#' For each task group, get the target_id entries from its target_metadata
#'
#' @return a list with one entry for each task group,
#' where each entry is a character vector of target_ids for that group
#'
#' @noRd
get_target_ids_by_task_group <- function(task_groups) {
  result <- purrr::map(
    task_groups,
    function(task_group) {
      purrr::map_chr(
        task_group[["target_metadata"]],
        function(target) target[["target_id"]]
      )
    }
  )

  return(result)
}


#' Get an integer vector with the indices of elements of
#' target_ids_by_task_group that contain the target_id
#' @noRd
get_task_group_idxs_w_target <- function(target_id, target_ids_by_task_group) {
  result <- purrr::map2(
    seq_along(target_ids_by_task_group), target_ids_by_task_group,
    function(i, target_ids) {
      if (target_id %in% target_ids) {
        return(i)
      }
      return(NULL)
    }
  ) |>
    purrr::compact() |>
    unlist()

  return(result)
}
