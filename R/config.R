#' Load and validate a webevals config file
#'
#' @param hub_path A path to the hub.
#' @param config_path A path to a yaml file that specifies the configuration
#' options for the evaluation.
#'
#' @return A list of configuration options for the evaluation.
read_config <- function(hub_path, config_path) {
  tryCatch(
    {
      config <- yaml::read_yaml(config_path, eval.expr = FALSE)
    },
    error = function(e) {
      # This handler is used when an unrecoverable error is thrown while
      # attempting to read the config file: typically the file does not
      # exist or can't be parsed by read_yaml().
      cli::cli_abort(c(
        "Error reading webevals config file at {.val {config_path}}:",
        conditionMessage(e)
      ))
    }
  )

  validate_config(hub_path, config)

  return(config)
}


#' Validate a webevals config object
#' @noRd
validate_config <- function(hub_path, config) {
  validate_config_vs_schema(config)
  validate_config_vs_hub_tasks(hub_path, config)
}


#' Validate a webevals config object against the config schema
#' @noRd
validate_config_vs_schema <- function(config) {
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE)

  schema_path <- system.file("schema", "config_schema.json",
                             package = "hubPredEvalsData")

  schema_json <- jsonlite::read_json(schema_path,
    auto_unbox = TRUE
  ) |>
    jsonlite::toJSON(auto_unbox = TRUE)

  valid <- jsonvalidate::json_validate(config_json, schema_json,
    engine = "ajv", verbose = TRUE,
    greedy = TRUE
  )

  if (!valid) {
    msgs <- attr(valid, "errors") |>
      dplyr::transmute(
        m = dplyr::case_when(
          .data$keyword == "required" ~ paste(.data$message, "."),
          .data$keyword == "additionalProperties" ~ paste0(
            .data$message, "; saw unexpected property '",
            .data$params$additionalProperty, "'."
          ),
          TRUE ~ paste("-", .data$instancePath, .data$message, ".")
        )
      ) |>
      dplyr::pull(.data$m)
    names(msgs) <- rep("!", length(msgs))

    raise_config_error(msgs)
  }
}


#' Validate a webevals config object against the hub tasks config
#' @noRd
validate_config_vs_hub_tasks <- function(hub_path, webevals_config) {
  hub_tasks_config <- hubUtils::read_config(hub_path, config = "tasks")
  task_id_names <- hubUtils::get_task_id_names(hub_tasks_config)

  if (length(hub_tasks_config[["rounds"]]) > 1) {
    raise_config_error("hubWebevals only supports hubs with a single round group specified in `tasks.json`.")
  }
  if (!hub_tasks_config[["rounds"]][[1]][["round_id_from_variable"]]) {
    raise_config_error("hubWebevals only supports hubs with `round_id_from_variable` set to `true` in `tasks.json`.")
  }

  task_groups <- hub_tasks_config[["rounds"]][[1]][["model_tasks"]]

  # checks for targets
  validate_config_targets(webevals_config, task_groups, task_id_names)

  # checks for eval_windows
  validate_config_eval_windows(webevals_config, hub_tasks_config)

  # checks for task_id_text
  validate_config_task_id_text(webevals_config, task_groups, task_id_names)
}


#' Validate the targets in a webevals config object. For each target, check that:
#' - target_id in the webevals config appears in the hub tasks as a target_id
#' - metrics are valid for the available output types for that target
#' - disaggregate_by entries are task id variable names
#'
#' @noRd
validate_config_targets <- function(webevals_config, task_groups, task_id_names) {
  for (target in webevals_config$targets) {
    target_id <- target$target_id

    # get task groups for this target
    task_groups_w_target <- filter_task_groups_to_target(task_groups, target_id)

    # check that target_id in the webevals config appears in the hub tasks
    if (length(task_groups_w_target) == 0) {
      raise_config_error(
        cli::format_inline("Target id {.val {target_id}} not found in any task group.")
      )
    }

    # check that metrics are valid for the available output types
    metric_name_to_output_type <- get_metric_name_to_output_type(
      task_groups_w_target,
      target$metrics
    )
    unsupported_metrics <- setdiff(
      target$metrics,
      metric_name_to_output_type$metric[!is.na(metric_name_to_output_type$output_type)]
    )

    if (length(unsupported_metrics) > 0) {
      available_output_types <- get_output_types(task_groups_w_target) # nolint: object_usage
      target_is_ordinal <- is_target_ordinal(task_groups_w_target)
      raise_config_error(
        c(
          cli::format_inline(
            "Requested scores for metrics that are incompatible with the ",
            "available output types for {.arg target_id} {.val {target_id}}."
          ),
          "i" = cli::format_inline(
            "Output type{?s}: {.val {available_output_types}}",
            ifelse(target_is_ordinal, " for ordinal target.", ".")
          ),
          "x" = cli::format_inline(
            "Unsupported metric{?s}: {.val {unsupported_metrics}}."
          )
        )
      )
    }

    # check that disaggregate_by are task id variable names
    extra_disaggregate_by <- setdiff(
      target$disaggregate_by,
      task_id_names
    )
    if (length(extra_disaggregate_by) > 0) {
      raise_config_error(
        cli::format_inline(
          "Disaggregate by variable{?s} {.val {extra_disaggregate_by}} not ",
          "found in the hub task id variables."
        )
      )
    }
  }
}


#' Validate the eval_windows in a webevals config object
#'  - check that min_round_id specified in webevals config is a valid round_id
#'    for the hub
#'
#' @noRd
validate_config_eval_windows <- function(webevals_config, hub_tasks_config) {
  hub_round_ids <- hubUtils::get_round_ids(hub_tasks_config)
  for (eval_window in webevals_config$eval_windows) {
    # check that min_round_id is a valid round_id
    # only do this check if eval_window$min_round_id is specified
    if (!"min_round_id" %in% names(eval_window)) {
      next
    }
    if (!eval_window$min_round_id %in% hub_round_ids) {
      raise_config_error(
        cli::format_inline(
          "Minimum round id {.val {eval_window$min_round_id}} for evaluation ",
          "window is not a valid round id for the hub."
        )
      )
    }
  }
}


#' Validate the task_id_text in a webevals config object
#' - check that all task_id_text items are valid task id variable names
#' - check that all values of the task id variable in the hub appear as task_id_text item keys
#'
#' @noRd
validate_config_task_id_text <- function(webevals_config, task_groups, task_id_names) {
  # all task_id_text items must be valid task id variable names
  extra_task_id_text_names <- setdiff(
    names(webevals_config$task_id_text),
    task_id_names
  )
  if (length(extra_task_id_text_names) > 0) {
    raise_config_error(
      cli::format_inline(
        "Specified `task_id_text` for task id variable{?s} {.val {extra_task_id_text_names}} ",
        "that {?is/are} not found in the hub task id variables."
      )
    )
  }

  # all values of the task id variable in the hub must appear as task_id_text item keys
  for (i in seq_along(webevals_config$task_id_text)) {
    task_id_text_item <- webevals_config$task_id_text[[i]]
    task_id_name <- names(webevals_config$task_id_text)[i]
    hub_task_id_values <- purrr::map(
      task_groups,
      function(task_group) {
        task_group$task_ids[[task_id_name]]
      }
    ) |>
      unlist() |>
      unique()

    missing_task_id_text_values <- setdiff(
      hub_task_id_values,
      names(task_id_text_item)
    )

    # check that task_id_name is a valid task id variable name
    if (length(missing_task_id_text_values) > 0) {
      raise_config_error(
        cli::format_inline(
          "`task_id_text` must contain text values for all possible levels of task id variables. ",
          "For task id variable {.val {task_id_name}}, missing the following values: ",
          "{.val {missing_task_id_text_values}}"
        )
      )
    }
  }
}


#' Raise an error related to the webevals config file
#' @noRd
raise_config_error <- function(msgs) {
  call <- rlang::caller_call()
  if (!is.null(call)) {
    call <- rlang::call_name(call)
  }

  cli::cli_abort(c(
    "Error in webevals config file:",
    msgs
  ))
}
