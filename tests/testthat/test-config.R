test_that(
  "read_webevals_config succeeds, valid yaml file",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_snapshot(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_valid.yaml")
      )
    )
  }
)

test_that(
  "read_webevals_config succeeds, valid yaml file with no min_round_id",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_snapshot(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_valid_no_min_round_id.yaml")
      )
    )
  }
)

test_that(
  "read_webevals_config succeeds, valid yaml file with no disaggregate_by",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_snapshot(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_valid_no_disaggregate_by.yaml")
      )
    )
  }
)

test_that(
  "read_webevals_config succeeds, valid yaml file with no task_id_text",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_snapshot(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_valid_no_task_id_text.yaml")
      )
    )
  }
)

test_that(
  "read_webevals_config fails, invalid yaml file",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_invalid_yaml.yaml")
      ),
      regexp = "Error reading webevals config file"
    )
  }
)

test_that(
  "read_webevals_config fails, multiple modeling round groups",
  {
    hub_path <- test_path("testdata", "test_hub_invalid_mult_rnd")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_valid.yaml")
      ),
      regexp = "hubWebevals only supports hubs with a single round group specified in `tasks.json`."
    )
  }
)

test_that(
  "read_webevals_config succeeds, round_id_from_variable false",
  {
    hub_path <- test_path("testdata", "test_hub_invalid_rifv_F")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_valid.yaml")
      ),
      regexp = "hubWebevals only supports hubs with `round_id_from_variable` set to `true` in `tasks.json`."
    )
  }
)

test_that(
  "read_webevals_config fails, invalid target id",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_invalid_targets_target_id.yaml")
      ),
      regexp = 'Target id "THIS IS NOT A REAL TARGET ID" not found in any task group.'
    )
  }
)

test_that(
  "read_webevals_config fails, invalid metrics",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_invalid_targets_metrics.yaml")
      ),
      regexp = 'Unsupported metrics: "THIS IS NOT A SUPPORTED METRIC" and "coverage_95".'
    )
  }
)

test_that(
  "read_webevals_config fails, invalid disaggregate by",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_invalid_targets_disaggregate_by.yaml")
      ),
      regexp = 'Disaggregate by variables "THIS IS NOT A TASK ID VARIABLE" and "NEITHER IS THIS"'
    )
  }
)

test_that(
  "read_webevals_config fails, invalid eval window n_last",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_invalid_window_n_last.yaml")
      ),
      regexp = "/eval_windows/1/n_last_round_ids must be >= 1"
    )
  }
)

test_that(
  "read_webevals_config fails, invalid eval window min_round_id",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_invalid_window_min_round_id.yaml")
      ),
      regexp = 'Minimum round id "THIS IS NOT A ROUND ID" for evaluation window is not a valid round id for the hub.'
    )
  }
)

test_that(
  "read_webevals_config fails, invalid task_id_text, not a task id variable",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_invalid_task_id_text_non_task_id.yaml")
      ),
      regexp = 'Specified `task_id_text` for task id variable "NOT_A_REAL_TASK_ID"'
    )
  }
)

test_that(
  "read_webevals_config fails, invalid task_id_text, missing entries",
  {
    hub_path <- test_path("testdata", "ecfh")
    expect_error(
      read_config(
        hub_path,
        test_path("testdata", "test_configs",
                  "config_invalid_task_id_text_missing.yaml")
      ),
      regexp = 'For task id variable "location", missing the following values: "US" and "25"'
    )
  }
)
