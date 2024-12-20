test_that(
  "get_target_ids_by_task_group works",
  {
    task_groups <- list(
      list(
        target_metadata = list(
          list(target_id = "target_id_1"),
          list(target_id = "target_id_2")
        )
      ),
      list(
        target_metadata = list(
          list(target_id = "target_id_3"),
          list(target_id = "target_id_4"),
          list(target_id = "target_id_5")
        )
      ),
      list(
        target_metadata = list(
          list(target_id = "target_id_4")
        )
      )
    )

    target_ids_by_task_group <- get_target_ids_by_task_group(task_groups)

    expect_equal(
      target_ids_by_task_group,
      list(
        c("target_id_1", "target_id_2"),
        c("target_id_3", "target_id_4", "target_id_5"),
        "target_id_4"
      )
    )
  }
)

test_that(
  "get_task_group_idxs_w_target works",
  {
    task_groups <- list(
      list(
        target_metadata = list(
          list(target_id = "target_id_1"),
          list(target_id = "target_id_2")
        )
      ),
      list(
        target_metadata = list(
          list(target_id = "target_id_3"),
          list(target_id = "target_id_4"),
          list(target_id = "target_id_5")
        )
      ),
      list(
        target_metadata = list(
          list(target_id = "target_id_4")
        )
      )
    )

    target_ids_by_task_group <- get_target_ids_by_task_group(task_groups)

    expect_equal(
      get_task_group_idxs_w_target("target_id_1", target_ids_by_task_group),
      1L
    )

    expect_equal(
      get_task_group_idxs_w_target("target_id_4", target_ids_by_task_group),
      c(2L, 3L)
    )
  }
)
