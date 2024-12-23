test_that(
  "filter_task_groups_to_target works",
  {
    task_groups <- list(
      list(
        group_number = 1,
        target_metadata = list(
          list(target_id = "target_id_1"),
          list(target_id = "target_id_2")
        )
      ),
      list(
        group_number = 2,
        target_metadata = list(
          list(target_id = "target_id_3"),
          list(target_id = "target_id_4"),
          list(target_id = "target_id_5")
        )
      ),
      list(
        group_number = 3,
        target_metadata = list(
          list(target_id = "target_id_4")
        )
      )
    )

    expect_equal(
      filter_task_groups_to_target(task_groups, "target_id_1"),
      list(
        list(
          group_number = 1,
          target_metadata = list(
            list(target_id = "target_id_1")
          )
        )
      )
    )

    expect_equal(
      filter_task_groups_to_target(task_groups, "target_id_4"),
      list(
        list(
          group_number = 2,
          target_metadata = list(
            list(target_id = "target_id_4")
          )
        ),
        list(
          group_number = 3,
          target_metadata = list(
            list(target_id = "target_id_4")
          )
        )
      )
    )
  }
)
