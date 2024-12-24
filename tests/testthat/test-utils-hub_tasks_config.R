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

    expect_equal(
      filter_task_groups_to_target(task_groups, "NOT A REAL TARGET ID"),
      list()
    )
  }
)


test_that(
  "get_output_types works",
  {
    task_groups <- list(
      list(
        output_type = list(
          "output_type_1" = "output_type_1_value",
          "output_type_2" = "output_type_2_value"
        )
      ),
      list(
        output_type = list(
          "output_type_2" = "output_type_2_value",
          "output_type_3" = "output_type_3_value"
        )
      ),
      list(
        output_type = list(
          "output_type_3" = "output_type_3_value"
        )
      )
    )

    expect_equal(
      get_output_types(task_groups),
      c("output_type_1", "output_type_2", "output_type_3")
    )
  }
)


test_that(
  "is_target_ordinal works",
  {
    task_groups_w_target <- list(
      list(
        target_metadata = list(
          list(target_type = "ordinal")
        )
      )
    )

    expect_true(is_target_ordinal(task_groups_w_target))

    task_groups_w_target <- list(
      list(
        target_metadata = list(
          list(target_type = "nominal")
        )
      )
    )

    expect_false(is_target_ordinal(task_groups_w_target))
  }
)
