test_that(
  "load_model_out_in_window succeeds, all rounds",
  {
    model_out_tbl <- load_model_out_in_window(
      hub_path = test_path("testdata", "ecfh"),
      target_id = "wk flu hosp rate category",
      eval_window = list(
        window_name = "all"
      )
    )

    expected_model_out_tbl <- hubData::connect_hub(
      test_path("testdata", "ecfh")
    ) |>
      dplyr::filter(
        target == "wk flu hosp rate category"
      ) |>
      dplyr::collect()

    expect_df_equal_up_to_order(
      model_out_tbl,
      expected_model_out_tbl
    )
  }
)


test_that(
  "load_model_out_in_window succeeds, min_round_id only",
  {
    model_out_tbl <- load_model_out_in_window(
      hub_path = test_path("testdata", "ecfh"),
      target_id = "wk flu hosp rate category",
      eval_window = list(
        window_name = "some subset",
        min_round_id = "2022-11-19"
      )
    )

    expected_model_out_tbl <- hubData::connect_hub(
      test_path("testdata", "ecfh")
    ) |>
      dplyr::filter(
        target == "wk flu hosp rate category",
        reference_date >= "2022-11-19"
      ) |>
      dplyr::collect()

    expect_df_equal_up_to_order(
      model_out_tbl,
      expected_model_out_tbl
    )
  }
)


test_that(
  "load_model_out_in_window succeeds, n_last_round_ids only",
  {
    # n_last_round_ids = 5: we expect 2022-12-17, 2022-12-24, 2022-12-31, 2023-01-07, 2023-01-14
    model_out_tbl <- load_model_out_in_window(
      hub_path = test_path("testdata", "ecfh"),
      target_id = "wk flu hosp rate category",
      eval_window = list(
        window_name = "some subset",
        n_last_round_ids = 5
      )
    )

    expected_model_out_tbl <- hubData::connect_hub(
      test_path("testdata", "ecfh")
    ) |>
      dplyr::filter(
        target == "wk flu hosp rate category",
        reference_date >= "2022-12-17"
      ) |>
      dplyr::collect()

    expect_df_equal_up_to_order(
      model_out_tbl,
      expected_model_out_tbl
    )

    # n_last_round_ids = 4: we expect 2022-12-24, 2022-12-31, 2023-01-07, 2023-01-14
    # (but note that ecfh has only 2023-01-14 from this set)
    model_out_tbl <- load_model_out_in_window(
      hub_path = test_path("testdata", "ecfh"),
      target_id = "wk flu hosp rate category",
      eval_window = list(
        window_name = "some subset",
        n_last_round_ids = 4
      )
    )

    expected_model_out_tbl <- hubData::connect_hub(
      test_path("testdata", "ecfh")
    ) |>
      dplyr::filter(
        target == "wk flu hosp rate category",
        reference_date >= "2023-01-14"
      ) |>
      dplyr::collect()

    expect_df_equal_up_to_order(
      model_out_tbl,
      expected_model_out_tbl
    )
  }
)


test_that(
  "load_model_out_in_window succeeds, min_round_id & n_last_round_ids, min_round_id superceeds",
  {
    model_out_tbl <- load_model_out_in_window(
      hub_path = test_path("testdata", "ecfh"),
      target_id = "wk flu hosp rate category",
      eval_window = list(
        window_name = "some subset",
        min_round_id = "2023-01-14",
        n_last_round_ids = 9
      )
    )

    expected_model_out_tbl <- hubData::connect_hub(
      test_path("testdata", "ecfh")
    ) |>
      dplyr::filter(
        target == "wk flu hosp rate category",
        reference_date >= "2023-01-14"
      ) |>
      dplyr::collect()

    expect_df_equal_up_to_order(
      model_out_tbl,
      expected_model_out_tbl
    )
  }
)


test_that(
  "load_model_out_in_window succeeds, min_round_id & n_last_round_ids, n_last_round_ids superceeds",
  {
    model_out_tbl <- load_model_out_in_window(
      hub_path = test_path("testdata", "ecfh"),
      target_id = "wk flu hosp rate category",
      eval_window = list(
        window_name = "some subset",
        min_round_id = "2022-11-19",
        n_last_round_ids = 5
      )
    )

    expected_model_out_tbl <- hubData::connect_hub(
      test_path("testdata", "ecfh")
    ) |>
      dplyr::filter(
        target == "wk flu hosp rate category",
        reference_date >= "2022-12-17"
      ) |>
      dplyr::collect()

    expect_df_equal_up_to_order(
      model_out_tbl,
      expected_model_out_tbl
    )
  }
)
