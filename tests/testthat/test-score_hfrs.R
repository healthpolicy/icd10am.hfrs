dfbase <- icd10am.utils::dfbase
dfdiag <- icd10am.utils::dfdiag
dfwide <- icd10am.utils::dfwide

test_that("score_hfrs returns expected columns in long format", {
  result <- score_hfrs(
    df      = dfbase,
    id      = recordID,
    format  = "long",
    df_diag = dfdiag,
    diag    = "diag"
  )

  expect_true("hfrs_score" %in% names(result))
  expect_true("hfrs_risk" %in% names(result))
  expect_equal(nrow(result), nrow(dfbase))
  expect_true(all(result$hfrs_score >= 0))
})

test_that("score_hfrs risk categories are valid", {
  result <- score_hfrs(
    df      = dfbase,
    id      = recordID,
    format  = "long",
    df_diag = dfdiag,
    diag    = "diag"
  )

  expect_true(all(result$hfrs_risk %in% c("low", "intermediate", "high")))
})

test_that("score_hfrs wide format produces same scores as long format", {
  df_episodes <- dfwide[, c("recordID", "ageyears", "agemonths")]

  diag_cols <- grep("^diag[0-9]+$", names(dfwide), value = TRUE)
  df_diag_long <- tidyr::pivot_longer(
    dfwide[, c("recordID", diag_cols)],
    cols      = dplyr::all_of(diag_cols),
    names_to  = "diagno",
    values_to = "diag"
  ) |>
    dplyr::filter(!is.na(diag), diag != "")

  result_long <- score_hfrs(
    df      = df_episodes,
    id      = recordID,
    format  = "long",
    df_diag = df_diag_long,
    diag    = "diag"
  )

  result_wide <- score_hfrs(
    df          = dfwide,
    id          = recordID,
    format      = "wide",
    diag_prefix = "diag"
  )

  scores_long <- result_long[order(result_long$recordID), "hfrs_score"][[1]]
  scores_wide <- result_wide[order(result_wide$recordID), "hfrs_score"][[1]]

  expect_equal(scores_long, scores_wide)
})

test_that("score_hfrs gives score 0 for episodes with no matching diagnoses", {
  empty_diag <- data.frame(
    recordID = dfbase$recordID[1],
    diag     = "ZZZZZ"
  )

  result <- score_hfrs(
    df      = dfbase[1, ],
    id      = recordID,
    format  = "long",
    df_diag = empty_diag,
    diag    = "diag"
  )

  expect_equal(result$hfrs_score, 0)
  expect_equal(result$hfrs_risk, "low")
})

test_that("score_hfrs produces non-zero scores for known episodes", {
  result <- score_hfrs(
    df      = dfbase,
    id      = recordID,
    format  = "long",
    df_diag = dfdiag,
    diag    = "diag"
  )

  expect_true(any(result$hfrs_score > 0))
})

test_that("score_hfrs accepts custom hfrs_map", {
  custom_map <- data.frame(icd10_group = "I10", weight = 99.0)

  diag_i10 <- data.frame(recordID = dfbase$recordID[1], diag = "I10")

  result <- score_hfrs(
    df       = dfbase[1, ],
    id       = recordID,
    format   = "long",
    df_diag  = diag_i10,
    diag     = "diag",
    hfrs_map = custom_map
  )

  expect_equal(result$hfrs_score, 99.0)
  expect_equal(result$hfrs_risk, "high")
})
