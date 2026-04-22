#' Calculate the Hospital Frailty Risk Score (HFRS)
#'
#' @description
#' Calculates the Hospital Frailty Risk Score for each admitted patient episode
#' using ICD-10-AM diagnosis codes. The score is derived by matching the first
#' three characters of each diagnosis code against 109 ICD-10 groups defined by
#' Gilbert et al. (2018), summing the corresponding weights. Each group is
#' counted at most once per episode regardless of how many diagnoses match it.
#'
#' Note: HFRS is based on ICD-10 (not ICD-10-AM specific) — the 3-character
#' group matching means it applies directly to ICD-10-AM coded data.
#'
#' Both wide-format (one row per episode, diagnoses in separate columns) and
#' long-format (separate diagnosis data frame) inputs are supported via the
#' `format` argument.
#'
#' @param df A data frame with one row per episode.
#' @param id The episode identifier column (unquoted name).
#' @param format Input format: `"long"` (default) or `"wide"`.
#' @param df_diag Long-format diagnosis data frame with one row per diagnosis.
#'   Required when `format = "long"`. Must contain the episode ID and a
#'   diagnosis code column.
#' @param diag Name of the diagnosis code column in `df_diag` (quoted string).
#'   Default: `"diag"`. Used only when `format = "long"`.
#' @param diag_prefix Character prefix shared by all diagnosis columns in `df`
#'   (e.g. `"diag"` matches `diag1`, `diag2`, ...). Default: `"diag"`. Used
#'   only when `format = "wide"`.
#' @param hfrs_map Optional. A data frame with columns `icd10_group`
#'   (three-character ICD-10 codes) and `weight` (numeric). Defaults to `NULL`,
#'   which uses the bundled dataset (109 groups, Gilbert et al. 2018).
#'
#' @return The input `df` with two additional columns:
#'   \describe{
#'     \item{`hfrs_score`}{Total HFRS score (numeric). Episodes with no
#'       matching diagnoses receive a score of 0.}
#'     \item{`hfrs_risk`}{Risk category: `"low"` (score < 5),
#'       `"intermediate"` (5 to 15), or `"high"` (> 15).}
#'   }
#'
#' @references
#' Gilbert T, Neuburger J, Kraindler J, et al. (2018). Development and
#' validation of a Hospital Frailty Risk Score focusing on older people in
#' acute care settings using electronic hospital records: an observational
#' study. *The Lancet*, 391(10132), 1775--1782.
#' \doi{10.1016/S0140-6736(18)30668-8}
#'
#' @export
#'
#' @importFrom dplyr all_of case_when distinct filter group_by if_else
#'   left_join mutate rename summarise
#' @importFrom rlang .data abort as_name ensym sym
#' @importFrom stats setNames
#' @importFrom icd10am.utils .to_long
#'
#' @examples
#' dfbase <- icd10am.utils::dfbase
#' dfdiag <- icd10am.utils::dfdiag
#'
#' # Long format
#' score_hfrs(
#'   df      = dfbase,
#'   id      = recordID,
#'   format  = "long",
#'   df_diag = dfdiag,
#'   diag    = "diag"
#' )
#'
#' # Wide format
#' dfwide <- icd10am.utils::dfwide
#' score_hfrs(
#'   df          = dfwide,
#'   id          = recordID,
#'   format      = "wide",
#'   diag_prefix = "diag"
#' )
score_hfrs <- function(
    df,
    id,
    format      = c("long", "wide"),
    df_diag     = NULL,
    diag        = "diag",
    diag_prefix = "diag",
    hfrs_map    = NULL
) {
  format <- match.arg(format)
  id_str <- rlang::as_name(rlang::ensym(id))
  if (is.null(hfrs_map)) hfrs_map <- get("hfrs_map", envir = asNamespace("icd10am.hfrs"))

  # --- Normalise to long format ---
  if (format == "wide") {
    converted <- icd10am.utils::.to_long(df, id = !!rlang::sym(id_str), diag_prefix = diag_prefix)
    df_diag <- converted$df_diag
    diag    <- "diag"
  }

  if (is.null(df_diag)) {
    rlang::abort("`df_diag` must be supplied when `format = 'long'`.")
  }

  # --- Clean and truncate codes to 3-character group ---
  diag_col <- diag
  df_diag_std <- df_diag |>
    dplyr::rename(
      episode_id = dplyr::all_of(id_str),
      diag       = dplyr::all_of(diag_col)
    ) |>
    dplyr::mutate(
      diag        = .clean_icd10(.data$diag),
      icd10_group = substr(.data$diag, 1, 3)
    ) |>
    dplyr::filter(!is.na(.data$icd10_group), .data$icd10_group != "")

  # --- One hit per group per episode ---
  df_deduped <- df_diag_std |>
    dplyr::distinct(.data$episode_id, .data$icd10_group)

  # --- Match to HFRS weights and sum ---
  df_score <- df_deduped |>
    dplyr::left_join(hfrs_map, by = "icd10_group") |>
    dplyr::filter(!is.na(.data$weight)) |>
    dplyr::group_by(.data$episode_id) |>
    dplyr::summarise(hfrs_score = sum(.data$weight, na.rm = TRUE), .groups = "drop")

  # --- Join back and categorise ---
  df |>
    dplyr::left_join(df_score, by = stats::setNames("episode_id", id_str)) |>
    dplyr::mutate(
      hfrs_score = dplyr::if_else(is.na(.data$hfrs_score), 0, .data$hfrs_score),
      hfrs_risk  = dplyr::case_when(
        .data$hfrs_score < 5   ~ "low",
        .data$hfrs_score <= 15 ~ "intermediate",
        TRUE                   ~ "high"
      )
    )
}

#' Clean ICD-10 codes
#'
#' Uppercase, remove dots, trim whitespace, blank to NA.
#'
#' @param x Character vector of ICD-10 codes.
#' @return Cleaned character vector.
#' @noRd
.clean_icd10 <- function(x) {
  x <- as.character(x)
  x <- toupper(x)
  x <- gsub("\\.", "", x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}
