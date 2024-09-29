# Author: Eric Mossotti
# CC BY-SA
# ----
# Programmatic db extraction and df transformation, utilizing tidy evaluation.
#
# Allows one to specify which columns to select, which columns to group by for 
# counting, and which column (if any) to transform. It be used for a variety of 
# similar data transformation tasks.
# 
# Results can be weighted to help certain modeling functions run faster while 
# not altering the results one would expect from using non-aggregated datasets 
# as input.
# ---- 
transformData <- function(conn,
                          path,
                          df,
                          select_cols,
                          group_cols = NULL,
                          binary_col = NULL,
                          ntile_col = NULL,
                          pred_col = NULL,
                          zero_val = NULL,
                          one_val = NULL,
                          qtile_levels = NULL,
                          doWeights = FALSE,
                          doQuantile = FALSE,
                          isDF = FALSE) {
    # Weight the data? ----
    if (isTRUE(doWeights)) {
        # Is the data from a df or db? ----
        if (isTRUE(isDF)) {
            freq_data <- df |>
                dplyr::select(dplyr::all_of(select_cols)) |>
                dplyr::add_count(dplyr::across(dplyr::all_of(group_cols))) |>
                dplyr::distinct() |>
                dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)))
        } else {
            freq_data <- dplyr::tbl(conn, path) |>
                dplyr::select(dplyr::all_of(select_cols)) |>
                dplyr::add_count(dplyr::across(dplyr::all_of(group_cols))) |>
                dplyr::distinct() |>
                dplyr::arrange(dplyr::across(dplyr::all_of(group_cols))) |>
                dplyr::collect()
        }
    } else {
        # Is the data from a df or db? ----
        if (isTRUE(isDF)) {
            freq_data <- df |>
                dplyr::select(dplyr::all_of(!!select_cols)) |>
                dplyr::arrange(dplyr::across(dplyr::all_of(!!select_cols)))
        } else {
            freq_data <- dplyr::tbl(conn, path) |>
                dplyr::select(dplyr::all_of(select_cols)) |>
                dplyr::arrange(dplyr::across(dplyr::all_of(select_cols))) |>
                dplyr::collect()
        }
    }
    # Do we want to transform a column to binary for modeling?
    if (!is.null(binary_col) &&
        binary_col %in% names(freq_data)) {
        freq_data <- freq_data |>
            dplyr::mutate(
                # Tidy-selects data with ':=' and bang-bang operators
                !!binary_col := factor(.data[[binary_col]],
                                       levels = c(zero_val, one_val))
            )
    }
    # Do we want to predict anything?
    if (isTRUE(doQuantile)) {
        freq_data <- freq_data |>
            dplyr::mutate(
                !!ntile_col := dplyr::ntile(.data[[pred_col]], n = 4),
                !!ntile_col := factor(.data[[ntile_col]],
                                      levels = c(1, 2, 3, 4),
                                      labels = as.vector(qtile_levels))
            )
    }
    
    return(freq_data)
}
