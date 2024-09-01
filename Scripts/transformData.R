# Header ----
# Author: Eric Mossotti
# Date: `r paste(Sys.Date())`
# Copyright (c) Eric Mossotti, `r paste(format(Sys.Date(), "%Y"))`
# Email: ecmossotti@gmail.com 

# Description ----
# 
# Programmatic db extraction and df transformation, utilizing tidy evaluation.
#
# Allows one to specify which columns to select, 
# which columns to group by for counting, 
# and which column (if any) to transform.
#  
# Can be used for a variety of similar data transformation tasks.
# 
# Results in weighted data that can help certain modeling functions
# run faster while not altering the results one would expect
# from using non-aggregated datasets as input.

# function ---- 
transformData <- function(dbconn,
                          tblPath_fltrd,
                          select_cols,
                          group_cols,
                          binary_col = NULL,
                          ntile_col = NULL,
                          pred_col = NULL,
                          zero_val = NULL,
                          one_val = NULL,
                          qtile_levels = NULL,
                          doWeights = FALSE) {
    
    if (isTRUE(doWeights)) {
        freq_data <- dplyr::tbl(dbconn, tblPath_fltrd) |>
            dplyr::select(dplyr::all_of(select_cols)) |>
            dplyr::add_count(dplyr::across(dplyr::all_of(group_cols))) |>
            dplyr::distinct() |>
            dplyr::arrange(dplyr::across(dplyr::all_of(group_cols))) |>
            dplyr::collect()
    } else {
        freq_data <- dplyr::tbl(dbconn, tblPath_fltrd) |>
            dplyr::select(dplyr::all_of(select_cols)) |>
            dplyr::arrange(dplyr::across(dplyr::all_of(select_cols))) |>
            dplyr::collect()
    }
    
    # Perhaps overboard, but ensures the column to be transformed to binary exists
    if (!is.null(binary_col) &&
        binary_col %in% names(freq_data)) {
        freq_data <- freq_data |>
            dplyr::mutate(
                # Tidy-selects data with ':=' and bang-bang operators
                !!binary_col := factor(.data[[binary_col]],
                                       levels = c(zero_val, one_val))
            )
    }
    
    if (!is.null(pred_col) && 
        pred_col %in% names(freq_data)) {
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
