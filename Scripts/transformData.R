
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
                          binary_col,
                          zero_val,
                          one_val) {
    
    freq_data <- dplyr::tbl(dbconn, tblPath_fltrd) |>
        dplyr::select(dplyr::all_of(select_cols)) |>
        dplyr::add_count(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::distinct() |>
        dplyr::arrange(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::collect()
    
    # Perhaps overboard, but ensures the column to be transformed to binary exists
    if (!is.null(binary_col) &&
        binary_col %in% names(freq_data)) {
        freq_data <- freq_data |>
            dplyr::mutate(
                # Tidy-selects data with ':=' and bang-bang operators
                !!binary_col := dplyr::case_when(
                    # Explicitly define values as integers
                    .data[[binary_col]] == zero_val ~ 0L,
                    .data[[binary_col]] == one_val ~ 1L,
                    # Assign NA to non-matching data
                    TRUE ~ NA_real_
                ),!!binary_col := forcats::as_factor(.data[[binary_col]])
            )
    }
    
    return(freq_data)
}
