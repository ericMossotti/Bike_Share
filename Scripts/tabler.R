# tabler function  ----

tabler <- function(tbl_name,
                   source_note = NULL,
                   title = NULL,
                   subtitle = NULL,
                   groupName = NULL,
                   by = NULL,
                   label = NULL,
                   value_columns = gt::everything(),
                   decimals = 0,
                   font_color = "SeaShell",
                   bg_color = "transparent",
                   vline_color = "gray20",
                   hline_color = "gray20",
                   hide_column_labels = FALSE,
                   isSummary = FALSE) {
    # Need to account for grouped tables and non-grouped tables
    if (!is.null(groupName)) {
        tbl <- gt::gt(tbl_name,
                      groupname_col = groupName,
                      row_group_as_column = TRUE) |>
            gt::tab_header(title = title, subtitle = subtitle) |>
            gt::fmt_number(decimals = decimals, columns = value_columns) |>
            gt::tab_options(
                table.background.color = bg_color,
                table.font.color = font_color,
                table_body.vlines.color = vline_color,
                table_body.hlines.color = hline_color,
                column_labels.hidden = hide_column_labels,
            ) |>
            gt::tab_source_note(source_note = source_note)
        
    } 
    
    else if (isSummary == TRUE) {
        tbl <- tbl_name |>
            gtsummary::tbl_summary(by = {{ by }}, label = label) |>
            gtsummary::add_p() |>
            gtsummary::as_gt() |>
            gt::tab_options(
                table.background.color = bg_color,
                table.font.color = font_color,
                table_body.vlines.color = vline_color,
                table_body.hlines.color = hline_color,
                column_labels.hidden = hide_column_labels
            ) |>
            gt::tab_header(title = title) |>
            gt::tab_source_note(source_note = source_note)
        
    } else {
        
        tbl <- gt::gt(tbl_name) |>
            gt::tab_header(title = title) |>
            gt::fmt_number(decimals = decimals, columns = value_columns) |>
            gt::tab_options(
                table.background.color = bg_color,
                table.font.color = font_color,
                table_body.vlines.color = vline_color,
                table_body.hlines.color = hline_color,
                column_labels.hidden = hide_column_labels
            )       |>
            gt::tab_source_note(source_note = source_note)
        
    }
    
    return(tbl)
}
