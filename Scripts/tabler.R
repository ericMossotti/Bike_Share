# tabler function  ----

tabler <- function(tbl_name,
                   source_note = NULL,
                   title = NULL,
                   subtitle = NULL,
                   groupName = NULL,
                   by = NULL,
                   label = NULL,
                   label_n = "N",
                   label_member = "member_casual",
                   colVar = as.character(colVar),
                   colValue,
                   location = location,
                   footnote = footnote,
                   value_columns = gt::everything(),
                   decimals = 0,
                   font_color = "SeaShell",
                   bg_color = "transparent",
                   vline_color = "gray20",
                   hline_color = "gray20",
                   hide_column_labels = FALSE,
                   isSummary = FALSE,
                   isBinary = FALSE) {
    
    # Need to account for grouped tables and non-grouped tables
    
    # Grouped ----
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
            gt::cols_label(n = label_n, 
                           member_casual = label_member) |>
            gt::tab_footnote(footnote = {{ footnote }},
                             location = gt::cells_column_labels(columns = {{ location }})) |>
            gt::tab_source_note(source_note = {{ source_note }})
    } 
    
    # Chi Square ----
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
        
    } 
    
    # Binary Logistic ----
    else if (isBinary == TRUE) {
        tbl <- tbl_name |>
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
        
        # Ungrouped ----
         tbl <- tbl_name |>
            gt::gt() |>
            gt::tab_header(title = title) |>
            gt::fmt_number(decimals = decimals, columns = value_columns) |>
            gt::tab_options(
                table.background.color = bg_color,
                table.font.color = font_color,
                table_body.vlines.color = vline_color,
                table_body.hlines.color = hline_color,
                column_labels.hidden = hide_column_labels
                
            ) |>
             gt::tab_source_note(source_note = source_note) |>
             gt::tab_footnote(footnote = {{ footnote }},
                              location = gt::cells_column_labels(columns = {{ location }}))
         }
    
    return(tbl)
}
