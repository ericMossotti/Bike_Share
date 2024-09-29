# ----
# Author: Eric Mossotti
# CC BY-SA
# ----
# Reduces duplicate code for generating publication ready tables.
# ----
tabler <- function(tbl_name,
                   source_note = NULL,
                   title = NULL,
                   subtitle = NULL,
                   groupName = NULL,
                   by = NULL,
                   label = NULL,
                   label_n = "n",
                   label_member = "member_casual",
                   location = NULL,
                   footnote = NULL,
                   footer_column = NULL,
                   value_columns = gt::everything(),
                   decimals = 0,
                   font_color = "seashell",
                   bg_color = "transparent",
                   vline_color = "gray20",
                   hline_color = "gray20",
                   hide_column_labels = FALSE,
                   isSummary = FALSE,
                   isBinary = FALSE,
                   note_list = NULL,
                   location_list = NULL,
                   noteColumns = FALSE,
                   noteRows = FALSE,
                   chi_result = NULL,
                   chiVar = NULL,
                   chiBy = NULL,
                   stub_label = NULL, 
                   stub_note = NULL,
                   stubName = NULL,
                   isStub = FALSE) {
    
    # Need to account for grouped tables and non-grouped tables
    # Grouped ----
    if (!is.null(groupName)) {
        tbl <- gt::gt(tbl_name,
                      groupname_col = groupName,
                      row_group_as_column = TRUE) |>
            gt::tab_header(title = title, subtitle = subtitle) |>
            gt::tab_stubhead(label = stub_label) |>
            gt::fmt_number(decimals = decimals, columns = value_columns) |>
            gt::tab_options(
                table.background.color = bg_color,
                table.font.color = font_color,
                table_body.vlines.color = vline_color,
                table_body.hlines.color = hline_color,
                row.striping.include_table_body = TRUE,
                row.striping.background_color = "gray10",
                column_labels.font.weight = "bold",
                column_labels.border.bottom.color = hline_color
            ) |>
            gt::tab_source_note(source_note = {{ source_note }})
    }
    
    # Chi Square ----
    if (isSummary == TRUE) {
        tbl <- tbl_name |>
            gtsummary::tbl_summary(by = {{ by }}, 
                                   label = label) |>
            gtsummary::modify_footnote(
                gtsummary::all_stat_cols() ~ paste0(
                    "n (%);   χ²  = ", 
                    round(chi_result$statistic, 2),
                    ";   df = ", 
                    chi_result$parameter)) |>  
            gtsummary::add_p() |>
            gtsummary::as_gt() |>
            gt::tab_header(title = title, subtitle = subtitle) |>
            gt::tab_style(
                style = list(
                    gt::cell_borders(sides = "bottom"),
                    gt::cell_text(
                        align = "left",
                        stretch = "semi-expanded",
                        whitespace = "break-spaces"
                    )
                ),
                locations = gt::cells_body(gt::everything())
            ) |>
            gt::tab_style(
                gt::cell_text(
                    align = "center",
                    stretch = "semi-expanded",
                    whitespace = "break-spaces"
                ),
                locations = list(
                    gt::cells_title(groups = c("title", "subtitle")),
                    gt::cells_column_labels(gt::everything())
                )
            ) |>
            gt::tab_options(
                table.background.color = bg_color,
                table.font.color = font_color,
                table_body.vlines.color = vline_color,
                table_body.hlines.color = hline_color,
                column_labels.hidden = hide_column_labels,
                row.striping.include_table_body = TRUE,
                row.striping.background_color = "gray10",
                column_labels.font.weight = "bold",
            ) |>
            gt::tab_source_note(source_note = source_note)
    }
    
    # Binary Logistic ----
    if (isBinary == TRUE) {
        tbl <- tbl_name |>
            gtsummary::as_gt() |>
            gt::tab_header(title = title, subtitle = subtitle) |>
            gt::tab_style(
                style = list(
                    gt::cell_borders(sides = "bottom"),
                    gt::cell_text(
                        align = "left",
                        stretch = "semi-expanded",
                        whitespace = "break-spaces"
                    )
                ),
                locations = gt::cells_body(gt::everything())
            ) |>
            gt::tab_style(
                gt::cell_text(
                    align = "center",
                    stretch = "semi-expanded",
                    whitespace = "break-spaces"
                ),
                locations = list(
                    gt::cells_title(groups = c("title", "subtitle")),
                    gt::cells_column_labels(gt::everything())
                )
            ) |> 
            gt::tab_options(
                table.background.color = bg_color,
                table.font.color = font_color,
                table_body.vlines.color = vline_color,
                table_body.hlines.color = hline_color,
                row.striping.include_table_body = TRUE,
                row.striping.background_color = "gray10",
                column_labels.font.weight = "bold",
            ) |>
            gt::tab_source_note(source_note = source_note)
    }
    # non - grouped, non - binary, non - summary ----
    if (is.null(groupName) &&
        isFALSE(isBinary) &&
        isFALSE(isSummary)) {
        tbl <- tbl_name |>
            gt::gt() |>
            gt::tab_header(title = title, subtitle = subtitle) |>
            gt::fmt_number(decimals = decimals, columns = value_columns) |>
            gt::tab_style(
                style = list(
                    gt::cell_borders(sides = c("right", "left"), color = vline_color),
                    gt::cell_text(#align = "left",
                        stretch = "semi-expanded", whitespace = "break-spaces")
                ),
                locations = gt::cells_body(gt::everything())
            ) |>
            gt::tab_style(
                style = list(
                    gt::cell_borders(sides = c("right", "left", "top"), color = vline_color),
                    gt::cell_text(
                        align = "center",
                        stretch = "semi-expanded",
                        whitespace = "break-spaces"
                    )
                ),
                locations = list(
                    gt::cells_title(groups = c("title", "subtitle")),
                    gt::cells_column_labels(gt::everything())
                )
            ) |> 
            gt::tab_options(
                table.background.color = bg_color,
                table.font.color = font_color,
                table_body.vlines.color = vline_color,
                table_body.hlines.color = hline_color,
                column_labels.vlines.color = vline_color,
                heading.border.lr.color = "transparent",
                table.border.bottom.color = "transparent",
                table.border.top.color = "transparent",
                row.striping.include_table_body = TRUE,
                row.striping.background_color = "gray10",
                column_labels.font.weight = "bold"
            ) |>
            gt::tab_source_note(source_note = source_note)
    }
    # To add footnotes to the various table types ----
    if (!is.null(note_list) && !is.null(location_list)) {
        tbl <- tbl |>
            add_multiple_footnotes(note_list, location_list, noteColumns, noteRows)
    }
    
    if (isTRUE(isStub)) {
        tbl <- tbl |>
            gt::tab_stubhead(label = stub_label) |>
            gt::cols_align(columns = groupName, align = "center") |>
            gt::tab_footnote(footnote = stub_note,
                             location = gt::cells_stubhead(),
                             placement = "left")
    }
    
    return(tbl)
}


# Footnotes function ----
add_multiple_footnotes <- function(tbl, note_list, location_list, noteColumns, noteRows) {
    
    if (length(note_list) != length(location_list)) {
        stop("The lengths of note_list and location_list must be equal.")
    }
    
    if (isTRUE(noteColumns)) {
        for (i in seq_along(note_list)) {
            tbl <- tbl |>
                gt::tab_footnote(
                    footnote = note_list[[i]],
                    location = gt::cells_column_labels(columns = location_list[[i]]),
                    placement = "left"
                )
        }
    }
    
    if (isTRUE(noteRows)) {
        for (i in seq_along(note_list)) {
            tbl <- tbl |>
                gt::tab_footnote(footnote = note_list[[i]],
                                 location = gt::cells_stub(rows = location_list[[i]]),
                                 placement = "left"
                ) 
        }
    }
    
    return(tbl)
}

