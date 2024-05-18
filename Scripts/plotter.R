# Plotter ----

# To help reduce duplicate code
plotter <- function(data,
                    x_col,
                    y_col,
                    color_col = NULL,
                    fill_col = "YlOrRd",
                    title = NULL,
                    x_label = NULL,
                    y_label = NULL,
                    lineGroup_palette = "Paired",
                    line_palette = "YlOrRd",
                    col_palette = "YlOrRd",
                    colGroup_palette = "YlGnBu",
                    x_lim = c(NA, NA),
                    bg_color = "#222222",
                    text_color = "seashell",
                    grid_color = "grey30",
                    axis_ticks_color = "LavenderBlush",
                    geomType = NULL,
                    isFaceted = FALSE,
                    is_lineGroup = FALSE) {
    # line ----
    if (geomType == "line") {
        if (is_lineGroup == TRUE) {
            plot <- data |> ggplot2::ggplot(mapping = ggplot2::aes(
                x = {{ x_col }},
                y = {{ y_col }},
                color = {{ color_col }}
            )) +
                ggplot2::geom_line(show.legend = TRUE) +
                ggplot2::scale_color_brewer(palette = lineGroup_palette, name = "")
        } else {
            plot <- data |> ggplot2::ggplot(mapping = ggplot2::aes(
                x = {{ x_col }},
                y = {{ y_col }},
                color = {{ y_col }}
            )) +
                ggplot2::geom_line(show.legend = TRUE) +
                ggplot2::scale_color_brewer(palette = line_palette, name = "")
        }
    }
    
    # col ----
    else if (geomType == "column") {
        plot <- data |>
            ggplot2::ggplot(ggplot2::aes(x = {{ x_col }}, y = {{ y_col }}, fill = {{ y_col }})) +
            ggplot2::geom_col(show.legend = FALSE)
# faceted or un-faceted ----
        if (isFaceted == TRUE) {
            plot <- plot +
                ggplot2::facet_grid(rows = "member_casual") +
                ggplot2::scale_fill_distiller(palette = colGroup_palette)
        } else {
            plot <- plot +
                ggplot2::scale_fill_distiller(palette = col_palette)
        }
        
    }
    
    # For the rest of the otherwise likely duplicated plot settings ----
    plot <- plot +
       # ggplot2::xlim(x_lim[1], x_lim[2]) +
        ggplot2::labs(title = title, x = x_label, y = y_label) +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
            plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
            text = ggplot2::element_text(color = text_color),
            # panel.grid = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_text(margin = grid::unit(c(5, 5, 5, 5), "mm")),
            axis.title.y = ggplot2::element_text(margin = grid::unit(c(5, 5, 5, 5), "mm")),
            axis.text.x = ggplot2::element_text(color = "Snow", margin = grid::unit(c(1, 1, 1, 1), "mm")),
            axis.text.y = ggplot2::element_text(color = "Snow", margin = grid::unit(c(2, 2, 2, 2), "mm")),
            axis.ticks = ggplot2::element_line(color = axis_ticks_color),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(color = grid_color, linetype = "dotted"),
            panel.grid.major.y = ggplot2::element_line(color = grid_color),
            legend.background = ggplot2::element_rect(fill = bg_color)
        )
    
}