# Plotter ----

# To help reduce duplicate code
plotter <- function(data,
                    x_col,
                    y_col,
                    group_col = NULL,
                    color_col = NULL,
                    title = NULL,
                    x_label = NULL,
                    y_label = NULL,
                    geomType = NULL,
                    lineGroup_palette = "Paired",
                    colGroup_palette = "Paired",
                    line_palette = "YlOrRd",
                    col_palette = "YlOrRd",
                    facetCol_palette = "YlGnBu",
                    fill_col = "YlOrRd",
                    axis_ticks_color = "LavenderBlush",
                    bg_color = "#222222",
                    text_color = "seashell",
                    grid_color = "grey30",
                    x_lim = c(NA, NA),
                    isFaceted = FALSE,
                    is_lineGroup = FALSE,
                    is_colGroup = FALSE) {
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
            plot <- data |> ggplot2::ggplot(mapping = ggplot2::aes(x = {{ x_col }}, 
                                                                   y = {{ y_col }}, 
                                                                   color = {{ y_col }})) +
                ggplot2::geom_line(show.legend = TRUE) +
                ggplot2::scale_colour_distiller(palette = line_palette, name = "")
        }
    }
    
    # col ----
    else if (geomType == "column") {
        if (is_colGroup == TRUE) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(
                    x = {{ x_col }},
                    y = {{ y_col }},
                    fill = {{ group_col }}
                )) +
                ggplot2::geom_col(show.legend = TRUE,
                                  position = "dodge") +
                ggplot2::scale_fill_brewer(palette = colGroup_palette, name = "")
        }
        
        # faceted or un-faceted ----
        else if (isFaceted == TRUE) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(x = {{ x_col }}, 
                                             y = {{ y_col }}, 
                                             fill = {{ y_col }})) +
                ggplot2::geom_col(show.legend = FALSE) +
                ggplot2::facet_grid(rows = "member_casual") +
                ggplot2::scale_fill_distiller(palette = facetCol_palette)
        } 
        # un-grouped and un-faceted
        else if (is_colGroup == FALSE && isFaceted == FALSE) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(
                    x = {{ x_col }},
                    y = {{ y_col }},
                    fill = {{ y_col }}
                )) +
                ggplot2::geom_col(show.legend = FALSE) +
                ggplot2::scale_fill_distiller(palette = col_palette)
            
        }
    }
    # For the rest of the otherwise likely duplicated plot settings ----
    plot <- plot +
        ggplot2::labs(title = title, x = x_label, y = y_label) +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
            plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
            text = ggplot2::element_text(color = text_color),
            panel.grid = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_text(margin = grid::unit(c(5, 5, 5, 5), "mm")),
            axis.title.y = ggplot2::element_text(margin = grid::unit(c(5, 5, 5, 5), "mm")),
            axis.text.x = ggplot2::element_text(color = "Snow", margin = grid::unit(c(1, 1, 1, 1), "mm")),
            axis.text.y = ggplot2::element_text(color = "Snow", margin = grid::unit(c(2, 2, 2, 2), "mm")),
            axis.ticks = ggplot2::element_line(color = axis_ticks_color),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(color = grid_color, 
                                                       linetype = "dotted"),
            panel.grid.major.y = ggplot2::element_line(color = grid_color),
            legend.background = ggplot2::element_rect(fill = bg_color),
            legend.title = ggplot2::element_blank()
        )
    
}