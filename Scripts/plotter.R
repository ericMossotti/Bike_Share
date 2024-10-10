#  ----
# Author: Eric Mossotti
# CC BY-SA
#  ----
# To help reduce duplicate code and implement a consistent theme throughout a
# markdown project. This doesn't account for all plot types possible yet.
# ----
plotter <- function(data,
                    x_col,
                    y_col,
                    group_col = NULL,
                    color_col = NULL,
                    title = NULL,
                    subtitle = NULL,
                    x_label = NULL,
                    y_label = NULL,
                    geomType = NULL,
                    lineGroup_palette = "Paired",
                    colGroup_palette = "Paired",
                    line_palette = "YlOrRd",
                    col_palette = "YlOrRd",
                    facetCol_palette = "YlGnBu",
                    fill_col = "YlOrRd",
                    axis_ticks_color = "lavenderblush",
                    bg_color = "#222222",
                    text_color = "seashell",
                    grid_color = "grey30",
                    x_lim = c(NA, NA),
                    isFaceted = FALSE,
                    is_lineGroup = FALSE,
                    is_colGroup = FALSE,
                    isHistogram = FALSE,
                    isDensity = FALSE,
                    breaks = NULL,
                    limits = NULL,
                    angle = ggplot2::waiver(),
                    n.dodge = 1,
                    binwidth = NULL,
                    areaFill = NULL,
                    density_fill = ggplot2::waiver(),
                    density_color = ggplot2::waiver(),
                    alpha = ggplot2::waiver(),
                    isROC = FALSE,
                    roc_color = NULL,
                    vline_color = NULL,
                    vline_size = NULL,
                    density_alpha = ggplot2::waiver(),
                    dnorm_color = ggplot2::wavier(),
                    bins = NULL,
                    histo_breaks = NULL,
                    low = NULL,
                    high = NULL,
                    isTime = FALSE,
                    date_breaks = ggplot2::waiver(),
                    date_labels = ggplot2::waiver(),
                    date_minor_breaks = ggplot2::waiver(),
                    lineColor = NULL,
                    colPosition = "stack",
                    labels = ggplot2::waiver(),
                    isTimeHist = FALSE,
                    quartiles = NULL,
                    qformat = NULL) {
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
            plot <- data |> ggplot2::ggplot(mapping = ggplot2::aes(x = {{ x_col }}, y = {{ y_col }})) +
                ggplot2::geom_line(show.legend = TRUE, color = lineColor)
        }
    }
    # col ----
    else if (geomType == "column") {
        # non-grouped, non-faceted, non-density, non-histogram ----
        if (isFALSE(is_colGroup) &&
            isFALSE(isDensity) &&
            isFALSE(isDensity) &&
            isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(
                    x = {{ x_col }},
                    y = {{ y_col }},
                    fill = {{ y_col }}
                )) +
                ggplot2::geom_col(show.legend = FALSE) +
                ggplot2::scale_fill_distiller(palette = col_palette)
        }
        # grouped, non - faceted, non - density, non - histogram----
        else if (!isFALSE(is_colGroup) &&
                 isFALSE(isFaceted) &&
                 isFALSE(isDensity) &&
                 isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(
                    x = {{ x_col }},
                    y = {{ y_col }},
                    fill = {{ group_col }}
                )) +
                ggplot2::geom_col(
                    show.legend = TRUE,
                    position = colPosition,
                    color = color_col
                ) +
                ggplot2::scale_fill_brewer(palette = colGroup_palette, name = "")
        }
        # grouped, faceted, non-density, non-histogram ----
        else if (!isFALSE(is_colGroup) &&
                 !isFALSE(isFaceted) &&
                 isFALSE(isDensity) &&
                 isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(
                    x = {{ x_col }},
                    y = {{ y_col }},
                    fill = {{ y_col }}
                )) +
                ggplot2::geom_col(show.legend = FALSE) +
                ggplot2::facet_grid(rows = "member_casual") +
                ggplot2::scale_fill_distiller(palette = facetCol_palette)
        }
        # grouped, faceted, density, non-histogram ----
        else if (!isFALSE(is_colGroup) &&
                 !isFALSE(isFaceted) &&
                 !isFALSE(isDensity) &&
                 isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(
                    x = {{ x_col }},
                    y = ..density..,
                    fill = {{ group_col }}
                )) +
                ggplot2::geom_density(alpha = alpha) +
                ggplot2::facet_grid(rows = "member_casual") +
                ggplot2::scale_x_continuous(
                    breaks = breaks,
                    limits = limits,
                    guide = ggplot2::guide_axis(n.dodge = 1, angle = angle)
                )
        }
        # grouped, non-faceted, density, histogram ----
        else if (!isFALSE(is_colGroup) &&
                 isFALSE(isFaceted) &&
                 !isFALSE(isDensity) &&
                 !isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(
                    x = {{ x_col }},
                    y = ..density..,
                    fill = {{ group_col }}
                )) +
                ggplot2::geom_histogram(
                    binwidth = binwidth,
                    color = color_col,
                    alpha = alpha,
                    breaks = histo_breaks
                ) +
                ggplot2::geom_density(alpha = alpha,
                                      color = density_color,
                                      fill = density_fill) +
                ggplot2::stat_function(fun = dnorm,
                                       args = list(mean = mean({{ x_col }}), sd = sd({{ x_col }}))) +
                ggplot2::scale_x_continuous(
                    breaks = breaks,
                    limits = limits,
                    guide = ggplot2::guide_axis(n.dodge = 1, angle = angle)
                )
        }
        # grouped, non-faceted, density, non-histogram ----
        else if (!isFALSE(is_colGroup) &&
                 isFALSE(isFaceted) &&
                 !isFALSE(isDensity) &&
                 isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(
                    x = {{ x_col }},
                    y = ggplot2::after_stat(density),
                    fill = {{ group_col }}
                )) +
                ggplot2::geom_density(alpha = density_alpha, color = color_col) +
                ggplot2::scale_x_continuous(
                    breaks = breaks,
                    limits = limits,
                    guide = ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                ) +
                ggplot2::scale_fill_brewer(palette = colGroup_palette, name = "")
        }
        # non-grouped, non-faceted, density, non-histogram ----
        else if (isFALSE(is_colGroup) &&
                 isFALSE(isFaceted) &&
                 !isFALSE(isDensity) &&
                 isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = {{ x_col }},
                        y = ..density..,
                        fill = density_fill,
                        color = density_color
                    )
                ) +
                ggplot2::geom_density(show.legend = FALSE) +
                ggplot2::geom_vline(
                    ggplot2::aes(xintercept = mean({{x_col}})),
                    color = vline_color,
                    size = vline_size,
                    linetype = "solid"
                ) +
                ggplot2::scale_x_continuous(
                    breaks = breaks,
                    limits = limits,
                    guide = ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                )
        }
        # grouped, non-faceted, non-density, histogram ----
        else if (!isFALSE(is_colGroup) &&
                 isFALSE(isFaceted) &&
                 isFALSE(isDensity) &&
                 !isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(x = {{ x_col }}, fill = {{ group_col }})) +
                ggplot2::geom_histogram(
                    binwidth = binwidth,
                    color = color_col,
                    alpha = alpha,
                    breaks = histo_breaks
                ) +
                ggplot2::scale_x_continuous(
                    breaks = breaks,
                    limits = limits,
                    guide = ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                )
        }
        # grouped, faceted, non-density, non-histogram ----
        else if (!isFALSE(is_colGroup) &&
                 !isFALSE(isFaceted) &&
                 isFALSE(isDensity) &&
                 isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(
                    x = {{ x_col }},
                    y = {{ y_col }},
                    fill = {{ group_col }}
                )) +
                ggplot2::geom_col(show.legend = TRUE, position = "dodge") +
                ggplot2::scale_fill_brewer(palette = colGroup_palette, name = "") +
                ggplot2::scale_x_discrete(
                    breaks = breaks,
                    limits = limits,
                    guide = ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                )
        }
        # grouped, faceted, non-density, histogram ----
        else if (!isFALSE(is_colGroup) &&
                 !isFALSE(isFaceted) &&
                 isFALSE(isDensity) &&
                 !isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(x = {{ x_col }}, fill = {{ group_col }})) +
                ggplot2::geom_histogram(
                    binwidth = binwidth,
                    color = color_col,
                    bins = bins,
                    breaks = breaks
                ) +
                ggplot2::geom_vline(
                    ggplot2::aes(xintercept = mean({{x_col}})),
                    color = vline_color,
                    size = vline_size,
                    linetype = "solid"
                ) +
                ggplot2::facet_grid(rows = "member_casual") +
                ggplot2::scale_x_continuous(
                    breaks = breaks,
                    limits = limits,
                    guide = ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                )
        }
        # non-grouped, non-faceted, non-density, histogram ----
        else if (isFALSE(is_colGroup) &&
                 isFALSE(isFaceted) &&
                 isFALSE(isDensity) &&
                 !isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(x = {{ x_col }})) +
                ggplot2::geom_histogram(
                    show.legend = FALSE,
                    color = color_col,
                    binwidth = binwidth,
                    ggplot2::aes(fill = ..count..)
                )
            if (!isFALSE(isTimeHist)) {
                plot <- plot +
                    ggplot2::scale_x_datetime(
                        date_breaks = date_breaks,
                        date_labels = date_labels,
                        date_minor_breaks = date_minor_breaks,
                        guide =  ggplot2::guide_axis(n.dodge = n.dodge, angle = angle),
                        sec.axis = ggplot2::sec_axis(
                            ~ .,
                            breaks = quartiles,
                            labels = scales::date_format(qformat),
                            guide =  ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                        )
                    )
            } else {
                plot <- plot +
                    ggplot2::scale_x_continuous(
                        breaks = breaks,
                        limits = limits,
                        guide = ggplot2::guide_axis(n.dodge = n.dodge, angle = angle),
                        sec.axis = ggplot2::sec_axis(
                            ~ .,
                            breaks = quartiles,
                            labels = scales::label_number(),
                            guide =  ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                        )
                    )
            }
            plot <- plot +
                ggplot2::scale_y_continuous() +
                ggplot2::scale_fill_gradient(low = low, high = high) +
                ggplot2::geom_vline(
                    ggplot2::aes(xintercept = mean({{x_col}})),
                    color = vline_color,
                    size = vline_size,
                    linetype = "solid"
                ) +
                ggplot2::geom_vline(
                    data = data.frame(q = quartiles),
                    ggplot2::aes(xintercept = q, color = factor(q)),
                    linetype = "dashed",
                    size = 1
                ) +
                ggplot2::scale_color_manual(
                    values = c("red", "green", "blue"),
                    labels = c("25th", "50th", "75th")
                )
        }
        # non-grouped, non-faceted, density, histogram ----
        else if (isFALSE(is_colGroup) &&
                 isFALSE(isFaceted) &&
                 !isFALSE(isDensity) &&
                 !isFALSE(isHistogram)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(x = {{ x_col }}, y = ..density..)) +
                ggplot2::geom_histogram(color = color_col,
                                        bins = bins,
                                        breaks = breaks) +
                ggplot2::geom_density(color = density_color,
                                      fill = density_fill,
                                      alpha = density_alpha) +
                ggplot2::geom_vline(
                    ggplot2::aes(xintercept = mean({{ x_col }})),
                    color = vline_color,
                    size = vline_size,
                    linetype = "solid"
                ) +
                ggplot2::scale_x_continuous(
                    breaks = breaks,
                    limits = limits,
                    guide = ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                )
        }
        # grouped, faceted, density, histogram ----
        else {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(x = {{ x_col }},
                                             fill = {{ group_col }})) +
                ggplot2::geom_histogram(
                    binwidth = binwidth,
                    color = color_col,
                    bins = bins,
                    breaks = histo_breaks
                ) +
                ggplot2::geom_density(color = density_color,
                                      fill = density_fill,
                                      alpha = density_alpha) +
                ggplot2::geom_vline(
                    ggplot2::aes(xintercept = mean({{ x_col }})),
                    color = vline_color,
                    size = vline_size,
                    linetype = "solid"
                ) +
                ggplot2::facet_grid(rows = "member_casual") +
                ggplot2::scale_x_continuous(
                    breaks = breaks,
                    limits = limits,
                    guide = ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                )
        }
        # other misc plot types ----
    } else {
        # pROC objects ----
        if (!isFALSE(isROC)) {
            plot <- data |>
                pROC::ggroc(aes = "linetype", color = roc_color)
        }
        # time-series data ----
        # grouped, non-faceted, density, non-histogram
        if (!isFALSE(isTime)) {
            plot <- data |>
                ggplot2::ggplot(ggplot2::aes(x = {{ x_col }},
                                             fill = {{ group_col }})) +
                ggplot2::geom_density(alpha = density_alpha, color = color_col) +
                ggplot2::scale_x_datetime(
                    date_breaks = date_breaks,
                    date_labels = date_labels,
                    date_minor_breaks = date_minor_breaks,
                    labels = labels,
                    guide =  ggplot2::guide_axis(n.dodge = n.dodge, angle = angle)
                ) +
                ggplot2::scale_fill_brewer(palette = colGroup_palette, name = "")
        }
    }
    # For the rest of the otherwise likely duplicated plot settings ----
    plot <- plot +
        ggplot2::labs(title = title, subtitle = subtitle, x = x_label, y = y_label) +
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
            panel.grid.major.x = ggplot2::element_line(color = grid_color, linetype = "dotted"),
            panel.grid.major.y = ggplot2::element_line(color = grid_color),
            legend.background = ggplot2::element_rect(fill = bg_color),
            legend.title = ggplot2::element_blank()
        )
}