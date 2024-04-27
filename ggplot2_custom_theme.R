# defines a custom theme for consistent and simple formatting 
custom_theme <- function(ggFig) {
    
    my_theme <- ggplot2::theme_get()
    
    my_theme <- my_theme +
        ggplot2::theme(
            ggplot2::theme(
                panel.background = ggplot2::element_rect(
                    fill = "black",
                    color = NA),
                plot.background = ggplot2::element_rect(
                    fill = 'grey5',
                    color = NA),
                text = ggplot2::element_text(
                    color = "seashell"),
                panel.grid = ggplot2::element_blank(),
                axis.title.x = ggplot2::element_text(
                    margin = grid::unit(c(5, 5, 5, 5), 
                                        "mm")
                    ),
                axis.title.y = ggplot2::element_text(
                    margin = grid::unit(c(5, 5, 5, 5), 
                                        "mm")
                    )
                )
            )
    
    return(my_theme)
}