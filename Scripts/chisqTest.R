# ----
# Author: Eric Mossotti
# CC BY-SA
# ----
# The code for returning chi-square test results as a tibble for use in tables.
# ----
chisqTest <- function(data, variable, by) {
    test_result <- chisq.test(x = data[[variable]], y = data[[by]]) |>
        broom::tidy() |>
        dplyr::select(statistic, parameter, p.value)
    
    return(test_result)
}