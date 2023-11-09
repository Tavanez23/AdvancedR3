#' Descriptive stats
#'
#' @param data
#'
#' @return A data.frame/tibble.
descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(across(value,list(mean = mean, sd = sd))) %>%
        dplyr::mutate(across(tidyselect::where(is.numeric), ~round(.x, digits = 1)))
}

#' ggplot function
#'
#' @param data
#'
#' @return A plot object
plot_distributions <- function(data) {
    data %>%
        ggplot2::ggplot(ggplot2::aes(x = value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}
