#' Descriptive stats
#'
#' @param data
#'
#' @return A data.frame/tibble.
descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(across(value,list(mean = mean, sd = sd))) %>%
        mutate(across(where(is.numeric), ~round(.x, digits = 1)))
}
