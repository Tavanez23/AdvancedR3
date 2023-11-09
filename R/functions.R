#' Descriptive stats
#'
#' @param data
#'
#' @return A data.frame/tibble.
descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(across(value, list(mean = mean, sd = sd))) %>%
    dplyr::mutate(across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
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


#' Convert colloumn values strings into snakecase
#'
#' @param data data with string colloumns
#' @param cols The colloumn that needs to be converted into snakecase
#'
#' @return a data frame

column_values_to_snake_case <- function(data, cols) {
  data %>%
    dplyr::mutate(dplyr::across({{ cols }}, snakecase::to_snake_case))
}

#' Transformation recipe to pre-process the data
#'
#' @param data The lipidomics dataset
#' @param metabolite_variable  the collumn of the metabolite variable
#'
#' @return Recipe with specifications

create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) %>%
    recipes::update_role({{ metabolite_variable }}, age, gender, new_role = "predictor") %>%
    recipes::update_role(class, new_role = "outcome") %>%
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}
