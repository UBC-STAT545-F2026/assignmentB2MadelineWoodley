
#' Title: Calculate grouped means
#'
#' Description: This function groups information within `data` based on values in `var1` and calculates the mean of values in `var2` based on the `var1` groupings, returning a summary table with 'var1' and a column with grouped `var2` means.
#' @param data A dataframe. Named `data` to indicate this is where the dataframe is called, as opposed to column names, for clarity.
#' @param var1 A column in `data` which the third argument, `var2` can be logically grouped by. Note that `var1` can be numerical or categorical. Named `var1` as shorthand for "variable 1" to indicate that this argument is looking for a column within desired dataframe, `data`.
#' @param var2 A column in `data` containing numeric data for which a mean can be calculated with the `var1` groupings. Named `var2` as shorthand for "variable 2" to indicate taht this argument is looking for a second column within the desired dataframe, `data`.
#' @param removeNAsvar2 An argument indicating whether NAs in `var2` should be dropped (`removeNAsvar2 = TRUE`) or not (default; `removeNAsvar2 = FALSE`). Named `removeNAsvar2` to indicate that the purpose of this argument is to determine whether NA values should be dropped from variable 2.
#' @return a table containing two columns: `var1` and `meanvar2` (grouped `var2` means)
#'
#' @importFrom dplyr %>% group_by filter summarise
#'
#' @examples
#' groupedmeans(data = palmerpenguins::penguins, var1 = species, var2 = bill_length_mm, removeNAsvar2 = TRUE)
#' groupedmeans(data = palmerpenguins::penguins, var1 = sex, var2 = body_mass_g, removeNAsvar2 = TRUE)
#' groupedmeans(data = gapminder::gapminder, var1 = year, var2 = lifeExp, removeNAsvar2 = TRUE)
#'
#' @md
#' @export


groupedmeans <- function(data, var1, var2, removeNAsvar2 = FALSE) { # code for function, defining what inputs will be defined by the user

  if(!is.numeric(dplyr::pull(data, {{var2}}))) { # return error if column assigned to var2 is not numeric (i.e., categorical data)
    stop("var2 is not numeric") # return associated error message
  }

  if(any(!is.na(dplyr::pull(data, {{var2}}))) &&!removeNAsvar2) { # return error if column assigned to var2 contains NA values
    stop("var2 contains NA values, cannot compute mean") # return explanatory error message
  }

  data %>% # identify the dataset
    group_by({{var1}}) %>% # group by variable 1
    filter(!is.na({{var1}})) %>% # drop rows where value for variable 1 is NA (doesn't tell us anything in the context of our function objective)
    summarise(meanvar2 = mean({{var2}}, na.rm = removeNAsvar2)) # calculate the mean for variable 2, within groups based on variable 1 values
}
