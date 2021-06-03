#' T-test maker
#'
#' This function preforms a t-test on a vector of data.
#'
#' @param x Vector input of numerics.
#' @param alternative String input of either "less", "greater", or "two.sided"
#'   which tells the function \code{my_t.test} which side of the area under the
#'   curve to calculate.
#' @param mu Single numeric value which is the mean of our test population.
#' @keywords inference
#'
#' @return List of a data frame representing the numeric test statistic \code{test_stat},
#'   the numeric degrees of freedom \code{df}, a string indicating which side of
#'   the model we were calculating \code{alternative}, and the numeric p value \code{p_val}.
#'
#' @examples
#' my_t.test(1:15, "less", 0)
#' my_t.test(c(10,11,12), "two.sided", 11)
#'
#' @export
my_t.test <- function(x, alternative, mu) {

  test_data_mean <- mean(x)

  test_data_se <- sd(x)/sqrt(length(x))

  test_stat = ((test_data_mean - mu) / test_data_se)

  deg_free = length(x) - 1

  if (alternative %in% c("less", "Less", "LESS")) {

    alternative = "Less"

    p_val = pt(test_stat, deg_free, lower.tail = TRUE)

  }

  else if (alternative %in% c("greater", "Greater", "GREATER")) {

    alternative = "Greater"

    p_val = pt(test_stat, deg_free, lower.tail = FALSE)

  }

  else if (alternative == "two.sided") {

    alternative = "two.sided"

    p_val = pt(test_stat, deg_free, lower.tail = FALSE) * 2

  }

  test_list <- as.list(
               data.frame("test_stat" = test_stat,
                          "df" = deg_free,
                          "alternative" = alternative,
                          "p_val" = p_val))

  return(test_list)

}
