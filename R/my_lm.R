#' Linear model maker
#'
#' This function calculates a linear model.
#'
#' @param formula A formula class object that describes which two numerics  will
#'   be used to calculate the linear model.
#' @param data An inputted data frame of numerics.
#' @keywords inference
#'
#' @return Table of four variables, including the estimated linear regression
#'   coefficient \code{Estimates}, the standard error \code{std.Error}, the
#'   t-value \code{t value}, and the p-value \code{Pr(>|t|)}.
#'
#' @examples
#' penguins <- na.omit(NewPackageLecture9::my_penguins)
#'
#' my_lm(body_mass_g ~ bill_length_mm + bill_depth_mm, penguins) #using the
#'   # palmerspenguins data that is included in this package
#' # my_lm(NUMERIC_VAR_3 ~ NUMERIC_VAR_1 + NUMERIC_VAR_2, DATA_FRAME)
#'
#' @export
my_lm <- function(formula, data) {

  X <- model.matrix(data = data, object = formula)

  mod_fra <- model.frame(data = data, formula = formula)

  Y <- model.response(data = mod_fra)

  X_trans <- t(X)

  est_mate <- solve(X_trans %*% X) %*% X_trans %*% Y

  deg_fre <- nrow(X) - ncol(X)

  sig_sqr <- sum((Y - X %*% est_mate)^2 / deg_fre)

  stand_error <- sqrt(diag(sig_sqr * (solve(X_trans %*% X))))

  t_val <- est_mate / stand_error

  pr_val <- 2 * pt(abs(t_val), deg_fre, lower.tail = FALSE)

  table_sum <- as.table(
               as.matrix(
               data.frame("Estimates" = est_mate,
                          "Std.Error" = stand_error,
                          "t value" = t_val,
                          "Pr(>|t|)" = pr_val)))

  colnames(table_sum)[4] <- "Pr(>|t|)"

  return(table_sum)

}
