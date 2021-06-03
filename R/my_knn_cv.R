#' K nearest neighbors approximation
#'
#' This function approximates a k-nearest neighbors algorithm for a data frame.
#'
#' @param train An inputted data frame of numerics to act as training data.
#' @param cl An inputted vector of the true class value of our training data.
#' @param k_nn The number of neighbors to based our training example from.
#' @param k_cv The number of fold we wish to split our data into.
#' @keywords inference and prediction
#'
#' @return A vector of predicted classes for our n-row observations, as well as
#'   a single numeric averaged misclassification error from the number of
#'   neighbors \code{k_nn}.
#'
#'
#' @importFrom stats filter model.frame model.matrix model.response predict pt sd na.omit
#'
#' @examples
#' # Gets rid of NA values from penguins data
#' penguins <- na.omit(NewPackageLecture9::my_penguins)
#'
#' # Creates data frame of just numeric values
#' pen_num <- penguins %>% dplyr::select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
#'
#' # Creates vector of the species classifications
#' pen_cl <- penguins %>% dplyr::pull(species)
#'
#' # Makes vector of strings of those classes
#' pen_cl <- as.character(pen_cl)
#'
#' my_knn_cv(pen_num, pen_cl, 5, 5)
#' # my_knn_cv(NUMERIC_df, CLASS_vector, 3, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {

  fold <- sample(rep(1:k_cv, length = nrow(train)))

  cv_err <-rep(NA, k_cv)

  for (i in 1:k_cv) {

    x_train <-  train[which(fold != i), ]

    x_test <- train[which(fold == i), ]

    y_train <- cl[which(fold != i)]

    y_test <- cl[which(fold == i)]

    class_predict <- class::knn(train = x_train,
                         test = x_test,
                         cl = y_train,
                         k = k_nn)

    cv_err[i] <- mean(class_predict != y_test)

  }

  good_pred <- class::knn(train = train, test = train, cl = cl, k = k_nn)

  pen_list <- list("class_pred" = good_pred,
                   "Error" = mean(cv_err))

  return(pen_list)

}
