#' Ensemble of regression models (the same as average_pred() but without KNN)
#'
#' When given training predictors, training responses, and predictors, this function
#' outputs a vector of predictions
#' The models used for this ensemble are: Random forest, LASSO, Ridge and linear
#'
#' @param X_train training predictor vector
#' @param y_train training response vector
#' @param X_test predictor vector
#'
#' @return A vector of predictions
#' @export

average_pred_without_KNN <- function(X_train, y_train, X_test, rfntree, rfmtry){
  if(missing(rfntree)){
    rfntree = 100
  }
  if(missing(rfmtry)){
    rfmtry = 6
  }
  # Creating ridge and lasso sets
  X_train_glmnet <- model.matrix(~.-1, data=X_train)
  X_test_glmnet <- model.matrix(~.-1, data=X_test)
  # LM
  fit_lm <- glmnet(X_train_glmnet, y_train, alpha=0, lambda=c(0))
  pred_lm <- predict(fit_lm, X_test_glmnet)[,1]
  # RF
  fit_rf <- randomForest(x=X_train, y=y_train, ntree=rfntree, mtry = rfmtry)
  pred_rf <- predict(fit_rf, X_test)
  # Ridge
  fit_ridge <- cv.glmnet(X_train_glmnet, y_train, alpha=0)
  pred_ridge <- predict(fit_ridge, X_test_glmnet, s='lambda.min')[,1]
  # LASSO
  fit_lasso <- cv.glmnet(X_train_glmnet, y_train, alpha=1)
  pred_lasso <- predict(fit_lasso, X_test_glmnet, s='lambda.min')[,1]
  # Computing average
  avg_preds <- rowMeans(matrix(c(pred_lm, pred_rf, pred_ridge, pred_lasso),ncol=4))
  return(avg_preds)
}
