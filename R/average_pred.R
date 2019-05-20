#' Ensemble of regression models
#'
#' When given training predictors, training responses, and predictors, this function
#' outputs a vector of predictions
#' The models used for this ensemble are: KNN, Random forest, LASSO, Ridge and linear
#'
#' @param X_train training predictor vector
#' @param y_train training response vector
#' @param X_test predictor vector
#' @param rfntree number of trees to grow. This should not be set to too small a number
#' @return A vector of predictions
#' @export
average_pred <- function(X_train, y_train, X_test, rfntree, KNNk, rfmtry, doKNN){
  if(missing(rfntree)){
    rfntree = 100
  }
  if(missing(KNNk)){
    KNNk = 5
  }
  if(missing(rfmtry)){
    rfmtry = 6
  }
  if(missing(doKNN)){
    doKNN = TRUE
  }
  # Creating ridge and lasso sets
  X_train_glmnet <- model.matrix(~.-1, data=X_train)
  X_test_glmnet <- model.matrix(~.-1, data=X_test)
  # LM
  fit_lm <- glmnet(X_train_glmnet, y_train, alpha=0, lambda=c(0))
  pred_lm <- predict(fit_lm, X_test_glmnet)[,1]
  # KNN
  pred_knn <- knn.reg(X_train, test=X_test, y=y_train, k=KNNk)$pred
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
  if(doKNN){
    avg_preds <- rowMeans(matrix(c(pred_lm, pred_knn, pred_rf, pred_ridge, pred_lasso),ncol=5))
    return(avg_preds)
  }
  else{
    avg_preds <- rowMeans(matrix(c(pred_lm, pred_rf, pred_ridge, pred_lasso),ncol=4))
    return(avg_preds)
  }

}
