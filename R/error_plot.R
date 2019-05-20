#' Graph performance of ensemble of regression models
#'
#' When given training predictors, training responses, and predictors, responses this function
#' outputs a plot mean squire errors of the models
#' The models used for this ensemble are: KNN, Random forest, LASSO, Ridge and linear
#'
#' @param X_train training predictor vector
#' @param y_train training response vector
#' @param X_test predictor vector
#' @param y_test responses vector
#' @param rfntree number of trees to grow. This should not be set to too small a number. Set to 100 by default
#' @param KNNk Number of neighbours considered it KNN, set to 5 if unspecified
#' @param rfmtry Number of variables randomly sampled as candidates at each split in random forest. Set to 6 by default
#' @param doKNN Whether to run KNN or not, TRUE by default
#' @return A vector of predictions
#' @export
error_plot <- function(X_train, y_train, X_test, y_test, rfntree, KNNk, rfmtry, doKNN){
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
  error_lm <- mean((pred_lm-y_test)^2)
  # KNN
  pred_knn <- knn.reg(X_train, test=X_test, y=y_train, k=KNNk)$pred
  error_knn <- mean((pred_knn-y_test)^2)
  # RF
  fit_rf <- randomForest(x=X_train, y=y_train, ntree=rfntree, mtry = rfmtry)
  pred_rf <- predict(fit_rf, X_test)
  error_rf <- mean((pred_rf-y_test)^2)
  # Ridge
  fit_ridge <- cv.glmnet(X_train_glmnet, y_train, alpha=0)
  pred_ridge <- predict(fit_ridge, X_test_glmnet, s='lambda.min')[,1]
  error_ridge <- mean((pred_ridge-y_test)^2)
  # LASSO
  fit_lasso <- cv.glmnet(X_train_glmnet, y_train, alpha=1)
  pred_lasso <- predict(fit_lasso, X_test_glmnet, s='lambda.min')[,1]
  error_lasso <- mean((pred_lasso-y_test)^2)

  # Computing average
  if(doKNN){
    avg_preds <- rowMeans(matrix(c(pred_lm, pred_knn, pred_rf, pred_ridge, pred_lasso),ncol=5))
    error_avg <- mean((avg_preds-y_test)^2)
    results <- data.frame(Model=c('LM','KNN','RF','Ridge','LASSO','Average'),
                          MSE=c(error_lm, error_knn, error_rf, error_ridge, error_lasso, error_avg))
    ggplot(results, aes(x=fct_reorder(Model, MSE), y=MSE))+
      geom_col(fill='red')+
      ggtitle('Comparing model performance')
  }
  else{
    avg_preds <- rowMeans(matrix(c(pred_lm, pred_rf, pred_ridge, pred_lasso),ncol=4))
    error_avg <- mean((avg_preds-y_test)^2)
    results <- data.frame(Model=c('LM','RF','Ridge','LASSO','Average'),
                          MSE=c(error_lm, error_rf, error_ridge, error_lasso, error_avg))
    ggplot(results, aes(x=fct_reorder(Model, MSE), y=MSE))+
      geom_col(fill='red')+
      ggtitle('Comparing model performance')
  }
}

