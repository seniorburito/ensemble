#' Ensemble of regression models
#'
#' This gives an mathematical mean of different model predictions
#'
#' @return A vector of predictions
#' @export
average_pred <- function(X_train, y_train, X_test){
  # Creating ridge and lasso sets
  X_train_glmnet <- model.matrix(~.-1, data=X_train)
  X_test_glmnet <- model.matrix(~.-1, data=X_test)
  # LM
  fit_lm <- glmnet(X_train_glmnet, y_train, alpha=0, lambda=c(0))
  pred_lm <- predict(fit_lm, X_test_glmnet)[,1]
  # KNN
  pred_knn <- knn.reg(X_train, test=X_test, y=y_train, k=5)$pred
  # RF
  fit_rf <- randomForest(x=X_train, y=y_train, ntree=100)
  pred_rf <- predict(fit_rf, X_test)
  # Ridge
  fit_ridge <- cv.glmnet(X_train_glmnet, y_train, alpha=0)
  pred_ridge <- predict(fit_ridge, X_test_glmnet, s='lambda.min')[,1]
  # LASSO
  fit_lasso <- cv.glmnet(X_train_glmnet, y_train, alpha=1)
  pred_lasso <- predict(fit_lasso, X_test_glmnet, s='lambda.min')[,1]
  # Computing average
  avg_preds <- rowMeans(matrix(c(pred_lm, pred_knn, pred_rf, pred_ridge, pred_lasso),ncol=5))
  return(avg_preds)
}