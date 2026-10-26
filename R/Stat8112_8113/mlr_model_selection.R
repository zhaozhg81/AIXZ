script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file)) {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script_file <- if (length(a)) sub("^--file=", "", a[1]) else NULL
}
script_dir <- if (is.null(script_file)) getwd() else dirname(normalizePath(script_file))
source(file.path(script_dir, "mlr_setup.R"))


# Four prespecified models, one fixed training/test split, one response scale.
d <- read.csv(file.path(mlr_data, "mlr_sales_selection.csv"))
stopifnot(nrow(d) == 180, !anyNA(d), !anyDuplicated(d$store_id))
train <- d[d$sample == "Training", ]
test <- d[d$sample == "Test", ]
engagement <- sprintf("engagement_%02d", 1:24)
full_formula <- reformulate(c("digital_ads_k", "traffic_100", "promotion",
                              "income_k", engagement), "sales_k")
fits <- list(
  Ads = lm(sales_k ~ digital_ads_k, data = train),
  Core = lm(sales_k ~ digital_ads_k + traffic_100 + promotion, data = train),
  Income = lm(sales_k ~ digital_ads_k + traffic_100 + promotion + income_k,
              data = train),
  Full = lm(full_formula, data = train))
n_train <- nrow(train)
sigma2_full <- deviance(fits$Full) / df.residual(fits$Full)
# Cp uses d = regression coefficients including intercept, not variance.
# AIC/BIC use k = d + 1, adding the Gaussian error-variance parameter.
selection_table <- do.call(rbind, lapply(fits, function(m) {
  d_model <- m$rank
  data.frame(d = d_model, training_SSE = deviance(m),
    training_RMSE = sqrt(mean(residuals(m)^2)),
    LOOCV_RMSE = sqrt(mean((residuals(m) / (1 - hatvalues(m)))^2)),
    AIC = AIC(m), BIC = BIC(m),
    Cp = deviance(m) / sigma2_full - n_train + 2 * d_model)
}))
print(selection_table)
print(c(AIC_choice = rownames(selection_table)[which.min(selection_table$AIC)],
        BIC_choice = rownames(selection_table)[which.min(selection_table$BIC)],
        Cp_choice = rownames(selection_table)[which.min(selection_table$Cp)]))
# Nested least-squares models must have nonincreasing training SSE.
stopifnot(all(diff(selection_table$training_SSE) <= 1e-8))
# Reference full-model Cp equals its regression dimension, by construction.
stopifnot(abs(selection_table["Full", "Cp"] - fits$Full$rank) < 1e-8)
write.csv(selection_table, file.path(mlr_output, "selection_criteria.csv"))

# Optional search demonstrations: use TRAINING data only.
# These restricted searches need not find the same model or a global optimum.
forward <- step(lm(sales_k ~ 1, train),
                scope = list(lower = ~1, upper = full_formula),
                direction = "forward", trace = 0)
backward <- step(fits$Full, direction = "backward", trace = 0)
both <- step(lm(sales_k ~ 1, train),
             scope = list(lower = ~1, upper = full_formula),
             direction = "both", trace = 0)
backward_bic <- step(fits$Full, direction = "backward", k = log(n_train), trace = 0)
print(lapply(list(forward = forward, backward = backward, both = both,
                 backward_bic = backward_bic), formula))
# step() uses extractAIC(); its absolute values differ by a constant from AIC()
# for these Gaussian lm models, but the rankings agree on the same observations.

if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("Install glmnet before running this script: install.packages('glmnet')")
}
# Identical column construction, no ID or split-label predictors.
x_train <- model.matrix(full_formula, data = train)[, -1, drop = FALSE]
x_test <- model.matrix(full_formula, data = test)[, -1, drop = FALSE]
stopifnot(identical(colnames(x_train), colnames(x_test)))
set.seed(811306)
foldid <- sample(rep(1:10, length.out = n_train))
# Each internal fold learns its own standardization; test data stay untouched.
lasso_cv <- glmnet::cv.glmnet(x_train, train$sales_k,
  family = "gaussian", alpha = 1, foldid = foldid,
  type.measure = "mse", standardize = TRUE)
print(c(lambda_min = lasso_cv$lambda.min, lambda_1se = lasso_cv$lambda.1se))
print(coef(lasso_cv, s = "lambda.min"))
print(coef(lasso_cv, s = "lambda.1se"))
coef_table <- data.frame(term = rownames(as.matrix(coef(lasso_cv))),
  lambda_min = as.vector(coef(lasso_cv, s = "lambda.min")),
  lambda_1se = as.vector(coef(lasso_cv, s = "lambda.1se")))
write.csv(coef_table, file.path(mlr_output, "lasso_coefficients.csv"), row.names = FALSE)

# Final held-out evaluation, after criteria and lambda choices are fixed.
# The two lambda rules are prespecified; do not choose between them on this test.
rmse <- function(observed, predicted) sqrt(mean((observed - predicted)^2))
test_rmse <- vapply(fits, function(m) rmse(test$sales_k, predict(m, test)), numeric(1))
for (rule in c("lambda.min", "lambda.1se")) {
  test_rmse[paste0("LASSO_", rule)] <- rmse(test$sales_k,
    as.vector(predict(lasso_cv, newx = x_test, s = rule)))
}
print(test_rmse)
# Verify the teaching comparison on the supplied fixed split.
stopifnot(test_rmse["Core"] < test_rmse["Full"],
          selection_table["Core", "LOOCV_RMSE"] < selection_table["Full", "LOOCV_RMSE"])
write.csv(data.frame(model = names(test_rmse), test_RMSE = unname(test_rmse)),
          file.path(mlr_output, "selection_test_errors.csv"), row.names = FALSE)
pdf(file.path(mlr_output, "model_selection.pdf"), width = 9, height = 5)
par(mfrow = c(1, 2))
barplot(selection_table$training_RMSE, names.arg = rownames(selection_table),
        ylab = "Training RMSE ($1,000)", main = "Fit on training stores")
barplot(test_rmse[1:4], ylab = "Test RMSE ($1,000)", main = "Predict held-out stores")
par(mfrow = c(1, 1))
plot(lasso_cv)
plot(lasso_cv$glmnet.fit, xvar = "lambda", label = TRUE)
dev.off()
