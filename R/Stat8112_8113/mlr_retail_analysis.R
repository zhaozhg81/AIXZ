# Multiple Linear Regression: Retail Store Case Study
# All monetary variables are measured in thousands of dollars.

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file)) {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script_file <- if (length(a)) sub("^--file=", "", a[1]) else NULL
}
script_dir <- if (is.null(script_file)) getwd() else dirname(normalizePath(script_file))
source(file.path(script_dir, "mlr_setup.R"))

retail <- read.csv(file.path(mlr_data, "retail_store_sales.csv"))
stopifnot(nrow(retail) == 120, !anyNA(retail), !anyDuplicated(retail$store_id))
pdf(file.path(mlr_output, "retail_diagnostics.pdf"))
retail$region <- factor(retail$region)
retail$promotion <- factor(retail$promotion,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

# 1. Inspect the data
print(str(retail))
print(summary(retail))
print(table(retail$region, retail$promotion))
pairs(retail[c("sales_k", "digital_ads_k", "traffic_100",
               "income_k", "competitor_km")])

# 2. Simple and multiple regression
m_simple <- lm(sales_k ~ digital_ads_k, data = retail)
m_main <- lm(sales_k ~ digital_ads_k + traffic_100 + income_k +
               competitor_km + promotion + region,
             data = retail)
print(summary(m_simple))
print(summary(m_main))
print(confint(m_main))

# 3. Interpret a categorical predictor and an interaction
m_interaction <- lm(sales_k ~ digital_ads_k * promotion + traffic_100 +
                      income_k + competitor_km + region,
                    data = retail)
print(summary(m_interaction))
print(anova(m_main, m_interaction))

# 4. Confidence interval for a mean and prediction interval for a new store
new_store <- data.frame(digital_ads_k = 12,
                        traffic_100 = 19,
                        income_k = 75,
                        competitor_km = 4,
                        promotion = factor("Yes", levels = c("No", "Yes")),
                        region = factor("Northeast", levels = levels(retail$region)))
print(predict(m_interaction, newdata = new_store,
        interval = "confidence", level = .95))
print(predict(m_interaction, newdata = new_store,
        interval = "prediction", level = .95))

# 5. Global and partial F tests
print(anova(m_main))
m_reduced <- lm(sales_k ~ digital_ads_k + traffic_100 + promotion,
                data = retail)
print(anova(m_reduced, m_main))

# 6. Package-free VIF calculation
vif_manual <- function(model) {
  X <- model.matrix(model)[, -1, drop = FALSE]
  keep <- apply(X, 2, sd) > 0
  X <- X[, keep, drop = FALSE]
  sapply(seq_len(ncol(X)), function(j) {
    1 / (1 - summary(lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared)
  }) |> setNames(colnames(X))
}
print(vif_manual(m_main))

# 7. Diagnostics and influence
par(mfrow = c(2, 2))
plot(m_main)
par(mfrow = c(1, 1))
influence_table <- data.frame(
  store_id = retail$store_id,
  leverage = hatvalues(m_main),
  studentized_residual = rstudent(m_main),
  cooks_distance = cooks.distance(m_main)
)
print(head(influence_table[order(-influence_table$cooks_distance), ], 5))

# 8. Sensitivity analysis excluding the most influential store
most_influential <- which.max(cooks.distance(m_main))
m_sensitivity <- update(m_main, subset = -most_influential)
print(cbind(full = coef(m_main), sensitivity = coef(m_sensitivity)))

# 9. Ten-fold cross-validation using base R
set.seed(8113)
fold <- sample(rep(1:10, length.out = nrow(retail)))
cv_error <- sapply(1:10, function(k) {
  fit_k <- lm(formula(m_main), data = retail[fold != k, ])
  pred_k <- predict(fit_k, newdata = retail[fold == k, ])
  mean((retail$sales_k[fold == k] - pred_k)^2)
})
print(sqrt(mean(cv_error)))


# Practice: test region as a block, and compare models using identical folds.
print(anova(update(m_main, . ~ . - region), m_main))
cv_rmse <- function(model) {
  errors <- numeric(nrow(retail))
  for (k in 1:10) {
    fit <- lm(formula(model), data = retail[fold != k, ])
    errors[fold == k] <- retail$sales_k[fold == k] -
      predict(fit, newdata = retail[fold == k, ])
  }
  sqrt(mean(errors^2))
}
print(c(main = cv_rmse(m_main), interaction = cv_rmse(m_interaction)))
# CI for the promotion-group advertising slope is a linear contrast.
q <- setNames(rep(0, length(coef(m_interaction))), names(coef(m_interaction)))
q["digital_ads_k"] <- q["digital_ads_k:promotionYes"] <- 1
estimate <- sum(q * coef(m_interaction))
se <- sqrt(drop(t(q) %*% vcov(m_interaction) %*% q))
print(c(estimate = estimate, lower = estimate - qt(.975, df.residual(m_interaction)) * se,
        upper = estimate + qt(.975, df.residual(m_interaction)) * se))
dev.off()
