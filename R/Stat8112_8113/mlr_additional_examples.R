script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file)) {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script_file <- if (length(a)) sub("^--file=", "", a[1]) else NULL
}
script_dir <- if (is.null(script_file)) getwd() else dirname(normalizePath(script_file))
source(file.path(script_dir, "mlr_setup.R"))

# Vehicle prices: adjusted slopes, condition block, prediction, collinearity.
cars <- read.csv(file.path(mlr_data, "mlr_vehicle_prices.csv"))
cars$manufacturer <- factor(cars$manufacturer,
                             levels = c("Economy", "Standard", "Premium"))
car_simple <- lm(price_k ~ mileage_1000, cars)
car_reduced <- lm(price_k ~ mileage_1000 + age_years, cars)
car_condition <- update(car_reduced, . ~ . + accident + prior_owners + service_record)
car_full <- update(car_condition, . ~ . + engine_l + manufacturer)
print(summary(car_simple)); print(summary(car_full)); print(confint(car_full))
print(anova(car_reduced, car_condition))
print(cor(cars$mileage_1000, cars$age_years))
profile <- data.frame(mileage_1000 = 60, age_years = 6, accident = 0,
  prior_owners = 2, service_record = 1, engine_l = 2,
  manufacturer = factor("Standard", levels = levels(cars$manufacturer)))
print(predict(car_full, profile, interval = "confidence"))
print(predict(car_full, profile, interval = "prediction"))

# Advertising: compare search and social slopes using their full covariance.
ads <- read.csv(file.path(mlr_data, "mlr_advertising_markets.csv"))
ads$quarter <- factor(ads$quarter, levels = c("Q1", "Q2", "Q3", "Q4"))
ad_fit <- lm(sales_k ~ search_ads_k + social_ads_k + market_size_1000 +
               income_k + competitors + quarter, ads)
print(summary(ad_fit))
q <- setNames(rep(0, length(coef(ad_fit))), names(coef(ad_fit)))
q["search_ads_k"] <- 1; q["social_ads_k"] <- -1
estimate <- sum(q * coef(ad_fit))
se <- sqrt(drop(t(q) %*% vcov(ad_fit) %*% q))
t_stat <- estimate / se
df <- df.residual(ad_fit)
print(c(search_minus_social = estimate, se = se, t = t_stat,
        p = 2 * pt(-abs(t_stat), df),
        lower = estimate - qt(.975, df) * se,
        upper = estimate + qt(.975, df) * se))

# The same single linear hypothesis using multcomp::glht.
if (!requireNamespace("multcomp", quietly = TRUE)) {
  stop("Install multcomp before running this script: install.packages('multcomp')")
}
channel_test <- multcomp::glht(ad_fit,
  linfct = "search_ads_k - social_ads_k = 0")
channel_summary <- summary(channel_test, test = multcomp::adjusted("none"))
print(channel_summary)
channel_ci <- confint(channel_test)$confint
print(channel_ci)
K <- matrix(q, nrow = 1, dimnames = list("Search - Social", names(q)))
channel_matrix <- multcomp::glht(ad_fit, linfct = K, rhs = 0)
print(summary(channel_matrix, test = multcomp::adjusted("none")))
stopifnot(isTRUE(all.equal(unname(channel_ci[1, "lwr"]),
                           unname(estimate - qt(.975, df) * se))),
          isTRUE(all.equal(unname(channel_ci[1, "upr"]),
                           unname(estimate + qt(.975, df) * se))))
# Check agreement with the manual t test and confidence interval.
stopifnot(isTRUE(all.equal(unname(channel_summary$test$tstat), unname(t_stat))),
          isTRUE(all.equal(unname(channel_summary$test$pvalues),
                           unname(2 * pt(-abs(t_stat), df)))))

# Customer interaction: difference between two promotion-associated differences.
customers <- read.csv(file.path(mlr_data, "mlr_customer_promotions.csv"))
customers$promotion <- factor(customers$promotion, levels = c(0, 1),
                              labels = c("No", "Yes"))
customers$customer_type <- factor(customers$customer_type, levels = c("Existing", "New"))
customer_fit <- lm(spending_dollars ~ promotion * customer_type, customers)
print(with(customers, tapply(spending_dollars, list(customer_type, promotion), mean)))
print(summary(customer_fit)); print(confint(customer_fit))
print(anova(update(customer_fit, . ~ . - promotion:customer_type), customer_fit))

# Insulation: existing local data, not simulated; before/after is observational.
gas <- read.table(file.path(mlr_data, "insulgas.txt"), header = TRUE)
gas$Insulate <- factor(gas$Insulate, levels = c("Before", "After"))
stopifnot(nrow(gas) == 56, all(table(gas$Insulate) == c(26, 30)))
gas$temp_centered <- gas$Temp - mean(gas$Temp)
gas_additive <- lm(Gas ~ Insulate + temp_centered, gas)
gas_interaction <- lm(Gas ~ Insulate * temp_centered, gas)
print(summary(gas_additive)); print(summary(gas_interaction))
print(anova(gas_additive, gas_interaction)); print(confint(gas_interaction))
pdf(file.path(mlr_output, "additional_diagnostics.pdf"))
for (fit in list(car_full, ad_fit, customer_fit, gas_interaction)) {
  par(mfrow = c(2, 2)); plot(fit)
}
dev.off()
