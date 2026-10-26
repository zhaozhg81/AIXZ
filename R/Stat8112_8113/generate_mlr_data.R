script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file)) {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script_file <- if (length(a)) sub("^--file=", "", a[1]) else NULL
}
script_dir <- if (is.null(script_file)) getwd() else dirname(normalizePath(script_file))
source(file.path(script_dir, "mlr_setup.R"))

# Synthetic teaching data: these are not observations from real businesses.
# Independent fixed seeds allow each example to be regenerated separately.
set.seed(811301)
n <- 240
age <- runif(n, 1, 12)
mileage <- pmax(2, 10 * age + rnorm(n, 0, 18))
manufacturer <- factor(sample(c("Economy", "Standard", "Premium"), n, TRUE),
                       levels = c("Economy", "Standard", "Premium"))
engine <- round(runif(n, 1.4, 3.8), 1)
accident <- rbinom(n, 1, .25)
owners <- 1 + rbinom(n, 3, age / 16)
service <- rbinom(n, 1, .7)
price <- 28 - .055 * mileage - .8 * age + 2 * (engine - 2) -
  2.5 * accident - .7 * (owners - 1) + 1.8 * service +
  c(0, 3, 11)[as.integer(manufacturer)] + rnorm(n, 0, 2.2)
cars <- data.frame(vehicle_id = sprintf("V%03d", 1:n), price_k = round(price, 2),
  mileage_1000 = round(mileage, 2), age_years = round(age, 2),
  engine_l = engine, accident = accident, manufacturer = manufacturer,
  prior_owners = owners, service_record = service)
write.csv(cars, file.path(mlr_data, "mlr_vehicle_prices.csv"), row.names = FALSE)

set.seed(811302)
n <- 160
market <- runif(n, 80, 600)
income <- runif(n, 35, 100)
competitors <- rpois(n, 4)
season <- factor(sample(c("Q1", "Q2", "Q3", "Q4"), n, TRUE))
search <- pmax(1, .055 * market + rnorm(n, 0, 6))
social <- pmax(1, .035 * market + rnorm(n, 0, 5))
sales <- 40 + 2.3 * search + 1.2 * social + .35 * market + .7 * income -
  4 * competitors + c(0, 8, 4, 25)[as.integer(season)] + rnorm(n, 0, 14)
ads <- data.frame(market_id = sprintf("M%03d", 1:n),
  sales_k = round(sales, 2), search_ads_k = round(search, 2),
  social_ads_k = round(social, 2), market_size_1000 = round(market, 2),
  income_k = round(income, 2), competitors = competitors, quarter = season)
write.csv(ads, file.path(mlr_data, "mlr_advertising_markets.csv"), row.names = FALSE)

set.seed(811303)
n <- 320
customer_type <- rep(c("Existing", "New"), each = n / 2)
# Balanced synthetic 2 x 2 design; each row represents a different customer.
promotion <- rep(rep(c(0, 1), each = n / 4), 2)
spending <- 65 - 12 * (customer_type == "New") + 8 * promotion +
  10 * promotion * (customer_type == "New") + rnorm(n, 0, 12)
customers <- data.frame(customer_id = sprintf("C%03d", 1:n),
  spending_dollars = round(spending, 2), promotion = promotion,
  customer_type = customer_type)
write.csv(customers, file.path(mlr_data, "mlr_customer_promotions.csv"), row.names = FALSE)

# Separate synthetic store sample for model-selection demonstrations.
set.seed(811304)
n <- 180
ads <- runif(n, 1, 15)
traffic <- runif(n, 8, 30)
promotion <- rbinom(n, 1, .4)
income <- runif(n, 35, 100)
digital <- matrix(rnorm(n * 24), nrow=n)
y <- 20 + 1.2 * ads + 2 * traffic + 10 * promotion + rnorm(n, 0, 8)
d <- data.frame(sales_k=round(y,2), digital_ads_k=round(ads,2), traffic_100=round(traffic,2),promotion=promotion,income_k=round(income,2),digital)
names(d)[6:29] <- sprintf('engagement_%02d',1:24)
set.seed(811305)
i <- sample(seq_len(n), 120)
d$store_id <- sprintf('P%03d', seq_len(n))
d$sample <- ifelse(seq_len(n) %in% i, 'Training','Test')
write.csv(d, file.path(mlr_data, "mlr_sales_selection.csv"), row.names = FALSE)
cat("Generated four synthetic datasets. Retail and insulation files are preserved.\n")
