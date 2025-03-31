library(dplyr)
library(purrr)

z <- read.delim("../raw/vnr-results-public-2022.txt", header = FALSE, na.strings = " ")
# Remove columns: 1 (index) and 9 (NA)
z <- z[, -c(1, 9)]
names(z) <- c("id", "name", "sex", "company", "mins_per_km", "km_per_hour", "time")

colon_split_list <- strsplit(z$time, split = ":")

get_mins <- function(i) {
  x <- colon_split_list[[i]]
  ifelse(length(x) == 2, as.integer(x[1]), as.integer(x[1]) * 60 + as.integer(x[2]))
}

z$mins <- sapply(1:nrow(z), get_mins)

comma_split_list <- strsplit(z$time, split = ",")

get_seconds <- function(i) {
  x <- colon_split_list[[i]] |> rev()
  x <- strsplit(x[1], split = ",")[[1]]
  as.integer(x[1])
}

z$secs <- sapply(1:nrow(z), get_seconds)

get_tenthsecs <- function(i) {
  x <- colon_split_list[[i]] |> rev()
  x <- strsplit(x[1], split = ",")[[1]]
  as.integer(x[2])
}

z$tenthsecs <- sapply(1:nrow(z), get_tenthsecs)

z$time_in_secs <- z$mins * 60 + z$secs + z$tenthsecs * 0.1
z$time_in_mins <- z$time_in_secs / 60

# 

z$mins_per_km_mins <- map(strsplit(z$mins_per_km, split = ":"), 1) |>
  unlist() |>
  as.integer()

z$mins_per_km_secs <- map(strsplit(z$mins_per_km, split = ":"), 2) |>
  unlist() |>
  as.integer()

z$secs_per_km <- z$mins_per_km_mins * 60 + z$mins_per_km_secs

summary(z$secs_per_km - floor(z$time_in_secs/5))

z$mins_per_km <- z$secs_per_km/60

# Some speed calculations have a discrepancy for unknown reason
# z %>% transmute(
#   km_per_hour,
#   5*60*60/time_in_secs,
#   km_per_hour_calc = floor(5*60*60/time_in_secs * 10)/10,
#   km_per_hour_diff = km_per_hour - km_per_hour_calc, tenthsecs
#   ) %>%
#   filter(abs(km_per_hour_diff) > 0)
# NOTE: This occurs when 5*60*60/time_in_secs * 10 is very small (TODO)

z$km_per_hour <- 5*60*60/z$time_in_secs

z <- z |> select(id, name, company, sex, time_in_mins)

z$sex[is.na(z$sex)] <- "NA"
z$sex <- as.factor(z$sex)

z$company[is.na(z$company)] <- "NA"
z$company <- as.factor(z$company)

saveRDS(z, file = "vnr-results-preprocessed-2022.rds")
