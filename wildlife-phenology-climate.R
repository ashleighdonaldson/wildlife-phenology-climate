
# Phenological cues and breeding timing in common chaffinches

install.packages(c("climwin", "dplyr", "ggplot2"))

library(climwin)
library(dplyr)
library(ggplot2)

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

data("Chaff", package = "climwin")
data("ChaffClim", package = "climwin")

head(Chaff)
head(ChaffClim)
str(Chaff)
str(ChaffClim)


# Prepare breeding and climate data

breeding <- Chaff %>%
  transmute(
    Year = as.integer(Year),
    LayingDay = as.numeric(Laydate)
  ) %>%
  filter(
    !is.na(Year),
    !is.na(LayingDay)
  )

climate <- ChaffClim %>%
  mutate(
    Date = as.Date(
      as.character(Date),
      format = "%d/%m/%Y"
    ),
    Year = as.integer(format(Date, "%Y")),
    Month = as.integer(format(Date, "%m"))
  ) %>%
  select(
    Date, Year, Month, Temp, Rain
  ) %>%
  filter(
    !is.na(Date),
    !is.na(Temp),
    !is.na(Rain)
  )

stopifnot(
  nrow(breeding) > 0,
  nrow(climate) > 0,
  !anyDuplicated(breeding$Year),
  !anyDuplicated(climate$Date)
)

print(summary(breeding))
print(summary(climate))


# Examine annual laying dates

annual_model <- lm(
  LayingDay ~ Year,
  data = breeding
)

print(summary(annual_model))

laying_plot <- ggplot(
  breeding,
  aes(Year, LayingDay)
) +
  geom_point(
    colour = "steelblue",
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = "black"
  ) +
  theme_classic() +
  labs(
    title = "Common chaffinch breeding phenology",
    x = "Year",
    y = "Mean laying date (days after 1 January)"
  )

print(laying_plot)


# Calculate monthly climate summaries

# January to March are selected as candidate
# environmental windows preceding spring breeding.

monthly_climate <- climate %>%
  filter(Month %in% 1:3) %>%
  group_by(Year, Month) %>%
  summarise(
    MeanTemp = mean(Temp),
    TotalRain = sum(Rain),
    Days = n(),
    .groups = "drop"
  )

# Check that every month contains a complete
# set of daily observations.

monthly_climate <- monthly_climate %>%
  mutate(
    ExpectedDays = as.integer(
      as.Date(sprintf(
        "%04d-%02d-01",
        Year + (Month == 12),
        ifelse(Month == 12, 1, Month + 1)
      )) -
        as.Date(sprintf(
          "%04d-%02d-01",
          Year,
          Month
        ))
    )
  ) %>%
  filter(Days == ExpectedDays)

monthly_data <- monthly_climate %>%
  inner_join(
    breeding,
    by = "Year"
  )

# Retain years with all three months available.

complete_years <- monthly_data %>%
  count(Year) %>%
  filter(n == 3) %>%
  pull(Year)

monthly_data <- monthly_data %>%
  filter(Year %in% complete_years)

cat(
  "Number of years included:",
  length(complete_years),
  "\n"
)

stopifnot(length(complete_years) > 10)


# Compare individual monthly climate effects

monthly_results <- data.frame()

for (m in 1:3) {
  
  subset_data <- monthly_data %>%
    filter(Month == m)
  
  temp_model <- lm(
    LayingDay ~ MeanTemp,
    data = subset_data
  )
  
  rain_model <- lm(
    LayingDay ~ TotalRain,
    data = subset_data
  )
  
  monthly_results <- bind_rows(
    monthly_results,
    data.frame(
      Month = month.abb[m],
      Variable = c("Temperature", "Rainfall"),
      Estimate = c(
        coef(temp_model)[2],
        coef(rain_model)[2]
      ),
      P_value = c(
        summary(temp_model)$coefficients[2, 4],
        summary(rain_model)$coefficients[2, 4]
      ),
      AIC = c(
        AIC(temp_model),
        AIC(rain_model)
      ),
      R2 = c(
        summary(temp_model)$r.squared,
        summary(rain_model)$r.squared
      )
    )
  )
}

monthly_results <- monthly_results %>%
  mutate(
    DeltaAIC = AIC - min(AIC)
  ) %>%
  arrange(AIC)

print(monthly_results)


# Plot monthly model comparison

monthly_results$Month <- factor(
  monthly_results$Month,
  levels = month.abb[1:3]
)

monthly_plot <- ggplot(
  monthly_results,
  aes(
    Month,
    DeltaAIC,
    fill = Variable
  )
) +
  geom_col(
    position = "dodge"
  ) +
  theme_classic() +
  labs(
    title = "Monthly environmental associations",
    x = "Month",
    y = "Delta AIC",
    fill = "Climate variable"
  )

print(monthly_plot)


# Define single- and multi-month windows

windows <- data.frame(
  Window = c(
    "Jan",
    "Feb",
    "Mar",
    "Jan-Feb",
    "Feb-Mar",
    "Jan-Mar"
  ),
  Start = c(1, 2, 3, 1, 2, 1),
  End = c(1, 2, 3, 2, 3, 3)
)


# Calculate climate conditions within each window

window_data <- data.frame()

for (i in seq_len(nrow(windows))) {
  
  window_climate <- monthly_data %>%
    filter(
      Month >= windows$Start[i],
      Month <= windows$End[i]
    ) %>%
    group_by(Year) %>%
    summarise(
      MeanTemp = weighted.mean(
        MeanTemp,
        Days
      ),
      TotalRain = sum(TotalRain),
      LayingDay = first(LayingDay),
      .groups = "drop"
    ) %>%
    mutate(
      Window = windows$Window[i]
    )
  
  window_data <- bind_rows(
    window_data,
    window_climate
  )
}

# Ensure every model uses the same years.

complete_window_years <- window_data %>%
  count(Year) %>%
  filter(n == nrow(windows)) %>%
  pull(Year)

window_data <- window_data %>%
  filter(Year %in% complete_window_years)

stopifnot(
  n_distinct(window_data$Year) > 10
)


# Compare temperature, rainfall and combined models

window_results <- data.frame()

for (i in seq_len(nrow(windows))) {
  
  subset_data <- window_data %>%
    filter(Window == windows$Window[i])
  
  temp_model <- lm(
    LayingDay ~ MeanTemp,
    data = subset_data
  )
  
  rain_model <- lm(
    LayingDay ~ TotalRain,
    data = subset_data
  )
  
  combined_model <- lm(
    LayingDay ~ MeanTemp + TotalRain,
    data = subset_data
  )
  
  candidate_models <- list(
    Temperature = temp_model,
    Rainfall = rain_model,
    Combined = combined_model
  )
  
  for (model_name in names(candidate_models)) {
    
    fit <- candidate_models[[model_name]]
    
    window_results <- bind_rows(
      window_results,
      data.frame(
        Window = windows$Window[i],
        Model = model_name,
        AIC = AIC(fit),
        R2 = summary(fit)$r.squared,
        AdjustedR2 = summary(fit)$adj.r.squared
      )
    )
  }
}

window_results <- window_results %>%
  arrange(AIC) %>%
  mutate(
    DeltaAIC = AIC - min(AIC),
    Weight = exp(-0.5 * DeltaAIC)
  ) %>%
  mutate(
    Weight = Weight / sum(Weight)
  )

print(window_results)


# Identify the best-supported climate window

best_window <- window_results$Window[1]
best_type <- window_results$Model[1]

cat(
  "Best environmental window:",
  best_window,
  "\n"
)

cat(
  "Best-supported model:",
  best_type,
  "\n"
)


# Plot climate window comparison

window_results$Comparison <- paste(
  window_results$Window,
  window_results$Model,
  sep = " - "
)

window_plot <- ggplot(
  window_results,
  aes(
    reorder(Comparison, DeltaAIC),
    DeltaAIC,
    fill = Model
  )
) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Comparison of climate windows",
    x = "Climate window and model",
    y = "Delta AIC",
    fill = "Model"
  )

print(window_plot)


# Fit the best-supported model

best_data <- window_data %>%
  filter(Window == best_window) %>%
  arrange(Year)

if (best_type == "Temperature") {
  
  final_model <- lm(
    LayingDay ~ MeanTemp,
    data = best_data
  )
  
} else if (best_type == "Rainfall") {
  
  final_model <- lm(
    LayingDay ~ TotalRain,
    data = best_data
  )
  
} else {
  
  final_model <- lm(
    LayingDay ~ MeanTemp + TotalRain,
    data = best_data
  )
  
}

print(summary(final_model))
print(confint(final_model))


# Check whether the relationship remains
# after accounting for a linear temporal trend

if (best_type == "Temperature") {
  
  trend_model <- lm(
    LayingDay ~ MeanTemp + Year,
    data = best_data
  )
  
} else if (best_type == "Rainfall") {
  
  trend_model <- lm(
    LayingDay ~ TotalRain + Year,
    data = best_data
  )
  
} else {
  
  trend_model <- lm(
    LayingDay ~ MeanTemp + TotalRain + Year,
    data = best_data
  )
  
}

print(summary(trend_model))

print(
  AIC(final_model, trend_model)
)


# Plot the temperature relationship

temperature_plot <- ggplot(
  best_data,
  aes(MeanTemp, LayingDay)
) +
  geom_point(
    colour = "steelblue",
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = "black"
  ) +
  theme_classic() +
  labs(
    title = paste(
      "Temperature during",
      best_window
    ),
    x = "Mean daily maximum temperature (°C)",
    y = "Mean laying date (days after 1 January)"
  )

print(temperature_plot)


# Plot the rainfall relationship

rainfall_plot <- ggplot(
  best_data,
  aes(TotalRain, LayingDay)
) +
  geom_point(
    colour = "steelblue",
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = "black"
  ) +
  theme_classic() +
  labs(
    title = paste(
      "Rainfall during",
      best_window
    ),
    x = "Total rainfall (mm)",
    y = "Mean laying date (days after 1 January)"
  )

print(rainfall_plot)


# Explore hypothetical warming scenarios

# The scenarios use a temperature and rainfall
# model fitted to the selected climate window.
# Rainfall is held constant at its observed mean.

scenario_model <- lm(
  LayingDay ~ MeanTemp + TotalRain,
  data = best_data
)

current_temp <- mean(best_data$MeanTemp)
current_rain <- mean(best_data$TotalRain)

scenarios <- data.frame(
  Scenario = factor(
    c(
      "Current",
      "+1°C",
      "+2°C",
      "+3°C"
    ),
    levels = c(
      "Current",
      "+1°C",
      "+2°C",
      "+3°C"
    )
  ),
  MeanTemp = current_temp + c(0, 1, 2, 3),
  TotalRain = current_rain
)

predictions <- predict(
  scenario_model,
  newdata = scenarios,
  interval = "confidence",
  level = 0.95
)

scenarios <- cbind(
  scenarios,
  as.data.frame(predictions)
)

scenarios$ChangeDays <-
  scenarios$fit - scenarios$fit[1]

scenarios$OutsideObservedRange <-
  scenarios$MeanTemp < min(best_data$MeanTemp) |
  scenarios$MeanTemp > max(best_data$MeanTemp)

print(scenarios)


# Plot warming scenarios

scenario_plot <- ggplot(
  scenarios,
  aes(Scenario, fit)
) +
  geom_point(
    colour = "steelblue",
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = lwr,
      ymax = upr
    ),
    width = 0.15,
    colour = "steelblue"
  ) +
  theme_classic() +
  labs(
    title = "Laying dates under hypothetical warming",
    subtitle = paste(
      "Climate window:",
      best_window,
      "| Rainfall held constant"
    ),
    x = "Temperature scenario",
    y = "Predicted mean laying date"
  )

print(scenario_plot)


# Examine model assumptions

par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))


# Check temporal autocorrelation in residuals

residual_data <- best_data %>%
  mutate(
    Residual = residuals(final_model)
  ) %>%
  arrange(Year)

acf(
  residual_data$Residual,
  main = "Autocorrelation of model residuals"
)

print(
  Box.test(
    residual_data$Residual,
    lag = 5,
    type = "Ljung-Box"
  )
)


# Compare the selected model with a null model

null_model <- lm(
  LayingDay ~ 1,
  data = best_data
)

print(
  AIC(null_model, final_model)
)


# Save tables and fitted models

write.csv(
  breeding,
  "results/breeding_data.csv",
  row.names = FALSE
)

write.csv(
  monthly_results,
  "results/monthly_comparison.csv",
  row.names = FALSE
)

write.csv(
  window_results,
  "results/window_comparison.csv",
  row.names = FALSE
)

write.csv(
  scenarios,
  "results/warming_scenarios.csv",
  row.names = FALSE
)

write.csv(
  residual_data,
  "results/model_residuals.csv",
  row.names = FALSE
)

saveRDS(
  final_model,
  "results/final_model.rds"
)

saveRDS(
  trend_model,
  "results/trend_model.rds"
)

saveRDS(
  scenario_model,
  "results/scenario_model.rds"
)


# Save figures

ggsave(
  "figures/laying_dates.png",
  laying_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/monthly_comparison.png",
  monthly_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/window_comparison.png",
  window_plot,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  "figures/temperature_relationship.png",
  temperature_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/rainfall_relationship.png",
  rainfall_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/warming_scenarios.png",
  scenario_plot,
  width = 8,
  height = 5,
  dpi = 300
)


# Save model summaries and session information

writeLines(
  capture.output(summary(final_model)),
  "results/final_model_summary.txt"
)

writeLines(
  capture.output(summary(trend_model)),
  "results/trend_model_summary.txt"
)

writeLines(
  capture.output(summary(scenario_model)),
  "results/scenario_model_summary.txt"
)

writeLines(
  capture.output(sessionInfo()),
  "results/session_info.txt"
)

