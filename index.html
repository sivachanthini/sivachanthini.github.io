# State-Level Determinants of Female Labour Force Participation in India
# PLFS 2021-22 | State-level cross-sectional analysis


# 1. Setup 
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(broom)


# 2. Import data 

df <- read_excel("df_female_plfs.xlsx")

glimpse(df)
summary(df)


# 3. Data preparation 
 
df_model <- df %>%
  mutate(
    female_lfpr = female_lfpr_percent,
    secondary_above = female_secondary_above_percent,
    log_nsdp = log(nsdp_2021_22),
    urbanisation = urbanisation_2011_percent,
    female_non_agri_share = female_non_agri_share_percent,
    married_before_18 = women_married_before_18_percent,
    tfr = tfr_percent
  ) %>%
  filter(state_ut != "Andaman & N. Island")


# Check analytical sample

nrow(df_model)
colSums(is.na(df_model))


# 4. Descriptive statistics
 
summary(
  df_model %>%
    select(
      female_lfpr,
      secondary_above,
      log_nsdp,
      urbanisation,
      female_non_agri_share,
      married_before_18,
      tfr
    )
)


# 5. Distribution of female LFPR
 
ggplot(df_model, aes(x = female_lfpr)) +
  geom_histogram(bins = 8) +
  labs(
    title = "Distribution of Female Labour Force Participation",
    x = "Female LFPR (%)",
    y = "Number of States/UTs"
  ) +
  theme_minimal()


# 6. Bivariate relationships 
 
ggplot(df_model, aes(x = secondary_above, y = female_lfpr)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Female LFPR and Female Secondary-or-Above Education",
    x = "Female secondary-or-above education (%)",
    y = "Female LFPR (%)"
  ) +
  theme_minimal()


ggplot(df_model, aes(x = log_nsdp, y = female_lfpr)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Female LFPR and State Economic Output",
    x = "Log NSDP",
    y = "Female LFPR (%)"
  ) +
  theme_minimal()


ggplot(df_model, aes(x = urbanisation, y = female_lfpr)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Female LFPR and Urbanisation",
    x = "Urbanisation (%)",
    y = "Female LFPR (%)"
  ) +
  theme_minimal()


ggplot(df_model, aes(x = female_non_agri_share, y = female_lfpr)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Female LFPR and Non-Agricultural Employment",
    x = "Female non-agricultural employment share (%)",
    y = "Female LFPR (%)"
  ) +
  theme_minimal()


ggplot(df_model, aes(x = married_before_18, y = female_lfpr)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Female LFPR and Early Marriage",
    x = "Women married before age 18 (%)",
    y = "Female LFPR (%)"
  ) +
  theme_minimal()


ggplot(df_model, aes(x = tfr, y = female_lfpr)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Female LFPR and Total Fertility Rate",
    x = "Total fertility rate",
    y = "Female LFPR (%)"
  ) +
  theme_minimal()


# 7. Correlation analysis 
 
correlation_matrix <- cor(
  df_model %>%
    select(
      female_lfpr,
      secondary_above,
      log_nsdp,
      urbanisation,
      female_non_agri_share,
      married_before_18,
      tfr
    ),
  use = "complete.obs"
)

round(correlation_matrix, 3)


# 8. Individual predictor models
 
model_education <- lm(
  female_lfpr ~ secondary_above,
  data = df_model
)

model_economic <- lm(
  female_lfpr ~ log_nsdp,
  data = df_model
)

model_urbanisation <- lm(
  female_lfpr ~ urbanisation,
  data = df_model
)

model_non_agri <- lm(
  female_lfpr ~ female_non_agri_share,
  data = df_model
)

model_early_marriage <- lm(
  female_lfpr ~ married_before_18,
  data = df_model
)

model_tfr <- lm(
  female_lfpr ~ tfr,
  data = df_model
)


# Model summaries

summary(model_education)
summary(model_economic)
summary(model_urbanisation)
summary(model_non_agri)
summary(model_early_marriage)
summary(model_tfr)


# 9. Compare individual models 

individual_models <- list(
  Education = model_education,
  Economic_Output = model_economic,
  Urbanisation = model_urbanisation,
  Non_Agricultural_Employment = model_non_agri,
  Early_Marriage = model_early_marriage,
  Fertility = model_tfr
)

individual_results <- bind_rows(
  lapply(names(individual_models), function(x) {
    tidy(individual_models[[x]]) %>%
      filter(term != "(Intercept)") %>%
      mutate(model = x)
  })
)

individual_fit <- bind_rows(
  lapply(names(individual_models), function(x) {
    glance(individual_models[[x]]) %>%
      mutate(model = x)
  })
) %>%
  select(model, r.squared, adj.r.squared, p.value)

individual_results
individual_fit


# 10. Education and economic development model

model_education_economic <- lm(
  female_lfpr ~ secondary_above + log_nsdp,
  data = df_model
)

summary(model_education_economic)


# 11. Demand-side model

model_demand <- lm(
  female_lfpr ~ urbanisation + female_non_agri_share,
  data = df_model
)

summary(model_demand)

vif(model_demand)


# 12. Supply-side model 

model_supply <- lm(
  female_lfpr ~ married_before_18 + tfr,
  data = df_model
)

summary(model_supply)

vif(model_supply)


# 13. Full multivariable model 

model_full <- lm(
  female_lfpr ~
    secondary_above +
    log_nsdp +
    urbanisation +
    female_non_agri_share +
    married_before_18 +
    tfr,
  data = df_model
)

summary(model_full)


# 14. Standardised full model 

df_standardised <- df_model %>%
  mutate(
    female_lfpr_z = as.numeric(scale(female_lfpr)),
    secondary_above_z = as.numeric(scale(secondary_above)),
    log_nsdp_z = as.numeric(scale(log_nsdp)),
    urbanisation_z = as.numeric(scale(urbanisation)),
    female_non_agri_share_z = as.numeric(scale(female_non_agri_share)),
    married_before_18_z = as.numeric(scale(married_before_18)),
    tfr_z = as.numeric(scale(tfr))
  )

model_full_standardised <- lm(
  female_lfpr_z ~
    secondary_above_z +
    log_nsdp_z +
    urbanisation_z +
    female_non_agri_share_z +
    married_before_18_z +
    tfr_z,
  data = df_standardised
)

summary(model_full_standardised)


# 15. Multicollinearity

vif(model_full)


# 16. Model diagnostics 

par(mfrow = c(2, 2))
plot(model_full)
par(mfrow = c(1, 1))


# 17. Cook's distance 

cooks_distance <- cooks.distance(model_full)

cook_results <- data.frame(
  state_ut = df_model$state_ut,
  cooks_distance = cooks_distance
) %>%
  arrange(desc(cooks_distance))

cook_results


# 18. Identify influential observations 

n <- nrow(df_model)
cook_threshold <- 4 / n

cook_results %>%
  filter(cooks_distance > cook_threshold)


# 19. Regression results table 

full_results <- tidy(model_full) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 3)
  )

full_results


# 20. Model fit summary 

model_comparison <- bind_rows(
  glance(model_education) %>%
    mutate(model = "Education"),

  glance(model_economic) %>%
    mutate(model = "Economic output"),

  glance(model_education_economic) %>%
    mutate(model = "Education + economic output"),

  glance(model_demand) %>%
    mutate(model = "Demand-side"),

  glance(model_supply) %>%
    mutate(model = "Supply-side"),

  glance(model_full) %>%
    mutate(model = "Full model")
) %>%
  select(
    model,
    r.squared,
    adj.r.squared,
    statistic,
    p.value
  )

model_comparison


# 21. Final predictor comparison 

final_predictors <- tidy(model_full_standardised) %>%
  filter(term != "(Intercept)") %>%
  select(
    term,
    estimate,
    std.error,
    statistic,
    p.value
  ) %>%
  arrange(desc(abs(estimate)))

final_predictors


# 22. Save analytical outputs 

write.csv(
  correlation_matrix,
  "correlation_matrix.csv"
)

write.csv(
  individual_results,
  "individual_model_coefficients.csv",
  row.names = FALSE
)

write.csv(
  individual_fit,
  "individual_model_fit.csv",
  row.names = FALSE
)

write.csv(
  model_comparison,
  "model_comparison.csv",
  row.names = FALSE
)

write.csv(
  full_results,
  "full_model_results.csv",
  row.names = FALSE
)

write.csv(
  final_predictors,
  "standardised_predictor_comparison.csv",
  row.names = FALSE
)

write.csv(
  cook_results,
  "cooks_distance.csv",
  row.names = FALSE
)


# End of analysis 
