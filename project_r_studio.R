data <- read.csv("US_Accidents_2023.csv")

#EDA

#overview
nrow(data)
ncol(data)
str(data)
summary(data)

#check missing values
colSums(is.na(data))

#drop non-informative columns
drop_cols <- c(
  "ID", "Source", "Description", "Street",
  "End_Time", "Weather_Timestamp",
  "End_Lat", "End_Lng", "Country",
  "Turning_Loop", "Wind_Chill.F.",
  "Precipitation.in.", "City",
  "Zipcode", "Airport_Code", "County")

data <- data[, !names(data) %in% drop_cols]

#time feature engineering
data$Start_Time <- as.POSIXct(
  data$Start_Time,
  format = "%Y-%m-%d %H:%M:%S")

data$Hour <- as.numeric(format(data$Start_Time, "%H"))
data$Weekday <- as.numeric(format(data$Start_Time, "%u"))

#drop raw timestamp
data$Start_Time <- NULL

#numerical weather variables
num_vars <- c(
  "Temperature.F.",
  "Humidity...",
  "Pressure.in.",
  "Visibility.mi.",
  "Wind_Speed.mph.")

library(tidyverse)
library(reshape2)

#distributions BEFORE imputation
long_data <- melt(
  data[, num_vars],
  variable.name = "Variable",
  value.name = "Value")

ggplot(long_data, aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~Variable, scales = "free") +
  labs(
    title = "Distributions of Numerical Weather Variables",
    x = "Value",
    y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#median imputation
for (v in num_vars) {
  data[[v]][is.na(data[[v]])] <- median(data[[v]], na.rm = TRUE)}

colSums(is.na(data))

#TARGET VARIABLE: Distance

#histogram
ggplot(data, aes(x = Distance.mi.)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Accident Distance",
    x = "Distance (miles)",
    y = "Frequency") +
  theme_minimal(base_size = 14)

#boxplot
ggplot(data, aes(y = Distance.mi.)) +
  geom_boxplot(
    fill = "salmon",
    outlier.color = "red",
    outlier.shape = 16) +
  labs(
    title = "Boxplot of Accident Distance",
    y = "Distance (miles)") +
  theme_minimal(base_size = 14)


#categorical variables
cat_vars <- c("State", "Timezone", "Weather_Condition", "Wind_Direction")
for (v in cat_vars) {
  data[[v]] <- as.factor(data[[v]])}


#binary infrastructure (0/1)
binary_vars <- c(
  "Amenity", "Bump", "Crossing", "Give_Way", "Junction",
  "No_Exit", "Railway", "Roundabout", "Station", "Stop",
  "Traffic_Calming", "Traffic_Signal")

for (v in binary_vars) {
  data[[v]] <- ifelse(data[[v]] == "True", 1, 0)}

#day / night cycles
cycle_vars <- c(
  "Sunrise_Sunset",
  "Civil_Twilight",
  "Nautical_Twilight",
  "Astronomical_Twilight")

for (v in cycle_vars) {
  data[[v]] <- as.factor(data[[v]])}

#Final checks
str(data)
colMeans(is.na(data)) * 100
summary(data)
names(data)[sapply(data, is.numeric)]


#Correlation heatmap
heat_vars <- c(
  "Distance.mi.",
  "Temperature.F.",
  "Humidity...",
  "Pressure.in.",
  "Visibility.mi.",
  "Wind_Speed.mph.")

cor_matrix <- cor(data[, heat_vars], use = "complete.obs")
cor_matrix

melted_cor <- melt(cor_matrix)

ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(
    low = "red4",
    mid = "white",
    high = "skyblue3",
    midpoint = 0,
    limit = c(-1, 1),
    name = "Correlation") +
  labs(
    title = "Correlation Heatmap of Numerical Variables",
    x = "",
    y = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1))

#Outliers
box_vars <- c(
  "Distance.mi.",
  "Temperature.F.",
  "Humidity...",
  "Pressure.in.",
  "Visibility.mi.",
  "Wind_Speed.mph.")


data_long <- data %>%
  select(all_of(box_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value")

ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Outlier Detection Using Boxplots",
    x = "",
    y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),strip.text = element_text(size = 11, face = "bold"))


#MODELS

#3.1.Numeric (testing the effect of weather variables)
#Hypothesis:
#H0: Weather variables have no effect on accident distance
#H1: At least one weather variable affects accident distance
m1 <- lm(Distance.mi. ~Temperature.F. +Humidity... +Pressure.in. +Visibility.mi. +Wind_Speed.mph.,
  data = data)
summary(m1)


par(mfrow = c(1, 2))
#Residuals vs Fitted
plot(
  m1$fitted.values,
  resid(m1),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted (M1)")
abline(h = 0, col = "red", lwd = 2)
#Q-Q plot
qqnorm(resid(m1),
       main = "Q-Q Plot of Residuals (M1)")
qqline(resid(m1), col = "red", lwd = 2)
#reset
par(mfrow = c(1, 1))


#3.2.Numeric + Quadratic (testing if adding squared terms improves fit)
m2 <- lm(Distance.mi. ~Temperature.F. +Humidity... +Pressure.in. +Visibility.mi. +Wind_Speed.mph. +
    I(Temperature.F.^2) +I(Humidity...^2) +I(Pressure.in.^2) +I(Visibility.mi.^2) +I(Wind_Speed.mph.^2),
  data = data)
summary(m2)

par(mfrow = c(1, 2))  
#Residuals vs Fitted
plot(
  m2$fitted.values,
  resid(m2),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted (M2)")
abline(h = 0, col = "red", lwd = 2)
#Q-Q Plot
qqnorm(
  resid(m2),
  main = "Q-Q Plot of Residuals (M2)")
qqline(resid(m2), col = "red", lwd = 2)
par(mfrow = c(1, 1))  #reset


#3.3: Numeric + Categorical (adding state and weather condition to see if they explain variation)
#H0: After accounting for numerical weather variables, accident distance does not vary across states or weather conditions.
#H1: Accident distance differs across states and/or weather conditions, even after controlling for numerical weather variables.
m3 <- lm(Distance.mi. ~Temperature.F. +Humidity... +Pressure.in. +Visibility.mi. +Wind_Speed.mph. +
    State +Weather_Condition,data = data)
summary(m3)


par(mfrow = c(1, 2))
#Residuals vs Fitted
plot(
  m3$fitted.values,
  resid(m3),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted (M3)")
abline(h = 0, col = "red", lwd = 2)
#Q-Q plot
qqnorm(resid(m3), main = "Q-Q Plot of Residuals (M3)")
qqline(resid(m3), col = "red", lwd = 2)
par(mfrow = c(1, 1))


#3.4: Numeric + Boolean (including traffic-related binary variables to check their effect)
m4 <-lm(
  Distance.mi. ~
    Temperature.F. +
    Humidity... +
    Pressure.in. +
    Visibility.mi. +
    Wind_Speed.mph. +
    Traffic_Signal +
    Junction +
    Crossing +
    Railway +
    Stop +
    Station,
  data = data)

summary(m4)

par(mfrow = c(1, 2))
#Residuals vs Fitted
plot(
  m4$fitted.values,
  resid(m4),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted (M4)")
abline(h = 0, col = "red", lwd = 2)
#Q-Q plot
qqnorm(resid(m4), main = "Q-Q Plot of Residuals (M4)")
qqline(resid(m4), col = "red", lwd = 2)
par(mfrow = c(1, 1))


#3.5 Numeric + Boolean + Categorical (model with numeric, binary, and categorical predictors)
m5 <- lm(
  Distance.mi. ~
    Temperature.F. +
    Humidity... +
    Pressure.in. +
    Visibility.mi. +
    Wind_Speed.mph. +
    Traffic_Signal +
    Junction +
    Crossing +
    Railway +
    Stop +
    State +
    Weather_Condition,
  data = data)

summary(m5)

par(mfrow = c(1, 2))
# Residuals vs Fitted
plot(
  m5$fitted.values,
  resid(m5),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted (M5)")
abline(h = 0, col = "red", lwd = 2)
#Q-Q plot
qqnorm(resid(m5), main = "Q-Q Plot of Residuals (M5)")
qqline(resid(m5), col = "red", lwd = 2)
par(mfrow = c(1, 1))

#3.6 Log-transformed y:applying log transformation to response to reduce skewness
m6 <- lm(
  log(Distance.mi. + 1) ~Temperature.F. +Humidity... +Pressure.in. +
    Visibility.mi. + Wind_Speed.mph. + Traffic_Signal +Junction +
    Crossing +State,data = data)

summary(m6)

par(mfrow = c(1, 2))
# Residuals vs Fitted
plot(
  m4$fitted.values,
  resid(m6),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted (M6)")
abline(h = 0, col = "red", lwd = 2)
#Q-Q plot
qqnorm(resid(m6), main = "Q-Q Plot of Residuals (M6)")
qqline(resid(m6), col = "red", lwd = 2)
par(mfrow = c(1, 1))


#3.7: log(y) ~ Num + Cat (testing log-transformed response with numeric and categorical predictors)
m7 <- lm(
  log(Distance.mi. + 1) ~
    Temperature.F. +
    Humidity... +
    Pressure.in. +
    Visibility.mi. +
    Wind_Speed.mph. +
    State +
    Weather_Condition,
  data = data)

summary(m7)

par(mfrow = c(1, 2))
#Residuals vs Fitted
plot(
  fitted(m7),
  resid(m7),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted (M7)")
abline(h = 0, col = "red", lwd = 2)

#Q-Q plot
qqnorm(resid(m7), main = "Q-Q Plot (M7)")
qqline(resid(m7), col = "red", lwd = 2)

par(mfrow = c(1, 1))


#3.8: log(y) ~ Num + Cat + Poly (adding squared terms to log(y) model)
m8 <- lm(
  log(Distance.mi. + 1) ~
    Temperature.F. +
    Humidity... +
    Pressure.in. +
    Visibility.mi. +
    Wind_Speed.mph. +
    I(Temperature.F.^2) +
    I(Humidity...^2) +
    I(Pressure.in.^2) +
    I(Visibility.mi.^2) +
    I(Wind_Speed.mph.^2) +
    State +
    Weather_Condition,
  data = data)

summary(m8)

par(mfrow = c(1, 2))
#Residuals vs Fitted
plot(
  fitted(m8),
  resid(m8),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted (M8)")
abline(h = 0, col = "red", lwd = 2)
#Q-Q plot
qqnorm(resid(m8), main = "Q-Q Plot (M8)")
qqline(resid(m8), col = "red", lwd = 2)
par(mfrow = c(1, 1))

#3.9 Mixed transformations:combination of quadratic, log-transformed predictors, and categorical variables
m_xt <- lm(
  Distance.mi. ~
    Temperature.F. +
    Humidity... +
    Pressure.in. + I(Pressure.in.^2) +
    log(Visibility.mi. + 0.1) +
    log(Wind_Speed.mph. + 1) +
    State +
    Weather_Condition,
  data = data)

summary(m_xt)

par(mfrow = c(1, 2))
#residuals vs fitted
plot(
  fitted(m_xt),
  resid(m_xt),
  main = "Residuals vs Fitted (X-transformed)",
  xlab = "Fitted values",
  ylab = "Residuals")
abline(h = 0, col = "red")
#Q-Q plot
qqnorm(resid(m_xt))
qqline(resid(m_xt), col = "red")
par(mfrow = c(1, 1))



#Evaluation of model performance using Mean Squared Error(MSE)
mse <- function(model, data) {
  mean((data$Distance.mi. - fitted(model))^2)}

#MSE for original-scale models
mse_M1 <- mse(m1, data)
mse_M2 <- mse(m2, data)
mse_M3 <- mse(m3, data)
mse_M4 <- mse(m4, data)
mse_M5 <- mse(m5, data)
mse_mxt<- mse(m_xt,data)

mse_M1
mse_M2
mse_M3
mse_M4
mse_M5
mse_mxt

#For log-transformed response models, back-transform predictions to original scale
#m6
pred_M6 <- exp(fitted(m6)) - 1
mse_M6 <- mean((data$Distance.mi. - pred_M6)^2)
mse_M6
#m7
pred_M7 <- exp(fitted(m7)) - 1
mse_M7 <- mean((data$Distance.mi. - pred_M7)^2)
mse_M7
#m8
pred_M8 <- exp(fitted(m8)) - 1
mse_M8 <- mean((data$Distance.mi. - pred_M8)^2)
mse_M8

#Combine all MSE values into a table for comparison
mse_table <- data.frame(
  Model = c("M1", "M2", "M3", "M4", "M5", "M6_log", "M7_log", "M8_log","M_xt"),
  MSE = c(mse_M1, mse_M2, mse_M3, mse_M4, mse_M5,
          mse_M6, mse_M7, mse_M8,mse_mxt ))

mse_table

#Compute 95% confidence intervals for final model(m5)
confint(m5, level = 0.95)


