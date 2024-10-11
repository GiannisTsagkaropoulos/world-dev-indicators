# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read the data
df <- read.csv(file.path("data", "data_tidy.csv"))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Descriptive Statistics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FertCat
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
#assign values to FertCat depending in which interval AdolFertRate belongs
df$FertCat[df$AdolFertRate > 0 & df$AdolFertRate <= 50] <- 1
df$FertCat[df$AdolFertRate > 50 & df$AdolFertRate <= 100] <- 2
df$FertCat[df$AdolFertRate > 100] <- 3

df$FertCat
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
#InflCat
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

df$InflCat <- 0
df$InflCat[df$Inflation > 2] <- 1 #assign InflCat = 1 if inflation > 2%

df$InflCat

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 3
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Quantitatives
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
CorControl <- df$CorControl
PolStab <- df$PolStab
VoiceAcc <- df$VoiceAcc

par(mfrow = c(1,3))

# Variables to analyze
quantitatives <- c("CorControl", "PolStab", "VoiceAcc")

# Loop through each variable to perform the analyses
QuantitativeStatistics <- function(var){
  cat("\n### Analysis for", var, "###\n")

  # Extract the column data
  data <- df[[var]]

  # Descriptive statistics
  ?mean
  mu <- mean(data)
  variance <- var(data)
  sd <- sd(data)
  median_val <- median(data)
  quantiles <- quantile(data)

  # Display descriptive statistics
  cat("Mean:", mu, "\n")
  cat("Variance:", variance, "\n")
  cat("Standard Deviation:", sd, "\n")
  cat("Median:", median_val, "\n")
  cat("Quantiles:\n")
  print(quantiles)

  # Histogram with density line
  hist(data, main = paste("Histogram of", var), breaks = 20, col = "#AAAAAA", freq = FALSE)
  lines(density(data), col = "blue")

  # Boxplot
  boxplot(data, main = paste("Boxplot of", var))

  # Q-Q plot with Q-Q line
  qqnorm(data, main = paste("Q-Q Plot of", var))
  qqline(data, col = "red")

  # Shapiro-Wilk test for normality
  shapiro_result <- shapiro.test(data)
  p_value <- shapiro_result$p.value

  # Interpretation of Shapiro-Wilk test
  if (p_value > 0.05) {
    cat("The data", var, "is normally distributed (p =", p_value, ")\n")
  } else {
    cat("The data", var, "is not normally distributed (p =", p_value, ")\n")
  }

  cat("\n------------------------------------\n")
}

QuantitativeStatistics("CorControl")
QuantitativeStatistics("PolStab")
QuantitativeStatistics("VoiceAcc")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Qualitatives
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

FertCat <- df$FertCat
InflCat <- df$InflCat

par(mfrow = c(1,2))

# FertCat
fert_freq <- table(FertCat)

barplot(fert_freq,xlab = "Fertility Category",
        names.arg = c("1", "2", "3"), col = "lightblue", width = c(2, 2), xlim = c (0, 10))

pie(fert_freq,
    col = c("cornsilk", "lightblue", "darkgreen"),
    labels = c("AdolFertRate in (0,50]",
               "AdolFertRate in (50,100]",
               "AdolFertRate in (100,Inf)"), cex=0.7
    )
par(xpd=TRUE)

legend(-1,1.6,
       fill =c("cornsilk", "lightblue", "darkgreen"),
       legend = c("AdolFertRate in (0,50]",
                  "AdolFertRate in (50,100]",
                  "AdolFertRate in (100,Inf)")
  )

mtext("Distribution of Countries by Fertility Category", outer = TRUE, line = 1, cex = 1.5 )

# InflCat

infl_freq <- table(InflCat)

barplot(infl_freq, xlab = "Inflation Category",
        names.arg = c("0", "1"), col = "lightblue", width = c(2, 2), xlim = c (0, 10))

pie(infl_freq,
    col = c("cornsilk", "darkgreen"),
    labels = c("Inflation <= 2%","Inflation > 2%"), cex=0.7
  )
par(xpd=TRUE)

legend(-0.5,1.6,
       fill = c("cornsilk", "darkgreen"),
       legend = c("Inflation <= 2%", "Inflation > 2%")
       )

mtext("Distribution of Countries by Inflation Category", outer = TRUE, line = -1, cex = 1.5 )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 1.4
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(psych)

#describeBy(df[, c("CorControl", "PolStab", "VoiceAcc")], group = FertCat, mat=TRUE)

describeBy(CorControl, FertCat, mat=TRUE)[,4:6]
describeBy(PolStab, FertCat, mat=TRUE)[,4:6]
describeBy(VoiceAcc, FertCat, mat=TRUE)[,4:6]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Linear Model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2.1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
full <- lm(CorControl ~ . -Country - PolStab - VoiceAcc, data = df)
summary(full)
full$coefficients
AIC(full)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

backward_stepwise <- step(m0, direction = "backward", criteria=~AIC)
stepwise_summary <- summary(backward_stepwise)

summary(backward_stepwise)

AIC(backward_stepwise)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2.3
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

r_squared <- summary(backward_stepwise)$r.squared
cat("R-squared:", r_squared, "\n")

r_squared_adj <- summary(backward_stepwise)$adj.r.squared
cat("Adjusted R-squared:", r_squared_adj, "\n")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2.5
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

bhat <- stepwise_summary$coefficients[, 1]
bhatse <- stepwise_summary$coefficients[, "Std. Error"]
#design matrix
X <- cbind(rep(1, nrow(backward_stepwise$model)), as.matrix(backward_stepwise$model[-1]))
X
n=nrow(X)
p=ncol(X)
length(bhat)

t_test <- function(X, bhat, bhatse, var, a){

  # Two-sided test
  # H0: b6 (GDP) = 0
  # H1: b6 != 0
  t_value <- bhat[var]/bhatse[var]

  # The test here is two-sided, so we need to compare our observed tstat value
  # with t*, the value for the t distribution, with n - d degrees of freedom that satisfies:
  # P(|T| > t*) = a <=> P(T > t* or T < - t*) = a
  # where T ~ t_(n-d).
  qt_a <- qt(1 - a / 2, n - p)
  qt_a <- qt(a / 2, n - p, lower.tail = FALSE)
  qt_a

  if (abs(t_value) > qt_a) {
    cat("We reject the null hypothesis H0 for the coefficient of", var,"in favor of the alternative hypothesis H1 at significance level alpha =", a, ", so the coefficient is statistically important.\n")
  } else {
    cat("We fail to reject the null hypothesis H0 for the coefficient of", var,"at significance level alpha =", a, " so the coefficient is not statistically important.\n")
  }
}

t_test(X, bhat, bhatse, "AgriLand", 0.05)
t_test(X, bhat, bhatse, "GDPdollars", 0.05)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2.6
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

a <- 0.01
width <- qt(1 - a / 2, df = n - length(bhat), lower.tail = TRUE) * bhatse
lower <- bhat - width
upper <- bhat + width
CI <- cbind(lower, upper)
CI

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2.7
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Call the attributes
coef <- backward_stepwise$coefficients
fitted <- backward_stepwise$fitted.values
res <- backward_stepwise$residuals

# We need to check the model assumptions. The assumptions are:
# 1. Independency
# 2. Homoskedasticity
# 3. Normality

# Plots: Residuals - Fitted Values, Histogram, Boxplot, Q-Q Plot
par(mfrow = c(2, 2))

plot(x = fitted, y = res,
     main = "Residuals - Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 20)
abline(h = 0, col = "red")

hist(res, main = "Histogram of Residuals")
boxplot(res, main = "Box Plot of Residuals")
qqnorm(res, main = "Q-Q Plot of Residuals")
qqline(res)

library(zoo)
library(lmtest)

# Breusch-Pagan test
# H0: Homoscedasticity
# H1: Heteroscedasticity
bptest(backward_stepwise)

# 3. Normality Assumption
# Test: Shapiro-Wilk
# H0: Normal Distribution
# H1: Not Normal Distribution
shapiro.test(res)

### OPTIONAL ---- MODEL IS GOOD no need for transformation ###

# None of the three plots seem "ideal".
# Could we transform one of the x?

CorControl <-df$CorControl

X <- cbind(rep(1, nrow(backward_stepwise$model)), as.matrix(backward_stepwise$model[-1]))
logGDPdollars <- log(X[,"GDPdollars"])
X <- cbind(X, logGDPdollars)

all_vars <- colnames(X)

# Step 2: Remove 'GDPdollars' from the list of variables
independent_vars <- setdiff(all_vars, c("","GDPdollars"))

b <- paste(independent_vars, collapse=" + ")
b
# Repeat the procedure
transformed_model <- lm(CorControl ~ ElectrAccess + AgriLand + CO2 + Internet + PopGrowth + AdolFertRate + PopPerc14 + WomBusiness + FertCat + logGDPdollars , data = df)
summary(transformed_model)

resnew <- transformed_model$residuals

# Repeat the heteroscedasticity test
bptest(transformed_model)

par(mfrow = c(1, 3))
hist(resnew, main = "Histogram of Residuals")
boxplot(resnew, main = "Box Plot of Residuals")
qqnorm(resnew, main = "Q-Q Plot of Residuals")
qqline(resnew)

# Repeat the normality test
shapiro.test(resnew)
pairs(cbind(X[,2:12], CorControl))
### OPTIONAL  finito ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2.8
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_new <- read.csv(file.path("data", "data_new.csv"))

reg <- backward_stepwise
summary(reg)

# Check if all required predictors are present in the new data
required_vars <- names(coef(backward_stepwise))[-1]  # Exclude the intercept
missing_vars <- setdiff(required_vars, names(df_new))

if (length(missing_vars) > 0) {
  stop(paste("The following required variables are missing from the new data:", paste(missing_vars, collapse = ", ")))
}

df_new$FertCat <- 0
df_new$FertCat[df_new$AdolFertRate > 50 & df_new$AdolFertRate <= 100] <- 2
df_new$FertCat[df_new$AdolFertRate > 0 & df_new$AdolFertRate <= 50] <- 1
df_new$FertCat[df_new$AdolFertRate > 100] <- 3

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2.8 i
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
predictions <- predict(backward_stepwise, newdata = df_new)
predictions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2.8 ii
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# mean prediction intervals
mean_prediction_intervals <- predict(backward_stepwise, newdata = df_new, interval = "confidence", level = 0.95)
mean_prediction_intervals

# individual prediction intervals
individual_prediction_intervals <- predict(backward_stepwise, newdata = df_new, interval = "prediction", level = 0.95)
individual_prediction_intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 3.1 i
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ANOVA model


yf1_infl0 <- CorControl[FertCat == 1 & InflCat == 0]
yf1_infl1 <- CorControl[FertCat == 1 & InflCat == 1]
yf2_infl0 <- CorControl[FertCat == 2 & InflCat == 0]
yf2_infl1 <- CorControl[FertCat == 2 & InflCat == 1]
yf3_infl0 <- CorControl[FertCat == 3 & InflCat == 0]
yf3_infl1 <- CorControl[FertCat == 3 & InflCat == 1]

yf1_infl0
yf1_infl1
yf2_infl0
yf2_infl1
yf3_infl0
yf3_infl1

aov_model <- aov(CorControl ~ factor(FertCat) + factor(InflCat), data = df)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 3.1 ii
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
anova_table <- summary(aov_model)
anova_table

a <- 0.05

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Interaction
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

p_values <- anova_table[[1]]$`Pr(>F)`

p_value_interaction <- p_values[3]

# A p-value less than 0.05 indicates that the main effect of that factor is statistically significant.
if (p_value_interaction < a) {
  cat("We reject the null hypothesis H0 at a level of statistical significance a=0.05, so there is a statistically significant interaction between the factors and then between the individual factors")
} else {
  cat("We fail to reject the null hypothesis H0 at the level of statistical significance a=0.05, so there is nota statistically significant interaction between the factors and then between the individual factors")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Factors
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~


f_values <- anova_table[[1]]$`F value`

f_value_FertCat <- f_values[1]
f_value_InflCat <- f_values[2]

m <- 3
qf_a_Fert <- qf(a, m - 1, n - m, lower.tail = FALSE)
qf_a_Fert


if (f_value_FertCat > qf_a_Fert) {
  cat("We reject the null hypothesis H0 at the level of statistical significance a=0.05, so the means of different levels of FertCat are identical, so the value of CorControl is dependent on the level of factor FertCat")
} else {
  cat("We fail to reject the null hypothesis H0 at the level of statistical significance a=0.05, so the means of different levels of FertCat are identical, so the value of CorControl is not dependent on the level of factor FertCat")
}

l <- 2
qf_a_Infl <- qf(a, l - 1, n - m, lower.tail = FALSE)
qf_a_Infl

if (f_value_InflCat > qf_a_Infl) {
  cat("We reject the null hypothesis H0 at a level of statistical significance a=0.05, so the means of different levels of InflCat are identical, so the value of CorControl is dependent on the level of factor InflCat")
} else {
  cat("We fail to reject the null hypothesis H0 at the level of statistical significance a=0.05, so the means of different levels of InflCat are identical, so the value of CorControl is not dependent on the level of factor InflCat")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

par(mfrow = c(1,1))
library(gplots)

interaction.plot(
  x.factor = df$FertCat,           # X-axis factor (Fertility categories)
  trace.factor = df$InflCat,       # Trace factor (Inflation categories)
  response = df$CorControl,        # Response variable (CorControl)
  fun = mean,                      # Function to apply (mean)
  type = "b",                      # Type of plot (both lines and points)
  col = c("bisque3", "darkgreen"),# Colors for the lines/points
  pch = c(16, 17),                 # Point characters (different for each level of InflCat)
  lwd = 2,                         # Line width
  xlab = "Fertility Category (FertCat)", # X-axis label
  ylab = "Mean of CorControl",     # Y-axis label
  trace.label = "Inflation Category (InflCat)", # Trace label
  main = "Interaction Plot of CorControl by FertCat and InflCat" # Title
)

# Adding a legend manually (optional, for clarity in some cases)
legend("topright", legend = c("Low Inflation (0)", "High Inflation (1)"),
       col = c("bisque3", "darkgreen"),, pch = c(16, 17), lty = 1, lwd = 2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 3.1 iii
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
resids <- aov_model$residuals
par(mfrow = c(2,2))

# Assumptions
# 1. Independence is assumed if the data collection is random.
# 2. Normality of residuals
hist(resid(anova_model), main = "Histogram of Residuals", xlab = "Residuals")
boxplot(resid(anova_model), main = "Boxplot of Residuals")

qqnorm(resid(anova_model))
qqline(resid(anova_model))

# 3. Homogeneity of variances
plot(anova_model, which = 1)  # Residuals vs Fitted


# Statistical Tests
# 1. Independence is assumed if the data collection is random.
# 2. Normality of residuals

shapiro.test(resids)

# 3. Homogeneity of variances
library(car)
levene_test <- leveneTest(resids ~ factor(FertCat) * factor(InflCat), data = df)
print(levene_test)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 3.1 iv
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
aov_fert <- aov(CorControl ~ factor(FertCat))

TukeyHSD(aov_fert)

# We are interested in the last column, titled "p adj", where we can see which
# difference is statistically significant, or, equivalently, in the two middle
# columns, titled "lwr" and "upr", where we can see the lower and upper bounds
# of each difference's confidence interval. Here we notice that the only not
# statistically significant difference is that between the means of the levels of fertility
# 2 and 3 (p-value > 0.05 or the confidence interval contains
# zero). Specifically:

# We reject mu_1 = mu_2 (p adj < 0.05)
# We reject mu_1 = mu_3 (p adj < 0.05)
# We do not reject mu_2 = mu_3 (p adj > 0.05)


aov_infl <- aov(CorControl ~ factor(InflCat))

TukeyHSD(anova_Infl)
