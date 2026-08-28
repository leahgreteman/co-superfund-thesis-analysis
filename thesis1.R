###################### FINAL ANALYSIS #########################

##### PACKAGES #####
library(dplyr)
library(ggplot2)
library(glm2)
library(lme4)
library(nlme)
library(psych)

#############################################################################
########### CHECKING DISTRIBUTIONS OF DATA ##################################

# Example dataset
data <- rnorm(100, mean = 50, sd = 10)

# Histogram for BLLs
ggplot(bllnoout, aes(x = Logbll)) + geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") + labs(title = "BLL Distribution", x = "BLLs", y = "Frequency") +  theme_minimal()
# veryyyyy left skewed (we knew this)

# histogram of 2008 and 2024 prop value
ggplot(didmore, aes(x = prop_value)) + geom_histogram(binwidth = 10000, fill = "magenta", color = "black") + labs(title = "Property Value Distribution", x = "$", y = "Frequency") +  theme_minimal()
# ugh why are labels though

# a better histogram 

# Q-Q plot for BLLs
ggplot(bllnoout, aes(sample = Logbll)) + stat_qq() + stat_qq_line(color = "red") + labs(title = "Q-Q Plot of BLLs") + theme_minimal()
# if you ignore the 'low's, it's pretty normal. as normal as it can be? not all around terrible

ggplot(didmore, aes(sample = Logbll)) + stat_qq() + stat_qq_line(color = "red") + labs(title = "Q-Q Plot of BLLs") + theme_minimal()

######################### SPEARMAN'S ##########################################

# SS = X, BLL = Y
cor.test(bllnoout$Sprfnd_, bllnoout$Rslt_Nm, method = "spearman") ### NA bc X must be vector/numeric

# Prop value = X, BLL = Y
cor.test(bllnoout$year_a, bllnoout$rslt_num, method = "spearman")

# Age = X, BLL = Y
cor.test(bllnoout$age, bllnoout$rslt_num, method = "spearman")

# Year home built = X, BLL = Y
cor.test(bllnoout$year_built, bllnoout$rslt_num, method = "spearman")



############################## T TEST FOR SOIL PB ####################################

# will attempt a two sample t test on log transformed data, or mann-whitney u-test if data is too abnormal

# log transform
ttest$logresult <- log(ttest$result)

# histogram
hist(ttest$logresult) # normal! yay
hist(ttest$result) # NOT normal! boo

# histograms showing both treatments at once
ggplot(ttest, aes(x = logresult, fill = treatment)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(
    x = "Log-transformed Pb concentration",
    y = "Count",
    fill = "Treatment"
  ) +
  theme_minimal(base_family = "Times New Roman") # NOT TODAY SATAN

# TRY AGAIN HISTOGRAMS
ggplot(ttest %>% filter(!is.na(treatment)),
       aes(x = logresult)) +
  geom_histogram(bins = 30, fill = "#660000", color = "black") +
  facet_wrap(~ treatment, ncol = 1, scales = "free_y") +
  labs(
    x = "Log-transformed Pb concentration",
    y = "Count"
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 16)

# treatment needs to be factor
ttest$treatment <- as.factor(ttest$treatment)

# checking for equal variances
var.test(logresult ~ treatment, data = ttest) # not equal, need to do welch's instead of two sample t test
exp(0.460) # 1.584074 lower bound CI
exp(0.973) # 2.64587 upper bound CI

# welch's t test
t.test(logresult ~ treatment, data = ttest)
exp(5.278118)  # ≈ 196 µg/kg before
exp(4.253186)  # ≈ 70 µg/kg after
exp(0.776) # CI lower bound = 2.172
exp(1.274) # CI upper bound = 3.575

# box plot for funsies
ttest$treatment <- factor(ttest$treatment,
                          levels = c("before", "after"))
ggplot(ttest %>% filter(!is.na(treatment)), aes(x = treatment, y = logresult)) + geom_boxplot() + labs(x = "Remediation", y = "Log-transformed Pb concentration"
) + theme_minimal(base_family = "Times New Roman")

ttest$treatment <- factor(ttest$treatment,
                          levels = c("before", "after"))
ggplot(ttest %>% filter(!is.na(treatment)), aes(x = treatment, y = logresult)) + geom_boxplot() + labs(x = "Remediation", y = "Log-transformed Pb concentration"
) + theme_minimal(base_family = "Times New Roman", base_size = 16)


############################## CHI SQ FOR ELEVATED BLLS ################################

# Make a contingency table
table <- table(data$elevated, data$superfund)

# View the table
table

# Run chi-squared test
chisq.test(table)

# bar chart
ggplot(data, aes(x = superfund, fill = elevated)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Superfund Status",
    y = "Percent of Children",
    fill = "Elevated BLL",
    title = "Proportion of Elevated BLLs by Superfund Status"
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 16)

# mosaic plot
par(family = "Times")   # set font for everything below
mosaicplot(
  tab,
  col = c("#a6cee3", "#b2182b"),   # pick any colors 
  main = "Elevated BLLs by Superfund Status"
)


############################### SPEARMAN'S #####################################

cor.test(chisq$superfund_cat, chisq$elevated, method = "spearman")

##### DOESN'T ACTUALLY MAKE SENSE TO DO THIS!


################################## LOG REG TESTS ##############################

# looking at superfund status as the dependent variable in an logistic regression
modelbll1 <- glm(Sprfnd_ ~ Rslt_Nm + Yer_blt + Age + `2024_LT` + `2014_LT`, data = bll, family = binomial)
summary(modelbll1)

## that was incredibly unsuccessful. Let's not do a log reg
### HAH jk we're trying again! let's be more reasonable and use 1 independent variable

# superfund site effects on BLL
logreg1 <- glm(rslt_report ~ superfund, data = bllnoout, family = binomial)
summary(logreg1)
exp(1.0389) # the odds ratio of kids inside the site having an elevated BLL

# home age effects on BLL
logreg2 <- glm(rslt_report ~ year_built, data = bllnoout, family = binomial)
summary(logreg2)
exp(-0.027510)

########################## PLOTTING THE LOG REGS #################################

ggplot(bllnoout, aes(x = superfund, y = rslt_report)) + geom_point(alpha = 0.4) + geom_smooth(method = "glm", method.args = list(family = "binomial"),  se = TRUE, color = "blue", fill = "lightblue") +  theme_minimal() + labs(x = "Predictor", y = "Probability of Response (1)") ## noooooope not this 

ggplot(bllnoout, aes(x = superfund, y = rslt_report)) + stat_summary(fun = mean, geom = "point", size = 3) + stat_summary(fun = mean, geom = "line", aes(group = 1)) + theme_minimal() + labs(y = "Proportion of Response = 1") ## NOT this either

pred1 <- data.frame(superfund = unique(bllnoout$superfund))
pred1$predicted_prob <- predict(logreg1, newdata = pred1, type = "response")

ggplot(bllnoout, aes(x = superfund, y = rslt_report)) + stat_summary(fun = mean, geom = "point", size = 3, color = "black") + geom_point(data = pred1, aes(x = superfund, y = predicted_prob), color = "blue", size = 4) + theme_minimal() + labs(title = "Probability of Elevated BLL by Superfund Status", x = "Superfund Site", y = "Predicted Probability of Elevated BLL")

## for a more continuous plot
logdata <- data.frame(
  HomeAge = seq(min(bllnoout$year_built, na.rm = TRUE),
                max(bllnoout$year_built, na.rm = TRUE),
                length.out = 100))
logdata$predicted_prob <- predict(logreg2, newdata = newdata2, type = "response")

########
################## NOT GETTING MUCH SUCCESS WITH THE ABOVE ###################
########

library(ggplot2)
library(dplyr)
library(tibble)

# fit (or reuse) model
model <- glm(rslt_report ~ superfund, family = binomial, data = bllnoout)

# Extract coefficients and covariance
beta <- coef(model)
V    <- vcov(model)
crit <- qnorm(0.975)

# Create smooth numeric x-grid 0 -> 1 and compute link SE using vcov
xgrid <- seq(0, 1, length.out = 200)
Xmat  <- cbind(1, xgrid)                         # design matrix rows [1, x]
fit_link <- as.vector(Xmat %*% beta)
se_link2  <- sqrt(rowSums((Xmat %*% V) * Xmat))  # var for each row -> sqrt gives se

# Back-transform to probability scale with asymmetric 95% CI
fit_prob <- plogis(fit_link)
lower    <- plogis(fit_link - crit * se_link2)
upper    <- plogis(fit_link + crit * se_link2)

sigmoid_df <- tibble(x = xgrid, fit_prob = fit_prob, lower = lower, upper = upper)

# Prepare plotted points: convert rslt_report to numeric 0/1 explicitly
bll_plot <- bllnoout %>%
  mutate(
    superfund_n   = ifelse(superfund, 1, 0),
    rslt_report_n = ifelse(as.logical(rslt_report), 1, 0)   # numeric 0/1 for plotting
  )

# Group-level observed proportions (using numeric column)
grp <- bll_plot %>%
  group_by(superfund_n) %>%
  summarize(n = n(),
            prop = mean(rslt_report_n, na.rm = TRUE),
            .groups = "drop")

# Plot: ribbon, smooth curve, jittered points (numeric), group props
ggplot() +
  geom_ribbon(data = sigmoid_df, aes(x = x, ymin = lower, ymax = upper),
              alpha = 0.18, inherit.aes = FALSE) +
  geom_line(data = sigmoid_df, aes(x = x, y = fit_prob), linewidth = 1.1) +
  geom_jitter(data = bll_plot, aes(x = superfund_n, y = rslt_report_n),
              width = 0.05, height = 0.03, alpha = 0.45, size = 1.6) +
  geom_point(data = grp, aes(x = superfund_n, y = prop),
             size = 3.6, shape = 21, fill = "white", color = "black") +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("Outside Superfund", "Inside Superfund")) +
  scale_y_continuous(limits = c(-0.05, 1.05), breaks = seq(0, 1, 0.2)) +
  labs(
    x = "Superfund status",
    y = "Predicted probability of elevated BLL (rslt_report = 1)",
    title = "Logistic regression: rslt_report ~ superfund",
    subtitle = "Smooth fitted probability (logistic curve) with 95% CI; jittered raw observations"
  ) +
  theme_minimal(base_size = 13)

########################## TOO MUCH #####################################

# simplify
# Fit the logistic regression
logplot <- glm(rslt_report ~ superfund, family = binomial, data = bllnoout)

# Create a prediction dataset
newdata <- data.frame(superfund = c(FALSE, TRUE))
newdata$predicted_prob <- predict(model, newdata, type = "response")

# Basic bar plot of predicted probabilities
library(ggplot2)
ggplot(newdata, aes(x = superfund, y = predicted_prob, fill = superfund)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = round(predicted_prob, 2)), vjust = -0.5, size = 5) +
  scale_x_discrete(labels = c("Outside Superfund", "Inside Superfund")) +
  labs(
    x = "Superfund status",
    y = "Predicted probability of elevated BLL",
  ) +
  theme_minimal(base_size = 12)

## had to adjust the top of the plot made above^ with these adjustments
ggplot(newdata, aes(x = superfund, y = predicted_prob, fill = superfund)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = round(predicted_prob, 2)), vjust = -0.5, size = 5) +
  scale_x_discrete(labels = c("Outside Superfund", "Inside Superfund")) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Superfund status",
    y = "Predicted probability of elevated BLL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )





################################## univariate ####################################

#>>>>>>>> I removed the outliers from the data, see bllnoout
#>>>>>>>> also made some changes to a few variables - added Age_Grp, added category 3 to Rem_Req
rm(bllnoout)

############################ BLL AS DEPENDENT/RESPONSE ####################

modelbll2 <- lm(rslt_num ~ superfund, data = bllnoout)
summary(modelbll2)
# NOT significant 

modelbll3 <- lm(rslt_num ~ age, data = bllnoout)
summary(modelbll3)
# NOT sig, p val = 0.757

modelbll4 <- lm(rslt_num ~ `2014_LT`, data = bllnoout)
summary(modelbll4)
# NOT sign, p val = 0.249

modelbll5 <- lm(rslt_num ~ Rem_1Y_2N_3Out, data = bllnoout)
summary(modelbll5)
# NOT sig, p val = 0.1449

modelbll6 <- lm(rslt_num ~ built_before, data = bllnoout)
summary(modelbll6)
## SIGNIFICANT, p val = 0.00198

modelbll7 <- lm(rslt_num ~ year_built, data = bllnoout)
summary(modelbll7)
## SIGNIFICANT, p val = 1.78e-06

modelbll8 <- lm(rslt_num ~ Btwn_0_, data = bllnoout)
summary(modelbll8)
# NOT sig, p val = 0.818

modelbll9 <- lm(rslt_num ~ `2024_LT`, data = bllnoout)
summary(modelbll9)
## SIGNIFICANT, p val = 0.0416

modelbll10 <- lm(rslt_num ~ Age_Grp, data = bllnoout)
summary(modelbll10)
# NOT sig, p val = 0.6134

modelbll11 <- lm(result ~ pb_iwd, data = blinterp)
summary(modelbll11)
# Not sig, p val = 0.6298

################# PLOTTING THE UNIVARIATE MODELS ###########################

# using ggplot2

# scatter plot looking at home age and BLL
ggplot(bllnoout, aes(x = year_built, y = rslt_num)) +
  geom_point(alpha = 0.6) +                        # scatterplot of data points
  geom_smooth(method = "lm", color = "blue",       # linear regression line
              fill = "lightblue", se = TRUE) +     # shaded 95% confidence band
  labs(
    title = "Relationship Between Home Age and Child BLL",
    x = "Year Built",
    y = "Child Blood Lead Level (µg/dL)"
  ) +
  theme_minimal(base_size = 14)

# adjusting for point size and font
ggplot(bllnoout, aes(x = year_built, y = rslt_num)) +
  geom_point(alpha = 0.6, size = 3) +              # increased point size
  geom_smooth(method = "lm",
              color = "blue",
              fill = "lightblue",
              se = TRUE,
              size = 1.2) +                        # thicker regression line
  labs(
    title = "Relationship Between Home Age and Child BLL",
    x = "Year Built",
    y = "Child Blood Lead Level (µg/dL)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      family = "Times New Roman",                  # font family
      face = "bold",                               # optional, makes it stand out
      size = 16,                                   # slightly larger title
      hjust = 0.5                                  # center the title
    )
  )

# scatter with bll / superfund status
## new model with interp data
ggplot(bllnoout, aes(x = superfund, y = rslt_num)) +
  geom_point(alpha = 0.6, size = 3) +              # increased point size
  geom_smooth(method = "lm",
              color = "blue",
              fill = "lightblue",
              se = TRUE,
              size = 1.2) +                        # thicker regression line
  labs(
    title = "Child BLL vs Superfund status",
    x = "Superfund status",
    y = "Child Blood Lead Level (µg/dL)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      family = "Times New Roman",                  # font family
      face = "bold",                               # optional, makes it stand out
      size = 16,                                   # slightly larger title
      hjust = 0.5                                  # center the title
    )
  )

# plotting child bl and interp soil
ggplot(blinterp, aes(x = pb_iwd, y = result_num)) +
  geom_point(alpha = 0.6, size = 3) +              # increased point size
  geom_smooth(method = "lm",
              color = "blue",
              fill = "lightblue",
              se = TRUE,
              size = 1.2) +                        # thicker regression line
  labs(
    title = "Child BLL vs Soil Pb Level",
    x = "Soil Pb (ppm)",
    y = "Child Blood Lead Level (µg/dL)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      family = "Times New Roman",                  # font family
      face = "bold",                               # optional, makes it stand out
      size = 16,                                   # slightly larger title
      hjust = 0.5                                  # center the title
    )
  )

# plotting the log model logit1
ggplot(blinterp, aes(x = pb_iwd, y = result)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Estimated Soil Pb Concentration (IDW, mg/kg)",
    y = "Predicted Probability of Reportable Child BLL",
  ) +
  theme_minimal(base_family = "Times New Roman")

# plotting log regs
# Create new data for prediction
newdat <- blinterp %>%
  summarise(
    pb_iwd_min = min(pb_iwd, na.rm = TRUE),
    pb_iwd_max = max(pb_iwd, na.rm = TRUE),
    age = mean(age, na.rm = TRUE),
    year = mean(year, na.rm = TRUE),
    year_m = mean(year_m, na.rm = TRUE),
    built_before = 0,  # reference category
    rem = 0            # reference category
  )

pred_df <- data.frame(
  pb_iwd = seq(newdat$pb_iwd_min, newdat$pb_iwd_max, length.out = 100),
  age = newdat$age,
  year = newdat$year,
  year_m = newdat$year_m,
  built_before = newdat$built_before,
  rem = newdat$rem
)

pred <- predict(
  logit1,   # replace with your glm object name
  newdata = pred_df,
  type = "link",
  se.fit = TRUE
)

pred_df <- pred_df %>%
  mutate(
    fit = plogis(pred$fit),
    lwr = plogis(pred$fit - 1.96 * pred$se.fit),
    upr = plogis(pred$fit + 1.96 * pred$se.fit)
  )

ggplot(pred_df, aes(x = pb_iwd, y = fit)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  labs(
    x = "Estimated Soil Pb Concentration (IDW, mg/kg)",
    y = "Predicted Probability of Reportable Child BLL",
    title = "Predicted Probability of Reportable Child BLLs by Soil Lead"
  ) +
  theme_minimal(base_family = "Times New Roman")

# a little more simple
library(margins)
m_pb <- margins(logit1, variables = "pb_iwd")
plot(m_pb)

################################## MLR ####################################

# do this after finishing interpolation

# testing the waters
modelmv1 <- lm(result ~ pb_iwd + age + age_group + year_m + year + built_before + year_built + sqft + rem, data = blinterp)
summary(modelmv1) 

# literally everything
modelmv2 <- lm(result ~ pb_iwd + age + age_group + youngest + year_m + year_a + year_b + year_c + year_d + year_e + year_f + year_g + year_h + year_i + year_j + year_l + year + built_before + year_built + sqft + beds + baths + rem + rem_compl, data = blinterp)
summary(modelmv2)

modelmv3 <- lm(result ~ pb_iwd + )

############################ BLL AS DEPENDENT/RESPONSE ####################

modelbll1 <- lm(rslt_num ~ Superfund + age + Bult_bf + `2024_LT` + `2014_LT`, data = bllnoout)
summary(modelbll1)

################################## AIC ####################################

AIC(modelbll1) # mlr on all variables except soil,    2030.362
AIC(modelbll2) # superfund status,                    2815.994
AIC(modelbll3) # child age,                           2818.245
AIC(modelbll4) # 2014 values,                         2388.943
AIC(modelbll5) # remediation required?,               2816.208 
AIC(modelbll6) # built before 1978?,                  2643.625
AIC(modelbll7) # year home was built,                 2622.895
AIC(modelbll8) # child between 0 and 6?,              2818.289
AIC(modelbll9) # 2024 values,                         2389.939
AIC(modelbll10) # age groups,                         2819.359
AIC(modelbll11) # interpolated soil Pb,               620.1046

########################## LOG TRANSFORM BLL ################################

bllnoout$Logbll <- log(bllnoout$Rslt_Nm)

### trying the univariate models again with log transformed blls

modelbll16 <- glm(Logbll ~ Sprfnd_, data = bllnoout, family = gaussian)
summary(modelbll16)

modelbll17 <- glm(Logbll ~ Age, data = bllnoout, family = gaussian)
summary(modelbll17)

modelbll18 <- glm(Logbll ~ `2014_LT`, data = bllnoout, family = gaussian)
summary(modelbll18)

modelbll19 <- glm(Logbll ~ Rem_1Y_0N_2Out, data = bllnoout, family = gaussian)
summary(modelbll19)

modelbll20 <- glm(Logbll ~ Bult_bf, data = bllnoout, family = gaussian)
summary(modelbll20)

modelbll21 <- glm(Logbll ~ Yer_blt, data = bllnoout, family = gaussian)
summary(modelbll21)

modelbll22 <- glm(Logbll ~ Btwn_0_, data = bllnoout, family = gaussian)
summary(modelbll22)

modelbll23 <- glm(Logbll ~ `2024_LT`, data = bllnoout, family = gaussian)
summary(modelbll23)

modelbll24 <- glm(Logbll ~ Age_Grp, data = bllnoout, family = gaussian)
summary(modelbll24)

modelbll25 <- glm(Logbll ~ Sqft, data = bllnoout, family = gaussian)
summary(modelbll25)

modelbll26 <- glm(Logbll ~ Bedr, data = bllnoout, family = gaussian)
summary(modelbll26)

####################### RESIDUALS #####################################

qqnorm(residuals(modelbll16))
qqline(residuals(modelbll16), col = "red", lwd = 2)

qqnorm(residuals(modelbll17))
qqline(residuals(modelbll17), col = "red", lwd = 2)

qqnorm(residuals(modelbll18))
qqline(residuals(modelbll18), col = "red", lwd = 2)

qqnorm(residuals(modelbll19))
qqline(residuals(modelbll19), col = "red", lwd = 2)

qqnorm(residuals(modelbll20))
qqline(residuals(modelbll20), col = "red", lwd = 2)

qqnorm(residuals(modelbll21))
qqline(residuals(modelbll21), col = "red", lwd = 2)

qqnorm(residuals(modelbll22))
qqline(residuals(modelbll22), col = "red", lwd = 2)

qqnorm(residuals(modelbll23))
qqline(residuals(modelbll23), col = "red", lwd = 2)

qqnorm(residuals(modelbll24))
qqline(residuals(modelbll24), col = "red", lwd = 2)

qqnorm(residuals(modelbll25))
qqline(residuals(modelbll25), col = "red", lwd = 2)

qqnorm(residuals(modelbll26))
qqline(residuals(modelbll26), col = "red", lwd = 2)


################################ PRELIM DID ######################################

library(pscl)

didmodel1 <- lm(prop_value ~ Superfund*Post, data = did)
summary(didmodel1)
# this is with all four years: 2008, 2011, 2016, 2024

did_multi <- lm(prop_value ~ factor(Year) * Superfund, data = did)
summary(did_multi)
# for all four years, fixed

didmodel2 <- glm(prop_value ~ Year + Superfund*Post, data = didmore, family = gaussian)
summary(didmodel2)
# this is for just the years 2008 and 2024

didmodel3 <- lm(prop_value ~ Year + Superfund*Post, data = didmore)
summary(didmodel3)
summary(didmodel3)$r.squared        # regular R²
summary(didmodel3)$adj.r.squared    # adjusted R²

didmodel4 <- glm(prop_value ~ Year + Superfund + Post + Superfund*Post, data = didmore, family = gaussian)
summary(didmodel4)
# I can't pull a r-squared on a glm model for some reason? The output is NULL

didmodel5 <- lm(prop_value ~ Year + Superfund + Post + Superfund*Post, data = didmore)
summary(didmodel5)

# variables Year and Post are superfluous, we only need Post
didmodel6 <- lm(prop_value ~ Superfund*Post, data = didmore)
summary(didmodel6)

didmodel7 <- lm(prop_value ~ Superfund*Post, data = didless)
summary(didmodel7)
# this is for just the years 2011 and 2016

########################### PLOTTING THE DID MEANS ###################################

library(dplyr)
library(ggplot2)

# for years 2008 and 2024
did_means2 <- didmore %>%
  group_by(Superfund, Post) %>%
  summarise(mean_prop = mean(prop_value, na.rm = TRUE),
            .groups = "drop")
print(did_means2)

ggplot(did_means2, aes(x = Post, y = mean_prop, group = Superfund, color = Superfund)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_discrete(labels = c("2008 (Pre)", "2024 (Post)")) +
  labs(title = "Difference-in-Differences: Property Values",
       x = "Year",
       y = "Mean Property Value") +
  theme_minimal()

# for years 2011 and 2016
did_means <- didless %>%
  group_by(Superfund, Post) %>%
  summarise(mean_prop = mean(prop_value, na.rm = TRUE),
            .groups = "drop")
print(did_means)

ggplot(did_means, aes(x = Post, y = mean_prop, group = Superfund, color = Superfund)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_discrete(labels = c("2011 (Pre)", "2016 (Post)")) +
  labs(title = "Difference-in-Differences: Property Values",
       x = "Year",
       y = "Mean Property Value") +
  theme_minimal()

# for all years 2008, 2011, 2016, 2024
did_means3 <- did %>%
  group_by(Superfund, Post) %>%
  summarise(mean_prop = mean(prop_value, na.rm = TRUE),
            .groups = "drop")
print(did_means3)

ggplot(did_means3, aes(x = Post, y = mean_prop, group = Superfund, color = Superfund)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_discrete(labels = c("2008 (Pre)", "2024 (Post)")) +
  labs(title = "Difference-in-Differences: Property Values",
       x = "Year",
       y = "Mean Property Value") +
  theme_minimal()

######################################################################################
######################################################################################
##################### IT'S HAPPENING - INTERPOLATION & KRIGING #######################
######################################################################################

# Spatial data packages needed
install.packages(c("sf","sp","gstat","automap","tidyverse","units"))
library(sf)
library(sp)
library(gstat)
library(automap)    # optional: automatic variogram fit
library(tidyverse)
library(units)


# creating sf objects
soil_sf  <- st_as_sf(pbinterp, coords = c("x", "y"), crs = 5070)
blood_sf <- st_as_sf(data, coords = c("x_5070", "y_5070"), crs = 5070)

# convert to geo data for gstat
soil_sp <- as(soil_sf, "Spatial")
blood_sp <- as(blood_sf, "Spatial")

# create log Pb column (avoid log(0))
soil_sp$log_pb <- log(soil_sp$pb)

######## for errors only, not for the model
# checking how to fix error
sum(st_is_empty(soil_sf))
sum(st_is_empty(blood_sf))

which(st_is_empty(soil_sf))
which(st_is_empty(blood_sf))

summary(pbinterp$x)
summary(pbinterp$y)

summary(blinterp$x)
summary(blinterp$y)

pbinterp <- pbinterp %>% filter(!is.na(x), !is.na(y))
blinterp <- blinterp %>% filter(!is.na(x), !is.na(y))
########################################

# compute variogram
vgm_emp <- variogram(pb ~ 1, soil_sp)
plot(vgm_emp)

# fit model, spherical method - NOT LOG TRANSFORMED
vgm_model <- fit.variogram(vgm_emp, vgm("Sph"))
vgm_model
#   model    psill    range
#   Nug   57346.57    0.000
#   Sph   11741.30 5811.296

# plot model
plot(vgm_emp, vgm_model)

# fit model, exponential
vgm_model <- fit.variogram(vgm_emp, vgm("Exp"))

# fit model, gaussian
vgm_model <- fit.variogram(vgm_emp, vgm("Gau"))

# LOG TRANSORMED SOIL DATA
vgm_model <- fit.variogram(vgm_emp, vgm("Sph"))
vgm_model
#   model   psill    range
#   Nug   1.06835   0.0000
#   Sph   0.00000 353.0706   # this is not working the way we want

# plot model
plot(vgm_emp, vgm_model)

# fit model, exponential
vgm_model <- fit.variogram(vgm_emp, vgm("Exp"))

# fit model, gaussian
vgm_model <- fit.variogram(vgm_emp, vgm("Gau"))

#######
####### trying to re-fit after log transform

vgm_emp_log <- variogram(log_pb ~ 1, soil_sp)
plot(vgm_emp_log, main="empirical variogram (log scale)")

# set initial guesses from the log variogram cloud
nugget_guess <- min(vgm_emp_log$gamma, na.rm=TRUE)
sill_guess   <- max(vgm_emp_log$gamma, na.rm=TRUE)
range_guess  <- max(vgm_emp_log$dist, na.rm=TRUE) / 3

initial_model_log <- vgm(psill = max(0, sill_guess - nugget_guess),
                         model = "Sph",
                         range = range_guess,
                         nugget = nugget_guess)

vgm_model_log <- try(fit.variogram(vgm_emp_log, initial_model_log), silent=TRUE)
vgm_model_log
plot(vgm_emp_log, vgm_model_log)

######## getting a nugget shape, other analyses are necessary (not kriging)

############################ IDW ##############################################

# trying inverse distance weighting since kriging didn't work

# assumptions:
# points closer together are more alike i.e. spatial autocorrelation
# influence diminishes as distance decreases
# defined search radius and adequate number of points to determine patterns
# location dependency

library(sf)
library(sp)
library(gstat)

# creating sf objects
soil_sf  <- st_as_sf(pbinterp, coords = c("x", "y"), crs = 5070)
blood_sf <- st_as_sf(data, coords = c("x_5070", "y_5070"), crs = 5070)

# convert to geo data for gstat
soil_sp <- as(soil_sf, "Spatial")
blood_sp <- as(blood_sf, "Spatial")

idw_child <- idw(
  formula = pb ~ 1,
  locations = soil_sp,
  newdata   = blood_sp,
  idp = 2)

data$soil_pb_idw <- idw_child$var1.pred

# plotting
ggplot(idw_df1, aes(x = x, y = y, fill = var1.pred)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Soil Pb (mg/kg)") +
  coord_equal() +
  labs(
    title = "IDW Interpolated Soil Lead Concentrations",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  theme_minimal()

write.csv(data, "Desktop/thesis/data/all_data_withIDW.csv")

# saving to create map in Q
install.packages("terra")
library(terra)

# Convert idw_surface1 (SpatialPointsDataFrame) → data.frame
idw_df1 <- as.data.frame(idw_surface1)

# Pull coordinates
coords <- coordinates(idw_surface1)
idw_df1$x <- coords[,1]
idw_df1$y <- coords[,2]

# Create raster from XYZ
r <- rast(idw_df1[, c("x", "y", "var1.pred")],
          type = "xyz",
          crs = "EPSG:5070")
plot(r) # this isn't as nice as the ggplot map

# plotting the interpolation
r_df <- as.data.frame(r, xy = TRUE)

ggplot(r_df, aes(x = x, y = y, fill = var1.pred)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Soil Pb (mg/kg)") +
  coord_equal() +
  labs(
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  theme_minimal()

writeRaster(r,
            filename = "Desktop/thesis/data/soil_pb_idw.tif",
            overwrite = TRUE)

# Convert blinterp to sf points
bl_sf <- st_as_sf(
  blinterp,
  coords = c("x", "y"),
  crs = 5070
)

# extract Pb values
pb_values <- terra::extract(r, vect(bl_sf))

pb_values <- pb_values$var1.pred

blinterp$pb_iwd <- pb_values

summary(blinterp$pb_iwd) # sanity check
plot(blinterp$pb_iwd, blinterp$result_num)


################# fix empty rows that need 5070 projection coords

need_convert <- blinterp %>% 
  filter(is.na(x) | is.na(y))

need_convert_sf <- st_as_sf(
  need_convert,
  coords = c("long", "lat"),
  crs = 4326
)

converted_5070 <- st_transform(need_convert_sf, 5070)

coords <- st_coordinates(converted_5070)

need_convert$x <- coords[, "X"]
need_convert$y <- coords[, "Y"]

# rows that already had coordinates
already_have <- blinterp %>% 
  filter(!is.na(x) & !is.na(y))

# combine back
blinterp_filled <- bind_rows(already_have, need_convert) %>%
  arrange(row_number())

blinterp <- blinterp_filled
rm(blinterp_filled)

#########################################################################################
##################################################################################################################################################################################
                               #### FINAL ANALYSES ##### 
#########################################################################################
################################# DID for blood/Pb ######################################

library(MASS)

# must tell polr that the dependent variable is an ordered factor
data$result_cat <- factor(
  data$result_cat,
  levels = c("low", "moderate", "high"),
  ordered = TRUE)

# modelord is the same as below except no soil/distance to variables
# here's the model:
modelord1 <- polr(result_cat ~ pre_npl * superfund + age + year_built + soil_pb_idw + HubDist, data = data, Hess = TRUE)
summary(modelord1)
ORs <- exp(coef(modelord1))
ORs

# let's put the ORs in a table with their CIs
ctable <- coef(summary(modelord))

ORs <- exp(ctable[, "Value"])
lowerCI <- exp(ctable[, "Value"] - 1.96 * ctable[, "Std. Error"])
upperCI <- exp(ctable[, "Value"] + 1.96 * ctable[, "Std. Error"])

OR_table <- data.frame(
  OR = ORs,
  CI_lower = lowerCI,
  CI_upper = upperCI,
  row.names = rownames(ctable)
)
OR_table

# just want to check these for multicollinearity
cor(data$soil_pb_idw, data$HubDist, use = "complete.obs")
# 0.010 indicates no multicollinearity, yay

# decided to take out interpolated soil values anyway, since I think the 2 variables' purposes are essentially the same. and distance to smelter is more reliable than the IDW in my opinion
# trying the model again, sans soil_pb_idw
modelord2 <- polr(result_cat ~ pre_npl * superfund + age + year_built + HubDist, data = data, Hess = TRUE)
summary(modelord2)
ORs <- exp(coef(modelord2))
ORs

# let's put the ORs in a table with their CIs
ctable2 <- coef(summary(modelord))

ORs2 <- exp(ctable[, "Value"])
lowerCI <- exp(ctable[, "Value"] - 1.96 * ctable[, "Std. Error"])
upperCI <- exp(ctable[, "Value"] + 1.96 * ctable[, "Std. Error"])

OR_table2 <- data.frame(
  OR = ORs,
  CI_lower = lowerCI,
  CI_upper = upperCI,
  row.names = rownames(ctable)
)
OR_table2

####
# plotting predicted probabilities with my final model (modelord2)
####
library(MASS)     # polr
library(dplyr)
library(tidyr)
library(ggplot2)

# new data frame for predictions
newdat <- expand.grid(
  pre_npl    = c(0, 1),
  superfund = c(FALSE, TRUE),
  age        = mean(data$age, na.rm = TRUE),
  year_built = mean(data$year_built, na.rm = TRUE),
  HubDist    = mean(data$HubDist, na.rm = TRUE)
)

# adding some labels
newdat$Group <- with(newdat,
                     paste(
                       ifelse(superfund, "Inside site", "Outside site"),
                       ifelse(pre_npl == 1, "Post", "Pre")
                     )
)

# calculating the predicted probabilities
pred <- predict(modelord2, newdata = newdat, type = "probs")

pred_df <- cbind(newdat, pred) %>%
  pivot_longer(
    cols = c("low", "moderate", "high"),
    names_to = "BLL_category",
    values_to = "Probability"
  )

# fix the ordering
pred_df$BLL_category <- factor(
  pred_df$BLL_category,
  levels = c("low", "moderate", "high"),
  ordered = TRUE
)
pred_df$Group <- factor(
  pred_df$Group,
  levels = c(
    "Inside site Pre",
    "Outside site Pre",
    "Inside site Post",
    "Outside site Post"
  )
)

# the actual plot
ggplot(pred_df, aes(x = Group, y = Probability, fill = BLL_category)) +
  geom_col(position = "stack") +
  labs(
    y = "Predicted probability",
    x = "",
    fill = "Child BLLs"
  ) + scale_fill_manual(
    values = c(
      "low" = "#a6cee3",
      "moderate" = "#FF6666",
      "high" = "#b2182b"
    ),
    labels = c(
      "Low <3.3 µg/dL",
      "Moderate 3.3–5.0 µg/dL",
      "High >5.0 µg/dL"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    text = element_text(family = "Times New Roman")
  )






