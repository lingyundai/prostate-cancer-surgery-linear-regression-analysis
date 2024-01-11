#+ setup, include=FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

#+ test-a, cache=FALSE
#'   
#' ---
#' title: "Homework #3"
#' author: Lingyun Dai
#' date: Oct 25th 2023
#' ---

# needed libraries 
library(tidyverse)
library(GGally)
library(ISLR)
library(FAwR)
library(car)
library(moments)
library(gridExtra)
library(MASS)

#' ##### The homework concerns data collected about Prostate cancer surgery. The prostate data frame has 97 rows and 9 columns. A study on 97 men with prostate cancer who were due to receive a radical prostatectomy.

#' ##### Data Dictionary - This data frame contains the following columns:

#' cavol - cancer volume

#' weight - prostate weight in grams

#' age - age of patient

#' bph - benign prostatic hyperplasia amount

#' svi - seminal vesicle invasion

#' cp - capsular penetration

#' gleason - Gleason score

#' pgg45 - percentage Gleason scores 4 or 5

#' psa - prostate specific antigen

#' Source - Andrews DF and Herzberg AM (1985): Data. New York: Springer-Verlag

#' Read the dataset into RStudio using the read.csv() function
prostateData <- read.csv('prostate.csv', header=TRUE)

 #' #head(prostate); # look at the data
head(prostateData)

### Exploratory Data Analysis (20 points)
########################################################################
### 1) Make a numerical and graphical summary of the data.  
####### i. Convert the variable "svi" to a factor.
####### ii. Use the summary() function to produce numerical summary of the variables in the data set. 
####### iii. Using the ggpairs() function from the GGaly to produce a scatter plot matrix of the variables. 
# Make sure you address the issue of overplotting of the axes labels. 
####### iv. Using ggplot, produce side-by-side boxplots of "psa" vs. "svi". 
####### Using ggplot, produce histograms of the numeric variables with suitably chosen numbers of bins. 
####### Provide a brief comment of what you observe about the data from the numeric and graphical summaries. 
prostateData$svi <- factor(prostateData$svi)
prostateData
summary(prostateData)
ggpairs(prostateData,
        upper = list(continuous = wrap("points", size = 0.2)),
        lower = list(continuous = wrap("points", size = 0.2)),
        title = "Prostate Dataset Scatterplots") +
  theme(axis.text=element_text(size=5),
        plot.title=element_text(hjust=0.5))

ggplot(prostateData, aes(x=psa, y=svi)) +
  geom_boxplot() +
  labs(x="PSA",
      y="SVI",
      title="PSA vs. SVI Side-by-side Boxplots") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_x_continuous(breaks=seq(0, 300, by=30),
                     limits=c(0, 300))

# age
ggplot(prostateData, aes(age)) +
  geom_histogram(binwidth=1, fill="tan", color="black") +
  labs(title="Prostate Dataset Age Histogram",
       x="Age",
       y="Count") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0, 12, by=1),
                     expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(40, 80, by=2))

# gleason - Gleason score
ggplot(prostateData, aes(gleason)) +
  geom_histogram(fill="tan",color="black",binwidth=1) +
  labs(title="Prostate Dataset Gleason Histogram",
       x="Gleason Score",
       y="Count") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0, 60, by=5),
                     expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(5, 10, by=1))

# pgg45 - percentage Gleason scores 4 or 5
ggplot(prostateData, aes(pgg45)) +
  geom_histogram(fill="tan",color="black",binwidth = 7) +
  labs(title="Prostate Dataset PGG45 Histogram",
       x="Percentage Gleason Scores 4 or 5",
       y="Count") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0, 40, by=5), expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 100, by=5))

# cavol - cancer volume
ggplot(prostateData, aes(cavol)) +
  geom_histogram(fill="tan",color="black",binwidth=2) +
  labs(title="Prostate Dataset CAVOL Histogram",
       x="Cancer Volume",
       y="Count") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 50, by=2))

# weight - prostate weight in grams
ggplot(prostateData, aes(weight)) +
  geom_histogram(fill="tan",color="black",binwidth=6) +
  labs(title="Prostate Dataset Weight Histogram",
       x="Prostate Weight in Grams",
       y="Count") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 460, by=30))

# bph - benign prostatic hyperplasia amount
ggplot(prostateData, aes(bph)) +
  geom_histogram(binwidth=0.5, fill="tan",color="black") +
  labs(title="Prostate Dataset BPH Histogram",
       x="Benign Prostatic Hyperplasia Amount",
       y="Count") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 12, by=1))
  
# cp - capsular penetration
ggplot(prostateData, aes(cp)) +
  geom_histogram(binwidth=1, fill="tan",color="black") +
  labs(title="Prostate Dataset CP Histogram",
       x="Capsular Penetration",
       y="Count") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_x_continuous(breaks=seq(0, 20, by=1)) +
  scale_y_continuous(expand=c(0, 0))
  
# psa - prostate specific antigen
ggplot(prostateData, aes(psa)) +
  geom_histogram(binwidth=6, fill="tan",color="black") +
  labs(title="Prostate Dataset PSA Histogram",
       x="Prostate Specific Antigen",
       y="Count") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_x_continuous(breaks=seq(0, 300, by=30)) +
  scale_y_continuous(expand=c(0, 0))


#' The median psa of svi type 1 is approximately 36, the median psa of svi type 0 is approximately 9. 
#' The psa of svi type 1 is right skewed, the minimum is approximately 9, the maximum is approximately 109. 
#' The psa of svi type 1 has outliers of approximately 171, 239 and 266. There is total of 3 outliers.
#' The 25th percentile is approximately 21, the 75th percentile is approximately 62. The interquartile range
#' is approximately 41. 
#' The psa of svi type 0 is right skewed, the minimum is approximately 0, the maximium is approximately 35.
#' The psa of svi type 0 has outliers of approximately, 34, 35, 40, and 55. There is total of 5 outliers.
#' The 25th percentile is approximately 5, the 75th percentile is approximately 16. The interquartile range
#' is approximately 11. 
#' The distribution of age is left skewed. Most people in this prostate dataset has prostate cancer 
#' surgery at age of 68. Very few people are under 57 or over 72.
#' The distribution of Gleason score is right skewed. The most common Gleason score among 
#' prostate cancer surgery patients in this dataset is 7, the total count is approximately 56. 
#' The lease common gleason score is 8 people, the total count is approximately 2 people.
#' The distribution of pgg45 is right skewed. The most common pgg45 among 
#' prostate cancer surgery patients in this dataset is 0, the total count is 
#' approximately 35. Most patients' pgg45 falls between 0 and 40.
#' Very few people with pgg45 between 90 and 100, approximately 6 people total.
#' The distribution of cancer volume is right skewed. The most common cancer volume among the 
#' patients in this dataset is approximately 2, the total count is approximately 23. 
#' Most patients' cancer volume is between appro. 0 and 8.
#' The distribution of prostate weight is right skewed. The most common patients' prostate weight 
#' is approximately 40. Most patients' prostate weight in this dataset is between approximately 20 to 60. 
#' The outliers are approximately 120 and 450 for a total count of 3 people.
#' The distribution of bph is right skewed. Most common patients' bph is approximately 0.5 for approximately 47 people.
#' The outlier is bph of 10 for approximately 4 people.
#' The distribution of cp is right skewed. Most common patients' cp is approximately 0 for approximately 50 people. 
#' The outlier is cp of approximately 18 for approximately 1 patient.
#' The distribution of psa is right skewed. Most common patients' psa is approximately 5. Most patients' psa
#' is between approximately between 5 and 20. The outlier is approximately 110, 175, 240 and 265. 


### Regression Model (30 points)
#################################################################################
# 2) Fit a multiple linear regression model with "psa" as the response variable 
# and all other variables as the predictors. 
#    Store this linear regression model in an R object called 'lm.all'.
#################################################################################

# Write your code in this section below this line
lm.all <- lm(psa ~ ., data = prostateData)

#################################################################################
# 3) Produce a model fit summary AND diagnostic plots associated with lm.all
#################################################################################
# summary
summary(lm.all)
# disgnostic plot
par(mfrow=c(2,2))
plot(lm.all)

# Write your code in this section below this line

# copy the summary output and plots into a separate document
# professor said this part is for people that do not wish to use compile report.
#################################################################################
# 4) Using ggplot and any other useful functions, display any plots that are 
# relevant for diagnostics on the model. Check the following assumptions:

#   i. Check the structure of the relationship between the predictors 
#     and the response i.e. check the linearity assumption
#   ii. The constant variance assumption for the errors.
#   iii. Check the normality assumption.
#   iv. Check for large outliers.
#   v. Check for influential points.

# check linear assumption by plotting residuals vs.fitted values
source(file = "rss_regress_funcs.R")
residFit(lm.all, title2 = "lm.all Using residFit()")
# The points are not all over the place, the model deviates form linear
# relationship

# check for independent error assumption
dwt(lm.all)
# Durbin-watson test p-value is not greater than 0.05, independent
# error assumption is violated.
acf(lm.all$residuals)
# Acf shows not drizzled, errors are not independent with each other

# check for equal-variance assumption
scaleLocation(lm.all, title2="lm.all Using scaleLocation()")
# The points are not scattered, the model is not
# equal-variance

# check for normality
skewness(lm.all$residuals)
# Residuals are right skewed because skewness generates positive number
# shows non-normality
kurtosis(lm.all$residuals)
# Computed kurtosis is greater than 3 means large proportions in
# the data have extreme values, normality assumption is violated

grid.arrange(
  responsePredictor(lm.all, "PSA vs. Other Variables",TRUE),
  normalQQ(lm.all, title2="lm.all Using normalQQ()"),
  nrow=2)
# From response vs predictor plot we see large residual outliers,
# in the qq plot the points deviates from the straight line,
# the above proves non-normality of the errors

# check for large outliers, 
# check for influential points
source(file="rss_regress_funcs.R")
residLeverage(lm.all, title2="lm.all Using residLeverage()")
# Point 97 is above 2*p/n with high cook's distance of 1.25 is influential on the model
# and can be further investigated. Point 32 is above leverage 3*p/n with cook's distance of 1 and high
# leverage shows that point 32 has high influence on the model and need to be further investigated.
# Point 96 is below 2*p/n with high residual and cook's distance 0f 0.50. Thia point is 
# not as influential on the model, but the error is big, it is an outlier need to be investigated further.

source(file="rss_regress_funcs.R")
cooksDistance(lm.all, title2="lm.all Using cooksDistance()")
# Point 97 has the largest cook's distance shows this point is highly influential on the model.
# Point 32 also has large cook's distance shows this point is highly influential on the model. 
# Point 96 also has relatively large cook's distance. 
# In summary, point 97, point 32 and point 96 are high influential and can be outliers. They need
# to be further investigated.

#################################################################################

# Write your responses in the separate document containing the model summary and plots
# Response is in above comments.
##############################################################################################
# 5) Do you see any serious problems in any of the diagnostic plots?
#    If so, how will you modify your data to address the problem?
#    Important: Focus on one serious problem, rather than addressing every minor issue in the diagnostic plots. 
#    Implement the modification(s) you suggested.
##############################################################################################
#' The serious problem I see in the diagnostic plots is the highly influential points. 
#' I want to apply box-cox transformation that pull in extreme values.
# apply box cox transformation
bc <- boxcox(lm.all)
# find the lambda we can use
lambda <- bc$x[which.max(bc$y)]
# produce transformed model
new_lm <- lm(((psa^lambda-1)/lambda) ~ ., data = prostateData)
summary(new_lm)
# qqplots
grid.arrange(
  normalQQ(lm.all, title2="Original"),
  normalQQ(new_lm, title2="Transformed"),
  nrow=2)
# after box cox transformation, the model is more normally distributed.

# Write your responses in the separate document containing the model summary and plots
# Response is in above comments.
#############################################################################################
# 6) Perform backward model selection until all variables left in your model are significant.
#    You can do this using an automated function in R or manually by repeatedly fitting models and dropping variables one at a time.
#    Important: Be sure to include any modifications to the data from the previous question when fitting the model.
#############################################################################################

# Write your code below this line to produce a summary of the model with all variables
summary(lm.all)

# Write your code below this line all code created to perform backward selection
# automated backward selection
step(lm.all, direction = "backward")
# write your code below this line to save the final model in an R object called 'lm.bw'
lm.bw <- step(lm.all, direction = "backward")

#################################################################################
# 7) Produce a model fit summary AND diagnostic plots associated with lm.bw
#################################################################################

# write your code in this section below this line
# summary
summary(lm.bw)
# disgnostic plot
par(mfrow=c(2,2))
plot(lm.bw)

source(file = "rss_regress_funcs.R")
residFit(lm.bw, title2 = "lm.bw Using residFit()")
dwt(lm.bw)
acf(lm.bw$residuals)
scaleLocation(lm.bw, title2="lm.bw Using scaleLocation()")
skewness(lm.bw$residuals)
kurtosis(lm.bw$residuals)
source(file="rss_regress_funcs.R")
residLeverage(lm.bw, title2="lm.bw Using residLeverage()")
source(file="rss_regress_funcs.R")
cooksDistance(lm.bw, title2="lm.bw Using cooksDistance()")

# copy the summary output and plots into a separate document

#################################################################################
# 8) Compare the diagnostic plots for lm.all and lm.bw.  
# Which model and data appear to satisfy the linear regression model assumptions? 
# Describe the characteristics of the diagnostic plots for lm.all and lm.bw and 
# how they support your claims.

#' Both models do not satisfy the linear regression model assumptions. 
#' The diagnostic plot for lm.bw violated the linear assumptions. The
#' diagnostic plot for lm.bw looks very similar to the plot for lm.all.
# The residuals vs fitted plot's points are not all over the place, the model deviates form linear
# relationship. Durbin-watson test p-value is not greater than 0.05, independent
# error assumption is violated. Acf shows not drizzled, errors are not independent with each other.
# In the scale location plot, points are not scattered, the model is not
# equal-variance. Residuals are right skewed because skewness generates positive number
# shows non-normality. Computed kurtosis is greater than 3 means large proportions in
# the data have extreme values, normality assumption is violated. 
# Comparing to the residuals vs. leverage plot for lm.all, lm.bw residuals vs. leverage
# plot has point 97 that is above 3*p/n with 1.6 cook's distance. The point 97 has
# higher leverage and cook's distance comparing to point 97 in lm.all. Point 32 is no
# longer influential. In cook's distance plot, comparing to lm.all, the influence of
# point 95, and 96 both decreased, the influence of point 32 is no longer visible in
# cook's distance plot for lm.bw. Point 97 is even more influential in lm.bw.


########################################################################

# Write your responses in the separate document containing the model summary and plots
# Response can be found above.