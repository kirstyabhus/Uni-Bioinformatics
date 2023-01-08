cell_measurements <- read.csv("./RNAseq/cell_measurements.csv")
View(cell_measurements)

### CREATING THE PAIRED DATAFRAMES
library(dplyr)

# TREATED
# Filter for Treated animals with Injury
treated_Injury <- cell_measurements %>%
  filter(Treatment == 1 & Injury == 1) %>%
  select(Animal, Velocity_Angle_X_mean)

# Filter for Treated animals without Injury
treated_NoInjury <- cell_measurements %>%
  filter(Treatment == 1 & Injury == 0) %>%
  select(Velocity_Angle_X_mean)

# combine the data sets to create the paired data (with Injury and No-Injury values for velocity_angle_x_mean side-by-side
treated_velocityX <- cbind(treated_Injury, treated_NoInjury)

# change the column names to be more descriptive
colnames(treated_velocityX)[2] <- "Velocity_Angle_X_mean_INJURY"
colnames(treated_velocityX)[3] <- "Velocity_Angle_X_mean_NO_INJURY"

View(treated_velocityX)

# Do the same for CONTROLS
# Filter for control animals with Injury
control_Injury <- cell_measurements %>%
  filter(Treatment == 0 & Injury == 1) %>%
  select(Animal, Velocity_Angle_X_mean)

# Filter for control animals without Injury
control_NoInjury <- cell_measurements %>%
  filter(Treatment == 0 & Injury == 0) %>%
  select(Velocity_Angle_X_mean)

# combine the data sets to create the paired data
control_velocityX <- cbind(control_Injury, control_NoInjury)
colnames(control_velocityX)[2] <- "Velocity_Angle_X_mean_INJURY"
colnames(control_velocityX)[3] <- "Velocity_Angle_X_mean_NO_INJURY"

View(control_velocityX)


### STATISTICAL TESTING
library(reshape2)

## TREATED
# create the treated box plot to see the distribution of velocity values (injured and non-injured) in treated animals
treated_m = melt(treated_velocityX, id.vars="Animal")
ggplot(treated_m, aes(variable, value))+geom_boxplot()
# From the boxplot for treated animals, we can see that the mean Velocity Angle X 
# for non-injured animals has a wider distribution, but only by a small amount.

# Shapiro-Wilk test treated (testing for normality, to check if the assumption is met for parametric test)
stat.desc(treated_velocityX$Velocity_Angle_X_mean_INJURY, basic=F, desc=F, norm=T)
stat.desc(treated_velocityX$Velocity_Angle_X_mean_NO_INJURY, basic=F, desc=F, norm=T)
# no significant departure from normality was found for injured or non-injured

# As there are two measurement groups (Injury & No Injury), and parametric assumptions
# have not been satisfied (the samples are not independent as this is time series data), the Wilcoxon signed rank test test has been chosen

wilcox.test(treated_velocityX[,"Velocity_Angle_X_mean_INJURY"], treated_velocityX[,"Velocity_Angle_X_mean_NO_INJURY"], paired = TRUE)
# p-value = 0.8769
# The p-value of the test is 0.8769, which is more than the significance level 
# alpha = 0.05. We can conclude that the mean velocity angle x of treated animals
# with injury is not significantly different from the mean velocity angle x of 
# treated animals with no injury with a p-value of 0.8769.

## CONTROL
# control box plot
control_m = melt(control_velocityX, id.vars="Animal")
ggplot(control_m, aes(variable, value))+geom_boxplot()
# From the boxplot for control animals, we can see that the  distribution of "mean Velocity Angle X" 
# for non-injured and Injured animals is very similar.

# Shapiro-Wilk test control
stat.desc(control_velocityX$Velocity_Angle_X_mean_INJURY, basic=F, desc=F, norm=T)
stat.desc(control_velocityX$Velocity_Angle_X_mean_NO_INJURY, basic=F, desc=F, norm=T)
# no significant departure from normality was found for injured or non-injured

# As there are two measurement groups (Injury & No Injury), and parametric assumptions
# have not been satisfied (the samples are not independent as this is time series data), the Wilcoxon signed rank test test has been chosen

wilcox.test(control_velocityX[,"Velocity_Angle_X_mean_INJURY"], control_velocityX[,"Velocity_Angle_X_mean_NO_INJURY"], paired = TRUE)
# p-value = 0.3573
# The p-value of the test is 0.3573, which is more than the significance level 
# alpha = 0.05. We can conclude that the mean velocity angle x of control animals
# with injury is not significantly different from the mean velocity angle x of 
# control animals with no injury with a p-value of 0.3573.
