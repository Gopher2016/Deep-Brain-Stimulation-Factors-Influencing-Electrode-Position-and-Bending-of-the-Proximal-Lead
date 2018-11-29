
rm(list=ls())

library(ggplot2)
library(nlme)
library(multcomp)

DBS <- read.csv("C:\\Users\\niede\\Desktop\\DBS_PostOp.csv")
Patients <- read.csv("C:\\Users\\niede\\Desktop\\DBS_PostOp_Patients.csv")


### Response Variable 1 - DBS Electrode Tip Migration ###

attach(DBS)

# Plot Distal Lead Migration vs. ICA Volume with ggplot2

ggplot(DBS, aes(x = PostOp_ICA, y = Distal_Lead_Migration)) +
  geom_point() +
  geom_smooth(method = "lm") 

# Potential correlation between ICA volume and DBS Electrode Tip Migration

boxplot(Distal_Lead_Migration ~ Surgery_Location, ylab = "Electrode Tip Migration (mm)")

boxplot(Distal_Lead_Migration ~ DBS_Electrode, ylab = "Electrode Tip Migration (mm)")

boxplot(Distal_Lead_Migration ~ Target, ylab = "Electrode Tip Migration (mm)") # Something may be going on here... 

boxplot(Distal_Lead_Migration ~ Sex, ylab = "Electrode Tip Migration (mm)") 

# Use ggplot2 for interesting boxplots

bp <- ggplot(DBS, aes(Target,Distal_Lead_Migration, color=Target)) + geom_boxplot() + 
  geom_jitter(width = 0.01) + theme_classic() + theme(text = element_text(size = 20))

bp + scale_color_manual(values=c("deeppink4","goldenrod3","dodgerblue1")) + theme(legend.position = "none") + 
  xlab("") + ylab("Distal Lead Migration (mm)")

bp2 <- ggplot(DBS, aes(DBS_Electrode,Distal_Lead_Migration, color=DBS_Electrode)) + geom_boxplot() + 
  geom_jitter(width = 0.01) + theme_classic() + theme(text = element_text(size = 20))

bp2 + scale_color_manual(values=c("goldenrod3","gray62","dodgerblue1")) + theme(legend.position = "none") + 
  xlab("") + ylab("Distal Lead Migration (mm)")

# Are the assumptions met? - Checking the distribution of the response variable

hist(DBS$Distal_Lead_Migration, main = "Distribution of Radiographic Measures of Distal Lead Migration", 
     xlab = "Distal Lead Migration (mm)", ylab = "Frequency", breaks = 5) # Appears normal - good!


# Create a linear mixed-effects model for distal lead migration

DBS.lmm <- lme(Distal_Lead_Migration ~ PostOp_ICA + Surgery_Location + DBS_Electrode + Target + 
                 Proximal_Lead_Bowing + Sex + Age, ~ 1|Subject_ID, data = DBS)
summary(DBS.lmm)

# Drop insignificant factors from the model

DBS.lmm <- lme(Distal_Lead_Migration ~ PostOp_ICA + Target, ~ 1|Subject_ID, data = DBS)
summary(DBS.lmm)
0.0242028  # Slope for ICA volume: 0.024 mm increase in distal lead migration per cm^3 increase in ICA volume
0.09223518 / (0.09223518 + 0.4233863) 

# The differences between subjects explains ~18% of the variance not explained by fixed effects

# Plot model estimated increase in lead bowing as a function of ICA volume

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
fun.1 <- function(x) 0.024*x

p + stat_function(fun = fun.1) + scale_color_manual(values=c("red2")) + xlim(0, 30) + ylim(0, 1) + 
  ggtitle("Model Estimated Increase in Distal Lead Migration vs. ICA Volume") + 
  xlab("ICA Volume (cm^3)") + ylab("Model Estimated Increase in Distal Lead Migration (mm)") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

plot(DBS.lmm) # No obvious patterns - looks alright

qqnorm(resid(DBS.lmm))
qqline(resid(DBS.lmm)) # Not perfect but looks alright


# Post hoc analysis

summary(glht(DBS.lmm, linfct=mcp(Target ="Tukey")))


# 95% Confidence Intervals 

0.4347 # True difference in euclidean lead migration between GPi and STN DBS targets
1.96 * 0.1500 # 95% CI:  0.4 ± 0.3 mm

0.5703 # True difference in euclidean lead migration between GPi and VIM DBS targets
1.96 * 0.1770 # 95% CI:  0.6 ± 0.3 mm




### Response Variable 2 - Proximal Lead Bowing ###

attach(DBS)

# Plot the data with ggplot2

ggplot(DBS, aes(x = PostOp_ICA, y = Proximal_Lead_Bowing)) +
  geom_point() +
  geom_smooth(method = "lm") 

# Apparent correlation between ICA volume and DBS Electrode Tip Migration

boxplot(Proximal_Lead_Bowing ~ Surgery_Location, ylab = "Proximal lead bowing (mm)")  

boxplot(Proximal_Lead_Bowing ~ DBS_Electrode, ylab = "Proximal lead bowing (mm)") # Something clearly going on here...  

boxplot(Proximal_Lead_Bowing ~ Sex,  ylab = "Proximal lead bowing (mm)") 

# Use ggplot2 for interesting boxplot

bp3 <- ggplot(DBS, aes(DBS_Electrode,Proximal_Lead_Bowing, color=DBS_Electrode)) + geom_boxplot() + 
  geom_jitter(width = 0.01) + theme_classic() + theme(text = element_text(size = 17))

bp3 + scale_color_manual(values=c("goldenrod3","gray62","dodgerblue1")) + theme(legend.position = "none") + 
  xlab("") + ylab("Proximal Lead Bowing (mm)")

# Are the assumptions met? - Checking the distribution of the response variable

hist(DBS$Proximal_Lead_Bowing, main = "Distribution of Radiographic Measures of Proximal Lead Bowing", 
     xlab = "Proximal Lead Bowing (mm)", ylab = "Frequency", breaks = 5) # Not normal!
hist(log(DBS$Proximal_Lead_Bowing), main = "Distribution of Natural Log Scaled Proximal Lead Bowing", 
     xlab = "Natural Log Scaled Proximal Lead Bowing", ylab = "Frequency", breaks = 5) # Appears normal - good!
DBS$Log_Bowing <- log(DBS$Proximal_Lead_Bowing) # Create new variable Log_Bowing


# Create a linear mixed-effects model for proximal lead bowing

DBS.lmm2 <- lme(Log_Bowing ~ PostOp_ICA + DBS_Electrode + Surgery_Location + Target + Sex + Age, 
                ~ 1|Subject_ID, data = DBS)
summary(DBS.lmm2)

# Drop insignificant factors from model
# Leave DBS electrode in model - potential influence on proximal lead bowing 
# Differences between lead models observed in boxplot

DBS.lmm2 <- lme(Log_Bowing ~ PostOp_ICA + DBS_Electrode, ~ 1|Subject_ID, data = DBS)
summary(DBS.lmm2) 
0.0448314  # Slope for ICA volume: 0.045 ln(x)/cm^3  (x is initial measure of proximal lead bowing in millimeters)
# Exponentiate slope find relationship between lead bowing and ICA volume
# Proximal Lead Bowing (mm) modeled as Y = e^0.045X (X = ICA volume in cm^3)
0.2451488 / (0.2451488 + 0.3201502) 

# The differences between subjects explains ~43% of the variance not explained by fixed effects

# Plot model estimated increase in lead bowing as a function of ICA volume

p2 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
fun.2 <- function(x) exp(0.045*x)

p2 + stat_function(fun = fun.2) + scale_color_manual(values=c("red2")) + xlim(0, 30) + ylim(0, 5) + 
  ggtitle("Model Estimated Increase in Proximal Lead Bowing vs. ICA Volume") + 
  xlab("ICA Volume (cm^3)") + ylab("Model Estimated Increase in Proximal Lead Bowing (mm)") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))


plot(DBS.lmm2) # No obvious patterns - looks alright

qqnorm(resid(DBS.lmm2))
qqline(resid(DBS.lmm2)) # Not perfect but looks alright


# Post hoc analysis

summary(glht(DBS.lmm2, linfct=mcp(DBS_Electrode ="Tukey")))


# 95% Confidence Intervals 

e <- exp(1)

0.3557 # True difference in proximal lead bowing between Medtronic 3389 and Abbott/SJM Infinity DBS leads (natural log scale)
1.96 * 0.1483 # 95% CI:  0.3557 ± 0.290668 (lne)
0.3557 + 0.290668 # Upper Endpoint (lne)
0.3557 - 0.290668 # Lower Endpoint (lne)
e^0.3557 # True difference in proximal lead bowing between Medtronic 3389 and Abbott/SJM Infinity DBS leads (mm)
e^0.646368 # Upper Endpoint (mm)
e^0.065032 # Lower Endpoint (mm)

# True difference in proximal lead bowing between Medtronic 3389 and Abbott/SJM Infinity DBS leads is 1.4 mm 
# The 95% confidence interval has an upper bound of 1.9 mm and a lower bound of 1.1 mm


0.4782 # True difference in proximal lead bowing between Medtronic 3389 and Boston Scientific Vercise DBS leads (natural log scale)
1.96 * 0.1597 # 95% CI:  0.4782 ± 0.313012 (lne)
0.4782 + 0.313012 # Upper Endpoint (lne)
0.4782 - 0.313012 # Lower Endpoint (lne)
e^0.4782 # True difference in proximal lead bowing between Medtronic 3389 and Abbott/SJM Infinity DBS leads (mm)
e^0.791212 # Upper Endpoint (mm)
e^0.165188 # Lower Endpoint (mm)

# True difference in proximal lead bowing between Medtronic 3389 and Abbott/SJM Infinity DBS leads is 1.6 mm 
# The 95% confidence interval has an upper bound of 2.2 mm and a lower bound of 1.2 mm


citation("ggplot2")
citation("nlme")
citation("multcomp")
