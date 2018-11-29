
rm(list=ls())

DBS <- read.csv("C:\\Users\\niede\\Desktop\\DBS_PostOp.csv")
Patients <- read.csv("C:\\Users\\niede\\Desktop\\DBS_PostOp_Patients.csv")


### Patient demographics and descriptive statistics

# Gender demographics

DBS.Female <- subset(Patients, Sex == "Female")
nrow(DBS.Female)  # 14 Female DBS Patients
DBS.Male <- subset(Patients, Sex == "Male")
nrow(DBS.Male)  # 19 Male DBS Patients

# Age demographics

mean.Age <- mean(Patients$Age)
sd.Age <- sd(Patients$Age)
n.Patients <- nrow(Patients)
error.Age <- qnorm(0.975) * sd.Age/sqrt(n.Patients)
mean.Age
error.Age  # Mean ± 95% CI for age of DBS patients:  64.4 ± 2.9 years

# DBS patients by Disease

DBS.Dystonia <- subset(Patients, Disease == "Dystonia")
nrow(DBS.Dystonia)  # 2 dystonia patients
DBS.ET <- subset(Patients, Disease == "Essential Tremor")
nrow(DBS.ET)  # 8 essential tremor patients
DBS.Parkinson <- subset(Patients, Disease == "Parkinson's Disease")
nrow(DBS.Parkinson)  # 23 Parkinson's Disease patients

# DBS patients by surgical method

DBS.Unilateral <- subset(Patients, Method == "Unilateral")
nrow(DBS.Unilateral)  # 16 unilateral DBS surgeries
DBS.Staged <- subset(Patients, Method == "Staged")
nrow(DBS.Staged)  # 10 staged bilateral DBS surgeries
DBS.Bilateral <- subset(Patients, Method == "Bilateral")
nrow(DBS.Bilateral)  # 7 bilateral DBS surgeries

# DBS leads by manufacturer

DBS.Abbott <- subset(DBS, DBS_Electrode == "Abbott/St. Jude Medical Infinity")
nrow(DBS.Abbott)  # 17 Abbott/St. Jude Medical Infinity DBS leads
DBS.Boston <- subset(DBS, DBS_Electrode == "Boston Scientific Vercise")
nrow(DBS.Boston)  #  15 Boston Scientific Vercise DBS leads
DBS.Medtronic <- subset(DBS, DBS_Electrode == "Medtronic 3389")
nrow(DBS.Medtronic)  # 18 Medtronic 3389 DBS leads

# DBS leads by target region

DBS.GPi <- subset(DBS, Target == "GPi")
nrow(DBS.GPi)  # 14 DBS leads targeted globus pallidus interna (GPi)
DBS.STN <- subset(DBS, Target == "STN")
nrow(DBS.STN)  # 25 DBS leads targeted subthalamic nucleus (STN)
DBS.VIM <- subset(DBS, Target == "VIM")
nrow(DBS.VIM)  # 11 DBS leads targeted the ventral intermediate (VIM) nucleus of the thalamus



# Create a user defined function for calculating 95% CI

CI.95 <- function(ICA_Vector){
  m <- mean(ICA_Vector)
  s <- sd(ICA_Vector)
  n <- length(ICA_Vector)
  e <- qnorm(0.975) * s/sqrt(n)
  CI <- list("mean" = m, "error" = e)
  return(CI)
}



### ICA Volume

# ICA volume all cases

CI.95(DBS$PostOp_ICA)  # Mean ± 95% CI ICA volume for all DBS cases:  8.8 ± 1.5 cm^3

boxplot.stats(DBS$PostOp_ICA)  # IQR ranges from 4.5 cm^3 (Q1) to 13.1 cm^3 (Q3)



# Visualize the data by surgery location

boxplot(PostOp_ICA ~ Surgery_Location, main = "Unilateral Measurements of Postoperative Air Volume (cm^3)", 
        ylab = "Intracranial Air Volume (cm^3)")



# No obvious differences in ICA volume exist between surgery locations

# Subset DBS leads by surgery location

DBS.Minnesota <- subset(DBS, Surgery_Location == "University of Minnesota")
nrow(DBS.Minnesota)  # 40 DBS leads implanted at the University of Minnesota
CI.95(DBS.Minnesota$PostOp_ICA)  # Mean ± 95% CI ICA volume for University of Minnesota DBS cases:  8.6 ± 1.6 cm^3

DBS.Utah <- subset(DBS, Surgery_Location == "University of Utah")
nrow(DBS.Utah)  # 10 DBS leads implanted at the University of Utah
CI.95(DBS.Utah$PostOp_ICA) # Mean ± 95% CI ICA volume for University of Utah DBS cases:  9.6 ± 4.3 cm^3

hist(DBS.Minnesota$PostOp_ICA)
hist(DBS.Utah$PostOp_ICA)
wilcox.test(DBS$PostOp_ICA ~ DBS$Surgery_Location, mu = 0, alt = "two.sided", conf.int = T)

# There is no evidence of a significant difference in subdural air volumes between cases performed at the 
# University of Minnesota and cases performed at the University of Utah (p = 0.88)


# Distal lead migration all cases

CI.95(DBS$Distal_Lead_Migration)  # Mean ± 95% CI ICA volume for all DBS cases:  1.1 ± 0.1 mm

# Distal lead migration by DBS target 

CI.95(DBS.GPi$Distal_Lead_Migration)  # Mean ± 95% CI distal lead migration for GPi DBS cases:  1.4 ± 0.2 mm
CI.95(DBS.STN$Distal_Lead_Migration)  # Mean ± 95% CI distal lead migration for STN DBS cases:  1.0 ± 0.2 mm
CI.95(DBS.VIM$Distal_Lead_Migration)  # Mean ± 95% CI distal lead migration for VIM DBS cases:  0.8 ± 0.2 mm



# Proximal lead bowing all cases

CI.95(DBS$Proximal_Lead_Bowing)  # Mean ± 95% CI proximal lead bowing for all DBS cases:  2.4 ± 0.3 mm

# Proximal lead bowing by DBS lead model

CI.95(DBS.Abbott$Proximal_Lead_Bowing)  # Mean ± 95% CI proximal lead bowing for all DBS cases:  2.0 ± 0.3 mm
CI.95(DBS.Boston$Proximal_Lead_Bowing)  # Mean ± 95% CI proximal lead bowing for all DBS cases:  2.1 ± 0.7 mm
CI.95(DBS.Medtronic$Proximal_Lead_Bowing)  # Mean ± 95% CI proximal lead bowing for all DBS cases:  3.0 ± 0.6 mm

