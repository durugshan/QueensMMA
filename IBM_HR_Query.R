####load libraries####
library(readxl)
library(car)

####data load####
setwd("C:/Users/Durugshan/Google Drive/Education/University - MMA/Courses/MMA 860 - Acquisition and Management of Data/Assignments/Term Project")
ibm_hr <- read.csv("IBM_HR_Attrition.csv")

####data exploration####

#fix names of columns
names(ibm_hr)[1] <- 'Age'

#check if all rows are complete/have no NAs
complete.cases(ibm_hr)

#structure and summary
str(ibm_hr)
summary(ibm_hr)
boxplot(ibm_hr$MonthlyIncome)

#convert binary from text to int
ibm_hr$Over18 <- ifelse(ibm_hr$Over18 == "Y", 1, 0)
ibm_hr$Attrition <- as.character(ibm_hr$Attrition)
ibm_hr$Attrition <- ifelse(ibm_hr$Attrition == "Yes", 1, 0)
ibm_hr$OverTime <- ifelse(ibm_hr$OverTime == "Yes", 1, 0)

####feature engineering####

#add new variables that can be of interest
ibm_hr$AvgYearsPerCompany <- ifelse(ibm_hr$TotalWorkingYears / ibm_hr$NumCompaniesWorked == Inf, 1, ibm_hr$TotalWorkingYears / ibm_hr$NumCompaniesWorked)
ibm_hr$PropWorkYearsIBM <- ibm_hr$YearsAtCompany / ibm_hr$TotalWorkingYears
ibm_hr$NumYearsSinceChange <- pmin(ibm_hr$YearsInCurrentRole, ibm_hr$YearsSinceLastPromotion, ibm_hr$YearsWithCurrManager)

write.csv(ibm_hr, file = "IBM_HR_Attrition_mod.csv")

####hypothesis testing####

#gender
ibm_hr$Male <- ifelse(ibm_hr$Gender == "Male", 1, 0)
ibm_hr$MaleLevel <- ifelse(ibm_hr$Gender == "Male", ibm_hr$JobLevel, 0)
reg_gen_sal <- lm(MonthlyIncome ~ JobLevel + Male + MaleLevel, ibm_hr)
mean(ibm_hr$MonthlyIncome[ibm_hr$Gender == "Male"])
mean(ibm_hr$MonthlyIncome[ibm_hr$Gender == "Female"])
summary(reg_gen_sal)

chow_0 <- lm(MonthlyIncome ~ JobLevel + Male + MaleLevel, ibm_hr)
summary(chow_0)
lh_chow0 <- linearHypothesis(chow_0, c("Male = 0", "MaleLevel = 0"))
lh_chow0

#new employees make more/less than old
#0-3 years as new and all else are old
ibm_hr$NewEmployee <- ifelse(ibm_hr$YearsAtCompany <= 3, 1, 0)
ibm_hr$NewLevel <- ifelse(ibm_hr$YearsAtCompany <= 3, ibm_hr$JobLevel, 0)

mean(ibm_hr$MonthlyIncome[ibm_hr$NewEmployee == 0])
mean(ibm_hr$MonthlyIncome[ibm_hr$NewEmployee == 1])
chow_1 <- lm(MonthlyIncome ~ JobLevel + NewEmployee + NewLevel, ibm_hr)
summary(chow_1)
lh_chow1 <- linearHypothesis(chow_1, c("NewEmployee = 0", "NewLevel = 0"))
lh_chow1

#Distance vs PercentSalaryHike
ibm_hr$CloseToWork <- ifelse(ibm_hr$DistanceFromHome <= 10, 1, 0)
ibm_hr$CloseDistance <- ifelse(ibm_hr$DistanceFromHome <= 10, ibm_hr$DistanceFromHome, 0)
ibm_hr$CloseYearsAtCompany <- ifelse(ibm_hr$DistanceFromHome <= 10, ibm_hr$YearsAtCompany, 0)

chow_2 <- lm(PercentSalaryHike ~ DistanceFromHome + YearsAtCompany + CloseToWork + CloseDistance + CloseYearsAtCompany, ibm_hr)
summary(chow_2)
lh_chow2 <- linearHypothesis(chow_2, c("CloseToWork = 0", "CloseDistance = 0", "CloseYearsAtCompany = 0"))
lh_chow2

#Distance vs JobSatisfaction
chow_3 <- lm(JobSatisfaction ~ DistanceFromHome + CloseToWork + CloseDistance, ibm_hr)
summary(chow_3)
lh_chow3 <- linearHypothesis(chow_3, c("CloseToWork = 0", "CloseDistance = 0"))
lh_chow3

#High Performer vs Salary
ibm_hr$HighPerformer <- ifelse(ibm_hr$PerformanceRating == 4, 1, 0)
ibm_hr$HighPerformerLevel <- ifelse(ibm_hr$PerformanceRating == 4, ibm_hr$JobLevel, 0)

chow_4 <- lm(MonthlyIncome ~ JobLevel + HighPerformer +  HighPerformerLevel, ibm_hr)
summary(chow_4)
lh_chow4 <- linearHypothesis(chow_4, c("HighPerformer = 0", "HighPerformerLevel = 0"))
lh_chow4


#High Performer vs Salary vs Level
chow_5 <- lm(MonthlyIncome ~ JobLevel + YearsAtCompany + HighPerformer +  HighPerformerLevel + NewLevel, ibm_hr)
summary(chow_5)
lh_chow5 <- linearHypothesis(chow_5, c("HighPerformer = 0", "HighPerformerLevel = 0"))
lh_chow5


####regression####
#monthly income

#Role dummy variables
ibm_hr$RoleSalesExecutive <- ifelse(ibm_hr$JobRole == 'Sales Executive', 1, 0)
ibm_hr$ResearchScientist <- ifelse(ibm_hr$JobRole == 'Research Scientist', 1, 0)
ibm_hr$LaboratoryTechnician <- ifelse(ibm_hr$JobRole == 'Laboratory Technician', 1, 0)
ibm_hr$ManufacturingDirector <- ifelse(ibm_hr$JobRole == 'Manufacturing Director', 1, 0)
ibm_hr$HealthcareRepresentative <- ifelse(ibm_hr$JobRole == 'Healthcare Representative', 1, 0)
ibm_hr$Manager <- ifelse(ibm_hr$JobRole == 'Manager', 1, 0)
ibm_hr$SalesRepresentative <- ifelse(ibm_hr$JobRole == 'Sales Representative', 1, 0)
ibm_hr$ResearchDirector <- ifelse(ibm_hr$JobRole == 'Research Director', 1, 0)
ibm_hr$HumanResources <- ifelse(ibm_hr$JobRole == 'Human Resources', 1, 0)

# reg_yrs_sal <- lm(MonthlyIncome ~ Age + Attrition + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + 
#                     EducationField + EmployeeCount + EmployeeNumber + EnvironmentSatisfaction + Gender + HourlyRate + 
#                     JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyRate + 
#                     NumCompaniesWorked + Over18 + OverTime + PercentSalaryHike + PerformanceRating +
#                     RelationshipSatisfaction + StandardHours + StockOptionLevel + TotalWorkingYears + 
#                     TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
#                     YearsWithCurrManager + AvgYearsPerCompany + PropWorkYearsIBM + NumYearsSinceChange + Male + Divorced + 
#                     Single + Married + NewEmployee + NewMonthlyIncome + RoleSalesExecutive + ResearchScientist + 
#                     LaboratoryTechnician + ManufacturingDirector + HealthcareRepresentative + Manager + SalesRepresentative + 
#                     ResearchDirector + HumanResources, ibm_hr)

#remove redundant variables that had dummy or have the rates of earning
reg_yrs_sal <- lm(MonthlyIncome ~ Age + Attrition + BusinessTravel + Department + DistanceFromHome + Education + 
                    EducationField + EmployeeCount + EmployeeNumber + EnvironmentSatisfaction + 
                    JobInvolvement + JobLevel + JobSatisfaction + 
                    NumCompaniesWorked + Over18 + OverTime + PercentSalaryHike + PerformanceRating +
                    RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                    TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
                    YearsWithCurrManager + AvgYearsPerCompany + PropWorkYearsIBM + NumYearsSinceChange + Male + Divorced + 
                    Single + Married + NewEmployee + RoleSalesExecutive + ResearchScientist + 
                    LaboratoryTechnician + ManufacturingDirector + HealthcareRepresentative + Manager + SalesRepresentative + 
                    ResearchDirector + HumanResources, ibm_hr)
summary(reg_yrs_sal)

#remove ordinal variables
reg_yrs_sal <- lm(MonthlyIncome ~ Age + Attrition + DistanceFromHome + 
                   EmployeeCount + JobInvolvement + JobLevel + 
                    NumCompaniesWorked + Over18 + OverTime + PercentSalaryHike + PerformanceRating +
                    RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                    TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
                    YearsWithCurrManager + AvgYearsPerCompany + PropWorkYearsIBM + NumYearsSinceChange + Male + Divorced + 
                    Single + Married + NewEmployee + RoleSalesExecutive + ResearchScientist + 
                    LaboratoryTechnician + ManufacturingDirector + HealthcareRepresentative + Manager + SalesRepresentative + 
                    ResearchDirector + HumanResources, ibm_hr)
summary(reg_yrs_sal)

#remove variables until all significant
reg_yrs_sal <- lm(MonthlyIncome ~ JobInvolvement + JobLevel + 
                    TotalWorkingYears + 
                    YearsSinceLastPromotion + 
                    YearsWithCurrManager + AvgYearsPerCompany + 
                    RoleSalesExecutive + ManufacturingDirector + HealthcareRepresentative + Manager + 
                    ResearchDirector, ibm_hr)
summary(reg_yrs_sal)

#job satisfaction
#use everything
reg_jobsat <- lm(JobSatisfaction ~ ., ibm_hr)
summary(reg_jobsat)

reg_jobsat <- lm(JobSatisfaction ~ Attrition, ibm_hr)
summary(reg_jobsat)

#attributes of high performers
reg_perf <- lm(PerformanceRating ~ PercentSalaryHike + 
                    NewEmployee, ibm_hr)

summary(reg_perf)

#risk of losing employees
reg_attr <- lm(Attrition ~ Age + BusinessTravel + DistanceFromHome + 
                  EnvironmentSatisfaction + 
                 JobInvolvement + JobSatisfaction + MaritalStatus +
                 NumCompaniesWorked + OverTime + 
                 RelationshipSatisfaction  + WorkLifeBalance  + YearsInCurrentRole + 
                 YearsWithCurrManager + PropWorkYearsIBM + NumYearsSinceChange + Male + NewEmployee + 
                 NewMonthlyIncome + 
                 NewEmployee, ibm_hr)


summary(reg_attr)
