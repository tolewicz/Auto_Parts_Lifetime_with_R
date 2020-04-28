library(tidyverse)
library(janitor)

MechaCar <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F) #import used car dataset

#using janator library to format column names to be readable 
MechaCar <- clean_names(MechaCar)

MechaCar_matrix <- as.matrix(MechaCar[,c("vehicle_length","vehicle_weight","spoiler_angle","ground_clearance","awd","mpg")]) #convert data frame into numeric matrix
cor(MechaCar_matrix)
corelation_matrix <- cor(MechaCar_matrix)

#summary table of all variables to confirm or reject Ho hypothesis  
summary(lm(mpg ~ vehicle_length+ground_clearance+spoiler_angle+ground_clearance+awd+vehicle_weight,MechaCar)) 

#summary table to confirm or reject Ho hypothesis for correlatad variables with r>3
summary(lm(mpg ~ vehicle_length+ground_clearance,MechaCar)) 
model <- lm(mpg ~ vehicle_length,MechaCar)

#plotting data with regression 
yvals <- model$coefficients['vehicle_length']*MechaCar$vehicle_length + model$coefficients['(Intercept)']
plt <- ggplot(MechaCar,aes(x=vehicle_length, y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red")

#loading suspension coil data file
Suspension_Coil <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F) #import coil dataset

#statistical measurements of whole population and saving it to Statistics_psi
summary(Suspension_Coil)
Statistics_psi <- summarize(Suspension_Coil,Mean_Psi=mean(Suspension_Coil$PSI), Median_Psi=median(Suspension_Coil$PSI), variance_Psi=var(Suspension_Coil$PSI), St.Dev_Psi=sd(Suspension_Coil$PSI))

#statistical measurements per lot and saving it to Statistics_psi_per_lot
Statistics_psi_per_lot <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean_Psi=mean(PSI), Median_Psi=median(PSI), variance_Psi=var(PSI), St.Dev_Psi=sd(PSI)) #create summary table

#t-test, splitting data into lot 1,2,3
Lot1 <- Suspension_Coil %>% filter(Manufacturing_Lot=="Lot1")
Lot2 <- Suspension_Coil %>% filter(Manufacturing_Lot=="Lot2")
Lot3 <- Suspension_Coil %>% filter(Manufacturing_Lot=="Lot3")

#paired t-test lot 1,2 vs lot 3 to see if lot 3 is outlier
t.test(Lot1$PSI,Lot3$PSI,paired = TRUE)
t.test(Lot2$PSI,Lot3$PSI,paired = TRUE)
t.test(Lot1$PSI,Lot2$PSI,paired = TRUE)


