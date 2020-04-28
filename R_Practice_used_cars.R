library(tidyverse)
population_table <- read.csv('used_car_data.csv',check.names = F,stringsAsFactors = F) #import used car dataset
plt <- ggplot(population_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

#creating sample dataset of 50
sample_table <- population_table %>% sample_n(50) 
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven)))#import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven))) #compare sample versus population means

sample_table <- population_table %>% sample_n(50) #sample nr 1
sample_table2 <- population_table %>% sample_n(50) #sample nr2

t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven))

mpg_data <- read.csv(file = 'mpg_modified.csv', check.names = F,stringsAsFactors = F)
mpg_1999 = mpg_data %>% filter(year==1999)
mpg_2008 <- mpg_data %>% filter(year==2008)

#null hypothesis =  the overall difference is zero, no difference
#if p is les than 0.05 null is rejected because mean of 2nd sample is
# beyond 95% distribution of 1st sample?aov(), there is a difference
#if p is grater than 0.05 then null is not rejected, no difference 
t.test(mpg_1999$hwy,mpg_2008$hwy,paired = TRUE) #compare the mean difference between two samples

mtcars_filt <- mtcars[,c("hp","cyl")] #filtering horsepower(hp) and cylinteds(cyl) from mtcars
mtcars_filt$cyl <- factor(mtcars_filt$cyl) #convert numeric column to factor
summary(aov(hp ~ cyl,data=mtcars_filt)) #result gives Pr(>F) {i.e. pvalue} much less than 0.05
# thus there is difference between engine types in terms of horse power

head(mtcars)

plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() #create scatter plot
cor(mtcars$hp,mtcars$qsec) #calculating correlation hp/qsec

#we check if there is correlation between used cars milleage and price
used_cars <- read.csv('used_car_data.csv',stringsAsFactors = F) #read in dataset
head(used_cars)

plt <- ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price)) #import dataset into ggplot2
plt + geom_point() #create a scatter plot
cor(used_cars$Miles_Driven,used_cars$Selling_Price) #calculate correlation coefficient
#cor is 0.02 i.e. negligilbe correlation/no correlation

#to not caluclate everything one by one we use correlation matrix to find relations
used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")]) #convert data frame into numeric matrix
cor(used_matrix)
#looks selling and present price are correlated, now we can use t-test and other good stuff
#lets plot it
plt <- ggplot(used_cars,aes(x=Selling_Price,y=Present_Price)) #import dataset into ggplot2
plt + geom_point() #create a scatter plot

summary(lm(qsec~hp,mtcars)) #summarize linear model
model <- lm(qsec ~ hp,mtcars) #create linear model
#the equation is qsec = coeff*hp+intercept
yvals <- model$coefficients['hp']*mtcars$hp + model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model

lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars) #generate multiple linear regression model

table(mpg$class,mpg$year) #contingency table
tbl <- table(mpg$class,mpg$year)
chisq.test(tbl) #compare categorical distributions
