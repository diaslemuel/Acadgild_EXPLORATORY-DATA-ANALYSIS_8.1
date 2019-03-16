install.packages("Rcmdr")
library("Rcmdr")
install.packages("RcmdrPlugin.IPSUR")
library("RcmdrPlugin.IPSUR")
data("RcmdrTestDrive")
RcmdrTestDrive

##a. Calculate the average salary by gender and smoking status.

library(dplyr)
str(RcmdrTestDrive)

AvgSalary <- RcmdrTestDrive%>%group_by(gender, smoking)%>%
  select(smoking, gender, salary)%>%summarise(mean(salary))
AvgSalary <- as.data.frame(AvgSalary)
AvgSalary$meansalary <- AvgSalary$`mean(salary)`
AvgSalary


stripchart(meansalary ~ gender, vertical=TRUE, method="jitter", 
           ylab="meansalary", data=AvgSalary)

##b. Which gender has the highest mean salary?
RcmdrTestDrive
tapply(RcmdrTestDrive$salary, RcmdrTestDrive$gender, mean)

##c. Report the highest mean salary.
mean(RcmdrTestDrive$salary)

##d. Compare the spreads for the genders by calculating the 
##standard deviation of salary by gender.

tapply(RcmdrTestDrive$salary, RcmdrTestDrive$gender, sd)
boxplot(salary~gender,data= RcmdrTestDrive,main="salary versus gender",xlab="gender",ylab="salary",col=topo.colors(2))
tapply(RcmdrTestDrive$salary, RcmdrTestDrive$gender, mean)
hist(which(RcmdrTestDrive$gender == "Male") ,xlab = "gender male", ylab = "frequency",main="histogram of gender",col="red")
hist(which(RcmdrTestDrive$gender == "Female") ,xlab = "gender female", ylab = "frequency",main="histogram of gender",col="blue")
