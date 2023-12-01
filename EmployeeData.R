library(tidyverse)
library(dslabs)


#Data Structure
str(EmployeeData)

#How many people in each department

dpr = EmployeeData$Department
department_count =table(dpr)

#Average salary

avg_salary = sum(EmployeeData$Salary)/max(EmployeeData$EmployeeID)

#Scatterplot Salary aginst years of experience

scr = EmployeeData %>% 
  ggplot(aes(YearsOfExperience, Salary))+
  geom_point() +
  xlab("Years of Experience")+
  ylab("Salary")+
  ggtitle("Salary vs Years of Experience")
  print(scr)

# Employee with highest salary in each department
  
highsalaryperdepartment = EmployeeData %>% group_by(Department) %>%
  slice(which.max(Salary))
print(highsalaryperdepartment)

# Department with most experience

highexperiencedepartment = EmployeeData$Department[which.max(EmployeeData$YearsOfExperience)]
print(highexperiencedepartment)

# Histogram to show salary distribution

salary_dist = EmployeeData %>% ggplot(aes(Salary))+
  geom_histogram(binwidth = 1000, fill = 'blue', col = 'black', alpha = 0.7)+
  ylab("Frequency")+
  xlab("Salary")+
  ggtitle("Salary Distribution")+
  print(salary_dist)

# Average Salary by Department

averagesalarybydepartment = EmployeeData %>% group_by(Department) %>% 
  summarise(AverageSalary = mean(Salary, na.rm = True))
print(averagesalarybydepartment)

# Calculate the correlation coefficient
correlation_coefficient <- cor(employee_data$YearsOfExperience, employee_data$Salary)

print(paste("Correlation Coefficient: ", round(correlation_coefficient, 3)))
  