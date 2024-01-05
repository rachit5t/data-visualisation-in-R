# Rachit Dhakal
# NPI000133

#Include library and import data
library(ggplot2)
library(dplyr)
employeeData <- read.csv("employee_attrition.csv")

#exploration
head(employeeData) # Prints first six rows
names(employeeData) # Names of columns
dim(employeeData) # Number of rows and columns
str(employeeData) # Structure of data
summary(employeeData) # Shows data like mean, median, min
colSums(is.na(employeeData)) # Number of NA values


#Preprocessing
colnames(employeeData)[1]   <- "employee_id"
colnames(employeeData)[16]  <- "status_year"
colnames(employeeData)[17]  <- "status"
colnames(employeeData)[18]  <- "business_unit"
employeeData = select(employeeData, -c(gender_full))
employeeData = select(employeeData, -c(status_year))
employeeData = select(employeeData, -c(status))
employeeData = mutate(employeeData, age_group = case_when(
  between(age, 0, 25) ~ "0-25",
  between(age, 25, 35) ~ "25-35",
  between(age, 35, 45) ~ "35-45",
  between(age, 45, 55) ~ "45-55",
  between(age, 55, 66) ~ "55-66",
))
employeeData = select(employeeData, -c(age))


#Question 1: What is the basic employee structure of company?

    #Analysis 1 : Employee Distribution By Age 
      ageGroupedData = as.data.frame(table(employeeData$age_group))
      ggplot(ageGroupedData, aes(x="", y = Freq, fill = Var1)) +
      theme_void() +
      geom_bar(width = 1, stat = "identity", color = "black") +
      geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      guides(fill = guide_legend(title = "Age Group")) +
      ggtitle("Employee Distribution By Age") + 
      labs(caption = "Student : NPI000133")
      
      
    #Analysis 2 : Employee Distribution By Department
      employeeData %>% ggplot(aes(y = department_name)) + 
        geom_bar(fill="steelblue") +
        xlab("Number Of Employees") +
        ylab("Departments") +
        ggtitle("Employee Distribution By Department")  + 
        labs(caption = "Student : NPI000133")
      
    #Analysis 3 : Loyalty of employees
      summarise(employeeData, mean(length_of_service, na.rm = TRUE))
      summarise(employeeData, median(length_of_service, na.rm = TRUE))
      employeeData %>% ggplot(aes(x = length_of_service)) + 
        geom_boxplot() + 
        xlab("Length Of Service In Years") + 
        labs(caption = "Student : NPI000133")
      
      
# Question 2 : What were the company's hiring patterns?
      
    #Analysis 1 : Total hiring according to year
    dateGroupedData = employeeData
    dateGroupedData = mutate(dateGroupedData, year = format(as.Date(dateGroupedData$orighiredate_key, format="%m/%d/%Y"),"%Y"))
    dateGroupedData = as.data.frame(table(dateGroupedData$year))
    ggplot(dateGroupedData, aes(x= strtoi(Var1), y = Freq)) + 
    geom_line() +
    geom_point() + xlab("Year") + ylab("Employee hired") + 
    ggtitle("Hiring Trend : Yearly") + 
    labs(caption = "Student : NPI000133")
    
    #Analysis 2 : hiring and Position
    employeeData %>% ggplot(aes(y=job_title)) + 
     geom_bar() + 
     ggtitle("Hiring Trend : Position Wise") + 
     xlab("Number Of Employees") + 
     ylab("Job Title") + 
    labs(caption = "Student : NPI000133")

# Question 3 : What were the reasons for termination?
    
    #Analysis 1 : Termination Types
    terminationGroupedData = as.data.frame(table(employeeData$termtype_desc))
    filter(terminationGroupedData, Var1 !=  "Not Applicable") %>% ggplot(aes(x="", y = Freq, fill = Var1)) +
      theme_void() +
      geom_bar(width = 1, stat = "identity", color = "black") +
      geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      guides(fill = guide_legend(title = "Termination Type")) + 
      labs(caption = "Student : NPI000133")
    
    #Analysis 2 : Termination In Relation To Age
    termAgeGroupedData = employeeData
    termAgeGroupedData = filter(termAgeGroupedData, termtype_desc !=  "Not Applicable")
    termAgeGroupedData = as.data.frame(table(termAgeGroupedData$age_group))
    ggplot(termAgeGroupedData, aes(x=Var1, y = Freq)) + 
    geom_bar(stat="identity", fill="blue") + 
    xlab("Age Group") + 
    ylab("Termination") + 
    ggtitle("Termination In Relation To Age") + 
    labs(caption = "Student : NPI000133")
    
    
    #Analysis 3 : Termination In Relation To Position
  employeeData %>% filter(termreason_desc !=  "Not Applicable") %>% ggplot(aes(y = job_title, fill = termreason_desc)) + 
    geom_bar() +
    guides(fill = guide_legend(title = "Type Of Termination")) +
    ggtitle("Termination In Relation To Position")
    xlab("Number Of Terminations") +
    ylab("Job Title") + 
    labs(caption = "Student : NPI000133")
  
  
# Question 4: When and where did the termination happen?
  
    #Analysis 1 : Termination In Relation To Time
    employeeData %>% filter(termtype_desc !=  "Not Applicable") %>% ggplot(aes(x=format(as.Date(terminationdate_key, format="%m/%d/%Y"),"%Y"), fill = termreason_desc)) + 
      geom_bar() + 
      guides(fill = guide_legend(title = "Termination Reason")) +
      xlab("Year") + 
      ylab("Number Of Terminations") + 
      ggtitle("Termination In Relation To Time") + 
      labs(caption = "Student : NPI000133")
    
    #Analysis 2 : Termination In Relation To Department
    employeeData %>% filter(termtype_desc !=  "Not Applicable") %>% ggplot(aes(y=department_name, fill = termreason_desc)) + 
    geom_bar() + 
    guides(fill = guide_legend(title = "Termination Reason")) +
    xlab("Number Of Terminations") + 
    ylab("Departments") + 
    ggtitle("Termination In Relation To Department") + 
      labs(caption = "Student : NPI000133")

    # Question 5 : how can we scale the company?
    
    #Analysis 1 : City in relation to department
    employeeData %>% ggplot(aes(x = city_name, y = department_name)) + geom_point() + 
      theme(axis.text.x = (element_text(angle = 90))) +
      xlab("City Name") + 
      ylab("Department Name") + 
      ggtitle("City in relation to department") + 
      labs(caption = "Student : NPI000133")
    
    #Analysis 2 : City in relation to stores
    employeeData %>% ggplot(aes(x = city_name, y = store_name)) + geom_point() +
      theme(axis.text.x = (element_text(angle = 90))) +
      xlab("City Name") + 
      ylab("Store Name") + 
      ggtitle("City in relation to stores") + 
      labs(caption = "Student : NPI000133")
    
    #Analysis 3 : City in relation to termination
    employeeData %>% filter(termtype_desc !=  "Not Applicable") %>% ggplot(aes(y = city_name, fill = termreason_desc)) + geom_bar() + 
      theme(axis.text.x = (element_text(angle = 90))) + 
      guides(fill = guide_legend(title = "Termination Reason")) +
      xlab("Number Of Employees") + 
      ylab("City") + 
      ggtitle("City in relation to termination") + 
      labs(caption = "Student : NPI000133")
    
    #Extra features
    #Feature 1 : labs(), ggtitle(), xlab(), ylab()
    #Feature 2: coord_polar(), theme_void(), guides(), geom_text()
    #Feature 3 : fill, format()
    
    