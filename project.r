library("readxl")
library("ggplot2")
library("dplyr")

file <- read_excel("S3225_Project_Data.xlsx")
file

data <- file

# QUESTION 1 #

data <- as.data.frame(read_excel("S3225_Project_Data.xlsx"))

# Column D
Program <- data$Program
businessProgram <- length(Program[Program==1]) / length(Program)

## Plus/minus confidence interval
businessProgram + qnorm(p=.12/2, lower.tail=TRUE)*(sqrt(businessProgram*(1-businessProgram)/110))
businessProgram - qnorm(p=.12/2, lower.tail=TRUE)*(sqrt(businessProgram*(1-businessProgram)/110))

## Pie chart
program <- table(data$Program)
program.prop <- prop.table(program)
program.prop<- round(program.prop*100, 2)
pie(x = program.prop,
    labels = c("1=University Transfer(78.18%)","2=Business Admin (7.27%)", "3=Transfer to BCIT (3.64%)", "4=Other (10.91%)"),
    clockwise = TRUE,
  main="Student Program of Study",
    col = c("yellow", "orange", "lightblue", "darkblue"))

# Column E 
courses <- data$Courses
t.test(courses, conf.level = 0.88)
hist(courses, ylab = "Number of Students", main ="Courses Taken by Students In Present Term", xlab="Courses Taken in Present Term")

# Column G
HoursWorked <- data$Hours
t.test(HoursWorked, conf.level = 0.92)
hist(HoursWorked, ylab = "Number of Students", main ="Hours Worked By Students Per Week", xlab="Hours Worked Per Week")

# Column I
calculator <- table(data$Calculators)
calculator.prop <- prop.table(calculator)
calculator.prop<- round(calculator.prop*100, 2)
barplot(calculator.prop, main="Level of Comfortableness with Calculators", xlab= "Level of Confort 1=Very Uncomfortable,2=Fairly Uncomfortable, 3=Average, 4=Fairly Confortable, 5=Very Comfortable", ylab = "Percentage of People")

cal <- data$Calculators
comfortable <- length(cal[cal==1]) / length(cal)
comfortable - qnorm(p=0.08/2, lower.tail=TRUE)*(sqrt((comfortable*(1-comfortable))/110))

# Column M
FinalMark <- data$Mark
t.test(FinalMark, conf.level = 0.96)
hist(FinalMark, ylab = "Number of Students", main ="Final Mark (Out of 100, 50 is passing)")

# Column K
MathAnxiety <- data$Anxiety
hasAnxiety <- length(MathAnxiety[MathAnxiety==1]) / length(MathAnxiety)
hasAnxiety - qnorm(p=0.04/2, lower.tail=TRUE)*(sqrt(hasAnxiety*(1-hasAnxiety)/110))

AnxietyProp <- table(data$Anxiety)

AnxietyProp  <- prop.table(AnxietyProp )

AnxietyProp <- round(AnxietyProp *100, 2)


pie(x = AnxietyProp ,
    labels = c(" No (73.64%)","Yes (26.36%)"),
    clockwise = TRUE,
    main="Student which Suffer From Math Anxiety",
    col = c( "orange", "lightblue"))

# QUESTION 2 # 

# Question 2A 
gen_emp_stats <- table(data$Gender, data$Employment)
gen_emp_stats

gender <- c("Male", "Female")
employed <- c(37, 29)
gen_emp_df <- data.frame(gender, employed)

ggplot(gen_emp_df, aes(x = gender, y = employed)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Gender") +
  ylab("Number of Employed Students") +
  ggtitle("Number of Employed Students Among Gender")

# Question 2B
gen_anx_stats <- table(data$Gender, data$Anxiety)
gen_anx_stats

anx <- c(14, 15)
gen_anx_df <- data.frame(gender, anx)

ggplot(gen_anx_df, aes(x = gender, y = anx)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Gender") +
  ylab("Number of Students with Anxiety") +
  ggtitle("Number of Students with Anxiety Among Gender")

# Question 2C
uni_emp_stats <- table(data$Program, data$Employment)
uni_emp_stats

table = matrix(c(48, 18, 38, 6), ncol = 2, byrow = TRUE)
chisq.test(table)

emp_uni <- c(48, 18)
emp_uni_labels <- c("University Transfer", "Non-University Transfer")
pct <- round(emp_uni/sum(emp_uni)*100)
emp_uni_labels <- paste(emp_uni_labels, pct)
emp_uni_labels <- paste(emp_uni_labels,"%",sep="")
pie(emp_uni, labels = emp_uni_labels, main = "Number of Employed Students Among Programs", col=rainbow(length(emp_uni_labels)))


# Question 2D
comp_anx_stats <- table(data$Computers, data$Anxiety)
comp_anx_stats

comp <- c("Have Computing Experience", "Don't Have Computing Experience")
comp_anx <- c(19, 10)
comp_anx_df <- data.frame(comp, comp_anx)

ggplot(comp_anx_df, aes(x = comp, y = comp_anx)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Computing Experience") +
  ylab("Number of Students with Anxiety") +
  ggtitle("Number of Students with Anxiety Among Computing Experience")



summary(file)


#Q 3
data_df <- data.frame(file)

data_df

#Q 3 A
data_df_filtered <- subset(data_df, select = c('Gender', 'Mark'))

male <- data_df_filtered[data_df_filtered$Gender == 1,]
male
summary(male$Mark)
nrow(male)

female <- data_df_filtered[data_df_filtered$Gender == 2,]
female
summary(female$Mark)
nrow(female)

  #F statistic 
SD_female <- sd(female$Mark)
SD_male <- sd(male$Mark)

#larger SD
SD_female
#Smaller SD
SD_male

var.test(female$Mark,male$Mark)

t.test(female$Mark,male$Mark, alternative = "two.sided" ,var.equal = TRUE)

# Q 3 B 
data <- file

# Extract data for employed students
emp_test <- filter(data, data$Employment >= 1)
emp_test <- data.frame(emp_test$Employment, emp_test$Mark)

# Extract data for unemployed students
non_emp_test <- filter(data, data$Employment == 0)
non_emp_test <- data.frame(non_emp_test$Employment, non_emp_test$Mark)

# Confirm n1 and n2 are >= 30
nrow(emp_test)
nrow(non_emp_test)

emp_marks <- emp_test$emp_test.Mark
non_emp_marks <- non_emp_test$non_emp_test.Mark

# QQPlot for Employed Students
qqnorm(emp_marks, main = "Marks of Employed Student Normal Probability Plot")
qqline(emp_marks)

# QQPlot for Unemployed Students
qqnorm(non_emp_marks, main = "Marks of Unemployed Student Normal Probability Plot")
qqline(non_emp_marks)

# F Test
var.test(emp_marks, non_emp_marks)

# Pooled T-Test
t.test(emp_marks, non_emp_marks, alternative = "two.sided", var.equal = TRUE)

# Q 3 B 
data_df_filtered <- subset(data_df, select = c('Employment', 'MDT'))
non_employed <- data_df_filtered[data_df_filtered$Employment == 0,]


# Q 3 C
data_df_filtered <- subset(data_df, select = c('Anxiety', 'Mark'))

data_df_filtered

noAnxiety_mark <- data_df_filtered[data_df_filtered$Anxiety == 0,]
noAnxiety_mark
summary(noAnxiety_mark$Mark)

withAnxiety_mark <- data_df_filtered[data_df_filtered$Anxiety == 1,]
withAnxiety_mark
summary(withAnxiety_mark$Mark)
#check for if the data is normalized -- > conduct a side by side boxplot

boxplot(noAnxiety_mark$Mark,withAnxiety_mark$Mark)
qqnorm(noAnxiety_mark$Mark)
qqnorm(withAnxiety_mark$Mark)


#F test first 
#we need the number of students with math anxiety
#and the number of students without math anxiety 
#although anxiety is a categorical variable


SD_withMathAnxiety <- sd(withAnxiety_mark$Mark)
SD_noMathAnxiety <- sd(noAnxiety_mark$Mark)

#has smaller SD
SD_noMathAnxiety

#has larger SD
SD_withMathAnxiety

var.test(withAnxiety_mark$Mark,noAnxiety_mark$Mark)
#the p value is larger than alpha, therefore we cannot reject H0, meaning that the two standard deviatations show no differenc

#therefore have to do a pooled T test 
t.test(withAnxiety_mark$Mark,noAnxiety_mark$Mark,mu = 0.2, var.equal = TRUE)


# Q 3 D
MDT_NonEnglish <- data[data["Language"]!=1,c("MDT")]
MDT_English <- data[data["Language"]==1,c("MDT")]

## Calculate n 
length(MDT_English) ## 43
length(MDT_NonEnglish) ## 67


var.test(MDT_English,MDT_NonEnglish) ## 1.7429 Df = 42, 66 
qf(0.9750,42,66) ## F-critical value = 1.709632


t.test(MDT_English,MDT_NonEnglish) ## P-value  0.0001826

boxplot(MDT_English,MDT_NonEnglish, xlab=" EnglIsh Speaking vs Non-English", main = "MDT Score of Students who have Mother Language English vs Non-English", ylab = "MDT Score")


#Q 3 E 
data_df_filtered <- subset(data_df, select = c('Mark','MDT'))
pass <- data_df_filtered[data_df_filtered$MDT > 24,]
pass
summary(pass$MDT)
nrow(pass)

fail <- data_df_filtered[data_df_filtered$MDT < 25,]
fail
summary(fail$MDT)
nrow(fail)

  #F statistic 

#larger
SD_pass <- sd(pass$Mark)
SD_pass
#smaller
SD_fail <- sd(fail$Mark)
SD_fail

var.test(pass$Mark,fail$Mark)
t.test(pass$Mark,fail$Mark, alternative = "two.sided" ,var.equal = TRUE)



#Q4. Boxplots
data_q4 <- subset(data_df, select = c('Employment', 'Mark'))
data_q4 
unemployed_mark <- data_q4[data_q4$Employment == 0,]
nrow(unemployed_mark)
part_mark <- data_q4[data_q4$Employment == 1,]
nrow(part_mark)
full_makr <- data_q4[data_q4$Employment == 2,]
nrow(full_makr)
boxplot(unemployed_mark$Mark,part_mark$Mark,full_makr$Mark)

boxplot(data_q4)








