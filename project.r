library("readxl")

file <- read_excel("S3225_Project_Data.xlsx")
file

summary(file)


data_df <- data.frame(file)

data_df

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
t.test(withAnxiety_mark$Mark,noAnxiety_mark$Mark, var.equal = TRUE)
