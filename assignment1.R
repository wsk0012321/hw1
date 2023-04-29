library(ggplot2)
library(lme4)
library(cAIC4)
library(dplyr)
library(tidyverse)
library(nortest)
library(ggpubr)
library(performanceEstimation)
# get current working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# read xlsx
data <- readxl::read_xlsx('papers_data.xlsx')

# arrange the data

## 1.chr to num
names(data)[10] <- "Rating"
data <- data %>% mutate(Rating = as.numeric(Rating))

## 2. empirical / theoretical
for (i in 1:nrow(data)) {
    if (!is.na(data[i,3])) {
    if(str_detect(data[i,3], "yes|Yes")) {
       data[i,4] <- "Empirical"
      }
    else if(str_detect(data[i,3], "no|No")) {
      data[i,4] <- "Theoretical"     }
    }
    else { }
}

## 3. unify the scoring
### 3.1 find all deviance
deviant_list <- list()
for (i in 1:nrow(data)) {
  row <- data[i,]
  if(row[10] > 4 & !(row[1] %in% deviant_list)) {
    deviant_list <- c(deviant_list, row[1])
  }
}
### 3.2 modify the value
for (i in 1:nrow(data)) {
  row <- data[i,]
  if(row[1] %in% deviant_list) {
    data[i,10] <- row[10] * 0.8
  }
}

#data$Empirical <- data$Empirical %>% mutate_if(case_when(str_detect(Empiricist, "yes|Yes") ~ "Empirical", 
                                    #          str_detect(Empiricist, "no|No") ~ "Theoretical"))
## 4. lecturer
data <- data %>% mutate(Professor = case_when(str_detect(Professor, "Balari|Foundations|Foundamental") ~ "Balari", str_detect(Professor, "Antoni|Architecture") ~ "Fornells",
                                                             str_detect(Professor, "Dediu") ~ "Dediu", str_detect(Professor, "Boeckx|Interdisciplinary") ~ "Boeckx",
                                                             str_detect(Professor, "Bosch") ~ "Bosch", str_detect(Professor, "Gregory") ~ "Gregory",
                                                             str_detect(Professor, "Toribio|Philosophy") ~ "Toribio", str_detect(Professor, "Maldonado") ~ "Maldonado",
                                                             str_detect(Professor, "Bonet") ~ "Bonet", str_detect(Professor, "Irene") ~ "Castellón",
                                                             str_detect(Professor, "Elvira") ~ "Elvira", str_detect(Professor, "Hernanz") ~ "Hernanz",
                                                             str_detect(Professor, "Rodriguez") ~ "Rodriguez", str_detect(Professor, "NLP|Mireia") ~ "Farrús",
                                                             str_detect(Professor, "Morphsyntax") ~ "Brochhagen", str_detect(Professor, "Manolo") ~ "Manolo", 
                                                             str_detect(Professor, "Marsili") ~ "Marsili", str_detect(Professor, "Quer|Contrastive") ~ "Quer",
                                                             str_detect(Professor, "MacPherson") ~ "MacPherson", str_detect(Professor, "Joana") ~ "Rosselló"))  

## 5. chr to fct
data <- data %>% mutate_if(is.character, as.factor)

#for checking
#openxlsx::write.xlsx(data, file = "df.xlsx", sheetName = "sheet1")
# extract the data we need
df <- data.frame(data$Empirical, data$Rating, data$Professor, data$Student)
colnames(df) <- c("Empirical", "Rating", "Professor", "Student")

# extract info related to students
df2 <- data.frame(df$Empirical, df$Rating, df$Student)
colnames(df2) <- c("Empirical", "Rating", "Student")
# info related to professors
df3 <- data.frame(df$Empirical, df$Professor)
colnames(df3) <- c("Empirical","Professor")

#test normality
type = table(df$Empirical)
prof = table(df$Professor)
student = table(df$Student)

## distribution of empirical/theoretical papers
binom.test(type, p = 0.5)
## distribution of how many papers proposed by each professor
lillie.test(prof)
## distribution of how many papers rated by each student
lillie.test(student)
# result: prof is not evenly distributed.

#students prederence
## 1.extract the scores for empirical and theoretical 
Empirical <- na.omit(c(case_when(str_detect(df2$Empirical, "Empirical") ~ df2$Rating)))
Theoretical <- na.omit(c(case_when(str_detect(df2$Empirical, "Theoretical") ~ df2$Rating)))

## 2. test the normality of scores for each type, result: normal
lillie.test(Theoretical)
lillie.test(Empirical)

## 3. evaluate the rating with median and interquartile range
med_emp <- median(Empirical)
med_the <- median(Theoretical)
iqr_emp <- IQR(Empirical)
IQR_the <- IQR(Theoretical)

## 4. overall preference
### 4.1 Create box plot
p <- ggplot(df2, aes(x = Empirical, y = Rating)) +
  geom_boxplot(col = c("gold","blue")) + geom_point(size = 3) + geom_jitter(width = 0.25) +
  theme_bw() + theme(axis.text.x = element_text(face="bold",angle = 45,hjust = 1,color = 'black',size = 10),
  legend.position = "none",legend.direction = "vertical",legend.title =element_blank())
ggsave("bp.png", p , width = 10, height = 15, dpi = 300)

### 4.2 model rating and types
model_st1 <- glm(Empirical ~ Rating, data=df, family = "binomial")
plot(model_st1)
summary(model_st1)

## 5. model each student's preference
model_st2 <- glm(Empirical ~ Student + Rating, data=df, family = "binomial")
plot(model_st2)
summary(model_st2)
## prediction
#test <- readxl::read_xlsx("test.xlsx")
#pred <- predict(model_st2, newdata = test, type = "response")
#pred

# model professors' preference

## 1. To counter the bias, we use oversampling
### 1.1 drop NA
df3 <- na.omit(df3)
### 1.2 apply smote
df3 <- smote(Professor ~ Empirical, df3, perc.over =100, k = 5, perc.under = 10)
### 1.3 test normality again, got p-value: 0.055
lillie.test(table(df3$Professor))

## 2. overall preference (assuming that the data is made balanced)
table(df3$Empirical)
## result: more theoretical than empirical

## 3. model each professor's preference
model_pr = glm(Empirical ~ Professor, data=df3, family = "binomial")
plot(model_pr)
summary(model_pr)

## 4. plot and table
## 4.1 table
table(df3)
## 4.2 histogram
p2 <- ggplot(df3, aes(Professor, fill = Empirical)) +
  geom_histogram(stat = "count", color = "white") 
ggsave("bp2.png", p2 , width = 20, height = 10, dpi = 300)

## prediction
#test2 <- readxl::read_xlsx("test2.xlsx")
#pred2 <- predict(model_pr, newdata = test2, type = "response")
#pred2

#test the models
cAIC(model_st1)
cAIC(model_st2)
cAIC(model_pr)
