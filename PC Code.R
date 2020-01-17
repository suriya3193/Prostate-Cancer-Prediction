library(corrplot)

#loading the data

PC_data<-read.csv("E:/Healthcare project/Cancer Dataset/pc_final.csv", header = TRUE,sep = ",")
View(PC_data)

PC_pred<-read.csv("E:/Healthcare project/Cancer Dataset/score_final.csv", header = TRUE,sep = ",")

#Removing the Variables
PC_data$tumor_6_months<-NULL
PC_data$psa_6_months<-NULL
PC_data$survival_1_year<- NULL
table(PC_data$tumor_6_months)
is.na(PC_data$tumor_6_months)



#Treatment of Missing Values

PC_data<-PC_data[complete.cases(PC_data), ]
View(PC_data)
table(PC_data$survival_7_years)

#Converting the variables to factors

PC_pred$Increase_decrease_psa<-as.factor(PC_pred$Increase_decrease_psa)


PC_data$n_score<-as.factor(PC_data$n_score)
PC_data$m_score<-as.factor(PC_data$m_score)
PC_data$stage<-as.factor(PC_data$stage)
PC_data$race<-as.factor(PC_data$race)
PC_data$family_history<-as.factor(PC_data$family_history)
PC_data$first_degree_history<-as.factor(PC_data$first_degree_history)
PC_data$previous_cancer<-as.factor(PC_data$previous_cancer) 
PC_data$smoker<-as.factor(PC_data$smoker)
PC_data$side<-as.factor(PC_data$side)
PC_data$tea<-as.factor(PC_data$tea)
PC_data$rd_thrpy<-as.factor(PC_data$rd_thrpy)
PC_data$h_thrpy<-as.factor(PC_data$h_thrpy)
PC_data$chm_thrpy<-as.factor(PC_data$chm_thrpy)
PC_data$cry_thrpy<-as.factor(PC_data$cry_thrpy)
PC_data$brch_thrpy<-as.factor(PC_data$brch_thrpy)
PC_data$rad_rem<-as.factor(PC_data$rad_rem)
PC_data$multi_thrpy<-as.factor(PC_data$multi_thrpy)
PC_data$survival_1_year<-as.factor(PC_data$survival_1_year)
PC_data$survival_7_years<-as.factor(PC_data$symptoms)

#Correlation

my_num_data <- PC_data[, sapply(PC_data, is.numeric)]
res<-cor(my_num_data)
View(res)
corrplot(res)

aov<- aov(PC_data$gleason_score ~ PC_data$stage)
summary(aov)

chisq.test(PC_data$n_score,PC_data$stage)



chisq.test(PC_data$t_score,PC_data$m_score)

chisq.test(PC_data$m_score,PC_data$n_score)

chisq.test(PC_data$family_history,PC_data$first_degree_history)

chisq.test(PC_data$rd_thrpy,PC_data$h_thrpy)

chisq.test(PC_data$rd_thrpy,PC_data$cry_thrpy)

chisq.test(PC_data$rd_thrpy,PC_data$brch_thrpy)

#chi-square test

chisq.test(PC_data$t_score,PC_data$survival_7_years)

chisq.test(PC_data$m_score,PC_data$survival_7_years)

chisq.test(PC_data$n_score,PC_data$survival_7_years)

chisq.test(PC_data$stage,PC_data$survival_7_years)

chisq.test(PC_data$race,PC_data$survival_7_years)

chisq.test(PC_data$family_history,PC_data$survival_7_years)

chisq.test(PC_data$first_degree_history,PC_data$survival_7_years)

chisq.test(PC_data$previous_cancer,PC_data$survival_7_years)

chisq.test(PC_data$smoker,PC_data$survival_7_years)

chisq.test(PC_data$side,PC_data$survival_7_years)

chisq.test(PC_data$tea,PC_data$survival_7_years)

chisq.test(PC_data$rd_thrpy,PC_data$survival_7_years, correct = FALSE)

chisq.test(PC_data$h_thrpy,PC_data$survival_7_years, correct = FALSE)

chisq.test(PC_data$chm_thrpy,PC_data$survival_7_years, correct = FALSE)

chisq.test(PC_data$cry_thrpy,PC_data$survival_7_years, correct = FALSE)

chisq.test(PC_data$rad_rem,PC_data$survival_7_years, correct = FALSE)

chisq.test(PC_data$multi_thrpy,PC_data$survival_7_years, correct = FALSE)

chisq.test(PC_data$survival_1_year,PC_data$survival_7_years, correct = FALSE)


table(PC_data$survival_1_year)


#ANOVA test

aov<- aov(PC_data$gleason_score ~ PC_data$survival_7_years)
summary(aov)

aov<- aov(PC_data$age ~ PC_data$survival_7_years)
summary(aov)

aov<- aov(PC_data$height ~ PC_data$survival_7_years)
summary(aov)

aov<- aov(PC_data$weight ~ PC_data$survival_7_years)
summary(aov)

aov<- aov(PC_data$tumor_diagnosis ~ PC_data$survival_7_years)
summary(aov)

aov<- aov(PC_data$psa_diagnosis ~ PC_data$survival_7_years)
summary(aov)

aov<- aov(PC_data$tumor_1_year ~ PC_data$survival_7_years)
summary(aov)

aov<- aov(PC_data$psa_1_year ~ PC_data$survival_7_years)
summary(aov)

aov<- aov(PC_data$gleason_score ~ PC_data$stage)
summary(aov)

#Train and Test
set.seed(123)
smp_size <- floor(0.70 * nrow(PC_data))

train_ind <- sample(seq_len(nrow(PC_data)), size = smp_size)
PC_train <- PC_data[train_ind, ]
PC_test <- PC_data[-train_ind, ]


#Model
attach(PC_train)
View(PC_train)

model <- glm (survival_7_years ~ stage+age+race+BMI+first_degree_history+side+smoker+tumor_1_year+Increase_decrease_psa+psa_1_year+rd_thrpy+chm_thrpy+cry_thrpy+brch_thrpy+rad_rem+U03+U06+U01+U02+S04+S07+S10+O11+U05+P03+P01+P02+O08+O01+O09+O10, data = PC_train, family = binomial)
summary(model)
exp(coef(model))

predict_result <- predict(model,PC_test,type = 'response')
fitted.results <- ifelse(predict_result > 0.5,1,0)
misClasificError <- mean(fitted.results != PC_test$survival_7_years)
print(paste('Accuracy',1-misClasificError))
colnames(PC_train)

predict_result <- predict(model,PC_pred,type = 'response')
fitted.results <- ifelse(predict_result > 0.5,1,0)
View(fitted.results)
table(fitted.results)
View(predict_result)
table(PC_test$survival_7_years)
View(PC_data)
