library(sandwich)
library(corrplot)
library(wooldridge)
library(tidyverse)
library(readxl)

library(plm)
library(AER)
library(dplyr)
library(stats)

library(car)
library(lmtest)
library(stargazer)
library(fastDummies)
library(nnet)
library(knitr)

data = read_xlsx("C:\\Users\\Datan.xlsx")
data = dummy_cols(data, select_columns = "Parsimony", remove_first_dummy = FALSE)
data = data %>% 
  mutate(across(everything(), ~as.numeric(as.character(.))))
attach(data)

statistics <- data %>%
  group_by(Treatment) %>%
  summarize(across(3:17, mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -Treatment, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = Treatment, values_from = Mean)

kable(statistics)

col_removed = c("Date", "Suggested.Rate", "Parsimony_1", "Parsimony_2", "Parsimony_3", "Parsimony_4", "Parsimony_5", "Distance", "Treatment", "Rate")
data_reduced = data[, !names(data) %in% col_removed]
correlation_matrix = round(cor(data_reduced), 2)

corrplot(correlation_matrix, method = "color")

## Balance test
T_group = matrix(0, nrow = 0, ncol = 23);
C_group = matrix(0, nrow = 0, ncol = 23);

for (i in 1:247)
{
  if (data[i, 1] == 1)
  {
    T_group <- rbind(T_group, data[i, ]);
  }
  else
  {
    C_group <- rbind(C_group, data[i, ]);
  }
}


Test = rep(0, length = ncol(T_group)-1);
for (i in 1:(ncol(T_group)-1))
{
  numerator = nrow(T_group)*(colMeans(T_group[, i+1]) - colMeans(data[, i+1]))^2 + nrow(C_group)*(colMeans(C_group[, i+1]) - colMeans(data[, i+1]))^2;
  denominator = (var(T_group[, i+1])*(nrow(T_group)-1) + var(C_group[, i+1])*(nrow(C_group)-1))/(nrow(data) - 2);
  Test[i] = numerator/denominator;
}

print_matrix = matrix(0, nrow = 14, ncol = 2)
print_matrix[1, 1] = 'Gender';
print_matrix[2, 1] = 'Age';
print_matrix[3, 1] = 'Schooling';
print_matrix[4, 1] = 'Job';
print_matrix[5, 1] = 'City';
print_matrix[6, 1] = 'Siblings';
print_matrix[7, 1] = 'Financial Status';
print_matrix[8, 1] = 'House';
print_matrix[9, 1] = 'Vehicles';
print_matrix[10, 1] = 'Share costs';
print_matrix[11, 1] = 'Financial knowledge';
print_matrix[12, 1] = 'Interest in finance';
print_matrix[13, 1] = 'Parsimony';
print_matrix[14, 1] = 'Financial market';

for (i in 1:14)
{
  print_matrix[i, 2] = Test[i];
}

kable(print_matrix, col.names = c('Parameter', 'F-value'))

reg1 = lm(Distance ~ Treatment, data = data)

reg2 = lm(Distance ~ Treatment + Gender + Age + Schooling + Job + City, data = data)

reg3 = lm(Distance ~ Treatment + Gender + Age + Schooling + Job + City + Financial.status + House/Siblings + Veichles + Share.costs, data = data )

reg4 = lm(Distance ~ Treatment + Gender + Age + Schooling + Job + City + Financial.status + House/Siblings + Veichles + Share.costs + Financial.Knowledge + Interested.in.Finance + Parsimony + Financial.Markets , data = data )

reg5 = lm(Distance ~ Treatment + Gender + Age + Schooling + Job + City + Financial.status + House/Siblings + Veichles + Share.costs + Financial.Knowledge + Interested.in.Finance + Treatment*Parsimony_1 + Treatment*Parsimony_2 + Treatment*Parsimony_3 + Treatment*Parsimony_4 + Treatment*Parsimony_5 + Financial.Markets , data = data)

r1 = coeftest(reg1, vcov.  = vcovHC)
r2 = coeftest(reg2, vcov.  = vcovHC)
r3 = coeftest(reg3, vcov.  = vcovHC)
r4 = coeftest(reg4, vcov.  = vcovHC)
r5 = coeftest(reg5, vcov.  = vcovHC)

stargazer(r1, r2, r3, r4, r5,
          header=FALSE, title="",
          keep.stat="n",digits=4, 
          single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,
          column.labels=c("reg1","reg2","reg3","reg4","reg5"),
          type = "text"
)

hist(Age, probability = T, nclass = 100)