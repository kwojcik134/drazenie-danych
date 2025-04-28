library(readr)
library(KbMvtSkew)
library(e1071)
library(car)
library(ggplot2)

data <- read_delim("train.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Scharakteryzuj zmienne ApplicantIncome, CoapplicantIncome, LoanAmount w zakresie: minimum, pierwszy kwartyl, medina, średnia, trzeci kwartyl, maksimum, odchylenie standardowe, wybrana miarę asymetrii i koncentracji w każdej z grup: według płci (Gender) i według stanu cywilnego (Married).

minimum <- aggregate(data$ApplicantIncome,by=list(data$Gender, data$Married),FUN=min)
colnames(minimum) <-c('Gender', 'Married', 'Income')
first_quant <- aggregate(data$ApplicantIncome,by=list(data$Gender, data$Married),FUN=quantile, probs=0.25)
colnames(first_quant)<-c('Gender', 'Married', 'Income')
median_ <- aggregate(data$ApplicantIncome,by=list(data$Gender, data$Married),FUN=median)
colnames(median_)<-c('Gender', 'Married', 'Income')
mean_ <- aggregate(data$ApplicantIncome,by=list(data$Gender, data$Married),FUN=mean)
colnames(mean_)<-c('Gender', 'Married', 'Income')
maximum <- aggregate(data$ApplicantIncome,by=list(data$Gender, data$Married),FUN=max)
colnames(maximum)<-c('Gender', 'Married', 'Income')
stand_dev <- aggregate(data$ApplicantIncome,by=list(data$Gender, data$Married),FUN=sd)
colnames(stand_dev)<-c('Gender', 'Married', 'Income')
skew <- aggregate(data$ApplicantIncome,by=list(data$Gender, data$Married),FUN=PearsonSkew) #asymetria Pearsona
colnames(skew)<-c('Gender', 'Married', 'Income')
concentr <- aggregate(data$ApplicantIncome,by=list(data$Gender, data$Married),FUN=kurtosis) #kurtoza
colnames(concentr)<-c('Gender', 'Married', 'Income')


#Dla par zmiennych ApplicantIncome, LoanAmount zrób wykres rozrzutu. Niech zmienna Married będzie tworzył serie. Dodał tytuł, opis osi, legendę.

ggplot(data, aes(x = ApplicantIncome, y = LoanAmount, color = Married)) + geom_point() + ggtitle('Loan Amount based on Applicant Income') + xlab('Applicant Income') + ylab('Loan Amount')

#Dla wybranych zmiennych zrób wykres ramka-wąsy (boxplot). Niech zmienna Gender będzie tworzyła serie. Dodał tytuł, opis osi, legendę.

ggplot(data, aes(x = CoapplicantIncome, y = LoanAmount, color = Gender)) + geom_boxplot() + ggtitle('Loan Amount based on Coapplicant Income') + xlab('Coapplicant Income') + ylab('Loan Amount')


#Sprawdź czy wariancja zmiennej ApplicantIncome różni się pomiędzy grupą osób, które otrzymały pożyczkę i nie (zmienna Loan_status)

loan_amount <- aggregate(data$ApplicantIncome,by=list(data$Loan_Status),FUN=var)
colnames(loan_amount) <- c('Loan Status', 'Income Variance')
