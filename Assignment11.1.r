#Assignment11.1

# Importing the bank data 
library(readr)
bank <- read.csv("C:/Users/vere/Downloads/bank-full.csv", sep=";")
View(bank)
dim(bank)      
str(bank)      


# A- Create a visual for representing missing values in the dataset.

library(psych) 
psych::describe(bank)
install.packages("VIM")
library(VIM)

missing <- bank
missing[missing == "unknown"] <- NA
aggr(missing, col=c('blue', 'red'),
     numbers=TRUE, sortvars= TRUE,
     labels=names(missing), cex.axis=0.5,
     gap=3, ylab=c("missing data","pattern"))

sapply(missing, function(x) sum(is.na(x)))



# B -Show a distribution of clients based on a Job.

t <- table(bank$job)

t    

title <- barplot(t, xlab = "Job", ylab = "Numbers", main = "Clients based on Job",
                 col = heat.colors(12), las=3)
text(title, 0, t, pos = 3, srt = 90)


# C- Check whether is there any relation between Job and Marital Status?
chisq.test(missing$job, missing$marital)
# Answer - There is NO association 


# d. Check whether is there any association between Job and Education?
chisq.test(missing$job, missing$education)
# Answer -  There is NO association 

#VereDias