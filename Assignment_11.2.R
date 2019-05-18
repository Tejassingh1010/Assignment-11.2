
str(bank.additional.full)
bank<-bank.additional.full
#Ques.1.a. Is there any association between Job and default?

bank$default<-as.numeric(bank$default)
bank$job<-as.numeric(bank$job)
cor.test(bank$job,bank$default)
cor(bank$job,bank$default)

#Ques.1.b.Is there any significant difference in duration of last call between people having housing loan 
            #or not?
avona<-aov(bank$duration~bank$loan)
summary(avona)
TukeyHSD(avona)
#(all the housing loan is significantly different from the duratiom of  call)


#Ques.1.c. Is there any association between consumer price index and job?

cor.test(bank$cons.price.idx,bank$job)
#(there is no association between the consumer price index and job.)

#Ques.1.d.Is the employment variation rate consistent across job types?

chisq.test(bank$job ,bank$emp.var.rate)
bank$emp.var.rate<-as.factor(bank$emp.var.rate)
bank$job<-as.factor(bank$job)
plot(bank$emp.var.rate~bank$job)

#Ques.1.e. Is the employment variation rate same across education?

library(car)
bank$education<-as.numeric(bank$education)
bank$emp.var.rate<-as.numeric(bank$emp.var.rate)
scatterplot(bank$education,bank$emp.var.rate)

#the employment variation rate  is not same across education

#Ques.1.f. Which group is more confident?

#did not get the question.sorry sir