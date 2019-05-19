# 1. Use the given link below and locate the bank marketing dataset. Data Set Link
# Perform the below operations:

# a. Is there any association between Job and default?

chisq.test(bank$job ,bank$default)

# data:  bank$job and bank$default
# X-squared = 9.3064, df = 11, p-value = 0.5936
# as p-value is > 0.05 there is no association between job and default

# b. Is there any significant difference in duration of last call between 
#    people having housing loan or not?

library(ggplot2)
bank$duration<-as.numeric(bank$duration)
ggplot(bank, aes(x=bank$duration, fill=bank$loan))+geom_histogram()
ggplot(bank, aes(x=bank$loan,y=bank$duration, fill=bank$loan))+geom_point()

# As per the plots it is visible that duration taken is more fpr customers without loan.

# c. Is there any association between consumer price index and consumer?

# not able to find consumer price index and consumer variable considering Bank balance and Loan association

ggplot(bank, aes(x=bank$balance, y= bank$loan, fill=bank$loan))+geom_point()

# as the geom_points are more crouded towards low balance account, 
# we can conclude that lower balance account may tend to the possibility that customer will take loan

# d. Is the employment variation rate consistent across job types?

chisq.test(bank.additional$job ,bank.additional$education)

# e. Is the employment variation rate same across education?

ggplot(bank, aes(x=bank$job, y=bank$education))+geom_jitter()

#Employment variation rate is not same as per the above plot
# Higher the education, Higher job profile

# f. Which group is more confident?

# Not covered in class