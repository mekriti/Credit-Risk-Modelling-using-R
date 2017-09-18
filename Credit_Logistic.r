data1<- readRDS("Loandata.rds") #reading data
head(data1) #reading the first few lines off the dataset
traindata<- sample(data1,0.75*nrow(data1))#preparing training data  
testdata<-sample(data1,-.75*nrow(data1))#preparing test data

#model 1 with loan amount, interest rate, annual income, age
result<-glm(formula=loan_status~loan_amnt+int_rate+annual_inc+age,family="binomial",data=traindata)
summary(result)

#model 2 with loan amount, interest amount, annual income, age and home ownership
result1<-glm(formula=loan_status~loan_amnt+int_rate+annual_inc+age+home_ownership,family="binomial",data=traindata)
summary(result1)

#model 3 with loan amount, interest rate, grade, employment length, annual income, age, home ownership 
result2<-glm(loan_status~.,family="binomial",data=traindata)
summary (result2)
#Least residual deviance

#predicting the result on test data

pred1<-predict(result,testdata,type="response")
pred2<-predict(result1,testdata,type="response")
pred<-predict(result2,testdata,type="response")

#Varying cut off for the best predictor on the model with least residual deviance

 #at if value below .15 then it is declined else excepted
cutoff1<-ifelse(pred>.15,1,0)

#at if value below .2 then it is declined else excepted
cutoff2<-ifelse(pred>.2,1,0)

#at if value below .25 then it is declined else excepted
cutoff3<-ifelse(pred>.25,1,0)

#confusion matrix to show Type 1 and 2 errors
confmat1<-table(testdata$loan_status,cutoff1)
confmat1

confmat2<-table(testdata$loan_status,cutoff2)
confmat2

confmat3<-table(testdata$loan_status,cutoff3)
confmat3


#checking accuracy of different models
logit1<-sum(diag(confmat1))/nrow(testdata)
logit1
logit2<-sum(diag(confmat2))/nrow(testdata)
logit2
logit3<-sum(diag(confmat3))/nrow(testdata)
logit3

R CODE OUTPUT
summary(result)

Call:
glm(formula = loan_status ~ loan_amnt + int_rate + annual_inc + 
    age, family = "binomial", data = traindata)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.0794  -0.5334  -0.4331  -0.3421   3.7236  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -3.265e+00  1.400e-01 -23.318   <2e-16 ***
loan_amnt    1.762e-07  4.127e-06   0.043    0.966    
int_rate     1.517e-01  7.257e-03  20.902   <2e-16 ***
annual_inc  -6.935e-06  7.700e-07  -9.005   <2e-16 ***
age         -5.271e-03  3.843e-03  -1.372    0.170    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 13800  on 19775  degrees of freedom
Residual deviance: 13226  on 19771  degrees of freedom
  (2043 observations deleted due to missingness)
AIC: 13236

Number of Fisher Scoring iterations: 5

 


summary(result1)


Call:
glm(formula = loan_status ~ loan_amnt + int_rate + annual_inc + 
    age + home_ownership, family = "binomial", data = traindata)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.0816  -0.5339  -0.4321  -0.3420   3.7963  

Coefficients:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)         -3.217e+00  1.442e-01 -22.311   <2e-16 ***
loan_amnt           -2.785e-08  4.133e-06  -0.007   0.9946    
int_rate             1.527e-01  7.329e-03  20.837   <2e-16 ***
annual_inc          -7.265e-06  8.070e-07  -9.002   <2e-16 ***
age                 -5.120e-03  3.843e-03  -1.332   0.1828    
home_ownershipOTHER  6.196e-01  3.072e-01   2.017   0.0437 *  
home_ownershipOWN   -1.487e-01  9.310e-02  -1.597   0.1103    
home_ownershipRENT  -6.259e-02  5.185e-02  -1.207   0.2274    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 13800  on 19775  degrees of freedom
Residual deviance: 13219  on 19768  degrees of freedom
  (2043 observations deleted due to missingness)
AIC: 13235

Number of Fisher Scoring iterations: 5

 


summary(result2)
Call:
glm(formula = loan_status ~ ., family = "binomial", data = traindata)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.0905  -0.5315  -0.4312  -0.3321   3.7253  

Coefficients:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)         -2.830e+00  2.166e-01 -13.066  < 2e-16 ***
loan_amnt            2.691e-07  4.230e-06   0.064 0.949276    
int_rate             8.519e-02  2.314e-02   3.681 0.000232 ***
gradeB               3.390e-01  1.092e-01   3.104 0.001909 ** 
gradeC               5.366e-01  1.581e-01   3.394 0.000688 ***
gradeD               6.203e-01  2.010e-01   3.086 0.002031 ** 
gradeE               7.253e-01  2.507e-01   2.893 0.003819 ** 
gradeF               9.959e-01  3.345e-01   2.977 0.002911 ** 
gradeG               1.192e+00  4.401e-01   2.707 0.006783 ** 
emp_length           3.406e-03  3.718e-03   0.916 0.359671    
home_ownershipOTHER  6.501e-01  3.085e-01   2.107 0.035129 *  
home_ownershipOWN   -1.740e-01  9.798e-02  -1.776 0.075728 .  
home_ownershipRENT  -5.825e-02  5.383e-02  -1.082 0.279175    
annual_inc          -6.929e-06  8.191e-07  -8.460  < 2e-16 ***
age                 -6.457e-03  3.963e-03  -1.629 0.103211    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 13214  on 19201  degrees of freedom
Residual deviance: 12637  on 19187  degrees of freedom
  (2617 observations deleted due to missingness)
AIC: 12667

Number of Fisher Scoring iterations: 5

 

We then set different cutoff to know which loan application to be denied and which to be accepted
confmat1
   cutoff1 
       0    1
  0 4494 1173
  1  446  256
confmat2
   cutoff2
       0    1
  0 5363  304
  1  614   88
confmat3
   cutoff3
       0    1
  0 5605   62
  1  674   28

 

Here, cutoff1=.15, cutoff2=.20 and cutoff3=.25
Accuracy at different cutoff were
logit1
[1] 0.6531005
logit2
[1] 0.7494844
logit3
[1] 0.7745085
*
