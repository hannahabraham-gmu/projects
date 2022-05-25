STAT515\_Final
================
Hannah Abraham
4/27/2022

``` r
library(R.matlab)
```

    ## Warning: package 'R.matlab' was built under R version 4.1.3

    ## R.matlab v3.6.2 (2018-09-26) successfully loaded. See ?R.matlab for help.

    ## 
    ## Attaching package: 'R.matlab'

    ## The following objects are masked from 'package:base':
    ## 
    ##     getOption, isOpen

``` r
library(ggplot2)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.2

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
bdata <- readMat("data_random.m", header = TRUE)

# transforming bc data to data frame 
bdata <- data.frame(bdata)

# inserting headers to dataset 
colnames(bdata) <- c("age", "bmi", "glucose", "insulin", "HOMA", "leptin", "adiponectin", "resistin", "MCP-1", "class")

#conducting further data cleaning... 
bdata <- transform(bdata, age=as.integer(age), class=as.factor(class))

#no NAs in dataset 
sum(is.na(bdata))
```

    ## [1] 0

## ggplot Visualization

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.1.3

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
#bar graph representation on the number of benign and malignant breast cancer in our data set 
ggplot(data = bdata, aes(class))+
  geom_bar(color="darkblue", fill="pink", width=0.3) +
  labs(title = "Number of Benign and Malignant Breast Cancer", x = " ", caption="1 indicates benign, 2 indicates malignant") + theme(plot.caption = element_text(hjust=0, face = "italic", size=12))
```

![](Final_Project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
pairs.panels(bdata)
```

![](Final_Project_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

## Using PCA to visualize our dataset

``` r
library(stats)
library(ggfortify)
```

    ## Warning: package 'ggfortify' was built under R version 4.1.3

``` r
bPCA<- prcomp(bdata[,-10],scale.=TRUE)

plot(bPCA)
```

![](Final_Project_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
autoplot(bPCA, data=bdata, colour='class', loadings=TRUE,loadings.label=TRUE)
```

![](Final_Project_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
autoplot(bPCA, data=bdata, colour='class')
```

![](Final_Project_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

## Correlation matrix

``` r
library(corrplot)
```

    ## corrplot 0.91 loaded

``` r
# visualizating to see if there's a correlation in our data 
correlations <- cor(bdata[,1:9])
corrplot(correlations, method="circle")
```

![](Final_Project_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# variables insulin and HOMA are highly correlated, so we're going to evaluate this, and if needed, remove a variable if it's considered significant to our model. 
```

## Creating training and test datasets

``` r
library(ISLR)
```

    ## Warning: package 'ISLR' was built under R version 4.1.3

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(tree)
```

    ## Warning: package 'tree' was built under R version 4.1.3

    ## Registered S3 method overwritten by 'tree':
    ##   method     from
    ##   print.tree cli

``` r
library(rpart)
library(rpart.plot)
```

    ## Warning: package 'rpart.plot' was built under R version 4.1.3

``` r
set.seed(13)

train=sample(c(TRUE,FALSE),size=nrow(bdata),prob=c(0.8,0.2),replace=TRUE)
test = bdata[-train,]
class.test=bdata$class[-train]

#fitting training data into the classification tree
tree.bdata=tree(class~., bdata, subset=train)

summary(tree.bdata)
```

    ## 
    ## Classification tree:
    ## tree(formula = class ~ ., data = bdata, subset = train)
    ## Variables actually used in tree construction:
    ## [1] "glucose"     "age"         "bmi"         "resistin"    "MCP.1"      
    ## [6] "adiponectin"
    ## Number of terminal nodes:  11 
    ## Residual mean deviance:  0.406 = 34.51 / 85 
    ## Misclassification error rate: 0.09375 = 9 / 96

``` r
#fitting test data into classification tree 

tree.pred=predict(tree.bdata, test, type="class")

confusionMatrix(tree.pred, class.test)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  1  2
    ##          1 41  6
    ##          2 10 58
    ##                                           
    ##                Accuracy : 0.8609          
    ##                  95% CI : (0.7839, 0.9183)
    ##     No Information Rate : 0.5565          
    ##     P-Value [Acc > NIR] : 2.653e-12       
    ##                                           
    ##                   Kappa : 0.7159          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.4533          
    ##                                           
    ##             Sensitivity : 0.8039          
    ##             Specificity : 0.9062          
    ##          Pos Pred Value : 0.8723          
    ##          Neg Pred Value : 0.8529          
    ##              Prevalence : 0.4435          
    ##          Detection Rate : 0.3565          
    ##    Detection Prevalence : 0.4087          
    ##       Balanced Accuracy : 0.8551          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
# calculating the misclassification rate 
mean(tree.pred!=class.test)
```

    ## [1] 0.1391304

``` r
# misclassification rate is 13.91% 

#false negative rate 
fn.tree<-10/(41+10)
fn.tree
```

    ## [1] 0.1960784

``` r
plot(tree.bdata)
text(tree.bdata, pretty=0)
```

![](Final_Project_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Tree pruning

``` r
set.seed(10)

cv.class=cv.tree(tree.bdata, FUN=prune.misclass)


plot(cv.class$size,cv.class$dev,type="b")
```

![](Final_Project_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
cv.class
```

    ## $size
    ## [1] 11  9  8  6  5  4  2  1
    ## 
    ## $dev
    ## [1] 34 32 34 38 37 41 38 51
    ## 
    ## $k
    ## [1] -Inf  0.0  1.0  2.0  3.0  4.0  4.5 13.0
    ## 
    ## $method
    ## [1] "misclass"
    ## 
    ## attr(,"class")
    ## [1] "prune"         "tree.sequence"

``` r
#we're going to select the best 9 nodes, since it has the lowest cross-validation error rate at 32, even though the cross-validation 
#error rate for our original tree (11 nodes) is 34, which isn't much of a difference 

prune.bdata=prune.misclass(tree.bdata, best=9)
plot(prune.bdata)
text(prune.bdata, pretty=0)
```

![](Final_Project_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
prune.predict=predict(prune.bdata, test, type="class")
confusionMatrix(prune.predict, class.test)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  1  2
    ##          1 41  6
    ##          2 10 58
    ##                                           
    ##                Accuracy : 0.8609          
    ##                  95% CI : (0.7839, 0.9183)
    ##     No Information Rate : 0.5565          
    ##     P-Value [Acc > NIR] : 2.653e-12       
    ##                                           
    ##                   Kappa : 0.7159          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.4533          
    ##                                           
    ##             Sensitivity : 0.8039          
    ##             Specificity : 0.9062          
    ##          Pos Pred Value : 0.8723          
    ##          Neg Pred Value : 0.8529          
    ##              Prevalence : 0.4435          
    ##          Detection Rate : 0.3565          
    ##    Detection Prevalence : 0.4087          
    ##       Balanced Accuracy : 0.8551          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
# there is no improvement in accuracy since there isn't much of a difference in error rate between tree with 11 nodes or 9 nodes
```

## Random forest

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 4.1.3

    ## randomForest 4.7-1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:psych':
    ## 
    ##     outlier

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
set.seed(1)

rf <- randomForest(class~.,data=bdata, subset=train, mtry=2)
rf
```

    ## 
    ## Call:
    ##  randomForest(formula = class ~ ., data = bdata, mtry = 2, subset = train) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 2
    ## 
    ##         OOB estimate of  error rate: 28.12%
    ## Confusion matrix:
    ##    1  2 class.error
    ## 1 28 15   0.3488372
    ## 2 12 41   0.2264151

``` r
pred = predict(rf, test, type = "class")
confusionMatrix(pred,test$class)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  1  2
    ##          1 48  3
    ##          2  3 61
    ##                                           
    ##                Accuracy : 0.9478          
    ##                  95% CI : (0.8899, 0.9806)
    ##     No Information Rate : 0.5565          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.8943          
    ##                                           
    ##  Mcnemar's Test P-Value : 1               
    ##                                           
    ##             Sensitivity : 0.9412          
    ##             Specificity : 0.9531          
    ##          Pos Pred Value : 0.9412          
    ##          Neg Pred Value : 0.9531          
    ##              Prevalence : 0.4435          
    ##          Detection Rate : 0.4174          
    ##    Detection Prevalence : 0.4435          
    ##       Balanced Accuracy : 0.9472          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
accuracy.rf<-(8+17)/(8+4+2+17)

misclass.rf<-(1-accuracy.rf)
misclass.rf
```

    ## [1] 0.1935484

``` r
#false negative rate 
fn.rf<-3/(48+3)
fn.rf
```

    ## [1] 0.05882353

``` r
#variable importance: most important variable to our prediction is glucose 

importance(rf)
```

    ##             MeanDecreaseGini
    ## age                 6.139397
    ## bmi                 6.387732
    ## glucose             7.655374
    ## insulin             3.969776
    ## HOMA                4.429780
    ## leptin              3.814901
    ## adiponectin         4.538623
    ## resistin            6.370766
    ## MCP.1               3.637569

``` r
varImpPlot(rf)
```

![](Final_Project_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Conducting boxTidwell test to check for linearity between logodds and the predictor variable

\#this is used to assess whether logistic regression is suitable model
for this dataset

``` r
library(car)
```

    ## Warning: package 'car' was built under R version 4.1.3

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:psych':
    ## 
    ##     logit

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
bcfit <- glm(class~. , data=bdata, family=binomial(link="logit"))
logodds<-bcfit$linear.predictors

boxTidwell(logodds~glucose+bmi+resistin+insulin+HOMA+age+leptin+adiponectin+MCP.1, data=bdata)
```

    ##             MLE of lambda Score Statistic (z) Pr(>|z|)  
    ## glucose                 1              0.1510  0.87996  
    ## bmi                     1             -0.3923  0.69486  
    ## resistin                1              0.3331  0.73904  
    ## insulin                 1              0.2296  0.81843  
    ## HOMA                    1             -0.1548  0.87694  
    ## age                     1             -1.1397  0.25440  
    ## leptin                  1             -1.7857  0.07414 .
    ## adiponectin             1             -1.0390  0.29881  
    ## MCP.1                   1             -0.1875  0.85130  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## iterations =  0

## Logistic Regression

``` r
### Model fit 
# fitting the all the variables to see which ones are the most significant to our model 
glm.full=glm(class~., data = bdata, family = "binomial")

summary(glm.full)
```

    ## 
    ## Call:
    ## glm(formula = class ~ ., family = "binomial", data = bdata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.2992  -0.8548   0.1847   0.7429   2.1632  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept) -5.6512154  3.3580998  -1.683  0.09240 . 
    ## age         -0.0233524  0.0156230  -1.495  0.13498   
    ## bmi         -0.1501231  0.0674938  -2.224  0.02613 * 
    ## glucose      0.1055941  0.0348082   3.034  0.00242 **
    ## insulin      0.2071782  0.2629802   0.788  0.43081   
    ## HOMA        -0.5978147  1.0898156  -0.549  0.58332   
    ## leptin      -0.0101709  0.0172662  -0.589  0.55582   
    ## adiponectin -0.0052619  0.0375568  -0.140  0.88858   
    ## resistin     0.0585546  0.0298523   1.961  0.04982 * 
    ## MCP.1        0.0006975  0.0008068   0.865  0.38730   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 159.57  on 115  degrees of freedom
    ## Residual deviance: 111.73  on 106  degrees of freedom
    ## AIC: 131.73
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
# fitting model with variables, glucose, bmi and resistin 
glm.model=glm(class~ glucose + bmi + resistin, data = bdata, family = "binomial" )

# p-values for each regression coefficient is statically significant 
summary(glm.model)
```

    ## 
    ## Call:
    ## glm(formula = class ~ glucose + bmi + resistin, family = "binomial", 
    ##     data = bdata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0464  -0.8604   0.2193   0.8738   1.9186  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -5.28518    2.03459  -2.598  0.00939 ** 
    ## glucose      0.08676    0.02061   4.210 2.56e-05 ***
    ## bmi         -0.13144    0.04675  -2.812  0.00493 ** 
    ## resistin     0.07023    0.03051   2.302  0.02135 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 159.57  on 115  degrees of freedom
    ## Residual deviance: 119.50  on 112  degrees of freedom
    ## AIC: 127.5
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# running chi-squared with anova function to compare the first and second model 

anova(glm.full, glm.model, test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: class ~ age + bmi + glucose + insulin + HOMA + leptin + adiponectin + 
    ##     resistin + MCP.1
    ## Model 2: class ~ glucose + bmi + resistin
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1       106     111.73                     
    ## 2       112     119.50 -6  -7.7729   0.2552

``` r
# our second model with 3 predictors fit as well as our first model with all the variables, which means that insulin, HOMA, leptin, adiponectin and MCP.1 doesn't add any contribution to predict if breast cancer if benign or malignant. We're going to use the second model for simplier interpretation 
```

``` r
glm.prob=predict(glm.model, type="response")

glm.pred=rep("No", nrow(bdata))
glm.pred[glm.prob>.5]="Yes"

table(glm.pred, bdata$class)
```

    ##         
    ## glm.pred  1  2
    ##      No  39 18
    ##      Yes 13 46

``` r
#calculating the accuracy of our predicted model
accuracy<-(39+46)/113

accuracy
```

    ## [1] 0.7522124

``` r
#misclassification rate 

misclassification <- (1-accuracy)
misclassification
```

    ## [1] 0.2477876

### Fitting model with 80% training data, 20% test data

``` r
set.seed(5)

train=sample(c(TRUE, FALSE), size=nrow(bdata), prob = c(0.8,0.2), replace=TRUE)
test=bdata[!train,]

model=glm(class~glucose + bmi + resistin, data=bdata, family="binomial", subset = train)
test.predicted=predict(model, test, type="response")

glm.predict=rep(1, nrow(test))

glm.predict[test.predicted>.5]=2

glm.predict<-as.factor(glm.predict)

str(glm.predict)
```

    ##  Factor w/ 2 levels "1","2": 1 1 2 1 2 2 1 2 1 2 ...

``` r
#model accuracy 
confusionMatrix(glm.predict, test$class)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  1  2
    ##          1  8  5
    ##          2  2 16
    ##                                          
    ##                Accuracy : 0.7742         
    ##                  95% CI : (0.589, 0.9041)
    ##     No Information Rate : 0.6774         
    ##     P-Value [Acc > NIR] : 0.1688         
    ##                                          
    ##                   Kappa : 0.521          
    ##                                          
    ##  Mcnemar's Test P-Value : 0.4497         
    ##                                          
    ##             Sensitivity : 0.8000         
    ##             Specificity : 0.7619         
    ##          Pos Pred Value : 0.6154         
    ##          Neg Pred Value : 0.8889         
    ##              Prevalence : 0.3226         
    ##          Detection Rate : 0.2581         
    ##    Detection Prevalence : 0.4194         
    ##       Balanced Accuracy : 0.7810         
    ##                                          
    ##        'Positive' Class : 1              
    ## 

``` r
table(glm.predict, test$class)
```

    ##            
    ## glm.predict  1  2
    ##           1  8  5
    ##           2  2 16

``` r
model.accuracy<- (8+16)/(8+16+5+2)

misclass.model<-(1-model.accuracy)

misclass.model
```

    ## [1] 0.2258065

``` r
#false negative rate 
glm.fn<-2/(8+2)
glm.fn
```

    ## [1] 0.2

## Splitting the Data

``` r
set.seed(13)

X=sample(c(TRUE,FALSE),size=nrow(bdata),prob=c(0.8,0.2),replace=TRUE)
train = bdata[X,]
test = bdata[-X,]
true.test =bdata$class[-X]
```

## kNN algorithm

### Optimal K Value:

``` r
library(class)
#First Attempt to Determine Right K####
bdata_acc<-rep(NA,50)

for(i in 1:50){
  #Apply knn with k = i
  predict<-knn(train[,-9],test[,-9], train$class,k=i)
  bdata_acc[i]<-mean(predict==true.test)
}
#Plot k= 1 through 50
plot(1-bdata_acc,type="l",ylab="Error Rate",
     xlab="K",main="Error Rate for Iris With Varying K")
```

![](Final_Project_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#Try many Samples of Iris Data Set to Validate K####
trial_sum<-rep(0,20)
trial_n<-rep(0,20)
set.seed(279)
for(i in 1:100){
  b_sample<-sample(1:nrow(bdata),size=nrow(bdata)*.8)
  b_train<-bdata[b_sample,]
  b_test<-bdata[-b_sample,]
  test_size<-nrow(b_test)
  for(j in 1:20){
    predict<-knn(b_train[,-9],b_test[,-9],
                 b_train$class,k=j)
    trial_sum[j]<-trial_sum[j]+sum(predict==b_test$class)
    trial_n[j]<-trial_n[j]+test_size
  }
}

plot(1-trial_sum / trial_n,type="l",ylab="Error Rate",
     xlab="K",main="Error Rate With Varying K")
```

![](Final_Project_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->
\#\#\#\# We can see that the error rate is lowest at around k = 9, which
is very close to the optimal k value rule (sqrt(\# of data points))

### Creating kNN Model

``` r
#plot(bdata[,1:9],col=bdata$class)

bdata_pred<-knn(train[,-9],test[,-9],
            cl = train$class,k=9)
```

### Confusion Matrix

``` r
library(caret)
confusionMatrix(bdata_pred,true.test)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  1  2
    ##          1 39 13
    ##          2 12 51
    ##                                          
    ##                Accuracy : 0.7826         
    ##                  95% CI : (0.696, 0.8541)
    ##     No Information Rate : 0.5565         
    ##     P-Value [Acc > NIR] : 3.522e-07      
    ##                                          
    ##                   Kappa : 0.5605         
    ##                                          
    ##  Mcnemar's Test P-Value : 1              
    ##                                          
    ##             Sensitivity : 0.7647         
    ##             Specificity : 0.7969         
    ##          Pos Pred Value : 0.7500         
    ##          Neg Pred Value : 0.8095         
    ##              Prevalence : 0.4435         
    ##          Detection Rate : 0.3391         
    ##    Detection Prevalence : 0.4522         
    ##       Balanced Accuracy : 0.7808         
    ##                                          
    ##        'Positive' Class : 1              
    ## 

### Misclassification Error

``` r
mean(bdata_pred!=true.test)
```

    ## [1] 0.2173913

Our misclassification error is around 20%.

## SVM

``` r
library(e1071)
svmfit = svm(class ~., data = train, kernel = "radial")
print(svmfit)
```

    ## 
    ## Call:
    ## svm(formula = class ~ ., data = train, kernel = "radial")
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  C-classification 
    ##  SVM-Kernel:  radial 
    ##        cost:  1 
    ## 
    ## Number of Support Vectors:  75

``` r
svm.predicted = predict(svmfit,test)
```

### Confusion Matrix

``` r
confusionMatrix(svm.predicted,true.test)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  1  2
    ##          1 44 10
    ##          2  7 54
    ##                                           
    ##                Accuracy : 0.8522          
    ##                  95% CI : (0.7739, 0.9115)
    ##     No Information Rate : 0.5565          
    ##     P-Value [Acc > NIR] : 1.253e-11       
    ##                                           
    ##                   Kappa : 0.7023          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.6276          
    ##                                           
    ##             Sensitivity : 0.8627          
    ##             Specificity : 0.8438          
    ##          Pos Pred Value : 0.8148          
    ##          Neg Pred Value : 0.8852          
    ##              Prevalence : 0.4435          
    ##          Detection Rate : 0.3826          
    ##    Detection Prevalence : 0.4696          
    ##       Balanced Accuracy : 0.8532          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

### Misclassification Error

``` r
mean(svm.predicted!=true.test)
```

    ## [1] 0.1478261
