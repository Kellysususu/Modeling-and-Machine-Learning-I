    library(randomForest)

    ## Warning: package 'randomForest' was built under R version 4.1.3

    ## randomForest 4.7-1

    ## Type rfNews() to see new features/changes/bug fixes.

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     combine

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(caret)

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     margin

    ## Loading required package: lattice

    library(ranger)

    ## Warning: package 'ranger' was built under R version 4.1.3

    ## 
    ## Attaching package: 'ranger'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     importance

    data <- 
      read.csv(url(
        'https://hastie.su.domains/ElemStatLearn/datasets/vowel.train'))
    vowel_train<-data[,2:ncol(data)]

## 1. Convert the response variable in the “vowel.train” data frame to a factor variable

    vowel_train$y<-as.factor(vowel_train$y)

## 2. Review random forest function

    ?randomForest

    ## starting httpd help server ... done

## 3. Fit randomforest model

    rf_fit<-randomForest(y~.,data=vowel_train)

## 4. Use 5-fold CV and tune the model

    # Set testing parameters
    mtry<-c(3,4,5)
    nodesize<-c(1,5,10,20,40,80)

    # Create 5 folds for the train control function
    control<-trainControl(method="cv",number=5,search="grid")


    set.seed(123)

    # Customizing the tuning grid
    grid<-expand.grid(.mtry=mtry,.min.node.size=nodesize,.splitrule='gini')
    fit<-train(y~.,data=vowel_train,trControl=control,method="ranger",tuneGrid=grid)

    # Transforming parameters into data-frame
    fit<-as.data.frame(fit[4])
    fit %>%
      arrange(desc(results.Accuracy))

    ##    results.mtry results.min.node.size results.splitrule results.Accuracy
    ## 1             3                     1              gini        0.9622744
    ## 2             3                     5              gini        0.9603347
    ## 3             4                     1              gini        0.9584466
    ## 4             5                     1              gini        0.9546201
    ## 5             4                     5              gini        0.9510226
    ## 6             3                    10              gini        0.9452717
    ## 7             5                     5              gini        0.9395041
    ## 8             4                    10              gini        0.9338424
    ## 9             5                    10              gini        0.9319023
    ## 10            4                    20              gini        0.8976655
    ## 11            3                    20              gini        0.8957770
    ## 12            5                    20              gini        0.8824267
    ## 13            3                    40              gini        0.7686809
    ## 14            4                    40              gini        0.7497207
    ## 15            5                    40              gini        0.7399992
    ## 16            3                    80              gini        0.6342126
    ## 17            4                    80              gini        0.5943516
    ## 18            5                    80              gini        0.5942804
    ##    results.Kappa results.AccuracySD results.KappaSD
    ## 1      0.9584870         0.03590832      0.03952023
    ## 2      0.9563508         0.03093629      0.03405268
    ## 3      0.9542748         0.03237776      0.03563578
    ## 4      0.9500631         0.02624687      0.02889216
    ## 5      0.9461028         0.03694596      0.04065992
    ## 6      0.9397775         0.04166376      0.04584865
    ## 7      0.9334320         0.03809561      0.04192524
    ## 8      0.9271999         0.03175696      0.03495282
    ## 9      0.9250696         0.03157201      0.03474529
    ## 10     0.8874126         0.04017195      0.04420563
    ## 11     0.8853258         0.04476628      0.04926216
    ## 12     0.8706410         0.05192838      0.05713139
    ## 13     0.7455268         0.04552480      0.05002296
    ## 14     0.7246274         0.06018541      0.06616738
    ## 15     0.7139690         0.07125586      0.07829280
    ## 16     0.5976424         0.05578027      0.06122360
    ## 17     0.5537984         0.05905490      0.06472918
    ## 18     0.5537183         0.06230310      0.06826623

    # Using highest accuracy parameter to fit random forest
    rf_fit1<-randomForest(y~.,data=vowel_train,nodesize=1,mtry=3)

## 5. With the tuned model, make predictions using the majority vote method, and compute the misclassification rate using the ‘vowel.test’ data.

    # Load test data 
    test_data<-read.csv(url(
        'https://hastie.su.domains/ElemStatLearn/datasets/vowel.test'))
    vowel_test<-test_data[,2:ncol(test_data)]

    # Convert response variable to factor
    test_y<-as.factor(vowel_test$y)

    # Use tuned model to make predictions
    pred<-predict(rf_fit1,newdata = vowel_test)

    # Calculate misclassificiation rate
    matrix<-confusionMatrix(test_y,pred)

    accuracy<-as.data.frame(matrix[3])[1,]

    mis_err<-1-accuracy

    mis_err

    ## [1] 0.4004329

    warnings()

The mis-classification rate is 40.48%
