library(survival)
library(nnet)
library(bayesm)
library(ggplot2)
data(margarine)

#Exercise 1
cat(" Means of Prices", fill=TRUE)
mat=apply(as.matrix(margarine$choicePrice[,3:12]), 2, mean)
print(mat)

cat(" Quantiles of choice price Variables", fill=TRUE)
mat=apply(as.matrix(margarine$choicePrice[,2:8]), 2, quantile)
print(mat)

# get the column names
cols <-colnames(margarine$choicePrice)[3:12]

#exercise 1
#plot the distribution of all variables

for( i in 3:12){
  p <- ggplot(margarine$choicePrice, aes_string(x=colnames(margarine$choicePrice)[i] )) + 
    geom_density()
  
  nRows = length(margarine$choicePrice[,i])
  avg = sum(margarine$choicePrice[,i]) / nRows
  p <- p+ geom_vline(aes(xintercept=avg),
                     color="red", linetype="dashed", size=1)
  print(p)
}

#Claculate the Market Share
mat=apply(as.matrix(margarine$choicePrice[,3:12]), 2, sum)

totalShare = sum(mat)

marketSharePer = mat / totalShare

share_df <- data.frame(share = marketSharePer, column = colnames(margarine$choicePrice)[3:12] )

bp<- ggplot(share_df, aes(x=column, y=share,fill=column))+
  geom_bar(width = 0.5, stat = "identity")+ coord_polar("y")

bp

#Correlation Plot
# Correlations with significance levels
library(Hmisc)
correlation<- rcorr(as.matrix(margarine$choicePrice), type="spearman") 
# type can be pearson or spearman

#mtcars is a data frame
correlation$r[2,] <- sort(correlation$r[2,], decreasing = TRUE)

barplot(correlation$r[2,],horiz = TRUE,col = "darkred")

#Exericise 2

#Model 1
#For the first model, we will use a conditional Logistic Regression
library(caret)

margarine$choicePrice$choice <- as.factor(margarine$choicePrice$choice)
margarine$choicePrice$choice2 = relevel(margarine$choicePrice$choice,ref="5")

resp <- levels(margarine$choicePrice$choice)
n <- nrow(margarine$choicePrice)
indx <- rep(1:n, length(resp))
logan2 <- data.frame(margarine$choicePrice[indx,],
                     id = indx,
                     tocc = factor(rep(resp, each=n)))
logan2$case <- (margarine$choicePrice$choice == logan2$tocc)
logan2 <- logan2[order(logan2$id),]
logan2[,c(3:12)] <- apply(logan2[,c(3:12)],2,as.numeric)

res.clogit <- clogit(case ~ tocc + PPk_Stk+PBB_Stk+PFl_Stk+PHse_Stk+PGen_Stk+PImp_Stk+PSS_Tub+PPk_Tub+PFl_Tub+PHse_Tub + strata(id), logan2)
summ.clogit <- summary(res.clogit)
summ.clogit

#Just like binary logistic regression, we need to convert the coefficients to odds by taking the exponential of the coefficients
exp(coef(res.clogit))

#Combine the demos dataset with the choice price dataset

hhID =  levels(as.factor(margarine$demos$hhid))
cp_df <- margarine$choicePrice
demos <- margarine$demos

print(length(cp_df$choice))
z = c()
for (i in margarine$choicePrice$hhid){
  z <- c(z,demos[demos[,1] == i,"Income"])
}

cp_df$Income <- z
length(cp_df$choice)

#Exercise 3
#Model 2 (Multinomial Logit from nnet package)
#split the data into the train and test sets

index <- createDataPartition( cp_df$choice, p = .8, list = FALSE)
train_2 <-  cp_df[index,]
test_2 <-  cp_df[-index,]

multinom_model_2 <- multinom("choice ~ Income", data = train_2)

# statistical summary of the model
summary(multinom_model_2)

exp(coef(multinom_model_2))

# Predicting the values for test dataset
test_2$ClassPredicted <- predict(multinom_model_2, newdata = test_2, "class")
# Building classification table
tab_2 <- table(test_2$choice, test_2$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab_2))/sum(tab_2))*100,2)

#Exercise 4
library("margins")
library(sjPlot)
#model 1
plot_model(res.clogit, type = "pred", terms = "tocc")
#model2
plot_model(multinom_model_2, type = "pred", terms = "Income")
plot_model(multinom_model_2, type = "eff", terms = "Income")

#Exercise 5
library(pscl) # McFadden's R2
clogit.model1<- multinom(cp_df$choice ~ PPk_Stk+PBB_Stk+PFl_Stk+PHse_Stk+PGen_Stk+PImp_Stk+PSS_Tub+PPk_Tub+PFl_Tub+PHse_Tub, data = cp_df)
summary(clogit.model1)
pR2(clogit.model1)
mlogit.model2<- multinom(cp_df$choice ~ PPk_Stk+PBB_Stk+PFl_Stk+PHse_Stk+PGen_Stk+PImp_Stk+PSS_Tub+PPk_Tub+PFl_Tub+PHse_Tub|Income, data =cp_df)
summary(mlogit.model2)
pR2(mlogit.model2)
