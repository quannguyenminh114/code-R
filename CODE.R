water_data <- read.csv("C:/Users/Minh Quan/Desktop/BTL MT2013/water_potability.csv")
head(water_data,10) 
str(water_data) 

library(questionr)
freq.na(water_data)

png("TXL_Hist & Boxplot.png", width = 2400, height = 1500, res = 300)
par(mfrow=c(2,3))
hist(water_data$ph,xlab="ph",main="Histogram of ph",col="red")
hist(water_data$Sulfate,xlab="Sulfate",main="Histogram of Sulfate",col="green")
hist(water_data$Trihalomethanes,xlab="Trihalomethanes",main="Histogram of Trihalomethanes",col="blue")
boxplot(ph~Potability,data=water_data,
        main="Boxplot of pH for Potability",
        col=c("darkgrey","lightcyan"))
boxplot(Sulfate~Potability,data=water_data,
        main="Boxplot of Sulfate for Potability",
        col=c("darkgrey","lightcyan"))
boxplot(Trihalomethanes~Potability,data=water_data,
        main="Boxplot of Trihalomethanes for Potability",
        col=c("darkgrey","lightcyan"))
dev.off()

water_data[is.na(water_data$ph),"ph"]<-median(water_data$ph,na.rm = TRUE)
water_data[is.na(water_data$Sulfate),"Sulfate"]<-median(water_data$Sulfate,na.rm = TRUE)
water_data[is.na(water_data$Trihalomethanes),"Trihalomethanes"]<-median(water_data$Trihalomethanes,na.rm = TRUE)

library(questionr)
freq.na(water_data)

outlier_summary <- function(x){
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  outliers <- x[x < lower | x > upper]
  n_outlier <- length(outliers)
  total <- length(x)
  ratio <- n_outlier / total
  return(c(Outlier_Count = n_outlier,
           Total = total,
           Outlier_Rate = ratio))}
outlier_table <- t(sapply(water_data[,1:9], outlier_summary))
outlier_table

water_data$Potability<-as.factor(water_data$Potability)



describe_function<-function(x){c(n=length(x),xtb=mean(x),s=sd(x),
                                 min=min(x),max=max(x),
                                 med=median(x),
                                 Q1=quantile(x, 0.25),
                                 Q3=quantile(x, 0.75))}
by(water_data[, 1:9], water_data$Potability, function(df) {
  t(sapply(df, describe_function))
})

table(water_data$Potability)

png("TKMT_Hist.png", width = 1200, height = 900, res = 120)
par(mfrow = c(3, 3), 
    mar = c(3, 3, 2, 1), 
    mgp = c(2, 0.7, 0), 
    cex.main = 1)
diagramdata <- water_data[, 1:9]
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
for (col in names(diagramdata)) {
  group0 <- diagramdata[water_data$Potability == 0, col]
  group1 <- diagramdata[water_data$Potability == 1, col]
  max_val <- max(c(group0, group1))
  min_val <- min(c(group0, group1))
  hist(group0, breaks = 20, col = rgb(1,0,0,0.5), xlim = c(min_val, max_val),
       main = paste("Histogram of", col),
       xlab = col, ylab = "Frequency")
  hist(group1, breaks = 20, col = rgb(0,0,1,0.5), add = TRUE)
  legend("topright", legend = c("Potability=0","Potability=1"),
         fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)),
         cex=0.88, bty = "n")}
dev.off()

png("TKMT_Boxplot.png", width = 4000, height = 2000, res = 200, pointsize = 22)
col_group <- c(rgb(1,0,0,0.5), rgb(0,0,1,0.5))  
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
vars <- c("ph", "Hardness", "Solids", "Chloramines", "Sulfate", 
          "Conductivity", "Organic_carbon", "Trihalomethanes", "Turbidity")
for (var in vars) {
  plot_formula <- as.formula(paste(var, "~ Potability"))
  boxplot(plot_formula, data = water_data,
          main = paste("Boxplot of", var, "for Potability"),
          col = col_group, 
          xlab = "Potability", 
          ylab = var) }
dev.off()

library(ggcorrplot)
cor_matrix <- cor(water_data[,1:9])
png("TKMT_cormatrix.png", width = 4000, height = 2000, res = 300)
ggcorrplot(cor_matrix,method = "square",type = "lower",lab = TRUE,          
           lab_size = 5,hc.order = TRUE,
           colors = c("red", "white", "blue"), 
           ggtheme = ggplot2::theme_minimal())
dev.off()



water_data$pH_group <- cut(water_data$ph,
                           breaks = c(-Inf, 6.5, 7.5, Inf),
                           labels = c("Acid", "Neutral", "Base"))
water_data$Sulfate_group <- cut(water_data$Sulfate,
                                breaks = c(-Inf, 300, 340, Inf),
                                labels = c("Low", "Medium", "High"))

table(water_data$pH_group)
table(water_data$Sulfate_group)
table(water_data$pH_group,water_data$Sulfate_group)

by(water_data$Organic_carbon,list(water_data$pH_group,water_data$Sulfate_group),shapiro.test)

library(car)
leveneTest(Organic_carbon ~ pH_group * Sulfate_group,
           data = water_data)

anova_model <- aov(Organic_carbon ~ pH_group * Sulfate_group,
                   data = water_data)
summary(anova_model)

TukeyHSD(anova_model, "Sulfate_group")



set.seed(8)
train.rows<-sample(rownames(water_data),dim(water_data)[1]*0.7)
train_data<-water_data[train.rows,]
test.rows<-setdiff(rownames(water_data),train.rows)
test_data<-water_data[test.rows,]
dim(train_data)
dim(test_data)

table(train_data$Potability)

weights <- ifelse(train_data$Potability == 1, 1414/879, 1)

model<-glm(Potability~ph+Hardness+Solids+Chloramines+Sulfate+
             Conductivity+Organic_carbon+Trihalomethanes+
             Turbidity,data=train_data,family="binomial",weights=weights)
summary(model)

model2<-glm(Potability~Solids+Organic_carbon,
            data=train_data,family="binomial",weights=weights)
summary(model2)

predicted <- predict(model2,test_data, type="response")
test_data$predicted<-round(predicted)
head(test_data,10)

library(caret)
confusionMatrix(as.factor(test_data$predicted),as.factor(test_data$Potability),positive="1")

library(pROC)
predicted<-predict(model2,test_data,type="response")
png("Logistic_ROCcurve.png", width = 4000, height = 2000, res = 200, pointsize = 22)
roc_curve<-roc(test_data$Potability, predicted)
plot(roc_curve,col="blue",lwd=2,main="ROC Curve - Logistic Regression")
abline(a = 1,b = -1,lty = 2,col="red")
dev.off()

auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value,3)))

library(car)    
library(lmtest)
vif(model2)

png("Logistic_Residuals vs Leverage.png", width = 4000, height = 2500, res = 300, pointsize = 22)
plot(model2,which=5)
dev.off()

png("Logistic_Solids-logitp.png", width = 4000, height = 2200, res = 300, pointsize = 22)
plot(train_data$Solids, 
     log(model2$fitted.values / (1 - model2$fitted.values)),
     main = "Linearity check: Solids vs log-odds",
     xlab = "Solids", ylab = "log-odds",col="purple")
dev.off()

png("Logistic_Organiccarbon-logitp.png", width = 4000, height = 2200, res = 300, pointsize = 22)
plot(train_data$Organic_carbon, 
     log(model2$fitted.values / (1 - model2$fitted.values)),
     main = "Linearity check: Organic_carbon vs log-odds",
     xlab = "Organic_carbon", ylab = "log-odds",col="red")
dev.off()



library(randomForest)
library(caret)
library(pROC)
set.seed(8)
modelrf<-randomForest(Potability~ph+Hardness+Solids+
                        Chloramines+Sulfate+
                        Conductivity+Organic_carbon+
                        Trihalomethanes+
                        Turbidity,data=train_data,
                      ntree=500,mtry=3,importance=TRUE) 
pred_classes<-predict(modelrf,test_data,type="class")
confMatrix<-confusionMatrix(pred_classes,test_data$Potability)
print(confMatrix)

pred_probs <- predict(modelrf, test_data,type="prob")
png("RandomForest_ROCcurve.png", width = 4000, height = 2000, res = 200, pointsize = 22)
roc_curve <- roc(test_data$Potability, pred_probs[,2])
plot(roc_curve, col="blue", lwd=2, main="ROC Curve - Random Forest")
abline(a=1, b=-1, lty=2, col="red")  
dev.off()

auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value,3)))

png("RandomForest_ImPlot.png", width = 4000, height = 2000, res = 200, pointsize = 22)
varImpPlot(modelrf)
dev.off()


