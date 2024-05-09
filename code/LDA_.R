##LDA ----

##duplicate the data set to group the 

library(MASS)


segmentation$Work <- as.factor(segmentation$Work)
segmentation$segment <- as.factor(segmentation$segment)

fit <- lda(segment ~ Married + Age + Income + Edcation + Work , data = segmentation)
plot(fit)

ldapred <- predict(fit, segmentation)

ld <- ldapred$x

ld

anova(lm(ld[,1]~segmentation$segment))

anova(lm(ld[,2]~segmentation$segment))

anova(lm(ld[,3]~segmentation$segment))

anova(lm(ld[,4]~segmentation$segment))

anova(lm(ld[,5]~segmentation$segment))

anova(lm(ld[,6]~segmentation$segment))

anova(lm(ld[,7]~segmentation$segment))


pred.seg <- predict(fit)$class


cf<- table(segmentation$segment, ldapred$class)
cf

#overal accuracy of the predicting model 
sum(diag(cf))/nrow(segmentation)


