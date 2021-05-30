############## lec 10 Credit data ############

defaultData = read.table("Default.txt",header=T)  


## LDA 

library(MASS)

default.lda = lda(default ~., data = defaultData)
default.pred = predict(default.lda, data= default[,-1])

table(default.pred$class,defaultData$default)

## change threshold
threshold1 = default.pred$post[,2]>0.075
aa = table(threshold1,defaultData$default)
# False positive 
aa[2,1]/sum(aa[,1])
# False negative
 aa[1,2]/sum(aa[,2])
 # overall error
(aa[2,1]+aa[1,2])/sum(sum(aa) )

