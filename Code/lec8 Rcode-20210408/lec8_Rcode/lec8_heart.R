############## lec 8 SAheart data ############

heartData = read.table("SAheart.txt",sep=",",head=T,row.names=1) 

heartData1 = heartData[,-c(4,6)]
cols = c('blue','red')

pairs(heartData1[,-8],col=cols[heartData1[,8]+1])

glm.fit=glm(chd~.,data=heartData1,family=binomial)

summary(glm.fit)