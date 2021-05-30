############## lec 4 TV advertising data ############

TVadData = read.csv("./Advertising.csv")  # read csv file

attach(TVadData)
TVadlm = lm(Sales~TV+Radio+Newspaper)
summary(TVadlm)

newdata = data.frame(TVadData[1,])
predict(TVadlm, newdata, interval="c", level=0.95)
predict(TVadlm, newdata, interval="p", level=0.95)

####################### end of code ##########################