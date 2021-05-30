#(a)
college = read.csv("College.csv",stringsAsFactors = TRUE)
fix(college)

#(b)
rownames(college)=college[,1]
fix(college)
college = college[,-1]
fix(college)

#(c)
summary(college)
pairs(college[,1:10])

plot(college$Outstate,college$Private)

Elite = rep("No", nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate)

par(mfrow = c(2,2))
hist(college$Books,xlab = "Books")
hist(college$PhD,xlab = "PhD")
hist(college$Enroll,xlab = "enroll")
hist(college$perc.alumni,xlab = "% alumni")


summary(college$PhD)
summary(college$Apps)
# why the proportation of phd of some universities' faculty
# exceed 100%? It is wired