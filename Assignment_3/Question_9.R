require(ISLR); require(tidyverse); require(ggthemes)
# (a)
set.seed(2)
complete = hclust(dist(USArrests), method = "complete")
plot(complete)


# (b)
cutree(complete, 3)
table(cutree(complete, 3))


# (c)
dsc = scale(USArrests)
hc.s.complete =  hclust(dist(dsc), method = "complete")
plot(hc.s.complete)


# (d)
cutree(hc.s.complete, 3)
table(cutree(hc.s.complete, 3))
table(cutree(hc.s.complete, 3), cutree(complete, 3))
