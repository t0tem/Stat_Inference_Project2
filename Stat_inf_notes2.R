data(ToothGrowth)
str(ToothGrowth)



table(ToothGrowth[2:3])
summary(ToothGrowth)

library(ggplot2)
g1 <- ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot(aes(fill=supp)) +
      facet_grid(.~dose) + guides(fill=FALSE)
print(g1)

g2 <- ggplot(ToothGrowth, aes(x=dose, y=len, group=dose)) + geom_boxplot()
print(g2)

      
OJ_05 <- ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 0.5]
OJ_10 <- ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 1.0]
OJ_20 <- ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 2.0]

VC_05 <- ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 0.5]
VC_10 <- ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 1.0]
VC_20 <- ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 2.0]

all_05 <- ToothGrowth$len[ToothGrowth$dose == 0.5]
all_10 <- ToothGrowth$len[ToothGrowth$dose == 1.0]
all_20 <- ToothGrowth$len[ToothGrowth$dose == 2.0]


t.test(OJ_05, VC_05, paired = FALSE, var.equal = FALSE, altern = "greater")
t.test(OJ_10, VC_10, paired = FALSE, var.equal = FALSE, altern = "greater")
t.test(OJ_20, VC_20, paired = FALSE, var.equal = FALSE, altern = "two.sided")

t.test(all_10, all_05, paired = FALSE, var.equal = FALSE, altern = "greater")
t.test(all_20, all_10, paired = FALSE, var.equal = FALSE, altern = "greater")



