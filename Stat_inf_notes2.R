data(ToothGrowth)
str(ToothGrowth)

library

par(mfrow=c(1,2))
plot(ToothGrowth$len[ ToothGrowth$supp == "VC"])
plot(ToothGrowth$len[ ToothGrowth$supp == "OJ"])

with(ToothGrowth, plot(len[, supp == "VC"]))

require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")
