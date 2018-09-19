
obj <- read.csv("systesting.csv")
plot(obj, type = "p")
barplot(obj$time, main = "spec rate", xlab = "systems", ylab = "time", col = rainbow(4), 
        names.arg = c("A.Raghu","A.Diyu","Anushree","Akansha"))