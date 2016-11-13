library(tidyverse)
library(apaTables)
library(haven)

my.data <- read_spss("Lecture 7 regression_example_data.sav")

glimpse(my.data)

#going to look at the correlation matrix
apa.cor.table(my.data)
apa.cor.table(my.data, filename="Table1_APA.doc", table.number=1)
#makes the table in a word doc 

psych::pairs.panels(as.data.frame(my.data))
#all looks good, no weird curvilinear relationships

#Single Regressions
#Does DV2 contribute to prediction of job performance beyond DV1?

#IV = job performance
#DV1 = GMA
#DV2 = con, ac, graph

#so let's do the first regression, GMA alone
regression.gma <- lm(jobperf ~ gma, data = my.data)
summary(regression.gma)
apa.reg.table(regression.gma)
#R2 = .26, 95% CI [.20, .32], sr2 = .26, 95% CI [.20, .32]

#GMA plus con
regression.1.con <- lm(jobperf ~ gma + con, data=my.data)
summary(regression.1.con)
apa.reg.table(regression.1.con)
#R2=.36, 95% CI[.29,.41], F(2,497)=137.5, p<.001
#sr2=.10, 95% CI[.05, .14] 

#GMA plus ac
regression.2.ac <- lm(jobperf ~ gma + ac, data=my.data)
summary(regression.2.ac)
apa.reg.table(regression.2.ac)

#GMA plus graph
regression.3.graph <- lm(jobperf ~ gma + graph, data=my.data)
summary(regression.3.graph)
apa.reg.table(regression.3.graph)

#PART 2, Using 2 Block Regressions
block1 = lm(jobperf ~ gma, data=my.data)
block2.con = lm(jobperf ~ gma + con, data=my.data)
block3.con = lm(jobperf ~ gma + ac, data = my.data)

apa.reg.table(block1,block2.con)

apa.reg.table(block1, block3.con)

block4.graph = lm(jobperf ~ gma + graph, data=my.data)
apa.reg.table(block1,block4.graph)

#PART 3, CI's and PI's
apa.cor.table(my.data)
#gma mean is 100, con mean = 120

range <- data.frame(gma = c(100), con = c(120))
CI_data <- predict(regression.1.con, newdata = range, interval = "confidence", level = .95)
CI_data <- as.data.frame(cbind(range, CI_data))
print(CI_data)
#CI is 100.28, 101.72
PI_data <- predict(regression.1.con, newdata = range, interval = "prediction", level = .95)
PI_data <- as.data.frame(cbind(range, PI_data))
print(PI_data)
#PI is 84.87, 117.13





