library(ggplot2)
library(lavaan)
library(semMediation)
glbwarm=read.csv("./inst/pmacro/data/glbwarm.csv",stringsAsFactors = FALSE)
glbwarm

fit=lm(govact~negemot*sex*age+posemot+ideology,data=glbwarm)
summary(fit)
interact_plot(fit,pred=negemot,modx=sex,mod2=age,mod2.values=c(30,50,70))
ss=sim_slopes(fit,pred=negemot,modx=sex,mod2=age,mod2.values=c(30,50,70),johnson_neyman = FALSE,digits=3)
ss
plot(ss)

probe_interaction(fit,pred=negemot,modx=sex,mod2=age)
probe_interaction(fit,pred=negemot,modx=sex,mod2=age,mod2.values=c(30,50,70))
sim_slopes(fit,pred=negemot,modx=sex,mod2=age,jnplot=TRUE)
sim_slopes(fit,pred=negemot,modx=age,mod2=sex,jnplot=TRUE)

attach(glbwarm)
glbwarm$interaction0<-negemot*sex*age
detach(glbwarm)
equation="
govact~negemot+sex+age+negemot:sex+negemot:age+sex:age+interaction0+posemot+ideology"
fit=sem(model=equation,data=glbwarm)
fit
colnames(res)[colnames(res)=="interaction0"]="xxxx"
rownames(res)[rownames(res)=="interaction0"]="xxxx"
res=corTable(fit)
res
result=estimatesTable(fit)
result$Predictors[result$Predictors=="interaction0"]
parameterEstimates(fit)
summary(semfit)

nodes=editData::editData(nodes)
devtools::use_data(nodes,overwrite = TRUE)
arrows=editData::editData(arrows)
devtools::use_data(arrows,overwrite = TRUE)
statisticalDiagram(3)
