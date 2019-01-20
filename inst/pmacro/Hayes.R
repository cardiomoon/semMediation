# chapter 2

glbwarm=read.csv("inst/pmacro/data/glbwarm.csv",stringsAsFactors = FALSE)
glbwarm
str(glbwarm)
model=lm(govact~negemot,data=glbwarm)
summary(model)
library(ggplot2)
ggplot(data=glbwarm,aes(x=negemot,y=govact))+geom_count()+geom_smooth(method="lm")


model1=lm(govact~sex,data=glbwarm)
summary(model1)

ggplot(data=glbwarm,aes(x=sex,y=govact))+geom_count()+geom_smooth(method="lm")

model2=lm(govact~.-partyid,data=glbwarm)
summary(model2)


# chapter 3

pmi=read.csv("inst/pmacro/data/pmi.csv",stringsAsFactors = FALSE)
pmi
str(pmi)
library(semMediation)
library(stringr)
dataFiles=list.files(path="inst/pmacro/data","*.csv")
dataFiles
dataNames=str_extract(dataFiles,"[^.]*")
for(i in seq_along(dataNames)){
    data=read.csv(paste0("inst/pmacro/data/",dataFiles[i]),stringsAsFactors = FALSE)
    assign(dataNames[i],data)
}
teams

