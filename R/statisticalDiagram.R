#'Draw statistical diagram
#'@param no process macro model number
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param xmargin horizontal margin of plot
#'@param arrowlabel logical whether or not draw arrowlabel
#'@param labels A character list
#'@param whatLabel What should the edge labels indicate in the path diagram? Choices are c("est","std","name")
#'@param fit An object of class lavaan. Result of lavaan::sem()
#'@param estimateTable A data.frame
#'@param covar Optional list of covariates
#'@param includeLatentVars A logical
#'@importFrom dplyr left_join
#'@export
#'@examples
#'statisticalDiagram(no=1)
#'covar=list(name=c("posemot","ideology","sex"),site=list(c("Y"),c("Y"),c("Y")))
#'statisticalDiagram(no=1,covar=covar)
#'covar=list(name=c("posemot","ideology","sex"),site=list(c("Mi","Y"),c("Mi","Y"),c("Mi","Y")))
#'statisticalDiagram(no=4,covar=covar)
#'statisticalDiagram(no=8,covar=covar)
#'#statisticalDiagram(no=1.1,fit=fit)
#'#labels=list(X="knowledge",Mi="empathy",Y="intervention")
#'#statisticalDiagram(no=4,fit=fit,includeLatentVars=TRUE,labels=labels,whatLabel="est",radx=0.06)
#'#statisticalDiagram(no=4,fit=fit,labels=list(X="knowledge",Mi="empathy",Y="intervention"))
#'#statisticalDiagram(no=4,fit=fit)
statisticalDiagram=function(no=1,radx=0.10,rady=0.04,xmargin=0.01,arrowlabel=TRUE,
                            labels=list(),whatLabel="name",fit=NULL,estimateTable=NULL,
                            covar=list(),
                            includeLatentVars=FALSE){

      # no=4;radx=0.10;rady=0.04;xmargin=0.01;arrowlabel=TRUE;labels=list()
      # whatLabel="est";estimateTable=NULL;covar=list()
      # labels=list(X="knowledge",Mi="empathy",Y="intervention")
      # fit=fit;includeLatentVars=TRUE;estimateTable=NULL

      # labels=list("d2"="protest=2",d3="protest=3")
      # covar=list(name="angry",site=list("liking"))

    if(!is.null(fit)) {
      if(is.null(estimateTable)) estimateTable<-estimatesTable(fit)
    }
    if(no==1.1) {
        nodes=est2Nodes(estimateTable)
    } else {
        nodes=nodes[nodes$no==no, ]
    }
    if(no==1.1){
        arrows1=est2Arrows(estimateTable)
    } else{
        arrows1=parrows[parrows$no==no,]
    }
    nodes
    # Add covariates
    if(no!=1.1) nodes=addNodes(nodes,covar,radx=radx,rady=rady,no=no)
    # print(nodes)

    covar
    if(no==1.1) {
        arrows2=arrows1
    } else {
        arrows2=addArrows(arrows1,covar)
    }
    arrows2
    # print(arrows)

    if( !is.null(estimateTable)) {
        if(no==1.1){
            arrows2$Predictors=arrows2$start
        } else{
           arrows2$Predictors=findNames(labels,arrows2$start)
        }
        arrows2$Variables=findNames(labels,arrows2$end)


        arrows2
        estimateTable
        # temp=c()
        # for(i in 1:nrow(estimateTable)){
        #    temp=c(temp,names(labels)[str_detect(labels,estimateTable$Variables[i])])
        # }
        # temp
        # estimateTable$start=temp
        # estimateTable
        arrows2
        if(includeLatentVars){
          arrows3<-full_join(arrows2,estimateTable,by=c("Predictors","Variables"))
          arrows3
          arrows3$no=  arrows3$no[1]
          arrows3$name[is.na(arrows3$name)]=""
          arrows3$start[is.na(arrows3$start)]=arrows3$Predictors[is.na(arrows3$start)]
          arrows3$end[is.na(arrows3$end)]=arrows3$Variables[is.na(arrows3$end)]
          arrows3$labelpos=0.5
          arrows3$arrpos=0.84
          arrows3$end=changeLabelName(arrows3$end,labels)

        } else{
          arrows3<-left_join(arrows2,estimateTable,by=c("Predictors","Variables"))
        }
        arrows3$lty=ifelse(arrows3$p<0.05,1,3)
        # print(arrows3)
    }  else{
      arrows2$lty=1
      arrows3<-arrows2
    }




    if(arrowlabel){
        if(whatLabel=="name") {
            arrows3$label=arrows3$name
        } else if(whatLabel=="est"){
            arrows3$label=arrows3$B
        } else{
            arrows3$label=arrows3[,ncol(arrows3)-1]
        }
    } else {
        arrows3$label=""
    }
    # print(arrows3)


    if((!is.null(fit))&(includeLatentVars)){
      nodes=addLatentNodes(nodes,fit,labels)
      nodes=adjustPosNodes(nodes)

    }
    arrows3
    nodes



    drawStatDiagram(no=no,arrows=arrows3,nodes=nodes,labels=labels,xmargin=xmargin,radx=radx,rady=rady,fit)
    # openplotmat()
    #
    # drawArrows(arrows3,nodes,xmargin=xmargin,rady=rady,radx=radx)
    #
    # for(i in 1:nrow(nodes)){
    #     xpos=nodes$xpos[i]
    #     xpos=adjustxpos(xpos,xmargin,radx)
    #     mid=c(xpos,nodes$ypos[i])
    #
    #    # label=ifelse(is.null(labels[[nodes$name[i]]]),nodes$name[i],labels[[nodes$name[i]]])
    #     label=ifelse(no==1.1,nodes$name[i],findName(labels,nodes$name[i]))
    #
    #     drawtext(mid,radx=radx,rady=rady,lab=label,latent=FALSE)
    #     if(no==1.1){
    #         if(i<=nrow(nodes)){
    #             label=findName(labels,nodes$name[i])
    #             if(label!=nodes$name[i]) textplain(mid+c(0,-0.07),radx=radx,rady=rady,lab=label,latent=FALSE)
    #         }
    #     }
    # }

}


#'Change Label Names
#'@param x A character vector
#'@param labels A list
#'@param add A logical
#'@export
#'@examples
#'labels=list(X="skeptic",Mi="empathy",Y="intervention",W="frame")
#'x=c("skeptic","test","empathy","skeptic:frame")
#'changeLabelName(x,labels)
#'changeLabelName(x,labels,add=TRUE)
changeLabelName=function(x,labels,add=FALSE){
  res=c()
  for(i in 1:length(x)){
      if(str_detect(x[i],":")){
         temp=unlist(strsplit(x[i],":"))
         temp2=c()
         for(j in 1:length(temp)){
           temp3=names(unlist(labels))[which(str_detect(labels,temp[j]))]
           temp2=c(temp2,temp3)
         }
         temp2=paste0(temp2,collapse=":")
         if(add){
           res=c(res,paste0(temp2,"(",x[i],")"))
         } else{
           res=c(res,temp2)
         }
      }  else if(x[i] %in% unlist(labels)){
          if(add){
            temp=names(unlist(labels))[which(str_detect(labels,x[i]))]
            res=c(res,paste0(temp,"(",x[i],")"))
          } else{
             res=c(res,names(unlist(labels))[which(str_detect(labels,x[i]))])
          }
      } else{
        res=c(res,x[i])
      }
  }
  res
}


#'Adjust position odf nodes
#'@param nodes A data.frame
adjustPosNodes=function(nodes){
   if(min(nodes$xpos)<0){
    nodes$xpos[(nodes$xpos<0.3)&(nodes$xpos>=0)]=nodes$xpos[(nodes$xpos<0.3)&(nodes$xpos>=0)]+0.2
    nodes$xpos[nodes$xpos<0]=0

  }
  if(max(nodes$xpos)>1){
    nodes$xpos[(nodes$xpos>0.7)&(nodes$xpos<=1)]=nodes$xpos[(nodes$xpos>0.7)&(nodes$xpos<=1)]-0.2
    nodes$xpos[nodes$xpos>1]=1
  }

  nodes
}

#'Extract Latent Variables Names
#'@param fit An object of class lavaan. Result of lavaan::sem()
extractLatentVarName=function(fit){
  res=parameterEstimates(fit)
  res=res[res$op=="=~",]
  unique(res$lhs)
}

#'Extract Latent Variables Data
#'@param fit An object of class lavaan. Result of lavaan::sem()
#'@param labels A list
extractLatentVar=function(fit,labels){
  res=parameterEstimates(fit)
  res=res[res$op=="=~",]
  if(nrow(res)>0){
  temp=c()
  for(i in 1:nrow(res)){
    temp=c(temp,names(labels)[str_detect(labels,res$lhs[i])])
  }
  temp
  res$name=temp
  }
  res
}


#'Add latent nodes information to nodes
#'@param nodes A data.frame
#'@param fit An object of class lavaan. Result of lavaan::sem()
#'@param labels A list
addLatentNodes=function(nodes,fit,labels){
  nodes
  res=extractLatentVar(fit,labels)
  no<-name<-xpos<-ypos<-c()
  yinterval=0.12

  count=length(res$name[res$name=="X"])
  start=ifelse(count>4,0.1+yinterval*(count-1),nodes$ypos[nodes$name=="X"]+yinterval*(count-1)/2)
  ypos=seq(start,by=-yinterval,length.out = count)
  no=rep(nodes$no[1],count)
  xpos=rep(-0.1,count)
  name=res$rhs[res$name=="X"]
  df=data.frame(no,name,xpos,ypos,stringsAsFactors = FALSE)
  nodes<-rbind(nodes,df)

  count=length(res$name[res$name=="Mi"])
  start=ifelse(count>4,0.1,nodes$xpos[nodes$name=="Mi"]-0.1*(count-1)/2)
  xpos=seq(start,by=0.1,length.out = count)
  no=rep(nodes$no[1],count)
  ypos=rep(nodes$ypos[nodes$name=="Mi"]+0.1,count)
  name=res$rhs[res$name=="Mi"]
  df=data.frame(no,name,xpos,ypos,stringsAsFactors = FALSE)
  df
  nodes<-rbind(nodes,df)

  count=length(res$name[res$name=="Y"])
  start=ifelse(count>4,0.1+yinterval*(count-1),nodes$ypos[nodes$name=="Y"]+yinterval*(count-1)/2)
  ypos=seq(start,by=-yinterval,length.out = count)
  no=rep(nodes$no[1],count)
  xpos=rep(1.1,count)
  name=res$rhs[res$name=="Y"]
  df=data.frame(no,name,xpos,ypos,stringsAsFactors = FALSE)
  df
  nodes<-rbind(nodes,df)
  nodes
}

#'draw StatDiagram
#'@param no process macro model number
#'@param arrows A data.frame
#'@param nodes A data.frame
#'@param labels A list
#'@param xmargin horizontal margin of plot
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param fit An object of class lavaan. Result of lavaan::sem()
#'@export
drawStatDiagram=function(no,arrows,nodes,labels,xmargin,radx,rady,fit=NULL){

  # print(nodes)
  # print(arrows)
  openplotmat()
  drawArrows(arrows,nodes,xmargin=xmargin,rady=rady,radx=radx)
  LVnames=c()
  if(!is.null(fit)) LVnames=extractLatentVarName(fit)
  for(i in 1:nrow(nodes)){
    xpos=nodes$xpos[i]
    xpos=adjustxpos(xpos,xmargin,radx)
    mid=c(xpos,nodes$ypos[i])
    # label=ifelse(is.null(labels[[nodes$name[i]]]),nodes$name[i],labels[[nodes$name[i]]])
    label=ifelse(no==1.1,nodes$name[i],findName(labels,nodes$name[i]))

    drawtext(mid,radx=radx,rady=rady,lab=label,latent=ifelse(label %in% LVnames,TRUE,FALSE))
    if(no==1.1){
      if(i<=nrow(nodes)){
        label=findName(labels,nodes$name[i])
        if(label!=nodes$name[i]) textplain(mid+c(0,-0.07),radx=radx,rady=rady,lab=label,latent=FALSE)
      }
    }
  }

}

#' Make arrows from estimatesTable
#' @param res A data.frame, result of estimatesTable
est2Arrows=function(res){
    no=rep(1.1,nrow(res))
    name=paste0("b",1:nrow(res))
    start=res$Predictors
    end=res$Variables
    labelpos=rep(0.5,nrow(res))
    arrpos=rep(0.84,nrow(res))
    data.frame(no,name,start,end,labelpos,arrpos,stringsAsFactors = FALSE)
}

#' Make nodes from estimatesTable
#' @param res A data.frame, result of estimatesTable
#' @param lastxno A numeric
est2Nodes=function(res,lastxno=2){
    res
    count=nrow(res)-1
    count
    yinterval=-0.8/(count-1)
    start=0.9
    y=seq(0.9,by=yinterval,length.out = count)
    y
    y=c(y,0.1,0.5)
    x=c(rep(0,count-1),seq(from=0.05,by=0.1,length.out=2),1)
    x
    no=rep(1.1,nrow(res)+1)
    name=c(res$Predictors,res$Variables[1])
    data.frame(no=no,name=name,xpos=x,ypos=y,stringsAsFactors = FALSE)
}


#'Draw arrows
#'@param arrows A data.frame
#'@param nodes A data.frame
#'@param xmargin horizontal margin of plot
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
drawArrows=function(arrows,nodes,xmargin=0.01,radx=0.10,rady=0.04){
    #print(arrows)
    for(i in 1:nrow(arrows)){

    if(is.na(arrows$lty[i])){
        myarrow2(nodes,from=arrows$start[i],to=arrows$end[i],
                 label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=arrows$labelpos[i],arr.pos=arrows$arrpos[i])

    } else{
        myarrow2(nodes, from=arrows$start[i],to=arrows$end[i],
                 label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=arrows$labelpos[i],arr.pos=arrows$arrpos[i],lty=arrows$lty[i])
    }
}
}

# glbwarm=read.csv("./inst/pmacro/data/glbwarm.csv",stringsAsFactors=FALSE)
# glbwarm
# covar=list(name=c("posemot","ideology","sex"),site=list("Y","Y","Y"))
# equation=modmedEquation(X="negemot",M=NULL,Y="govact",
#                         moderator=list(name="age",site=list("c")),
#                         covar=covar)
# cat(equation)
# require(lavaan)
# fit=sem(model=equation,data=glbwarm)
# table1=estimatesTable(fit)
# table1
# estimateTable=table1
# labels=list()
# labels$X="negemot"
# labels$M="age"
# labels$Y="govact"
# require(diagram)
# require(tidyverse)
# no=1
# arrows1=arrows[arrows$no==1,]
# arrows1
# result=addArrows(arrows1,covar)
# result
# statisticalDiagram(no=1,labels=labels,estimateTable=table1,covar=covar,whatLabel="name")
# statisticalDiagram(no=1,labels=labels,estimateTable=table1)
#
# arrows
# labels=list(X="age",Mi="educ",Y="interest",W="male")

# name="MiX"
# length(labels)
# names(labels)
# findName(labels,name)

#'Add covariates to nodes
#'@param nodes A data.frame
#'@param covar A list of covariates
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param no A numeric
#'@export
addNodes=function(nodes,covar,radx=0.10,rady=0.04,no=NULL){

    if(length(covar$name)>0){
        if(no==1.1){

        }
        number<-name<-xpos<-ypos<-c()
        minypos=min(nodes$ypos)
        maxxpos=min(nodes$xpos[nodes$ypos==minypos])
        if(nodes$no[1]==4.2) {
            maxxpos=-(radx/2)
            minypos=0.4
        }
        for(i in 1:length(covar$name)){
            number=c(number,nodes$no[1])
            name=c(name,covar$name[i])
            xpos=c(xpos,maxxpos+radx/2*i)
            ypos=c(ypos,minypos-(rady*2+0.02)*i)
        }
        df=data.frame(no=number,name=name,xpos=xpos,ypos=ypos)
        nodes=rbind(nodes,df)
        nodes=adjustNodes(nodes)
    }

    nodes
}

#'Add covariates to arrows
#'@param arrows A data.frame
#'@param covar A list of covariates
#'@export
addArrows=function(arrows,covar){
    if(length(covar$name)>0){
        number<-name<-start<-end<-labelpos<-arrpos<-c()
        count=1
        for(i in 1:length(covar$name)){
            for(j in 1:length(covar$site[[i]])){
                number=c(number,arrows$no[1])
                name=c(name,paste0("h",count))
                start=c(start,covar$name[i])
                end=c(end,covar$site[[i]][j])
                labelpos=c(labelpos,0.5)
                arrpos=c(arrpos,0.84)
                count=count+1
            }

        }
        number
        name
        start
        end
        labelpos
        arrpos
        df=data.frame(no=number,name=name,start=start,end=end,labelpos=labelpos,arrpos=arrpos)
        arrows=rbind(arrows,df)

    }
    arrows
}
#'Adjust y position of nodes
#'@param nodes A data.frame
adjustNodes=function(nodes){
    miny=min(nodes$ypos)
    if(miny<0.05){
        if(miny>=0) {
            nodes$ypos=nodes$ypos+0.05
        } else{
            nodes$ypos=nodes$ypos+0.05-miny
        }
    }
    if(max(nodes$ypos)>0.95) {
        nodes$ypos=nodes$ypos*0.95/max(nodes$ypos)
    }
    nodes
}

#'convert a vector of names with list
#'@param labels A named list
#'@param names A character vector to look for
#'@param exact A logical
#'@export
findNames=function(labels,names,exact=FALSE){
    result=c()
    for(i in 1:length(names)){
        result=c(result,findName(labels,names[i],exact=exact))
    }
    result
}

#'convert name with list
#'@param labels A named list
#'@param name A name to look for
#'@param exact A logical
findName=function(labels,name="MiX",exact=FALSE){

    if(length(labels)==0) {
        result=name
    } else if(!is.null(labels[[name]])) {
        result=labels[[name]]
    } else if(!exact){
        temp=c()
        for(i in 1:length(labels)){
            grep(names(labels)[i],name)
            if(length(grep(names(labels)[i],name))>0)
                temp=c(temp,labels[[names(labels[i])]])
            temp
        }
        temp
        if(length(temp)<1) {
            result=name
        } else{
            result=paste0(temp,collapse=":")
        }
    } else{
        result=name
    }
    result
}

#'Adjust x position
#'@param xpos x position
#'@param xmargin horizontal margin of plot
#'@param radx horizontal radius of the box.
adjustxpos=function(xpos,xmargin=0.01,radx=0.12){
    ifelse(xpos==0.5,0.5,
           ifelse(xpos>0.5,
                  1-xmargin-radx-(1.0-xpos)*10*(xmargin+2*radx),
                  xmargin+radx+(xpos)*10*(xmargin+2*radx)))
}

#' Draw arrow with adjustment of a position
#' @param nodes A data.frame
#' @param from coordinates (x,y) of the point *from* which to draw arrow.
#' @param to coordinates (x,y) of the point *to* which to draw arrow.
#' @param label label to display
#' @param no process macro model number
#' @param radx horizontal radius of the box.
#' @param rady vertical radius of the box.
#' @param xmargin horizontal margin of plot
#' @param label.pos label position
#' @param arr.pos arrow position
#' @param ... Further argument to be passed to straightarrow()
myarrow2=function(nodes,from,to,label="",no,radx=0.12,rady=0.04,xmargin=0.01,label.pos=0.5,arr.pos=NULL,...){

    #nodes=nodes[nodes$no==no, ]
    # from="X";no=1;to="Y";label="66"
    xpos=nodes$xpos[nodes$name==from]
    xpos=adjustxpos(xpos,xmargin,radx)
    ypos=nodes$ypos[nodes$name==from]
    start=c(xpos,ypos)

    xpos=nodes$xpos[nodes$name==to]
    xpos=adjustxpos(xpos,xmargin,radx)
    ypos=nodes$ypos[nodes$name==to]
    end=c(xpos,ypos)
    if(!is.numeric(label)){
    if(nchar(label)>1) {


        if(nchar(label==3)){
            temp1=paste0("expression(",substr(label,1,1),"[",substr(label,2,2),"]","[",substr(label,3,nchar(label)),"])")

        } else{
        temp2=substr(label,2,nchar(label))
        temp2
        temp1=paste0("expression(",substr(label,1,1),"[",temp2,"])")
        temp1
        }
        temp=eval(parse(text=temp1))
        label=temp
    }
    }
    myarrow(from=start,to=end,label=label,label.pos=label.pos,arr.pos=arr.pos,...)

}



