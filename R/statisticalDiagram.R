#'Draw statistical diagram
#'@param no process macro model number
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param xmargin horizontal margin of plot
#'@param arrowlabel logical whether or not draw arrowlabel
#'@param labels A character list
#'@param whatLabel What should the edge labels indicate in the path diagram? Choices are c("est","std","name")
#'@param estimateTable A data.frame. Result of estimateTable()
#'@param covar Optional list of covariates
#'@importFrom dplyr left_join
#'@export
#'@examples
#'statisticalDiagram(no=1)
#'covar=list(name=c("posemot","ideology","sex"),site=list(c("Y"),c("Y"),c("Y")))
#'statisticalDiagram(no=1,covar=covar)
#'covar=list(name=c("posemot","ideology","sex"),site=list(c("Mi","Y"),c("Mi","Y"),c("Mi","Y")))
#'statisticalDiagram(no=4,covar=covar)
statisticalDiagram=function(no=1,radx=0.10,rady=0.04,xmargin=0.01,arrowlabel=TRUE,
                            labels=list(),whatLabel="name",estimateTable=NULL,covar=list()){

     # no=1;radx=0.10;rady=0.04;xmargin=0.01;arrowlabel=TRUE;labels=list()
    # whatLabel="name"
    nodes=nodes[nodes$no==no, ]
    arrows1=arrows[arrows$no==no,]
    # Add covariates
    nodes=addNodes(nodes,covar,radx=radx,rady=rady)
    print(nodes)
    arrows1
    covar
    arrows2=addArrows(arrows1,covar)
    nodes
    # print(arrows)

    openplotmat()
    if( !is.null(estimateTable)) {
        arrows2$Predictors=findNames(labels,arrows2$start)
        arrows2$Variables=findNames(labels,arrows2$end)
        arrows3<-left_join(arrows2,estimateTable)
        arrows3$lty=ifelse(arrows3$p<0.05,1,3)
        # print(arrows)


    } else{
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
    drawArrows(arrows3,nodes,xmargin=xmargin,rady=rady,radx=radx)
    nodes
    for(i in 1:nrow(nodes)){
        xpos=nodes$xpos[i]
        xpos=adjustxpos(xpos,xmargin,radx)
        mid=c(xpos,nodes$ypos[i])

       # label=ifelse(is.null(labels[[nodes$name[i]]]),nodes$name[i],labels[[nodes$name[i]]])
        label=findName(labels,nodes$name[i])

        drawtext(mid,radx=radx,rady=rady,lab=label,latent=FALSE)
    }

}


#'Draw arrows
#'@param arrows A data.frame
#'@param nodes A data.frame
#'@param xmargin horizontal margin of plot
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
drawArrows=function(arrows,nodes,xmargin=0.01,radx=0.10,rady=0.04){
    print(arrows)
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
addNodes=function(nodes,covar,radx=0.10,rady=0.04){
    if(length(covar$name)>0){
        number<-name<-xpos<-ypos<-c()
        minypos=min(nodes$ypos)
        maxxpos=min(nodes$xpos[nodes$ypos==minypos])
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

#'convert a vextor of names with list
#'@param labels A named list
#'@param names A character vector to look for
findNames=function(labels,names){
    result=c()
    for(i in 1:length(names)){
        result=c(result,findName(labels,names[i]))
    }
    result
}

#'convert name with list
#'@param labels A named list
#'@param name A name to look for
findName=function(labels,name="MiX"){

    if(length(labels)==0) {
        result=name
    } else if(!is.null(labels[[name]])) {
        result=labels[[name]]
    } else{
        temp=c()
        for(i in 1:length(labels)){
            grep(names(labels)[i],name)
            if(length(grep(names(labels)[i],name))>0) temp=c(temp,labels[[names(labels[i])]])
            temp
        }
        temp
        if(length(temp)<1) {
            result=name
        } else{
            result=paste0(temp,collapse=":")
        }
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



