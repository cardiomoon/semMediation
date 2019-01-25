#'Draw statistical diagram
#'@param no process macro model number
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param xmargin horizontal margin of plot
#'@param arrowlabel logical whether or not draw arrowlabel
#'@param labels A character list
#'@param whatLabel What should the edge labels indicate in the path diagram? Choices are c("est","std","name")
#'@param estimateTable A data.frame. Result of estimateTable()
#'@importFrom dplyr left_join
#'@export
#'@examples
#'statisticalDiagram(4)
statisticalDiagram=function(no=1,radx=0.10,rady=0.04,xmargin=0.01,arrowlabel=TRUE,
                            labels=list(),whatLabel="name",estimateTable=NULL){

    # no=2;radx=0.10;rady=0.04;xmargin=0.01;arrowlabel=TRUE;labels=list()
    nodes=nodes[nodes$no==no, ]
    arrows=arrows[arrows$no==no,]

    openplotmat()
    if( !is.null(estimateTable)) {
        arrows$Predictors=findNames(labels,arrows$start)
        arrows$Variables=findNames(labels,arrows$end)
        arrows<-left_join(arrows,estimateTable)


    }
    for(i in 1:nrow(arrows)){
        #cat("i=",i,"\n")
        if(arrowlabel){
            if(whatLabel=="name") {
              label=arrows$name[i]
            } else if(whatLabel=="est"){
                label=arrows$B[i]
            } else{
                label=arrows[,ncol(arrows)][i]
            }
        } else {
            label=""
        }
         myarrow2(from=arrows$start[i],to=arrows$end[i],
                 label=label,no=no,xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=arrows$labelpos[i],arr.pos=arrows$arrpos[i])
    }
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


# labels=list(X="age",Mi="educ",Y="interest",W="male")
# name="MiX"
# length(labels)
# names(labels)
# findName(labels,name)


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
myarrow2=function(from,to,label="",no,radx=0.12,rady=0.04,xmargin=0.01,label.pos=0.5,arr.pos=NULL,...){

    nodes=nodes[nodes$no==no, ]
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



