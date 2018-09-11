#' Draw arrow
#' @param from coordinates (x,y) of the point *from* which to draw arrow.
#' @param to coordinates (x,y) of the point *to* which to draw arrow.
#' @param lwd line width
#' @param adjust adjust position
#' @param label label
#' @param arr.pos arrow position
#' @param ... Further argument to be passed to straightarrow()
#' @importFrom diagram textplain straightarrow
myarrow=function(from,to,lwd=1,adjust=1,label="",arr.pos=NULL,...){

    if(is.null(arr.pos)){
        if(adjust){
            if(from[2]==to[2]) arr.pos=0.8
            else if(from[2]>to[2]) arr.pos=0.7
            else arr.pos=0.68
        } else{
            distance=abs(to[2]-from[2])+abs(to[1]-from[1])
            if(distance<=0.23) {
                arr.pos=0.86

            } else if(distance>0.5){
                arr.pos=0.95
            } else{
                arr.pos=0.94
            }
        }
        distance=abs(to[2]-from[2])+abs(to[1]-from[1])
        # str(distance)
        # str(arr.pos)
    }
    if(label!="") {
        mid=0.5*(from+to)
        textplain(mid=mid,lab=label,adj=c(1,-1))
    }
    straightarrow(from=from,to=to,lwd=lwd,arr.pos=arr.pos,arr.type="triangle",...)
}

#' Draw node
#' @param ... Further argument to be passed to textellipse() or textrect()
#' @param latent Logical
#' @importFrom diagram textellipse textrect
drawtext=function(...,latent=TRUE){
    if(latent) textellipse(...)
    else textrect(...)
}

midPoint=function(from=0,to=1,length.out=2){
    res=seq(from,to,length.out = length.out+2)
    res[c(-1,-length(res))]
}

#'Make concept Diagram
#'@param X character Name of independent variable
#'@param M character Name of mediator variable
#'@param Y character Name of dependent variable
#'@param latent Logical. whether or not X,Y and Z are latent variables or not
#'@param xb Logical. if positive draw linew between X and (Y+Z)
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param xmargin horizontal margin of plot
#'@param yinterval vertical interval bewteen box
#'@param moderator optional lists of moderator
#'@param labels optionallabels of X,Y and Z variables
#'@importFrom diagram openplotmat
#'@examples
#'labels=list(X="Time Spent in\n Grad School", M="# of\n Publications", Y="# of Job Offers")
#'conceptDiagram(xb=TRUE,labels=labels)
#'moderator=list(name="Z1",label="Time Spent\n with Alex",pos=3,
#'     site=list(c("a","b","c")),latent=FALSE)
#'conceptDiagram(moderator=moderator,labels=labels)
#'moderator=list(name=c("Z1","Z2"),label=c("Time Spent\n with Alex","Z2label"),pos=c(3,3),
#'     site=list(c("a","b","c"),c("b","c")),latent=c(FALSE,FALSE))
#'conceptDiagram(moderator=moderator,labels=labels,yinterval=0.4)
#'@export
conceptDiagram=function(X="X",M="M",Y="Y",latent=rep(FALSE,3),xb=FALSE,
                        radx=0.12,rady=0.05,xmargin=0.03,yinterval=NULL,
                        moderator=list(),labels=list()){

    # radx=0.12;rady=0.05;xmargin=0.03;yinterval=NULL
    # X="X";M="M";Y="Y";latent=rep(FALSE,3);xb=FALSE

    if(is.null(yinterval)) yinterval=rady*6
    openplotmat()
    x=c(0+radx+xmargin,0.5)
    y=c(1-(radx+xmargin),0.5)
    m=c(0.5,0.5+yinterval)


    select=which(moderator$pos==3)
    xpos=midPoint(0,1,length(select))
    select
    for(j in seq_along(select)){
        temp=c(xpos[j],0.5-yinterval+0.05)
        assign(paste0("z",select[j]),temp)
    }

    select=which(moderator$pos==2)
    if(length(select)==1){
        xpos=1-(radx+xmargin)
        ypos=0.5+yinterval-rady
    } else{
       xpos=midPoint(0.5+2*radx,1-radx,length(select))
       ypos=midPoint(0.5+yinterval+2*rady,0.5+rady,length(select))
    }
    for(j in seq_along(select)){
        temp=c(xpos[j],ypos[j])
        assign(paste0("z",select[j]),temp)
    }
    select=which(moderator$pos==1)
    if(length(select)==1){
        xpos=radx+xmargin
        ypos=0.5+yinterval-rady
    } else{
    xpos=midPoint(radx-xmargin,0.5-2*radx,length(select))
    ypos=midPoint(0.5+rady,0.5+yinterval+2*rady,length(select))
    }
    for(j in seq_along(select)){
        temp=c(xpos[j],ypos[j])
        assign(paste0("z",select[j]),temp)
    }

    startpos=list()
    sum=1
    for(i in seq_along(moderator$pos)){
        for(j in 1:length(moderator$site[[i]])){
            startpos[[sum]]=get(paste0("z",i))
            sum=sum+1
        }
    }
    startpos
    xlab=ifelse(is.null(labels$X),X,labels$X)
    mlab=ifelse(is.null(labels$M),M,labels$M)
    ylab=ifelse(is.null(labels$Y),Y,labels$Y)

    myarrow(from=x,to=y,label="c'")

    if(!is.null(M)){
        myarrow(from=x,to=m,label="a")
        myarrow(from=m,to=y,label="b")
    }
    if(xb) myarrow(from=x,to=0.5*(m+y))

    endpos=moderator2pos(moderator,x,y,m)
    endpos

    for(i in seq_along(endpos)){
        myarrow(from=startpos[[i]],to=endpos[[i]],adjust=0)
    }

    drawtext(x,radx=radx,rady=rady,lab=xlab,latent=latent[1])
    drawtext(y,radx=radx,rady=rady,lab=ylab,latent=latent[3])
    if(!is.null(M)) {
        drawtext(m,radx=radx,rady=rady,lab=ylab,latent=latent[2])
    }
    for(i in seq_along(moderator$pos)){
        z=eval(parse(text=paste0("z",i)))
        lab=ifelse(is.null(moderator$label[i]),paste0("z",i),moderator$label[i])
        drawtext(z,radx=radx,rady=rady,lab=lab,latent=moderator$latent[i])
    }
}

#'get position from moderator
#'@param moderator A list
#'@param x position of x
#'@param y position of y
#'@param m position of m
moderator2pos=function(moderator=list(),x,y,m){
    result=unlist(moderator$site)
    result
    count=rep(0,3)
    total=c(sum(result=="a"),sum(result=="b"),sum(result=="c"))
    pos=list()
    x
    y
    m
    (x+y)*2/3
    for(i in seq_along(result)){
       if(result[i]=="a"){
           count[1]=count[1]+1
           pos[[i]]=(m*count[1]+x*(total[1]+1-count[1]))/(total[1]+1)
       } else if(result[i]=="b"){
            count[2]=count[2]+1
            pos[[i]]=(y*count[2]+m*(total[2]+1-count[2]))/(total[2]+1)
       } else{
           count[3]=count[3]+1
           pos[[i]]=(y*count[3]+x*(total[3]+1-count[3]))/(total[3]+1)
        }
    }
    pos
}

