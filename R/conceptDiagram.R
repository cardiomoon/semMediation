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

# require(diagram)
#
# par(family="AppleGothic")
#
# openplotmat()
# radx=0.12
# rady=0.05
# x=c(0.15,0.5)
# y=c(0.85,0.5)
# m=c(0.5,0.7)
# z=c(0.5,0.3)
#
# xlab="Time Spent in\n Grad School"
# mlab="# of\n Publications"
# ylab="# of Job Offers"
# zlab="Time Spent\n with Alex"
#
# myarrow(from=x,to=m,label="a")
# myarrow(from=m,to=y,label="b")
# myarrow(from=x,to=y,label="c'")
# myarrow(from=z,to=0.5*(m+x),adjust=0)
# myarrow(from=z,to=0.5*(m+y),adjust=0)
# myarrow(from=z,to=0.5*(x+y),adjust=0)
#
# textellipse(x,radx=radx,rady=rady,lab=xlab)
# textrect(y,radx=radx,rady=rady,lab=ylab)
# textrect(m,radx=radx,rady=rady,lab=mlab)
# textellipse(z,radx=radx,rady=rady,lab=zlab)
#


#' Draw node
#' @param ... Further argument to be passed to textellipse() or textrect()
#' @param latent Logical
#' @importFrom diagram textellipse textrect
drawtext=function(...,latent=TRUE){
    if(latent) textellipse(...)
    else textrect(...)
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
#'moderator=list(Z1=list(label="Time Spent\n with Alex",pos=3,site=c("a","b","c"),latent=FALSE))
#'conceptDiagram(M=NULL,moderator=moderator,labels=labels)
#'moderator$Z2=list(label="Z2label",pos=1,site=c("a","b"),latent=TRUE)
#'conceptDiagram(moderator=moderator,labels=labels)
#'@export
conceptDiagram=function(X="X",M="M",Y="Y",latent=rep(FALSE,3),xb=FALSE,
                        radx=0.12,rady=0.05,xmargin=0.03,yinterval=NULL,
                        moderator=list(),labels=list()){

    # radx=0.12;rady=0.05;xmargin=0.03;yinterval=NULL

    if(is.null(yinterval)) yinterval=rady*4
    openplotmat()
    x=c(0+radx+xmargin,0.5)
    y=c(1-(radx+xmargin),0.5)
    m=c(0.5,0.5+yinterval)

    for(i in seq_along(moderator)){
        if(moderator[[i]]$pos==3) {
            temp=c(0.5,0.5-yinterval+0.05)
        } else if(moderator[[i]]$pos==2){
            temp=c(1-(radx+xmargin),0.5+yinterval-0.05)
        } else{
            temp=c((radx+xmargin),0.5+yinterval-0.05)
        }
        assign(paste0("z",i),temp)
    }

    xlab=ifelse(is.null(labels$X),X,labels$X)
    mlab=ifelse(is.null(labels$M),M,labels$M)
    ylab=ifelse(is.null(labels$Y),Y,labels$Y)

    myarrow(from=x,to=y,label="c'")

    if(!is.null(M)){
        myarrow(from=x,to=m,label="a")
        myarrow(from=m,to=y,label="b")
    }
    if(xb) myarrow(from=x,to=0.5*(m+y))
    for(i in seq_along(moderator)){
       z=eval(parse(text=paste0("z",i)))
       if(!is.null(M)){
           if("a" %in% moderator[[i]]$site) myarrow(from=z,to=0.5*(m+x),adjust=0)
           if("b" %in% moderator[[i]]$site) myarrow(from=z,to=0.5*(m+y),adjust=0)
       }
       if("c" %in% moderator[[i]]$site) myarrow(from=z,to=0.5*(x+y),adjust=0)

    }
    drawtext(x,radx=radx,rady=rady,lab=xlab,latent=latent[1])
    drawtext(y,radx=radx,rady=rady,lab=ylab,latent=latent[3])
    if(!is.null(M)) {
        drawtext(m,radx=radx,rady=rady,lab=ylab,latent=latent[2])
    }
    for(i in seq_along(moderator)){
        z=eval(parse(text=paste0("z",i)))
        lab=ifelse(is.null(moderator[[i]]$label),paste0("z",i),moderator[[i]]$label)
        drawtext(z,radx=radx,rady=rady,lab=lab,latent=moderator[[i]]$latent)
    }
}

