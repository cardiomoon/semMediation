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
#'conceptDiagram2(xb=TRUE,labels=labels)
#'moderator=list(name="Z1",label="Time Spent\n with Alex",pos=3,
#'     site=list(c("a","b","c")),latent=FALSE)
#'conceptDiagram2(moderator=moderator,labels=labels)
#'moderator=list(name=c("Z1","Z2"),label=c("Time Spent\n with Alex","Z2label"),pos=c(3,3),
#'     site=list(c("a","b","c"),c("b","c")),latent=c(FALSE,FALSE))
#'conceptDiagram2(moderator=moderator,labels=labels,yinterval=0.4)
#'@export
conceptDiagram2=function(X="X",M="M",Y="Y",latent=rep(FALSE,3),xb=FALSE,
                        radx=0.12,rady=0.06,xmargin=0.03,yinterval=NULL,
                        moderator=list(),labels=list()){

     # radx=0.12;rady=0.05;xmargin=0.03;yinterval=NULL
     # xb=FALSE
    # labels

     # X="X";M="M";Y="Y";latent=rep(FALSE,3);xb=FALSE

    if(is.null(yinterval)) yinterval=rady*6
    openplotmat()
    x=c(0+radx+xmargin,0.5)
    y=c(1-(radx+xmargin),0.5)
    m=c(0.5,0.5+yinterval)


    moderator
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
    labels

    (xlab=ifelse(is.null(labels[[X]]),X,labels[[X]]))
    if(!is.null(M)) (mlab=ifelse(is.null(labels[[M]]),M,labels[[M]]))
    (ylab=ifelse(is.null(labels[[Y]]),Y,labels[[Y]]))


    if(!is.null(M)){
        myarrow(from=x,to=y,label="c'")
        myarrow(from=x,to=m,label="a")
        myarrow(from=m,to=y,label="b")
    } else{
        myarrow(from=x,to=y,label="")
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
        drawtext(m,radx=radx,rady=rady,lab=mlab,latent=latent[2])
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


#' Make a data.frame for conceptDiagram
#'
#' @param fit An object of class lavaan. Result of sem function of package lavaan
#' @importFrom stringr str_flatten str_detect str_extract_all str_replace
#'@export
fit2df2=function(fit){
    res=parameterEstimates(fit,standardized=TRUE)
    res
    ## latent variable
    res1=res[res$op=="~",]
    res1
    text<-group<-x<-y<-latent<-c()
    count=0
    res1
    for(i in 1:nrow(res1)){
        #i=1
        temp=res1$lhs[i]
        temp
        if(!(temp %in% text)){
            text=c(text,temp)
            # whether temp is a latent varible
            tempres=any(res1[res1$lhs==temp,]$op=="=~")
            latent<-c(latent,tempres)
            # group determination #
            (tempgroup=seekGroup(temp,res1,group))

            group=c(group,tempgroup)

        }
        temp=res1$rhs[i]
        if(!(temp %in% text)){
            text=c(text,temp)
            # whether temp is a latent varible
            tempres=any(res1[res1$lhs==temp,]$op=="=~")
            latent<-c(latent,tempres)
            # group determination #
            (tempgroup=seekGroup(temp,res1,group))

            group=c(group,tempgroup)

        }
    }
    group
    df=data.frame(text,latent,group,stringsAsFactors = FALSE)
    df
    res1
    label=c()
    to=c()
    for(i in seq_along(df$text)){
        temp=stringr::str_flatten(res1$label[res1$rhs==df$text[i]],",")
        label=c(label,ifelse(length(temp)>0,temp,""))
        temp=stringr::str_flatten(res1$lhs[res1$rhs==df$text[i]],",")
        to=c(to,ifelse(length(temp)>0,temp,""))
    }
    label
    df$label=label
    df$to=to
    df
    mod=df[stringr::str_detect(df$text,":"),]
    moderator=c()

    for(i in seq_along(mod$text)){
       moderator=c(moderator,unlist(stringr::str_split(mod$text[i],":"))[2])
    }
    moderator
    df<-df[!stringr::str_detect(df$text,":"),]
    role=c()
    df
    for(i in seq_along(df$text)){
        if(df$text[i] %in% moderator) temp="Z"
        else if(stringr::str_detect(df$group[i],"M")) temp="M"
        else if(stringr::str_detect(df$group[i],"Y")) temp="Y"
        else temp="X"
        role=c(role,temp)
    }
    df$role=role
    temp=stringr::str_replace(df$label,"dash","")
    df$site=unlist(lapply(stringr::str_extract_all(temp,"[a-c,]"),myflatten))
    df$pos=0
    df$pos[stringr::str_detect(df$site,"c")]=3
    df$pos[df$pos==0 & stringr::str_detect(df$site,"a")]=1
    df$pos[df$pos==0 & stringr::str_detect(df$site,"b")]=2
    df$pos[df$pos==0 & df$site==""]=3
    df
}

#' flatten string
#' @param x character to flatten
#' @export
myflatten=function(x){
    result=stringr::str_flatten(x)
    if(length(result)==0) result=""
    result
}

#' Make conceptDiagram
#'@param fit An object of class lavaan. Result of sem function of package lavaan
#'@param labels labels
#'@importFrom stringr str_split
#'@export
conceptDiagram=function(fit,labels=NULL){
    df=fit2df2(fit)
    df
    (X=df$text[df$role=="X"])
    (Y=df$text[df$role=="Y"])
    (M=df$text[df$role=="M"])
    if(length(M)==0) {
        M<-NULL
        latent=df$latent[c(which(df$role=="X"),NA,which(df$role=="Y"))]
    } else{
        latent=df$latent[c(which(df$role=="X"),which(df$role=="M"),which(df$role=="Y"))]
    }
    xb=FALSE
    df=df[df$role=="Z",]

    label=df$text
    label
    if(length(label)>0) {
        if(!is.null(labels)){
           label=c()
           for(i in seq_along(df$text)){
              temp=labels[[df$text[i]]]
              label=c(label,temp)
           }
        }
    }
    label
    moderator=list(name=df$text,pos=df$pos,latent=df$latent,label=label)
    moderator$site=stringr::str_split(df$site,",")

    for(i in seq_along(moderator$site)){
        if(moderator$site[[i]]=="") moderator$site[[i]]<-"c"
    }
    if(is.null(M)){
        conceptDiagram2(X=X,M=NULL,Y=Y,latent=latent,moderator=moderator,labels=labels)
    } else {
       conceptDiagram2(X=X,M=M,Y=Y,latent=latent,moderator=moderator,labels=labels)
    }
}
