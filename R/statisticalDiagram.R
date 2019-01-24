#'Draw statistical diagram
#'@param no process macro model number
#'@param radx horizontal radius of the box.
#'@param rady vertical radius of the box.
#'@param xmargin horizontal margin of plot
#'@param arrowlabel logical whether or not draw arrowlabel
#'@export
#'@examples
#'statisticalDiagram(76)
statisticalDiagram=function(no=1,radx=0.10,rady=0.04,xmargin=0.01,arrowlabel=TRUE){

    nodes=nodes[nodes$no==no, ]
    arrows=arrows[arrows$no==no,]

    openplotmat()
    for(i in 1:nrow(arrows)){
        cat("i=",i,"\n")
        label=ifelse(arrowlabel,arrows$name[i],"")
        myarrow2(from=arrows$start[i],to=arrows$end[i],
                 label=label,no=no,xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=arrows$labelpos[i],arr.pos=arrows$arrpos[i])
    }

    for(i in 1:nrow(nodes)){
        xpos=nodes$xpos[i]
        xpos=adjustxpos(xpos,xmargin,radx)
        mid=c(xpos,nodes$ypos[i])
        drawtext(mid,radx=radx,rady=rady,lab=nodes$name[i],latent=FALSE)
    }

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
    myarrow(from=start,to=end,label=label,label.pos=label.pos,arr.pos=arr.pos,...)

}



