library(shiny)
library(semMediation)
library(stringr)
library(DT)
library(editData)
library(shinyWidgets)
library(lavaan)
library(flextable)
library(semTools)
library(jtools)
library(ggplot2)

dataFiles=list.files(path="data","*.csv")
dataNames=str_extract(dataFiles,"[^.]*")
# for(i in seq_along(dataNames)){
#     data=read.csv(paste0("data/",dataFiles[i]),stringsAsFactors = FALSE)
#     assign(dataNames[i],data)
# }

actionBttn3=function(...){
   div(style="display:inline-block;",actionBttn(...))
}

pickerInput3=function(...){
    div(style="display:inline-block;",pickerInput(...))
}

ui=fluidPage(

    h2("Select Data"),
    fluidRow(
        column(3,
               fileInput("file","Upload File or"),
               radioButtons("dataname","Select example",choices=dataNames),
               textInput("mydata","Data Name")
        ),
        column(9,
               DTOutput('table')

        )
    ),
    h2("Select Process Macro Model Number"),
    fluidRow(
        column(2,
               selectInput("modelno","Model No",choices=sort(pmacro$no),selectize=FALSE,size=28)
               ),
        column(10,

               radioGroupButtons(
                   inputId = "plotChoice",
                   label = "Select Plot",
                   choices = c("Conceptual Diagram"=1, "Statistical Diagram"=2),
                   status = "primary"
               ),

               plotOutput("modelPlot",height="500px",width="700px")
        )
    ),
    fluidRow(
        column(6,
            h2("Assign Variables"),
            uiOutput("Vars")
            ),
         column(6,
            h2("Add Covariates"),
            actionButton('insertBtn', 'Add Covariate',icon=icon("plus")),
            actionButton('removeBtn', 'Remove Covariate',icon=icon("trash-alt")),
            actionButton('resetBtn', 'Reset All',icon=icon("ban")),
            tags$div(id = 'placeholder'))
    ),
    fluidRow(
        h2("Make Equation"),
        column(2,actionButton("makeEq","make Equation",width="150px"),
               hr(),
               actionButton("resetEq","reset Equation",width="150px")
        ),
        column(4,
               textAreaInput("equation",NULL,rows=10,placeholder="You can edit equation.")
        )
    ),
    h2("Analysis"),
    actionButton("Analysis","Analysis"),
    uiOutput("result")
)

server=function(input,output,session){

    RV=reactiveValues(number=0,triple=0)


    output$modelPlot=renderPlot(
        if(input$plotChoice==1) {
            pmacroModel(as.numeric(input$modelno))
        } else{
                statisticalDiagram(as.numeric(input$modelno))
            }

    )

    observeEvent(input$file,{

        updateTextInput(session,"mydata",value="uploaded")

    })

    observeEvent(input$dataname,{
        updateTextInput(session,"mydata",value=input$dataname)
    })

   data=reactive({

        if(input$mydata=="uploaded") {
            data<-myimport(input$file$datapath)
        } else if(input$mydata %in% c("caskets","disaster","estress","glbwarm","pmi","protest","teams")) {
            data<-read.csv(paste0("data/",input$dataname,".csv"),stringsAsFactors = FALSE)
        } else {
            data<-eval(parse(text=input$mydata))
        }
       data
    })


    mylist=reactive({
        i=as.numeric(input$modelno)
        select=pmacro$no==i
        mylist=pmacro$X[select]
        if(pmacro$M[select]!="") {
            mediators=unlist(strsplit(pmacro$M[select],":"))
            mylist=c(mylist,mediators)
        }
        mylist=c(mylist,pmacro$Y[select])
        mylist=setdiff(mylist,"")
        if(pmacro$modName[select]!="") {
            moderators=unlist(strsplit(pmacro$modName[select],":"))
            mylist=c(mylist,moderators)
        }
        if(i==3) mylist=c(mylist,"Z")

        RV$varsNo=length(mylist)

        mylist
    })

    output$table=renderDT(
        data()
    )

    output$Vars=renderUI({

        output$assignVars=renderUI({
            inputlist=list()
            for(i in 1:length(mylist())){
                inputlist[[4*i-3]]=actionBttn3(paste0("addVar",i),NULL,style="simple",color="success",icon=icon("arrow-right"))
                inputlist[[4*i-2]]=label3(mylist()[i],width=20)
                inputlist[[4*i-1]]=pickerInput3(mylist()[i],NULL,
                                                choices=c("",colnames(data())),selected="",
                                                width="150px",options=list(title="Select..."))

                inputlist[[4*i]]=p("")
            }

            do.call(tagList,inputlist)

        })

         tagList(


            fluidRow(
                column(4,selectInput("chooser",NULL,
                                     choices=colnames(data()),
                                     selectize=FALSE,
                                     size=min(10,length(data())))),
                column(8,uiOutput("assignVars"),
                       if(input$modelno==1) checkboxInput("factorX","Treat X as factor",value=FALSE))
            )
        )
    })

    choices1=function(){
        count=length(mylist())
        selected=c()
        for(i in 1:count){
            selected=c(selected,input[[mylist()[i]]])
        }
        result=setdiff(setdiff(colnames(data()),selected),getCovNames())
        result
    }



    lapply(1:7,FUN=function(i){
        observeEvent(input[[paste0("addVar",i)]],{
            updateSelectInput(session,mylist()[i],selected=input$chooser)
        })
    })

    lapply(1:7,FUN=function(i){
        observeEvent(input[[mylist()[i]]],{
            updateSelectInput(session,"chooser",choices=choices1())
        })
    })

    lapply(1:10,FUN=function(i){
        observeEvent(input[[paste0("cov",i)]],{
            updateSelectInput(session,"chooser",choices=choices1())
        })
    })

    observeEvent(c(input$insertBtn,input$resetBtn,input$removeBtn),{
        updateSelectInput(session,"chooser",choices=choices1())
    })



    observeEvent(input$resetEq,{
        updateTextAreaInput(session,"equation",value="")
    })


    observeEvent(input$makeEq,{

        i=as.numeric(input$modelno)
        select=pmacro$no==i
        data1<-data()
        if(i==1){
            if(is.factor(data1[[input$X]]) |(input$factorX)){
                   i<-1.1
            }
        }
        if(i==1.1){
            model=catInteraction(Y=input$Y,W=input$W,count=length(unique(data()[[input$X]])),
                                 covar=getCovariates2())
        } else if(i %in% c(3)){
            model=tripleEquation(Y=input$Y,vars=c(input$X,input$W,input$Z),dep=input$Y,
                                 covar=getCovariates2())
        } else if(i %in% c(4.2,6,6.3,6.4)){
            temp=unlist(strsplit(pmacro$M[select],":"))
            mediators=c()
            for(j in 1:length(temp)){
                mediators=c(mediators,input[[temp[j]]])
            }
            add=ifelse(i==4.2,FALSE,TRUE)
            cat("i=",i,",add=",add,"\n")
            model=makeEquation(X=input$X,M=mediators,Y=input$Y,add2ndMediation = add,
                               covar=getCovariates2())
        } else{
            select=pmacro$no==i
            #select=3
            if(pmacro$modName[select]!=""){
                name=unlist(strsplit(pmacro$modName[select],":"))
                name
                modname=c()
                for(i in 1:length(name)){
                    modname=c(modname,input[[name[i]]])
                }
                modname
                temp=unlist(strsplit(pmacro$modSite[select],":"))
                temp
                sites=list()
                for(i in 1:length(temp)){
                    sites[[i]]=unlist(strsplit(temp[i],","))
                }
                sites
                # moderator=list(name=name,site=sites)
                moderator=list(name=modname,site=sites)
                #str(moderator)

            } else{
                moderator=NULL
            }

            pmacro$M[select]
            if(pmacro$M[select]==""){
                model=modmedEquation(X=input$X,Y=input$Y,moderator=moderator,
                                     covar=getCovariates())
            } else {
                moderator
                # model=modmedEquation(X="X",M="M",Y="Y",moderator=moderator)
                #model
                model=modmedEquation(X=input$X,M=input$Mi,Y=input$Y,moderator=moderator,
                                     covar=getCovariates())
            }
        }
        #cat(model)

        updateTextAreaInput(session,"equation",value=model)
    })

    output$result=renderUI({

        req(input$Analysis)

        data1<-data()
        if(input$modelno==3){
            data1[["interaction0"]]<-data1[[input$X]]*data1[[input$W]]*data1[[input$Z]]
        }

        modelno<-input$modelno
        if(input$modelno==1){
            if(is.factor(data1[[input$X]]) |(input$factorX)){
                data1<-addCatVar(data1,input$X)
                modelno<-1.1
            }
        }

       fit=sem(model=isolate(input$equation),data=data1)

       if(input$modelno %in% 1:3){
           probs=c(0.16,0.5,0.84)
           if(length(unique(data()[[input$W]]))<6){
               temp=sort(unique(data()[[input$W]]))
           } else{
               temp=quantile(data()[[input$W]],probs)
           }
           modValues=paste(temp,collapse=",")
           if(input$modelno %in% 2:3){
           if(length(unique(data()[[input$Z]]))<6){
               temp2=sort(unique(data()[[input$Z]]))
           } else{
               temp2=quantile(data()[[input$Z]],probs)
           }
           modValues2=paste(temp2,collapse=",")
           }
       }


        output$text=renderPrint({


                if(input$equation!=""){
                cat("model='",input$equation,"'\n")
                cat("fit=sem(model=model,data=",input$mydata,")\n")
                cat("summary(fit)\n\n")

                summary(fit)
                cat("parameterEstimates(fit)\n\n")
                print(parameterEstimates(fit))
                cat("\n\n")
                if(!is.null(discriminantValidityTable(fit))){
                    cat("\n\ndiscriminantValidityTable(fit)\n\n")
                    print(discriminantValidityTable(fit))
                }
                if(!is.null(reliabilityTable(fit))){
                cat("reliablityTable(fit)\n\n")
                print(reliabilityTable(fit))
                }
                }

        })

        output$estimateTable=renderUI({



            if(input$equation!=""){

                seek=NULL
                replace=NULL
                if(input$modelno==3){
                    seek="interaction0"
                    replace=paste(input$X,input$W,input$Z,sep=":")
                }
                estimatesTable2(fit,vanilla=input$vanilla,
                                digits=as.numeric(input$digits),
                                seek=seek,
                                replace=replace) %>%
                htmltools_value()
            }

        })

        output$corTable=renderUI({
                if(input$equation!=""){
                    seek=NULL
                    replace=NULL
                    if(input$modelno==3){
                        seek="interaction0"
                        replace=paste(input$X,input$W,input$Z,sep=":")
                    }
                    corTable2(fit,vanilla=input$vanilla,seek=seek,replace=replace) %>%
                        htmltools_value()
                }
        })

        output$corPlot=renderPlot({
            if(input$equation!=""){
                seek=NULL
                replace=NULL
                if(input$modelno==3){
                    seek="interaction0"
                    replace=paste(input$X,input$W,input$Z,sep=":")
                }
                corPlot(fit,seek=seek,replace=replace)
            }
        })

        output$reliabilityTable=renderUI({
            if(input$equation!=""){
                reliabilityTable2(fit,vanilla=input$vanilla)  %>%
                    htmltools_value()
            }
        })

        output$discriminantValidityTable=renderUI({
            if(input$equation!=""){

                discriminantValidityTable2(fit,vanilla=input$vanilla) %>%
                    htmltools_value()
            }
        })

        output$modelFitTable=renderUI({
            if(input$equation!=""){

                modelFitTable2(fit,vanilla=input$vanilla) %>%
                    htmltools_value()
            }
        })


        # output$diagram=renderGrViz({
        #
        #
        #         if(input$equation!=""){
        #             semDiagram(fit)
        #         }
        #
        # })

        output$concept=renderPlot({
            if(input$equation!=""){


            names<-mylist()
            labels=list()
            for(i in 1:length(names)){
                labels[[names[i]]]=input[[names[i]]]
            }

            covar=getCovariates()

            pmacroModel(no=as.numeric(input$modelno),labels=labels,covar=covar)
            }
        })
        output$statDiagram=renderPlot({

            input$makeEq

            names<-mylist()
            labels=list()


            for(i in 1:length(names)){
                labels[[names[i]]]=input[[names[i]]]
            }
            table1=estimatesTable(fit,digits=as.numeric(input$digits))
            if(input$modelno==3){
                temp=paste(input$X,input$W,input$Z,sep=":")
                table1$Predictors[table1$Predictors=="interaction0"]=temp
            }
            no=as.numeric(input$modelno)
            if(no==1){
                if(is.factor(data1[[input$X]]) |(input$factorX)){
                    no<-1.1
                }
            }
            if(no==1.1){
                for(i in 2:length(levels(data1[[input$X]]))){
                    labels[[paste0("d",i)]]=paste0(input$X,"=",levels(data1[[input$X]])[i])
                    labels[[paste0("d",i,":",input$W)]]=paste0(input$X,"=",levels(data1[[input$X]])[i],":",input$W)
                }
            }

            statisticalDiagram(no=no,labels=labels,
                               whatLabel = input$whatLabel,estimateTable=table1,
                               radx=as.numeric(input$radx),
                               covar=getCovariates())


        })

        output$moderationPlot=renderPlot({

            input$applyValue

            # data1<-data()
            temp=paste0("lm(",getRegEq(),",data=data1)")
            # str(data1)
            # print(temp)
            fit=eval(parse(text=temp))

            probs<-modx.values<-NULL
            if(isolate(input$probs)!="") probs=as.numeric(unlist(strsplit(input$probs,",")))
            if(isolate(input$mod1values)!="") modx.values=as.numeric(unlist(strsplit(input$mod1values,",")))

            if(modelno==1.1){
                # str(fit)
                temp=paste0("interact_plot(model=fit,pred=",input$W,",modx=",input$X,
                            ",interval=",input$interval,
                            ",int.type='",input$inttype,"',int.width=",input$intwidth,
                            ",plot.points=",input$plotpoints,
                            ",linearity.check=",input$linearity,")")
                # print(temp)
                eval(parse(text=temp))
                # ,modx.values=modx.values,
                #                    plot.points=input$plotpoints,
                #                    linearity.check=input$linearity)
            } else {
                pred=input$X
                modx=input$W
            condEffect(fit=fit,pred=pred,modx=modx,show.Effect=input$showeffect,
                       switchVars=input$switchMod,probs=probs,modx.values=modx.values,
                       plot.points=input$plotpoints,interval=input$interval,int.type=input$inttype,int.width=input$intwidth,
                       linearity.check=input$linearity)
            }
        })



        output$interactPlot2=renderPlot({

            input$applyValue

            # data1<-data()
            temp=paste0("lm(",getRegEq(),",data=data1)")
            # print(temp)
            fit=eval(parse(text=temp))

            mod1=input$W
            mod2=input$Z
            mod1values=vector2string(isolate(input$mod1values))
            mod2values=vector2string(isolate(input$mod2values))
            if(input$switchMod){
                mod1=input$Z
                mod2=input$W

            }
            temp=paste0("interact_plot(fit,pred=",input$X,",modx=",mod1,",modx.values = ",
                        mod1values,",mod2=",mod2,",mod2.values=",mod2values,
                        ",plot.points=",input$plotpoints,",interval=",input$interval,
                        ",int.type='",input$inttype,"',int.width=",input$intwidth,
                        ",linearity.check=",input$linearity,")")
            # print(temp)
            eval(parse(text=temp))
        })

        output$ss=renderPrint({

            input$applyValue

            # data1<-data()
            temp=paste0("lm(",getRegEq(),",data=data1)")

            fit=eval(parse(text=temp))

            pred=input$X
            modx=input$W
            if(modelno==1.1){
                pred=input$W
                modx=input$X
            }
            if(isolate(input$mod1values)=="") {
                temp=paste0("sim_slopes(fit,pred=",pred,",modx=",modx,",confint =", input$interval2,",digits=3)")
            } else{
                modx.values=as.numeric(unlist(strsplit(input$mod1values,",")))
                modx1=paste0("c(",paste(modx.values,collapse=","),")")
                temp=paste0("sim_slopes(fit,pred=",pred,",modx=",modx,
                            paste0(",mod",ifelse(modelno==1.1,"2","x"),".values="),modx1,
                            ",confint =", input$interval2,",digits=3)")
            }
            # print(temp)
            ss=eval(parse(text=temp))
            ss
        })

        output$ss2=renderPrint({

            input$applyValue

            # data1<-data()
            temp=paste0("lm(",getRegEq(),",data=data1)")
            mod1=input$W
            mod2=input$Z
            mod1values=vector2string(isolate(input$mod1values))
            mod2values=vector2string(isolate(input$mod2values))
            if(input$switchMod){
                mod1=input$Z
                mod2=input$W

            }
            fit=eval(parse(text=temp))
            temp=paste0("sim_slopes(fit,pred=",input$X,",modx=",mod1,",mod2=",mod2,
                        ",modx.values=",mod1values,",mod2.values=",mod2values,
                        ",confint =", input$interval2,")")

            # cat("In ss2 :",temp,"\n")
            ss=eval(parse(text=temp))
            ss
        })



        output$ssPlot=renderPlot({
            input$applyValue

            # data1<-data()
            temp=paste0("lm(",getRegEq(),",data=data1)")

            fit=eval(parse(text=temp))
            pred=input$X
            modx=input$W
            if(modelno==1.1){
                pred=input$W
                modx=input$X
            }
            if(isolate(input$mod1values)=="") {
                temp=paste0("sim_slopes(fit,pred=",pred,",modx=",modx,",confint =", input$interval2,")")
            } else{
                modx.values=as.numeric(unlist(strsplit(input$mod1values,",")))
                modx1=paste0("c(",paste(modx.values,collapse=","),")")
                temp=paste0("sim_slopes(fit,pred=",pred,",modx=",modx,
                            paste0(",mod",ifelse(modelno==1.1,"2","x"),".values="),modx1,
                            ",confint =", input$interval2,")")
            }
            ss=eval(parse(text=temp))
            plot(ss)
        })

        output$ssPlot2=renderPlot({

            input$applyValue

            # data1<-data()
            temp=paste0("lm(",getRegEq(),",data=data1)")
            mod1=input$W
            mod2=input$Z
            mod1values=vector2string(isolate(input$mod1values))
            mod2values=vector2string(isolate(input$mod2values))
            if(input$switchMod){
                mod1=input$Z
                mod2=input$W

            }
            fit=eval(parse(text=temp))
            temp=paste0("sim_slopes(fit,pred=",input$X,",modx=",mod1,",mod2=",mod2,
                        ",modx.values=",mod1values,",mod2.values=",mod2values,
                        ",confint =", input$interval2,")")

            #cat("In ss2 :",temp,"\n")
            ss=eval(parse(text=temp))

            plot(ss)
        })

        output$JNText=renderPrint({
            # data1<-data()
            fit=eval(parse(text=paste0("lm(",getRegEq(),",data=data1)")))


            if(modelno==1.1){
                pred=input$W
                modx=input$X

                johnson_neyman(fit,pred=pred,modx=modx,plot=FALSE,digits=3)
            }else{
                pred=input$X
                modx=input$W
                pred=ifelse(input$switchMod,input$W,input$X)
                modx=ifelse(input$switchMod,input$X,input$W)

                johnson_neyman(fit,pred=pred,modx=modx,alpha=input$alpha,plot=FALSE,digits=3)
            }



        })
        output$JNPlot=renderPlot({
            # data1<-data()
            fit=eval(parse(text=paste0("lm(",getRegEq(),",data=data1)")))
            # pred=ifelse(input$switchMod,input$W,input$X)
            # modx=ifelse(input$switchMod,input$X,input$W)
            if(modelno==1.1){
                pred=input$W
                modx=input$X

                johnson_neyman(fit,pred=pred,modx=modx)
            }else{
                pred=input$X
                modx=input$W
                pred=ifelse(input$switchMod,input$W,input$X)
                modx=ifelse(input$switchMod,input$X,input$W)


            XM=paste(input$X,input$W,sep=":")
            label=paste0("italic(theta) [italic(X) %->% italic(Y)] == ",
                         sprintf("%.03f",fit$coef[pred]),"+",sprintf("%.03f",fit$coef[XM]),"*italic(W)")

            result=johnson_neyman(fit,pred=pred,modx=modx,plot=FALSE,alpha=input$alpha)
            pos=relpos(result$plot)
            result$plot+
                annotate("text",x=result$bounds,y=-Inf,label=round(result$bounds,3),
                         vjust=-0.5,hjust=-0.1)+
                annotate("text",x=pos[1],y=pos[2],label=label,parse=TRUE)
        }


        })

        output$JNPlot2=renderPlot({

            input$applyValue
            # data1<-data()
            fit=eval(parse(text=paste0("lm(",getRegEq(),",data=data1)")))
            mod1=input$W
            mod2=input$Z
            mod1values=vector2string(isolate(input$mod1values))
            mod2values=vector2string(isolate(input$mod2values))
            if(input$switchMod){
                mod1=input$Z
                mod2=input$W

            }
            temp=paste0("sim_slopes(fit,pred=",input$X,",modx=",mod1,",mod2=",mod2,
                        ",modx.values=",mod1values,",mod2.values=",mod2values,",jnplot=TRUE)")

            # cat("In JNPlot2 :",temp,"\n")
            eval(parse(text=temp))

        })

        output$regEquation=renderPrint({


            eq=getRegEq()
            # data1<-data()
            if(input$modelno=="1"){
                if(input$factorX){
                    data1[[input$X]]=factor(data1[[input$X]])
                }
            }
            # print(eq)
            fit=eval(parse(text=paste0("lm(",eq,",data=data1)")))

            summary(fit)
        })


        tagList(
            checkboxInput("vanilla","vanilla table",value=FALSE),
            verbatimTextOutput("text"),
            h2("Conceptual Diagram"),
            plotOutput("concept",height="500px",width="700px"),
            h2("Estimates Table"),
            uiOutput("estimateTable"),
            h2("Statistical Diagram"),
            selectInput3("whatLabel","whatLabel",choices=c("est","std","name")),
            selectInput3("digits","digits",choices=c(2,3,4),selected=3),
            selectInput3("radx","box width",
                         choices=c("0.04","0.06","0.08","0.10","0.12","0.14"),
                         selected="0.10"),
            plotOutput("statDiagram",height="500px",width="700px"),
            h2("Correlation Table"),
            uiOutput("corTable"),
            h2("Correlation Plot"),
            plotOutput("corPlot"),
            h2("Model Fit Table"),
            uiOutput("modelFitTable"),
            if(input$modelno %in% 1:3) h2("Moderation Effect"),
            if(input$modelno %in% 1:3) checkboxInput3("switchMod","switch moderator",
                                                      value=FALSE,width=200),
            if(input$modelno==1) textInput3("probs","probs",
                                            value="",placeholder="0.16,0.5,0.84",width=150),
            if(input$modelno %in% 1:3)
                textInput3("mod1values","mod1 values",value="",placeholder=modValues,width=150),
            if(input$modelno %in% 2:3)
                textInput3("mod2values","mod2 values",value="",placeholder=modValues2,width=150),
            if(input$modelno %in% 1:3) actionButton("applyValue","Apply Values"),
            if(input$modelno %in% 1:3) br(),
            if(modelno==1) checkboxInput3("showeffect","show effect",value=TRUE,width=120),
            if(input$modelno %in% 1:3)
                checkboxInput3("plotpoints","show points",value=FALSE,width=120),
            if(input$modelno %in% 1:3)
                checkboxInput3("interval","show interval",value=FALSE,width=120),
            if(input$modelno %in% 1:3)
                pickerInput3("inttype","type",choices=c("confidence","prediction"),width=120),
            if(input$modelno %in% 1:3)
                numericInput3("intwidth","width",value=0.95,min=0.1,max=1,step=0.01),

            if(input$modelno %in% 1:3) checkboxInput3("linearity","linearity check",
                                                      value=FALSE,width=200),
            if(input$modelno==1) plotOutput("moderationPlot"),
            if(input$modelno %in% c(2,3)) plotOutput("interactPlot2"),
            if(input$modelno %in% 1:3) h2("Simple Slope Analysis"),
            if(input$modelno %in% 1:3) checkboxInput3("interval2","show confidence interval",
                                                      value=FALSE,width=220),
            if(input$modelno==1) verbatimTextOutput("ss"),
            if(input$modelno==1) plotOutput("ssPlot"),

            if(input$modelno %in% c(2,3)) verbatimTextOutput("ss2"),
            if(input$modelno %in% c(2,3)) plotOutput("ssPlot2"),

            if(modelno %in% 1:3) h2("Johnson-Neyman Intervals"),
            if(modelno==1) verbatimTextOutput("JNText"),
            if(modelno==1) numericInput3("alpha","alpha",value=0.05,min=0.01,max=1,step=0.01),

            if(modelno==1) plotOutput("JNPlot"),
            if(input$modelno %in% c(2,3)) verbatimTextOutput("JNText2"),
            if(input$modelno %in% c(2,3)) plotOutput("JNPlot2"),
            verbatimTextOutput("regEquation")
            # h2("Reliability Table"),
            # uiOutput("reliabilityTable"),
            # h2("Discriminant Validity Table"),
            # uiOutput("discriminantValidityTable")



        )

    })

    observeEvent(input$switchMod,{
        probs=c(0.16,0.5,0.84)
        if(input$modelno==1){
            var1=ifelse(input$switchMod,input$X,input$W)
            modValues=getPlaceholder(var1)
            updateTextInput(session,"mod1values",label=paste0(var1," values"),
                            value="",placeholder = modValues)
        }  else if(input$modelno %in% c(2,3)){
            var1=ifelse(input$switchMod,input$Z,input$W)
            var2=ifelse(input$switchMod,input$W,input$Z)
            modValues1=getPlaceholder(var1)
            modValues2=getPlaceholder(var2)
            updateTextInput(session,"mod1values",label=paste0(var1," values"),
                            value=input$mod2values,placeholder = modValues1)
            updateTextInput(session,"mod2values",label=paste0(var2," values"),
                            value=input$mod1values,placeholder = modValues2)
        }
    })

    getPlaceholder=function(name){
        probs=c(0.16,0.5,0.84)
        if(length(unique(data()[[name]]))==2){
            temp=sort(unique(data()[[name]]))
        } else{
            temp=quantile(data()[[name]],probs)
        }
        paste(temp,collapse=",")
    }

    observeEvent(input$linearity,{
        if(input$linearity) {
            updateCheckboxInput(session,"showeffect",value=FALSE)
        }
    })

    observeEvent(input$showeffect,{
        if(input$showeffect) {
            updateCheckboxInput(session,"linearity",value=FALSE)
        }
    })


   observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        id <- length(RV$inserted)/3+1
        if(input$modelno %in% c(1,2,3,6.3,6.4)) {
            covchoices="Y"
        } else if(input$modelno %in% c(4.2,6)) {
            covchoices=c("M1,M2,Y","M1,Y","M2,Y","Y")
        } else if(input$modelno == "6.3"){
            covchoices=c("M1,M2,M3,Y","Y")
        } else if(input$modelno == "6.4"){
            covchoices=c("M1,M2,M3,M4,Y","Y")
        } else{
            covchoices=c("Mi,Y","Mi","Y")
        }

        insertUI(
            selector = '#placeholder',
            ## wrap element in a div with id for ease of removal
            ui = tagList(
                tags$div(h4(paste0("Covariate ",id,"    Site")),id=paste0(id)),
                selectInput3(paste0("cov",id),NULL,choices=choices1()),
                selectInput3(paste0("site",id),NULL,choices=covchoices)
            )
        )
        RV$inserted <-c(paste0("div:has(> #cov",id,")"),paste0('#',id),paste0("div:has(> #site", id,")"),RV$inserted)
        RV$number=as.numeric(RV$number)+1

    })

    observeEvent(input$removeBtn, {


        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[1]
        )
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[2]
        )
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[3]
        )

        RV$inserted <- RV$inserted[-(1:3)]
        RV$number=ifelse(RV$number>0,as.numeric(RV$number)-1,0)
    })

    observeEvent(input$resetBtn, {
        while(RV$number>0){
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[1]
        )
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[2]
        )
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[3]
        )

        RV$inserted <- RV$inserted[-(1:3)]
        RV$number=ifelse(RV$number>0,as.numeric(RV$number)-1,0)
        }


    })

    getCovNames=reactive({
        count=RV$number

        temp=c()
        if(count) for(i in 1:count){
            temp=c(temp,input[[paste0("cov",i)]])
        }
        temp
    })

    getCovSites=reactive({
        count=RV$number

        result=list()
        if(count>0) for(i in 1:count){
            temp=input[[paste0("site",i)]]
            result[[i]]<-unlist(str_split(temp,","))
        }
        result
    })

    getCovSites2=reactive({
        count=RV$number

        result=list()
        if(count>0) for(i in 1:count){
            temp=input[[paste0("site",i)]]
            temp1<-unlist(str_split(temp,","))
            temp2=c()
            for(j in 1:length(temp1)){
                temp2=c(temp2,input[[temp1[j]]])
            }
            result[[i]]<-temp2

        }
        result
    })

    getCovariates=reactive({
        result<-list()
        if(RV$number>0) {
            result$name=getCovNames()
            result$site=getCovSites()
        }
        result
    })

    getCovariates2=reactive({
        result<-list()
        if(RV$number>0) {
            result$name=getCovNames()
            result$site=getCovSites2()
        }
        result
    })

    getModerator=reactive({
    i=as.numeric(input$modelno)
    select=pmacro$no==i
    if(i %in% c(4.2,6,6.3,6.4)){
        # temp=unlist(strsplit(pmacro$M[select],":"))
        # mediators=c()
        # for(j in 1:length(temp)){
        #     mediators=c(mediators,input[[temp[j]]])
        # }
        # add=ifelse(i==4.2,FALSE,TRUE)
        # cat("i=",i,",add=",add,"\n")
        # model=makeEquation(X=input$X,M=mediators,Y=input$Y,add2ndMediation = add,
        #                    covar=getCovariates2())
        moderator=NULL
    } else{
        select=pmacro$no==i
        #select=3
        if(pmacro$modName[select]!=""){
            name=unlist(strsplit(pmacro$modName[select],":"))
            name
            modname=c()
            for(i in 1:length(name)){
                modname=c(modname,input[[name[i]]])
            }
            modname
            temp=unlist(strsplit(pmacro$modSite[select],":"))
            temp
            sites=list()
            for(i in 1:length(temp)){
                sites[[i]]=unlist(strsplit(temp[i],","))
            }
            sites
            # moderator=list(name=name,site=sites)
            moderator=list(name=modname,site=sites)
            #str(moderator)

        } else{
            moderator=NULL
        }
        moderator
    }
    })

    getRegEq=reactive({
        if(input$modelno==3){
           tripleEquation(Y=input$Y,vars=c(input$X,input$W,input$Z),dep=input$Y,
                          covar=getCovariates2(),mode=1)
        } else{
           regEquation(X=input$X,Y=input$Y,moderator=getModerator(),covar=getCovariates2())
        }
    })

    vector2string=function(x){
        if(x=="") {
            result="NULL"
        } else{
            temp=unlist(strsplit(x,","))
            result=paste0("c(",paste0(temp,collapse=","),")")
        }
        result
    }



}

shinyApp(ui,server)

