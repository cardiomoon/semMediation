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
library(mediation)
library(interactions)

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

theme_set(theme_bw(base_family = "NanumGothic"))

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
        column(3,actionButton("makeEq","make Equation",width="150px"),
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


    output$modelPlot=renderPlot({
        par(family="NanumGothic")

        if(input$plotChoice==1) {
            pmacroModel(as.numeric(input$modelno))
        } else{
                statisticalDiagram(as.numeric(input$modelno))
            }

    })

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
        if(i %in% c(3,11:13,18:20)) mylist=c(mylist,"Z")

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
            model=tripleEquation(X=input$X,Y=input$Y,
                                 vars=getTripleVars(),covar=getCovariates2())
        } else if(i %in% c(11:13,18:20)){
            model=tripleEquation(X=input$X,M=input$Mi,Y=input$Y,
                                 vars=getTripleVars(),
                                 moderator=getModerator(),
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
        } else if(i %in% c(1,2)){
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
        } else{
            model=tripleEquation(X=input$X,M=input$Mi,Y=input$Y,
                                 vars=getTripleVars(),
                                 moderator=getModerator(),
                                 covar=getCovariates2())

        }
        #cat(model)

        updateTextAreaInput(session,"equation",value=model)
    })

    output$result=renderUI({

        req(input$Analysis)

        data1<-data()
        if(input$modelno %in% c(3,11:13)){
            data1[["interaction0"]]<-data1[[input$X]]*data1[[input$W]]*data1[[input$Z]]
        } else if(input$modelno %in% c(18:20)){
            data1[["interaction0"]]<-data1[[input$Mi]]*data1[[input$W]]*data1[[input$Z]]
            if(input$modelno==19){
                data1[["interaction1"]]<-data1[[input$X]]*data1[[input$W]]*data1[[input$Z]]
            }
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
                if(input$modelno %in% c(3,11:13)){
                    seek="interaction0"
                    replace=paste(input$X,input$W,input$Z,sep=":")
                } else if(input$modelno %in% c(18:20)){
                    seek="interaction0"
                    replace=paste(input$Mi,input$W,input$Z,sep=":")
                    if(input$modelno==19){
                        seek="interaction1"
                        replace=paste(input$X,input$W,input$Z,sep=":")
                    }
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
                    if(input$modelno %in% c(3,11:13)){
                        seek="interaction0"
                        replace=paste(input$X,input$W,input$Z,sep=":")
                    } else if(input$modelno %in% c(18:20)){
                        seek="interaction0"
                        replace=paste(input$Mi,input$W,input$Z,sep=":")
                        if(input$modelno==19){
                            seek="interaction1"
                            replace=paste(input$X,input$W,input$Z,sep=":")
                        }
                    }
                    corTable2(fit,vanilla=input$vanilla,seek=seek,replace=replace) %>%
                        htmltools_value()
                }
        })

        output$corPlot=renderPlot({
            if(input$equation!=""){
                seek=NULL
                replace=NULL
                if(input$modelno %in% c(3,11:13)){
                    seek="interaction0"
                    replace=paste(input$X,input$W,input$Z,sep=":")
                } else if(input$modelno %in% c(18:20)){
                    seek="interaction0"
                    replace=paste(input$Mi,input$W,input$Z,sep=":")
                    if(input$modelno==19){
                        seek="interaction1"
                        replace=paste(input$X,input$W,input$Z,sep=":")
                    }
                }
                corPlot(fit,seek=seek,replace=replace)+
                    theme(text=element_text(family="NanumGothic"))
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

        output$mediationTable=renderUI({
            if(input$equation!=""){
                data1<-data()
                eq=getRegEq()

            eq=unlist(strsplit(eq,"\n"))

            temp1=paste0("lm(",eq[1],",data=data1)")
            print(temp1)
            fit1=eval(parse(text=temp1))
            temp2=paste0("lm(",eq[2],",data=data1)")
            print(temp2)
            fit2=eval(parse(text=temp2))
            x=modelsSummary(fit1,fit2)
            modelsSummaryTable(x) %>%
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
            par(family="NanumGothic")
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
            par(family="NanumGothic")

            input$makeEq

            names<-mylist()
            labels=list()


            for(i in 1:length(names)){
                labels[[names[i]]]=input[[names[i]]]
            }
            table1=estimatesTable(fit,digits=as.numeric(input$digits))

            if(input$modelno %in% c(3,11:13)){
                temp=paste(input$X,input$W,input$Z,sep=":")
                table1$Predictors[table1$Predictors=="interaction0"]=temp
            } else if(input$modelno %in% c(18:20)){

                temp=paste(input$Mi,input$W,input$Z,sep=":")
                table1$Predictors[table1$Predictors=="interaction0"]=temp
                if(input$modelno==19){
                    temp1=paste(input$X,input$W,input$Z,sep=":")
                    table1$Predictors[table1$Predictors=="interaction1"]=temp1
                }
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

        # output$moderationPlot=renderPlot({
        #
        #     input$applyValue
        #
        #     # data1<-data()
        #     temp=paste0("lm(",getRegEq(),",data=data1)")
        #     # str(data1)
        #     # print(temp)
        #     fit=eval(parse(text=temp))
        #
        #     probs<-modx.values<-NULL
        #     if(isolate(input$probs)!="") probs=as.numeric(unlist(strsplit(input$probs,",")))
        #     if(isolate(input$mod1values)!="") modx.values=as.numeric(unlist(strsplit(input$mod1values,",")))
        #
        #     if(modelno==1.1){
        #         # str(fit)
        #         temp=paste0("interact_plot(model=fit,pred=",input$W,",modx=",input$X,
        #                     ",interval=",input$interval,
        #                     ",int.type='",input$inttype,"',int.width=",input$intwidth,
        #                     ",plot.points=",input$plotpoints,
        #                     ",linearity.check=",input$linearity,")")
        #         # print(temp)
        #         eval(parse(text=temp))
        #         # ,modx.values=modx.values,
        #         #                    plot.points=input$plotpoints,
        #         #                    linearity.check=input$linearity)
        #     } else {
        #         pred=input$X
        #         modx=input$W
        #     condEffect(fit=fit,pred=pred,modx=modx,show.Effect=input$showeffect,
        #                switchVars=input$switchMod,probs=probs,modx.values=modx.values,
        #                plot.points=input$plotpoints,interval=input$interval,int.type=input$inttype,int.width=input$intwidth,
        #                linearity.check=input$linearity)
        #     }
        # })
        #
        #
        #
        # output$interactPlot2=renderPlot({
        #
        #     input$applyValue
        #
        #     # data1<-data()
        #     temp=paste0("lm(",getRegEq(),",data=data1)")
        #     # print(temp)
        #     fit=eval(parse(text=temp))
        #
        #     mod1=input$W
        #     mod2=input$Z
        #     mod1values=vector2string(isolate(input$mod1values))
        #     mod2values=vector2string(isolate(input$mod2values))
        #     if(input$switchMod){
        #         mod1=input$Z
        #         mod2=input$W
        #
        #     }
        #     temp=paste0("interact_plot(fit,pred=",input$X,",modx=",mod1,",modx.values = ",
        #                 mod1values,",mod2=",mod2,",mod2.values=",mod2values,
        #                 ",plot.points=",input$plotpoints,",interval=",input$interval,
        #                 ",int.type='",input$inttype,"',int.width=",input$intwidth,
        #                 ",linearity.check=",input$linearity,")")
        #     print(temp)
        #     eval(parse(text=temp))
        # })

        output$interactPlot3=renderPlot({

            input$applyValue

            # data1<-data()
            eq<- getRegEq()
            eq<-unlist(strsplit(eq,"\n"))
            if(length(eq)>1) eq=eq[2]
            temp=paste0("lm(",eq,",data=data1)")
            cat("interactionPlot3\n")
            cat("temp=",temp,"\n")
            fit=eval(parse(text=temp))
            print(summary(fit))

            mod1=input$moderator1
            mod2=input$moderator2
            mod3<-""
            if(!is.null(input$moderator3)) mod3=input$moderator3
            if(mod3==""){
            temp=paste0("interact_plot(fit,pred=",mod1,",modx=",mod2,
                           # ",modx.values = ",mod1values,
                        # ",mod2=",mod2,
                        # ",mod2.values=",mod2values,
                        ",plot.points=",input$plotpoints,",interval=",input$interval,
                        ",int.type='",input$inttype,"',int.width=",input$intwidth,
                        ",linearity.check=",input$linearity,")")
            } else{
                temp=paste0("interact_plot(fit,pred=",mod1,",modx=",mod2,
                            # ",modx.values = ",mod1values,
                            ",mod2=",mod3,
                            # ",mod2.values=",mod2values,
                            ",plot.points=",input$plotpoints,",interval=",input$interval,
                            ",int.type='",input$inttype,"',int.width=",input$intwidth,
                            ",linearity.check=",input$linearity,")")
            }
            print(temp)
            p<-eval(parse(text=temp))
            p+theme(text=element_text(family="NanumGothic"))

        })

        output$ss=renderPrint({

            input$applyValue

            # data1<-data()
            temp=paste0("lm(",getRegEq(),",data=data1)")

            fit=eval(parse(text=temp))

            pred=input$moderator1
            modx=input$moderator2

            # if(isolate(input$mod1values)=="") {
                temp=paste0("sim_slopes(fit,pred=",pred,",modx=",modx,",confint =", input$interval2,",digits=3)")
            # } else{
            #     modx.values=as.numeric(unlist(strsplit(input$mod1values,",")))
            #     modx1=paste0("c(",paste(modx.values,collapse=","),")")
            #     temp=paste0("sim_slopes(fit,pred=",pred,",modx=",modx,
            #                 paste0(",mod",ifelse(modelno==1.1,"2","x"),".values="),modx1,
            #                 ",confint =", input$interval2,",digits=3)")
            # }
            # print(temp)
            ss=eval(parse(text=temp))
            ss
        })

        output$ss2=renderPrint({

            input$applyValue

            # data1<-data()
            temp=paste0("lm(",getRegEq(),",data=data1)")
            pred=input$moderator1
            mod1=input$moderator2
            mod2=input$moderator3
            # mod1values=vector2string(isolate(input$mod1values))
            # mod2values=vector2string(isolate(input$mod2values))
            # if(input$switchMod){
            #     mod1=input$Z
            #     mod2=input$W
            #
            # }
            fit=eval(parse(text=temp))
            temp=paste0("sim_slopes(fit,pred=",input$X,",modx=",mod1,",mod2=",mod2,
                        # ",modx.values=",mod1values,",mod2.values=",mod2values,
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

            pred=input$moderator1
            modx=input$moderator2
            # mod2=input$moderator3pred=input$X
            # modx=input$W
            # if(modelno==1.1){
            #     pred=input$W
            #     modx=input$X
            # }
            # if(isolate(input$mod1values)=="") {
                temp=paste0("sim_slopes(fit,pred=",pred,",modx=",modx,",confint =", input$interval2,")")
            # } else{
            #     modx.values=as.numeric(unlist(strsplit(input$mod1values,",")))
            #     modx1=paste0("c(",paste(modx.values,collapse=","),")")
            #     temp=paste0("sim_slopes(fit,pred=",pred,",modx=",modx,
            #                 paste0(",mod",ifelse(modelno==1.1,"2","x"),".values="),modx1,
            #                 ",confint =", input$interval2,")")
            # }
            ss=eval(parse(text=temp))
            plot(ss)+theme(text=element_text(family="NanumGothic"))
        })

        output$ssPlot2=renderPlot({

            input$applyValue

            # data1<-data()
            temp=paste0("lm(",getRegEq(),",data=data1)")
            pred=input$moderator1
            mod1=input$moderator2
            mod2=input$moderator3
            # mod1=input$W
            # mod2=input$Z
            # mod1values=vector2string(isolate(input$mod1values))
            # mod2values=vector2string(isolate(input$mod2values))
            # if(input$switchMod){
            #     mod1=input$Z
            #     mod2=input$W
            #
            # }
            fit=eval(parse(text=temp))
            temp=paste0("sim_slopes(fit,pred=",pred,",modx=",mod1,",mod2=",mod2,
                        # ",modx.values=",mod1values,",mod2.values=",mod2values,
                        ",confint =", input$interval2,")")

            #cat("In ss2 :",temp,"\n")
            ss=eval(parse(text=temp))

            plot(ss)+theme(text=element_text(family="NanumGothic"))
        })

        output$JNText=renderPrint({
            # data1<-data()
            temp= paste0("lm(",getRegEq(),",data=data1)")
            cat("fit=lm(",getRegEq(),",data=",input$mydata,")\n")
            fit=eval(parse(text=temp))

            pred=input$moderator1
            modx=input$moderator2


            temp=paste0("johnson_neyman(fit,pred=",pred,",modx=",modx,",alpha=",input$alpha,",plot=FALSE)")
            cat(temp,"\n\n")
            eval(parse(text=temp))


        })
        output$JNPlot=renderPlot({
            # data1<-data()
            fit=eval(parse(text=paste0("lm(",getRegEq(),",data=data1)")))
            # pred=ifelse(input$switchMod,input$W,input$X)
            # modx=ifelse(input$switchMod,input$X,input$W)
            pred=input$moderator1
            modx=input$moderator2

            # XM=paste(pred,modx,sep=":")
            # label=paste0("italic(theta) [italic(X) %->% italic(Y)] == ",
            #              sprintf("%.03f",fit$coef[pred]),"+",sprintf("%.03f",fit$coef[XM]),"*italic(W)")

            temp=paste0("johnson_neyman(fit,pred=",pred,",modx=",modx,",alpha=",input$alpha,")")
            # print(temp)
            p<-eval(parse(text=temp))
            p$plot+theme(text=element_text(family="NanumGothic"))
            # pos=relpos(result$plot)
            # result$plot
            # +
            #     annotate("text",x=result$bounds,y=-Inf,label=round(result$bounds,3),
            #              vjust=-0.5,hjust=-0.1)+
            #     annotate("text",x=pos[1],y=pos[2],label=label,parse=TRUE)
        })

        output$JNPlot2=renderPlot({

            input$applyValue
            # data1<-data()
            fit=eval(parse(text=paste0("lm(",getRegEq(),",data=data1)")))
            pred=input$moderator1
            mod1=input$moderator2
            mod2=input$moderator3


            temp=paste0("sim_slopes(fit,pred=",pred,",modx=",mod1,",mod2=",mod2,
                        # ",modx.values=",mod1values,",mod2.values=",mod2values,
                        ",jnplot=TRUE)")

            # cat("In JNPlot2 :",temp,"\n")
            eval(parse(text=temp))+theme(text=element_text(family="NanumGothic"))

        })

        output$regEquation=renderPrint({

            # cat("getTripleVars()\n")
            # str(getTripleVars())
            # cat("getModerator()\n")
            # str(getModerator())


            eq=getRegEq()

            # cat("eq=",eq,"\n")
            # str(eq)
            # cat("input$modelno=",input$modelno,"\n")
            # cat("input$modelno %in% c(11:13,18:20)=",input$modelno %in% c(11:13,18:20),"\n")

            # data1<-data()
            if(input$modelno=="1"){
                if(input$factorX){
                    data1[[input$X]]=factor(data1[[input$X]])
                }
            }
            eq=unlist(strsplit(eq,"\n"))

            cat("getAllModerators()\n")
            print(getAllModerators())
            cat("\n")

            if(length(eq)==1){

               cat("Regression Analysis\n\n")
               fit=eval(parse(text=paste0("lm(",eq,",data=data1)")))
               cat("fit=lm(",eq,",data=",input$mydata,")\nsummary(fit)\n")
               summary(fit)
            } else{
                cat("Regression Analysis for Mediator\n\n")
                temp=paste0("lm(",eq[1],",data=data1)")
                cat("fit1=",paste0("lm(",eq[1],",data=",input$mydata,")"),"\n")
                fit1=eval(parse(text=temp))
                print(summary(fit1))

                cat("\n\nRegression Analysis for Dependent Variable\n\n")
                temp=paste0("lm(",eq[2],",data=data1)")
                cat("fit2=",paste0("lm(",eq[2],",data=",input$mydata,")"),"\n")
                fit2=eval(parse(text=temp))
                print(summary(fit2))

                cat("\n\nTable Summarizing Model Coefficients\n\n")
                x=modelsSummary(fit1,fit2)
                print(x)
                cat("\n\nMediation Effect\n\n")
                cat(paste0("fitMed=mediate(fit1,fit2,treat='",input$X,
                    "',mediator='",input$Mi,"')\n"))
                fitMed=mediate(fit1,fit2,treat=input$X,mediator=input$Mi)
                cat("summary(fitMed)\n")
                print(summary(fitMed))
                cat("\n\nBootstrap\n\n")
                cat(paste0("fitMedBoot=mediate(fitM,fitY,boot=TRUE,sims=10,
                                   treat='",input$X,"',mediator='",input$Mi,"')\n"))
                cat("summary(fitMedBoot)\n")
                fitMedBoot=mediate(fit1,fit2,boot=TRUE,sims=10,
                                   treat=input$X,mediator=input$Mi)
                print(summary(fitMedBoot))


            }
        })

        allModerator<-getAllModerators()


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
            if(as.numeric(modelno)>3) h2("Summary of Model Coefficient"),
            if(as.numeric(modelno)>3) uiOutput("mediationTable"),
            if(length(allModerator)>0) h2("Moderation Effect"),
            # if(input$modelno %in% 1:3) checkboxInput3("switchMod","switch moderator",
            #                                           value=FALSE,width=200),
            # if(input$modelno==1) textInput3("probs","probs",
            #                                 value="",placeholder="0.16,0.5,0.84",width=150),
            # if(input$modelno %in% 1:3)
            #     textInput3("mod1values","mod1 values",value="",placeholder=modValues,width=150),
            # if(input$modelno %in% 2:3)
            #     textInput3("mod2values","mod2 values",value="",placeholder=modValues2,width=150),
            # if(input$modelno %in% 1:3) actionButton("applyValue","Apply Values"),
            # if(input$modelno %in% 1:3) br(),
            # if(modelno==1) checkboxInput3("showeffect","show effect",value=TRUE,width=120),
            if(length(allModerator)>0)
                checkboxInput3("plotpoints","show points",value=FALSE,width=120),
            if(length(allModerator)>0)
                checkboxInput3("interval","show interval",value=FALSE,width=120),
            if(length(allModerator)>0)
                pickerInput3("inttype","type",choices=c("confidence","prediction"),width=120),
            if(length(allModerator)>0)
                numericInput3("intwidth","width",value=0.95,min=0.1,max=1,step=0.01),

            if(length(allModerator)>0) checkboxInput3("linearity","linearity check",
                                                      value=FALSE,width=200),
            # if(input$modelno==1) plotOutput("moderationPlot"),
            # if(input$modelno %in% c(2,3)) plotOutput("interactPlot2"),
            br(),

            if(length(allModerator)>0) pickerInput3("moderator1","predictor",choices=allModerator,width="150px"),
            if(length(allModerator)>0) pickerInput3("moderator2","moderator1",choices=allModerator,width="150px"),
            if(length(allModerator)>2) pickerInput3("moderator3","moderator2",choices=c("",allModerator),width="150px",options=list(title="Select...")),
            if(length(allModerator)>0) plotOutput("interactPlot3"),
            if(input$modelno %in% 1:3) h2("Simple Slope Analysis"),
            if(input$modelno %in% 1:3) checkboxInput3("interval2","show confidence interval",
                                                      value=FALSE,width=220),
            if(input$modelno==1) verbatimTextOutput("ss"),
            if(input$modelno==1) plotOutput("ssPlot"),

            if(input$modelno %in% c(2,3)) verbatimTextOutput("ss2"),
            if(input$modelno %in% c(2,3)) plotOutput("ssPlot2"),

            if(modelno %in% c(1:3)) h2("Johnson-Neyman Intervals"),
            if(modelno %in% c(1)) verbatimTextOutput("JNText"),
            if(modelno %in% c(1:3)) numericInput3("alpha","alpha",value=0.05,min=0.01,max=1,step=0.01),

            if(modelno %in% c(1)) plotOutput("JNPlot"),
            if(modelno %in% c(2:3)) verbatimTextOutput("JNText2"),
            if(modelno %in% c(2:3)) plotOutput("JNPlot2"),
            verbatimTextOutput("regEquation")
            # h2("Reliability Table"),
            # uiOutput("reliabilityTable"),
            # h2("Discriminant Validity Table"),
            # uiOutput("discriminantValidityTable")



        )

    })

    observeEvent(input$moderator1,{
        allModerator=getAllModerators()
        temp=setdiff(allModerator,input$moderator1)
        updatePickerInput(session,"moderator2",choices=temp)
        updatePickerInput(session,"moderator3",choices=c("",temp))
    })

    observeEvent(input$moderator2,{
        allModerator=getAllModerators()
        temp=setdiff(allModerator,c(input$moderator1,input$moderator2))
        updatePickerInput(session,"moderator3",choices=c("",temp))
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

        moderator=NULL
    } else if( i %in% c(13,20)){
       moderator=list(name=input$W,site=list("c"))
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

    getAllModerators=reactive({
        data1<-data()
        i=input$modelno
        if(i==1){
            if(is.factor(data1[[input$X]]) |(input$factorX)){
                i<-1.1
            }
        }
        if(i==1.1){
          res=c(input$W,input$X)
        } else{
        eq<- getRegEq()
        eq<-unlist(strsplit(eq,"\n"))
        if(length(eq)>1) eq=eq[2]
        temp=paste0("lm(",eq,",data=data1)")
        # print(temp)
        fit=eval(parse(text=temp))
        modNames=names(fit$coef)
        print(modNames)
        temp=modNames[str_detect(modNames,":")]
        res=unique(unlist(strsplit(temp,":")))
        }
        res
    })

    getRegEq=reactive({

        if(input$modelno==3){
           result=tripleEquation(X=input$X,Y=input$Y,
                          vars=getTripleVars(),
                          covar=getCovariates2(),mode=1)
        } else if(input$modelno %in% c(11:13,18:20)){
            result=tripleEquation(X=input$X,M=input$Mi,Y=input$Y,
                           vars=getTripleVars(),
                           moderator=getModerator(),
                           covar=getCovariates2(),mode=1)
        } else if(input$modelno %in% c(1:2)){
           result=regEquation(X=input$X,Y=input$Y,moderator=getModerator(),covar=getCovariates2())
        } else{
            result=tripleEquation(X=input$X,M=input$Mi,Y=input$Y,
                                  vars=getTripleVars(),
                                  moderator=getModerator(),
                                  covar=getCovariates2(),mode=1)
        }
        result
    })

    getTripleVars=reactive({
        vars=NULL
        if(input$modelno %in% c(3,11:13)){
            name=list(c(input$W,input$Z))
            if(input$modelno==3) site=list("c")
            else if(input$modelno %in% c(11,13)) site=list("a")
            else if(input$modelno==12) site=list(c("a","c"))
            vars=list(name=name,site=site)
        } else if(input$modelno %in% c(18:20)){
            name=list(c(input$W,input$Z))
            if(input$modelno %in% c(18,20)) site=list("b")
            else if(input$modelno==19) site=list(c("b","c"))
            vars=list(name=name,site=site)
        }
        vars
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

