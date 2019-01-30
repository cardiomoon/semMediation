library(shiny)
library(editData)

ui=fluidPage(

    actionButton('insertBtn', 'Add Covariate',icon=icon("plus")),
    actionButton('removeBtn', 'Remove Covariate',icon=icon("trash-alt")),
    tags$div(id = 'placeholder'),
    verbatimTextOutput("text1")
)

server=function(input, output) {

    ## keep track of elements inserted and not yet removed
    RV=reactiveValues(number=0)

    observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        id <- length(RV$inserted)/3+1
        insertUI(
            selector = '#placeholder',
            ## wrap element in a div with id for ease of removal
            ui = tagList(
                tags$div(h4(paste0("Covariate ",id,"    Site")),id=paste0(id)),
                selectInput3(paste0("cov",id),NULL,choices=colnames(mtcars)),
                selectInput3(paste0("site",id),NULL,choices=c("M","Y","M,Y"))
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

    getCovariates=reactive({
        result<-list()
        if(RV$number>0) {
           result$name=getCovNames()
           result$site=getCovSites()
        }
        result
    })

    output$text1=renderPrint({
        count=RV$number
        cat("현재 ", count,"개의 covariate가 있습니다.")
        covar=getCovariates()
        str(covar)
    })

}

shinyApp(ui,server)
