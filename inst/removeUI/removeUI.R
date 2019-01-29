library(shiny)

ui=fluidPage(
    selectInput('select1','select1',choices=colnames(mtcars)),
    actionButton('insertBtn', 'Insert'),
    actionButton('removeBtn', 'Remove'),
    verbatimTextOutput("text"),

    tags$div(id = 'placeholder')
)

server=function(input, output) {

    ## keep track of elements inserted and not yet removed
    RV=reactiveValues()

    observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        id <- length(RV$inserted)/3+1
        insertUI(
            selector = '#placeholder',
            ## wrap element in a div with id for ease of removal
            ui = tagList(
                radioButtons(paste0("radio",id),paste0("radio",id),choices=colnames(mtcars)),
                tags$div(tags$p(paste0("label"),id),id=id),
                selectInput(paste0("site",id),NULL,choices=c("M","Y","M,Y"))
            )
        )
        RV$inserted <-c(paste0("#radio",id),paste0('#',id),paste0("div:has(> #site", id,")"),RV$inserted)
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
    })

    output$text=renderPrint({
         print(RV$inserted)
         print(length(RV$inserted)/3+1)
    })

}

shinyApp(ui,server)
