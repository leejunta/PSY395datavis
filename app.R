library(ggplot2)
library(readr)

bodyparts <- c("Hips" = "X01",
              "RightUpLeg" = "X02",
              "RightLeg" = "X03",
              "RightFoot" = "X04",
              "LeftUpLeg" = "X05",
              "LeftLeg" = "X06",
              "LeftFoot" = "X07",
              "Spine" = "X08",
              "Spine1" = "X09",
              "Spine2" = "X10",
              "Spine3" = "X11",
              "Neck" = "X12",
              "Head" = "X13",
              "RightShoulder" = "X14",
              "RightArm" = "X15",
              "RightForeArm" = "X16",
              "RightHand" = "X17",
              "RightHandThumb1" = "X18",
              "RightHandThumb2" = "X19",
              "RightHandThumb3" = "X20",
              "RightInHandIndex" = "X21",
              "RightHandIndex1" = "X22",
              "RightHandIndex2" = "X23",
              "RightHandIndex3" = "X24",
              "RightInHandMiddle" = "X25",
              "RightHandMiddle1" = "X26",
              "RightHandMiddle2" = "X27",
              "RightHandMiddle3" = "X28",
              "RightInHandRing" = "X29",
              "RightHandRing1" = "X30",
              "RightHandRing2" = "X31",
              "RightHandRing3" = "X32",
              "RightInHandPinky" = "X33",
              "RightHandPinky1" = "X34",
              "RightHandPinky2" = "X35",
              "RightHandPinky3" = "X36",
              "LeftShoulder" = "X37",
              "LeftArm" = "X38",
              "LeftForeArm" = "X39",
              "LeftHand" = "X40",
              "LeftHandThumb1" = "X41",
              "LeftHandThumb2" = "X42",
              "LeftHandThumb3" = "X43",
              "LeftInHandIndex" = "X44",
              "LeftHandIndex1" = "X45",
              "LeftHandIndex2" = "X46",
              "LeftHandIndex3" = "X47",
              "LeftInHandMiddle" = "X48",
              "LeftHandMiddle1" = "X49",
              "LeftHandMiddle2" = "X50",
              "LeftHandMiddle3" = "X51",
              "LeftInHandRing" = "X52",
              "LeftHandRing1" = "X53",
              "LeftHandRing2" = "X54",
              "LeftHandRing3" = "X55",
              "LeftInHandPinky" = "X56",
              "LeftHandPinky1" = "X57",
              "LeftHandPinky2" = "X58",
              "LeftHandPink" = "X59")
types <- c("Position"="X",
           "Velocity"="V",
           "Quaternion"="Q",
           "Acceleration"="A",
           "Gyro"="W")
axes <- c("X" = "x",
          "Y" = "y",
          "Z" = "z",
          "Magnitude" = "m")

tenPlace <- function(x) {
    #For any number.number*10^digits, returns digits
    options(scipen=999)
    if (!is.numeric(x)) return("Not a Number")
    str <- as.character(abs(x))
    if (substring(str,1,1) == "0") {
        str <- gsub('.*\\.','',str)
        -(nchar(gsub('[1-9].*','',str)) + 1)
    } else {
        nchar(gsub('\\..*','',str)) - 1
    }
}

tenPercRound <- function(x,type) {
    maxVal <- max(x,na.rm = TRUE)
    minVal <- min(x,na.rm = TRUE)
    rangeTenPerc <- (maxVal - minVal)*0.1
    if (type == "min") {
        floor(minVal/10^(tenPlace(minVal)-2))*10^(tenPlace(minVal)-2)
    } else if (type == "max") {
        ceiling(maxVal/10^(tenPlace(maxVal)-2))*10^(tenPlace(maxVal)-2)
    } else {
        return("Choose 'min' or 'max'")
    }
}

ui <- fluidPage(
    titlePanel("Visualizing MoCap Data","MoCap Shiny"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose calc File",
                      accept = c(".calc")
            ),
            tags$hr(),
            textInput("title", "Plot Title:", value = "Visualization"),
            selectInput("bodypart", "Body Part:", bodyparts),
            selectInput("information", "Type:", types),
            selectInput("axis", "Axis:", axes, multiple = TRUE, selected = "m"),
            sliderInput("yaxis", "Plot Scale:", min = -1, max = 1, value = c(-1,1), 
                        step = 0.001, sep = "",ticks = FALSE),
            sliderInput("xaxis", "Index Scale:", min = 1, max = 10000, value = c(1,10000), 
                        step = 1, sep = "", ticks = FALSE)
        ),
        
        mainPanel(
            conditionalPanel(
                condition = "output.fileUploaded",
                plotOutput("plot")
            ),
            conditionalPanel(
                condition = "!output.fileUploaded",
                plotOutput("plotnone")
            )
        )
    )
)

server <- function(input, output, session) {
    options(shiny.maxRequestSize=-1)
    
    #Read Data
    dataInput <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        columnNames <- colnames(read.table(inFile$datapath,skip=5,nrows = 1,header=T))
        data <- read_tsv(inFile$datapath, skip = 5)
        data[,947] <- NULL
        colnames(data) <-  columnNames
        data
    })
    
    output$fileUploaded <- reactive({
        return(!is.null(dataInput()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    output$plotnone <- renderPlot({
        #Data Type String
        if (input$information == "X") {
            datatype <- "Position"
        } else if (input$information == "V") {
            datatype <- "Velocity"
        } else if (input$information == "Q") {
            datatype <- "Quaternion"
        } else if (input$information == "A") {
            datatype <- "Acceleration"
        } else {
            datatype <- "Gyro"    
        }
        
        #Plot none
        ggplot(data = data.frame()) + 
            geom_line()  + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 20),
                  axis.title.x = element_text(face = "bold", size = 14),
                  axis.title.y = element_text(face = "bold", size = 14),
                  legend.position = "right",
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 10)) + 
            labs(title="No Data",
                 y = datatype,
                 x = "Index") +
            ylim(input$yaxis[1],input$yaxis[2]) + 
            xlim(input$xaxis[1],input$xaxis[2])
    })
    
    output$plot <- renderPlot({
        
        #Data Type String
        if (input$information == "X") {
            datatype <- "Position"
        } else if (input$information == "V") {
            datatype <- "Velocity"
        } else if (input$information == "Q") {
            datatype <- "Quaternion"
        } else if (input$information == "A") {
            datatype <- "Acceleration"
        } else {
            datatype <- "Gyro"    
        }
        
        #Get Data
        dataF <- dataInput()
        
        #Add all selected axes to data frame
        numberAxes <- length(input$axis)
        part <- list()
        dataf <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(dataf) <- c("Index", "Part", "Axis")
        axisnames <- input$axis
        for (axes in 1:numberAxes) {
            if (input$axis[axes] == "x") {
                axisnames[axes] <- "X"
            } else if (input$axis[axes] == "y") {
                axisnames[axes] <- "Y"
            } else if (input$axis[axes] == "z") {
                axisnames[axes] <- "Z"
            } else {
                axisnames[axes] <- "Mag"
            }
            
            if (axisnames[axes] == "Mag") {
                columnName <- paste(input$bodypart,input$information,sep = ".")
                part <- sqrt((as.data.frame(dataF[,paste(columnName,"x",sep = ".")]))[,1]^2 + 
                                 (as.data.frame(dataF[,paste(columnName,"y",sep = ".")]))[,1]^2 + 
                                 (as.data.frame(dataF[,paste(columnName,"z",sep = ".")]))[,1]^2)
            } else {
                columnName <- paste(input$bodypart,input$information,input$axis[axes],sep = ".")
                part <- (as.data.frame(dataF[,columnName]))[,1]
            }
            
            index <- c(1:length(part))
            dataf <- rbind(dataf, 
                           data.frame("Index" = index, 
                                      "Part" = part, 
                                      "Axis" = rep(axisnames[axes],length(part))))
        }
        
        indices <- vector()
        for (axes in 0:(numberAxes-1)) {
            axesIndexStart <- input$xaxis[1]+axes*dim(dataf)[1]/numberAxes
            axesIndexEnd <- input$xaxis[2]+axes*dim(dataf)[1]/numberAxes
            indices <- c(indices,c(axesIndexStart:axesIndexEnd))
        }
        plotData <- dataf[indices,]
        
        observe({
            XmaxVal <- length(index)
            updateSliderInput(session, "xaxis", min = 1, max = XmaxVal, 
                              step = 1)
            YminVal <- tenPercRound(plotData$Part,'min')
            YmaxVal <- tenPercRound(plotData$Part,'max')
            updateSliderInput(session, "yaxis", min = YminVal, max = YmaxVal,
                              step = (YmaxVal-YminVal)/100)
        })
        
        ggplot(data = plotData, aes(x=Index, y=Part, color=Axis)) + 
            geom_line()  + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 20),
                  axis.title.x = element_text(face = "bold", size = 14),
                  axis.title.y = element_text(face = "bold", size = 14),
                  legend.position = "right",
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 10)) + 
            labs(title=input$title,
                 y = datatype,
                 x = "Index") +
            ylim(input$yaxis[1],input$yaxis[2]) + 
            xlim(input$xaxis[1],input$xaxis[2])
        
        
        
        
        
    })
}

shinyApp(ui, server)