
#library(knitr)
library(stringr)

library(e1071)
library(ineq)
library(gtools) #for -combinations and maybe other functions
#library(ggplot2) # to create a bubble chart
library(DT)
library(STRINGdb)
require(rCharts)

library(rmarkdown)

library(shinyjs)





     # Load data once
# URL <- "https://raw.githubusercontent.com/christophergandrud/d3Network/master/JSONdata/miserables.json"
# MisJson <- getURL(URL, ssl.verifypeer = FALSE)

# # Convert JSON arrays into data frames
# MisLinks <- JSONtoDF(jsonStr = MisJson, array = "links")
# MisNodes <- JSONtoDF(jsonStr = MisJson, array = "nodes")

#  MisNodes$ID <- 1:nrow(MisNodes)   



shinyServer(function(input, output,session){

##################################
################# Upload tab
##################################

  # Get data
  
  data <- reactive({
    if (input$DataInput == 1) {
      data <- read.table("demo_5Ags.csv", header = TRUE,sep = ";", na.strings="")

    } else if (input$DataInput == 5) {
      data <- read.table("demoBaraniskin.csv", header = TRUE,sep = ";", na.strings="")
      
    } else  {
      # Get uploaded file
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      data <- read.table(inFile$datapath, header = input$header,sep = input$sep,na.strings="")
                       
    }
  })
  
  # Class A data (DISEASE CLASS)
classA<-reactive({
       d.input<-data()
       classA<-d.input[1:length(which(d.input$Class=="A")),]
  })
  # Class B data (CONTROL CLASS)
classB<-reactive({
       d.input<-data()
       start<-length(which(d.input$Class=="A"))+1
       end<-start+(length(which(d.input$Class=="B"))-1)
       classB<-d.input[start:end,]
  })
  

  
  # Specificity filter
  mySpe<-reactive({
        input$SPE
        
  })


 ####### Reactive plot dimensions for Boxplot
   heightplot<-reactive({input$plotheight})
   widthplot<-reactive({input$plotwidth})

 

  
  
  

####### Show Loaded Data (Ag Table)
    output$AgTable <- DT::renderDataTable({
         if (is.null(data()))
             return(NULL)
        else {
           options(scipen=999)
            DT::datatable(data(),extensions = c('TableTools','Responsive'),options = list(paging = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25','50','100','All')),searching = TRUE,initComplete = JS(
            "function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf'))))  #, autoWidth = TRUE, columnDefs = list(list(className = 'dt-center',width = '0px',targets=c(1,2,3,4)))
          }
          })
           
   
 
    
    # observe({
    # updateSelectInput(session, "ev", choices = names(data()))
    # })

####### Print Data Details
     output$uploadDetails=renderPrint({
      if(is.null(data()))
          return(NULL)
      else{    
     dInput<-data()
     ns=dim(dInput)
     nclass=unique(dInput[,2])
     # to check if there exists not numeric values in the data sheet
     NumericVars <- NULL
     Data=dInput[,-c(1,2)]
     for (Var in names(Data)) {
           if(class(Data[,Var]) == 'integer' | class(Data[,Var]) == 'numeric') {
            NumericVars <- c(NumericVars,Var)
          }
       }
    # to check if there exists missing values in the data sheet
    NotMvVars<- NULL
    for (Var in names(Data)) {
           if(!anyNA(Data[,Var], recursive = FALSE)) {
             NotMvVars<- c(NotMvVars,Var)
          }
       }
     #type=unique(apply(dInput[,-c(1,2)],2,is.numeric))  
     #plurals=ifelse(ns!=1, "s","")
      str1<-p(str_c("The dataset contains ", ns[1], " samples and ", ns[2]-2, " features.",colllapse=" ")) #str_c("A total of ",  length(nclass), " groups (", nclass[1], ",", nclass[2], ") were detected."))
     if(length(nclass)==2){
        str2<-p(str_c("A total of ",  length(nclass), " groups (", nclass[1], ",", nclass[2], ") were detected."))
     }else{
        str2<-tags$strong(style="color: red",p(str_c("Be careful! More than two classes are detected; only the pairwise comparison (A vs B) will be performed.")))
     }   
     if(length(NumericVars)==dim(Data)[2]){
         str3<-p(str_c("All data values are numeric."))
      }else{
         NotNumericFeatures=setdiff(colnames(Data),NumericVars)
         str3<-tags$strong(style="color: red",p(str_c("Be careful! Your data contain not numeric values. (See the ",str_c(NotNumericFeatures,collapse=", "),").")))
      }
      if(length(NotMvVars)==dim(Data)[2]){
         str4<-p(str_c("No missing values were detected."))
       }else{
          MVFeatures=setdiff(colnames(Data),NotMvVars)
         str4<-tags$strong(style="color: red",p(str_c("Be careful! Your data contain missing values. (See the ",str_c(MVFeatures,collapse=", "),").")))
      }
     HTML(paste(str1,str2, str3,str4,sep='<p>'))
     }})
     
     

##################################
################# Plots tab
##################################

#  *****     Box plot     *****
#  *****                  *****
####### Define the whisker type
boxRange<- reactive({
  if(input$whisker=="Tukey"){boxRange<-c(1.5)}
  else if(input$whisker=="Spear"){boxRange<-c(0)}
  else if(input$whisker=="Altman"){boxRange<-c(2.5)}
  return(boxRange)
  })

####### Function to generate box plot
     MakePlot<-function(plotdata,TypeClass){
      par(mar=c(5,8,4,2))
      df<-plotdata
     #Select the min and max in the df
     mindf <-min(plotdata[,-c(1,2)])
     maxdf<-max(plotdata[,-c(1,2)])
     myLim<-as.numeric(range(df[,-c(1,2)],na.rm=T)+c(-50,+1))
     if(input$ylimit==""){myLim<-range(df[,-c(1,2)],na.rm=T)+c(-50,+1)}else
     {myLim<-as.numeric(strsplit(input$ylimit,",")[[1]])}



     # Select numerical variables
     numVar <- sapply(1:ncol(df),function(x){is.numeric(df[,x])})


      ##### rotate the x ticks
       if(input$rotationlabels) {xStyleLabels<-2 } else  {xStyleLabels<-1 }
       
      ##### box plot orientation
       if(input$orientation=='vertical') {boxplotOrientation<-F } else  {boxplotOrientation<-T }
           par(mar=c(12.1,4,4.1,12))        #sets the bottom, left, top and right margins  par(mar=c(12.1,11.1,4.1,2.1)) 
     if(TypeClass=='A'){  ##class A
         if (input$scale) df[,numVar] <- scale(df[,numVar]) ##scale the variables


         boxplot(as.data.frame(df[,numVar]),main=input$myboxtitle_A,las=xStyleLabels, #,xlab=input$myxlab,ylab=input$myylab
          cex=0.7,col=input$colour_A,cex.main=input$fontsizetitle/10,cex.lab=input$fontsizeaxislabel/10,cex.axis=input$fontsizeaxis/10,horizontal=boxplotOrientation,ylim=myLim,range=boxRange())
         # the labels on the X/Y axis isaredrawn right below the labels and it does not look good.
         mtext(input$myxlab,side=1,line=7,cex=(input$fontsizeaxislabel/10))   #the option side takes an integer between 1 and 4, with these meaning: 1=bottom, 2=left, 3=top, 4=right
         mtext(input$myylab,side=2,line=7,cex=(input$fontsizeaxislabel/10))   #the option side takes an integer between 1 and 4, with these meaning: 1=bottom, 2=left, 3=top, 4=right
        ##### add the marker means
         if(input$MMeans==TRUE){
             boxplotMeans_A<- apply(df[,numVar],2,mean,na.rm=TRUE)
             points(x=1:ncol(df[,numVar]),y=boxplotMeans_A,pch="+",cex=2,col="red")
         }
        ##### add the sample number
        if(input$NPoints==TRUE){
            boxstat_A<-boxplot(as.data.frame(df[,numVar]),na.rm=TRUE,plot=FALSE)
            text(x=1:ncol(df[,numVar]),y=myLim[1]-200,labels=boxstat_A$n)  #y=maxdf-500/y=yLim[1]/yLim[2]
        }

     }else {    ##class B
        if (input$scale) df[,numVar] <- scale(df[,numVar]) ##scale the variables

        par(mar=c(12.1,4,4.1,12)) 

         boxplot(as.data.frame(df[,numVar]),main=input$myboxtitle_B,las=xStyleLabels,  #,xlab=input$myxlab,ylab=input$myylab,
          cex=0.7,col=input$colour_B,cex.main=input$fontsizetitle/10,cex.lab=input$fontsizeaxislabel/10,cex.axis=input$fontsizeaxis/10,horizontal=boxplotOrientation,ylim=myLim,range=boxRange())
         # the labels on the X/Y axis isaredrawn right below the labels and it does not look good.
          mtext(input$myxlab,side=1,line=7,cex=(input$fontsizeaxislabel/10))  #the option side takes an integer between 1 and 4, with these meaning: 1=bottom, 2=left, 3=top, 4=right
          mtext(input$myylab,side=2,line=7,cex=(input$fontsizeaxislabel/10))  #the option side takes an integer between 1 and 4, with these meaning: 1=bottom, 2=left, 3=top, 4=right
          ##### add the marker means 
           if(input$MMeans==TRUE){
             boxplotMeans_B<- apply(df[,numVar],2,mean,na.rm=TRUE)
             points(x=1:ncol(df[,numVar]),y=boxplotMeans_B,pch="+",cex=2,col="red")   # points(x=1:ncol(classB()[,-c(1,2)]),y=boxplotMeans_B,pch="+",cex=2,col="red")
         }
         ##### add the sample number
        if(input$NPoints==TRUE){
            boxstat_B<-boxplot(as.data.frame(df[,numVar]),na.rm=TRUE,plot=FALSE)
            text(x=1:ncol(df[,numVar]),y=myLim[1]-140,labels=boxstat_B$n)   
        }

         
     }
       ##### add the grid
       if(input$addgrid=='x and y'){grid()}
       else if (input$addgrid=='x'){grid(ny=NA)}
       else if (input$addgrid=='y'){grid(NA,ny=NULL)}
       else if (input$addgrid=='none'){}

  }




     output$A_boxPlot <- renderPlot({
           MakePlot(classA(),'A')
     },height=heightplot,width=widthplot)       
     
     output$B_boxPlot <- renderPlot({
          MakePlot(classB(),'B')
      },height=heightplot,width=widthplot)

####### Get boxplot statistics
      output$A_boxstatistics<-DT::renderDataTable({
          d.input<-data()
          ns=dim(d.input)
          classA<-d.input[1:length(which(d.input$Class=="A")),]
          tempstat<-boxplot(classA,plot=F)
          # remove the fisrt two columns: Patient_ID and Class
          A_boxstat<-tempstat$stats[,3:ns[2]]
          rownames(A_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker")
          colnames(A_boxstat)<-tempstat$names[3:ns[2]]
          A_basicboxstat<-t(apply.dataSummary(classA))
          A_boxstat<-rbind(A_boxstat,t(A_basicboxstat[,1]),t(A_basicboxstat[,3]),t(A_basicboxstat[,4]),t(A_basicboxstat[,5]),t(A_basicboxstat[,6]),t(A_basicboxstat[,7]),t(A_basicboxstat[,8]))
          rownames(A_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker","Mean","Min","Max","Range","Sd","Skewness","CV")
          #A_boxstat
          DT::datatable(A_boxstat,extensions = c('TableTools','Responsive'),options = list(paging = TRUE,pageLength=12,lengthMenu = list(c(5, 10, -1), c('5', '10','All')), searching = TRUE,initComplete = JS(
            "function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf'))))
         })
         
      output$B_boxstatistics<-DT::renderDataTable({
          d.input<-data()
          ns=dim(d.input)
          start<-length(which(d.input$Class=="A"))+1
          end<-start+(length(which(d.input$Class=="B"))-1)
          classB<-d.input[start:end,]
          #classB<-d.input[1:length(which(d.input$Class=="B")),]
          
          tempstat<-boxplot(classB,plot=F)
          # remove the fisrt two columns: Patient_ID and Class
          B_boxstat<-tempstat$stats[,3:ns[2]]
          rownames(B_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker")
          colnames(B_boxstat)<-tempstat$names[3:ns[2]]
          B_basicboxstat<-t(apply.dataSummary(classB))
          B_boxstat<-rbind(B_boxstat,t(B_basicboxstat[,1]),t(B_basicboxstat[,3]),t(B_basicboxstat[,4]),t(B_basicboxstat[,5]),t(B_basicboxstat[,6]),t(B_basicboxstat[,7]),t(B_basicboxstat[,8]))
          rownames(B_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker","Mean","Min","Max","Range","Sd","Skewness","CV")
          #B_boxstat
          DT::datatable(B_boxstat,extensions = c('TableTools','Responsive'),options = list(paging = TRUE, pageLength = 12,lengthMenu = list(c(5, 10, -1), c('5', '10','All')),searching = TRUE,initComplete = JS(
            "function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf'))))
         })


####### Download PDF file
output$downloadBoxPlotPDF<-downloadHandler(
    filename<-function(){paste('BoxPlot.pdf')},
    content<-function(file){
      pdf(file,width=input$plotwidth/72,height=input$plotheight/72)
      MakePlot(classA(),'A')
      MakePlot(classB(),'B')
      dev.off()
    },
    contentType='application/pdf'
    )
####### Download EPS file
# output$downloadBoxPlotEPS<-downloadHandler(
#     filename<-function(){paste('BoxPlot.eps')},
#     content<-function(file){
#       postscript(file,horizontal=FALSE,onefile=TRUE,paper="special",width=input$plotwidth/72,height=input$plotheight/72)
#       MakePlot(classA(),'A')
#       MakePlot(classB(),'B')
#       dev.off()
#     },
#     contentType='application/postscript'
#     )
####### Download SVG file
# output$downloadBoxPlotSVG<-downloadHandler(
#     filename<-function(){paste0('Rplot%03d.svg')},
#     content<-function(file){
#       svg(file,onefile=FALSE,width=input$plotwidth/72,height=input$plotheight/72)
#       MakePlot(classA(),'A')
#       MakePlot(classB(),'B')
#       dev.off()
#     },
#     contentType='image/svg'
#     )




 

#  *****    Marker plot   *****
#  *****                  *****

####### Select the marker for the marker plot
    ProfileVar <- reactive({
      df=data()[,-c(1,2)]
     vars <- as.list(names(df))
      return(vars)
    })

    output$Profilevariable = renderUI({
      selectInput('profilevariable', 'Variables', ProfileVar())
    })

# ####### Plot the marker graph
# ProfileObj<-eventReactive(input$profileButton,{
#       df=data()
#       df$Name=input$profilevariable
#       df$Status=""
#       Class=c(rep("Class A",dim(classA())[1]),rep("Class B",dim(classB())[1]))
#       df$Status=Class
#       df$SampleId=df[,1]
#        #retrieve the column number
#        NColumn=which(as.numeric(str_detect(input$profilevariable,colnames(df)))==1)
#        df$Mean_Fluorescence_Intensity=df[,NColumn]
#        df$Sample_Number=seq(1,dim(df)[1],1)
#        d1 <- dPlot(
#            Mean_Fluorescence_Intensity ~ Sample_Number,
#            groups = c("Name","SampleId","Status"),
#            data = df,
#            type = "bubble",height=600, #800
#            width="400", #"100%"
#           bounds = list(x=80, y=80, width=300, height=300) #mi blocca i margini bounds = list(x=60, y=25, width=400, height=350) width=550, height=430
#           )
#       d1$defaultColors(c("#6F8DE9","#E96F8D"))  #"#377EB8", "#4DAF4A",#af1416
#       d1$xAxis( type = "addMeasureAxis")
#       d1$yAxis( type = "addMeasureAxis" )


#       d1$legend(
#         x = 650,    #465
#         y = 80,
#         width = 50,
#         height = 1000,
#         horizontalAlign = "left")
#        #d1$set(width="200%")

       
#        return(d1)
# }) 

# output$myChart<-renderChart2({
#      ProfileObj()

# }) 


 ####### bubble graph with size: the number of marker in the combiantions
     ProfileObj<- eventReactive(input$profileButton,{
              input$profileButton
  
             # Highcharts bubble via rCharts
               df=data()
               df$Names=input$profilevariable
      df$Status=""
      Class=c(rep("Class A",dim(classA())[1]),rep("Class B",dim(classB())[1]))
      df$Status=Class
      df$SampleId=df[,1]
       #retrieve the column number
       NColumn=which(as.numeric(str_detect(input$profilevariable,colnames(df)))==1)
       df$Mean_Fluorescence_Intensity=df[,NColumn]
       df$Sample_Number=seq(1,dim(df)[1],1)
       df$Occurence=rep(0.7,dim(df)[1])
          h1 <- hPlot(  name='Marker',
                        y = 'Mean_Fluorescence_Intensity', 
                        x = 'Sample_Number',
                        group='Status', 
                        size='Occurence',
                        type = 'bubble', 
                        #name='Marker',
                        data = df)
         
               # h1$series(name=input$profilevariable, y = 'Mean_Fluorescence_Intensity',  x = 'Sample_Number',  group='Status',  size='Occurence', type = 'bubble',  data = df)
               h1$title(text=input$profilevariable)
               h1$legend(layout='vertical',title=list(text="Class Type"),labelFormatter="#! function() { return this.name + ' (click to hide)';} !#")
               #h1$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'SE: ' + Highcharts.numberFormat((this.point.y), 2) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 2);} !#")

               h1$colors(c("#E96F8D","#6F8DE9"))
              # h1$series(name=input$profilevariable)
               h1$exporting(enabled=TRUE)
               h1$chart(zoomType = "xy",plotBorderWidth=1,options3d=list(enabled=TRUE))

               h1$xAxis(title = list(text = "Sample Number"),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE)
               h1$yAxis(title = list(text = "Signal Intensity"),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE)
              

               h1$plotOptions(bubble=list(minSize=min(df$Occurence)*10,maxSize=max(df$Occurence)*10,sizeBy="width")) # to set the bubble size
                h1$tooltip(crosshairs=c(TRUE,TRUE))
               h1$set(width = 400,height=700)    #to set the plot width
                   h1$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'Status: ' + this.series.name +'<br/>'+'Signal Intensity: ' + Highcharts.numberFormat((this.point.y), 2) +'<br/>'+ 'Sample Number: '+ Highcharts.numberFormat((this.point.x), 2);} !#")
               return(h1)



               #  h2 <-rCharts::Highcharts$new()
               #  plot_data<-vector("list",dim(df)[1])
               #  for (i in 1:dim(df)[1]){

               #  plot_data[[i]]<-list(x=df$Sample_Number,y=df$Mean_Fluorescence_Intensity)
               #  }
               #  h2$series(name=input$profilevariable,
               #    type="bubble",
               #    data=plot_data)

               #     h2$title(text=input$profilevariable)
               # h2$legend(layout='vertical',title=list(text="Class Type"),labelFormatter="#! function() { return this.name + ' (click to hide)';} !#")
               # #h1$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'SE: ' + Highcharts.numberFormat((this.point.y), 2) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 2);} !#")

               # h2$colors(c("#6F8DE9","#E96F8D"))
               # h2$exporting(enabled=TRUE)
               # h2$chart(zoomType = "xy",plotBorderWidth=1,options3d=list(enabled=TRUE))

               # h2$xAxis(title = list(text = "Sample Number"),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE)
               # h2$yAxis(title = list(text = "Signal Intensity"),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE)
              

               # h2$plotOptions(bubble=list(minSize=min(df$Occurence)*10,maxSize=max(df$Occurence)*10,sizeBy="width")) # to set the bubble size
               #  h2$tooltip(crosshairs=c(TRUE,TRUE))
               # h2$set(width = 400,height=700)    #to set the plot width
               #     h2$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'Status: ' + this.series.name +'<br/>'+'Signal Intensity: ' + Highcharts.numberFormat((this.point.y), 2) +'<br/>'+ 'Sample Number: '+ Highcharts.numberFormat((this.point.x), 2);} !#")
               # return(h2)





           #      input1<- SE_SPmatrix()[,1]
           #      input2 <- SE_SPmatrix()[,4]
           #      input3 <-cbind(input1,input2)
           #      nCount<-str_count(rownames(SE_SPmatrix()),pattern="M")
           #      a=input3
           #    #  a=(unique(input3))
           #    # nCount=array(0,dim=c(dim(a)[1],1))
           #    # #Names=rep("Simple Duple",dim(a)[1])
           #    # for (i in 1:dim(a)[1]){
           #    #      nCount[i,1]<- length(intersect(which(a[i,1]==input3[,1]), which(a[i,2]==input3[,2])))
           #    #  }
           #     gold = which(a[,1]>=input$sinePhase & a[,2]>=input$sineAmplitude)
             

           #     Colours<- rep("Under the thresholds",dim(a)[1])
           #     if(length(gold)!= 0){Colours[gold]<- "Gold"}
               
           #      #Colours=c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0) 
           #      # Colours=c("white","gold","white","white","white","white","white","white","white","white","white","white",
           #      #       "white","white","white","white","white","white","white","white","white","white","white","white","white","gold","white")
           #      Names<-Colours

           # df <-data.frame(SE=a[,1],SP=a[,2],Occurence=nCount,Name=Names,Colour=Colours)  #,Class=Gender
           #     h1 <- hPlot(
           #              y = 'SE', 
           #              x = 'SP',
           #              group='Colour', 
           #              size='Occurence',
           #              type = 'bubble', 
           #              name='Names',
           #              data = df)
           #              #group='Class') #ci puo essere anche group
           #     h1$title(text="Gold combinations")
           #     h1$legend(layout='vertical',title=list(text="Class Type"))
             
           #   #check the range the thresholds in order to visualize the appropriate colour
           #   if(length(gold) != 0){
           #      h1$colors(c("#ffe976","#4480BB"))   #funziona ma non governo opacità e quindi devo scegliere i colori opportuni #b1e5ff"
           #   } else if (length(gold)==0)  {
           #   # h1$colors(c("#4480BB"))           #only white combinations
           #     validate(not_value2(gold))        #error message
           #     } 
               
           #   #h1$colors(list(radialGradient=list(cx=0.5,cy=0.3,r=0.5),stops=list(c(0, 'yellow'))))   #NON VAAAAAAAA
              
           #    # https://gist.github.com/ramnathv/d640d4e29755667626f7#file-code-r
           #    # to radialize the color
           #    # colors = "#! Highcharts.map(Highcharts.getOptions().colors, function(color) {
           #    #    return {
           #    #       radialGradient: { cx: 0.4, cy: 0.3, r: 0.5 },
           #    #          stops: [
           #    #                  [0, color],
           #    #              [1,  Highcharts.Color(color).brighten(-0.3).get('rgb')] // darken
           #    #                 ]
           #    #            }}) !#"
           #    #    h1$plotOptions(bubble = list(colors = "#0000FF"))


                


           #    #h1$plotOptions(series= list(fillColor=list(linearGradient=c(0,0,0,300))))
               
           #     #h1$plotOptions(series = list(colors = ("lightblue"))) # to change the bubble color (it works with one colour!!!)
           #     #h1$plotOptions(type = "addColorAxis",colorSeries = "Colour",palette = c("lightblue","gold") )
           #     h1$legend(layout='vertical',title=list(text="Combination Type")) # labelFormat=list("{name1}","{name2}"),labelFormatter="#! function() {return this.series.name;} !#"
               

          

           #     h1$exporting(enabled=TRUE)
           #     h1$chart(zoomType = "xy",plotBorderWidth=1,options3d=list(enabled=TRUE))
           #     h1$xAxis(min=0,max=100,title = list(text = "Specificity (%)"),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="black",dashStyle="dot",width=2,value=input$sineAmplitude,label=list(align='middle',rotation=0,y=15,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="your chosen specificity"),zIndex=3)))
           #     h1$yAxis(min=0,max=100,title = list(text = "Sensitivity (%)"),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="black",dashStyle="dot",width=2,value=input$sinePhase,label=list(align='middle',rotation=0,y=15,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="your chosen sensitivity"),zIndex=3)))
           #     # h1$plotOptions(bubble = list(dataLabels = list(enabled = TRUE, x = 0, 
           #     #                                formatter="#! function() {
           #     #                                  return this.point.name;
           #     #                                } !#", style=list(color= 'black'))))

           #     h1$plotOptions(bubble=list(minSize=min(nCount)*10,maxSize=max(nCount)*10,sizeBy="width")) # to set the bubble size
           #    # h1$series(marker=list(fillColor=list(radialGradient=list(cx=0.4,cy=0.3,r=0.5),stops=list(c(0, 'blue'), c(1, 'orange')))))   #marker=list(fillColor=list(radialGradient=list(cx=0.4,cy=0.3,r=0.5),stops=list(c(0, "#003399"), c(1, '#3366AA'))))
           #     h1$tooltip(crosshairs=c(TRUE,TRUE))
           #     h1$set(width = 500,height=600)    #to set the plot width
              

           #      #h1$s?eries(data=df[which(df$SE>=input$sinePhase),which(df$SP>=input$sineAmplitude)],color='#FFEB7F')
           #     #h1$tooltip(useHTML=TRUE,headerFormat='<table>',pointFormat='<tr><th colspan="2"><h3>{Simple Duple}</h3></th></tr>'+'<tr><th>Sensitivity:</th><td>{point.x}</td></tr>',footerFormat='</table>',followPointer=TRUE)
           #      # pointFormat='<tr><th colspan="2"><h3>{Simple Duple}</h3></th></tr>' +
           #      # '<tr><th>Sensitivity:</th><td>{point.x}g</td></tr>' +
           #      # '<tr><th>Specificity:</th><td>{point.y}g</td></tr>' +
           #      # '<tr><th>Occurence:</th><td>{point.z}%</td></tr>',footerFormat='</table>', followPointer=TRUE)
                
           #      return(h1)
                  
      })


 # output$testPlot <- renderChart2({
 #     df=data()
 #      df$Name=input$profilevariable
 #      df$Status=""
 #      Class=c(rep("disease",dim(classA())[1]),rep("control",dim(classB())[1]))
 #      df$Status=Class
 #      df$SampleId=df[,1]
 #       #retrieve the column number
 #       NColumn=which(as.numeric(str_detect(input$profilevariable,colnames(df)))==1)
 #       df$FI_Value=df[,NColumn]
 #       df$SampleN=seq(1,dim(df)[1],1)

 #      h1 <- Highcharts$new()
 #      h1$chart(type = "scatter",zoomType="xy")
      
 #      h1$xAxis(categories=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
 #      h1$tooltip(shared=TRUE,useHTML=TRUE,headerFormat='<small>{point.key}</small><table>',  pointFormat='<tr><td style="color: {series.color}">{series.name}: </td>' +
 #                '<td style="text-align: right"><b>{point.y} EUR</b></td></tr>',footerFormat= '</table>',valueDecimals=2)
 #     h1$series(name='Short',data=df[1:12,3])
 #     h1$series(name='Long named series',data=df[40:52,3])
 #     #h1$series(data =df[1:40,3],name='Disease',color='rgba(223, 83, 83, .5)')
 #     #h1$series(data = df[41:170,3],name='Control',color='rgba(119, 152, 191, .5)')
 #     #h1$labels(items=list(html=df$SampleId))
 #     #h1$plotOptions()
 #     # h1$legend(layout = "vertical",align="left",verticalAlign="top",x=100,y=70,borderWidth=1)
 #      h1$title(text=input$profilevariable)
 #      h1$xAxis(title=list(text="Sample ID",enabled=TRUE))
 #      h1$yAxis(title=list(text="FI Value"))

 #      h1$exporting(enabled=TRUE)
 #      return(h1)

 #    })

output$myChart<-renderChart2({
     ProfileObj()

}) 














####### Get the Marker statistics

ProfTable<-eventReactive(input$profileButton,{
          d.input<-data()
          ns=dim(d.input)

          # Get Class A (DISEASE)
          classA<-d.input[1:length(which(d.input$Class=="A")),]
          tempstat<-boxplot(classA,plot=F)
          # remove the fisrt two columns: Patient_ID and Class
          A_boxstat<-tempstat$stats[,3:ns[2]]
          rownames(A_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker")
          colnames(A_boxstat)<-tempstat$names[3:ns[2]]
          A_basicboxstat<-t(apply.dataSummary(classA))
          A_boxstat<-rbind(A_boxstat,t(A_basicboxstat[,1]),t(A_basicboxstat[,3]),t(A_basicboxstat[,4]),t(A_basicboxstat[,5]),t(A_basicboxstat[,6]),t(A_basicboxstat[,7]),t(A_basicboxstat[,8]))
          rownames(A_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker","Mean","Min","Max","Range","Sd","Skewness","CV")
          # Get Class B (CONTROL)
          start<-length(which(d.input$Class=="A"))+1
          end<-start+(length(which(d.input$Class=="B"))-1)
          classB<-d.input[start:end,]
          #classB<-d.input[1:length(which(d.input$Class=="B")),]
          
          tempstatB<-boxplot(classB,plot=F)
          # remove the fisrt two columns: Patient_ID and Class
          B_boxstat<-tempstatB$stats[,3:ns[2]]
          rownames(B_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker")
          colnames(B_boxstat)<-tempstatB$names[3:ns[2]]
          B_basicboxstat<-t(apply.dataSummary(classB))
          B_boxstat<-rbind(B_boxstat,t(B_basicboxstat[,1]),t(B_basicboxstat[,3]),t(B_basicboxstat[,4]),t(B_basicboxstat[,5]),t(B_basicboxstat[,6]),t(B_basicboxstat[,7]),t(B_basicboxstat[,8]))
          rownames(B_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker","Mean","Min","Max","Range","Sd","Skewness","CV")
          #Get Marker Column
            #pos=which(matchNames[,2]==input$variables2)
          MarkerColumn=which(colnames(B_boxstat)==input$profilevariable)
          Marker_boxstat=cbind(A_boxstat[,MarkerColumn],B_boxstat[,MarkerColumn])
          rownames(Marker_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker","Mean","Min","Max","Range","Sd","Skewness","CV")
          colnames(Marker_boxstat)<-c("Class A","Class B")
    

          return(Marker_boxstat)

})


   output$Oggi<-DT::renderDataTable ({
    input$profileButton
        #MarkerName=input$profilevariable
            
            #colnames(ProfTable())<-paste0(str_c(input$profilevariable),"Disease","Control")
           
            # colnames(ProfTable()[c(2,3)]) <- paste0('<span style="color:',c("red","blue"),'">',colnames(ProfTable()[c(2,3)]),'</span>')
            # colnames(ProfTable()) = c('<span style="color:red">Column 1</span>', '<em>Column 2</em>','<em>Column 3</em>')
            # datatable(ProfTable(), escape = FALSE)
            dat<- datatable(ProfTable(),rownames = TRUE,extensions = c('TableTools','Responsive','Scroller'),options = list(autoWidth=FALSE,columnDefs = list(list(width = '2px', targets = c(1,2))),paging = TRUE,pageLength = 12,lengthMenu = list(c(5, 10, -1), c('5', '10','All')),searching = TRUE,initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))) %>%
             formatStyle('Class A',   backgroundColor = '#E96F8D', fontWeight = 'bold',fontSize='12px') %>%  #color="red"
             formatStyle('Class B',   backgroundColor = '#6F8DE9', fontWeight = 'bold',fontSize='12px') 
          
            
          })

   




# plotInput <- reactive({
#     plot<- renderChart2(ProfileObj())
#     plot$exporting(enabled = TRUE)
#     return(plot)
#     })

# ####### Download PDF file
# output$downloadMarkerProfilePlotSVG<-downloadHandler(
#     filename<-function(){paste('MarkerProfilePlot.svg')},
#     content<-function(file){
#       #pdf(file,width=input$plotwidth/72,height=input$plotheight/72)
#       plotInput()$save('mychart1.svg', cdn = TRUE)
      
      
#       dev.off()
#     },
#     contentType='image/svg'
#     )
####### Download EPS file
# output$downloadBoxPlotEPS<-downloadHandler(
#     filename<-function(){paste('BoxPlot.eps')},
#     content<-function(file){
#       postscript(file,horizontal=FALSE,onefile=TRUE,paper="special",width=input$plotwidth/72,height=input$plotheight/72)
#       MakePlot(classA(),'A')
#       MakePlot(classB(),'B')
#       dev.off()
#     },
#     contentType='application/postscript'
#     )
####### Download SVG file
# output$downloadBoxPlotSVG<-downloadHandler(
#     filename<-function(){paste0('Rplot%03d.svg')},
#     content<-function(file){
#       svg(file,onefile=FALSE,width=input$plotwidth/72,height=input$plotheight/72)
#       MakePlot(classA(),'A')
#       MakePlot(classB(),'B')
#       dev.off()
#     },
#     contentType='image/svg'
#     )



##################################
################# Preprocessing tab
##################################

Preproctable<-reactive({
   df<-data()
   
   if(input$datatransf=='none' | input$datascaling=='none'){
    df
   }
   if(input$datatransf=='natural logarithm (log2)'){
      df[df==0]<-1   #to avoid -Inf value in the dataframe (log2(1)=0)
      df<-round(log2(df[,-c(1,2)]),3)
      df<-cbind(data()[,c(1,2)],df)
   }
  if(input$datascaling=='unit variance scaling'){
     df<-apply(df[,-c(1,2)], 2, function(x){(x - mean(x))/sd(x);})
     df<-cbind(data()[,c(1,2)],round(df,3))  
   }
   if(input$datascaling=='pareto scaling'){
       df<-apply(df[,-c(1,2)], 2, function(x){(x - mean(x))/sqrt(sd(x));})
        df<-cbind(data()[,c(1,2)],round(df,3)) 
   }
  return(df)
  })


    output$preproctable<-DT::renderDataTable ({
            Preproctable()
            },rownames = FALSE,extensions = c('TableTools','Responsive'),options = list(paging = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25','50','100','All')),searching = TRUE,initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))
            )

    # Get data
  data2 <- reactive({
     df<-Preproctable()
     return(df)
  }) 

  classA2<-reactive({
       d.input<-data2()
       classA2<-d.input[1:length(which(d.input$Class=="A")),]
  })
  # Class B data (control)
classB2<-reactive({
       d.input<-data2()
       start<-length(which(d.input$Class=="A"))+1
       end<-start+(length(which(d.input$Class=="B"))-1)
       classB2<-d.input[start:end,]
  })

####### Print Data Details
     output$Error=renderPrint({
      if(is.null(data2()))
          return(NULL)
      else{    
     dInput<-data2()
   
     # to check if there exists negative numbers in the datasheet

     NegativeNumbers <- NULL
     Data=dInput[,-c(1,2)]
     for (Var in names(Data)) {
          if(any((Data[,Var]+1)<0)){
           NegativeNumbers<-c(NegativeNumbers,Var)
           }
       }
   }
   if(!is.null(NegativeNumbers)){
       str1<-tags$strong(style="color: red",p(str_c("Be careful!!! CombiROC works with raw data."))) #(See the ",str_c(NegativeNumbers,collapse=", "),").
        str2<-tags$strong(style="color: red",p(str_c("Do not enter negative values and then perform ROC analysis!")))
   }else{
      str1<-tags$strong(style="color: green",p(str_c("Your data are good.")))
        str2<-tags$strong(style="color: green",p(str_c("You are ready to go to the next step!")))
   }

     HTML(paste(str1,str2,sep='<p>'))
     })



##################################
################# Combinatorial Analysis tab
##################################

         
 ####### Plot the sensitivity and specificity distributions 
   nbar<-eventReactive(input$runButton, {
             #remove patients and class columns
              dfA<-(classA2()[,3:dim(classA2())[2]])
              dfB<-(classB2()[,3:dim(classB2())[2]])
              dfA<-t(dfA)
              dfB<-t(dfB)
              n_features<-length(rownames(dfA))
              k<-1:n_features
              K<-2^n_features-1      #total number of combinations
              nAntigen<-K
              class_A<-1
              class_B<-2
              name_classA<-"disease"
              name_classB<-"control"
              nSamples_A<-length(colnames(dfA))
              nSamples_B<-length(colnames(dfB))

             # list of all possible combinations
              listCombinationAntigens<-array(0,dim=c(K,1))
             # relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
             frequencyCombinationAntigens<-array(0,dim=c(K,2))

             index<-1
             for (i in 1:length(k)){
                temp<- combinations(n_features,k[i],rownames(dfA))
                # storage the row index to calculate the relative frequency
                row_index_combination<-combinations(n_features,k[i],k)
                for (j in 1:dim(temp)[1]){
                listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
                ## single antigen
                if(dim(temp)[2]==1){ ## 1 antigen combination
                  frequencyCombinationAntigens[index,class_A]<-length(which(dfA[row_index_combination[j,],]>=input$signalthr))    #input$signalthr
                  frequencyCombinationAntigens[index,class_B]<-length(which(dfB[row_index_combination[j,],]>=input$signalthr))    #input$signalthr
                }else{ ## more than 1 antigen (combination)
                  frequencyCombinationAntigens[index,class_A]<- length(which((colSums(dfA[row_index_combination[j,],]>=input$signalthr))>=input$combithr))   #input$signalthr))>=input$combithr
                  frequencyCombinationAntigens[index,class_B]<-length(which((colSums(dfB[row_index_combination[j,],]>=input$signalthr))>=input$combithr))     #input$signalthr))>=input$combithr
                   }
                  index<-index+1
                     }
                     }
                     #set the row/col names
                     rownames(frequencyCombinationAntigens)<-listCombinationAntigens
                     colnames(frequencyCombinationAntigens)<-c(name_classA,name_classB)
                     names(dimnames(frequencyCombinationAntigens)) <- c("Antigens", "")

             # Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
            SE_SP<-array(0,dim=c(K,2*2))
            SE_SP[,class_A]<- round(frequencyCombinationAntigens[,class_A]*100/nSamples_A,digits=0)
            SE_SP[,class_B] <-round(frequencyCombinationAntigens[,class_B]*100/nSamples_B,digits=0)
            rownames(SE_SP)<-listCombinationAntigens
            colnames(SE_SP)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
            SE_SP[,class_A+2]<-100-SE_SP[,class_A]
            SE_SP[,class_B+2]<-100-SE_SP[,class_B]

            if(input$combithr!=n_features){
                SE_SPselected<-SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr),]
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               } 
               #only the combination with all features has been discovered
               else{  
               SE_SPselected<-t(SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr),])
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               rownames(SE_SPselected) <-rownames(SE_SP)[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr)]
               }
            SE_SPselected

            # Highcharts pyramid via rCharts
            breaks_upper<-c(10,20,30,40,50,60,70,80,90,100)
            breaks_lower<-c(0,11,21,31,41,51,61,71,81,91)
            breaks<-cbind(breaks_lower,breaks_upper)
            temp<-array(0,dim=c(10,2))
               for(i in 1:10){
                 temp[i,1]<-sum(SE_SPselected[,1]>=breaks[i,1] & SE_SPselected[,1]<=breaks[i,2])
                  temp[i,2]<-sum(SE_SPselected[,4]>=breaks[i,1] & SE_SPselected[,4]<=breaks[i,2])
                  }
                  Age<-c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100")
                  temp[,1]<-temp[,1]*(-1)
                  Population<-c(temp)
                  Gender<-c(rep("SE",10),rep("SP",10))
                  df <-data.frame(Frequency=Population,Intervals=rep(Age,2),Class=Gender)


                   h1 <- hPlot(
                        y = 'Frequency', 
                        x = 'Intervals', 
                        type = 'bar', 
                        data = df,
                        group = 'Class')
                  h1$plotOptions(series = list(stacking = 'normal', pointPadding = 0, borderWidth = 0))
                  h1$tooltip(formatter = "#! function() { return '<b>'+ this.series.name +', intervals '+ this.point.category +'</b><br/>' + 'Frequency: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);} !#")
                  # to change the bar colors
                  # colors = c('blue', 'red')
                  # h1$colors(colors)
                   if (max(df$Frequency >= 0)) {
                       h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value)) ;} !#"), 
                                 title = list(enabled = TRUE, text = 'Frequency'))
                  } else {
                      h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value)) ;} !#"), 
                                 title = list(enabled = TRUE, text = 'Frequency'))
                   }
                  h1$exporting(enabled=TRUE)
                  return(h1)
})


        # nview<-eventReactive(input$runButton, {
        #        #remove patients and class columns
        #       dfA<-(classA2()[,3:dim(classA2())[2]])
        #       dfB<-(classB2()[,3:dim(classB2())[2]])
        #       dfA<-t(dfA)
        #       dfB<-t(dfB)
        #       n_features<-length(rownames(dfA))
        #       k<-1:n_features
        #       K<-2^n_features-1      #total number of combinations
        #       nAntigen<-K
        #       class_A<-1
        #       class_B<-2
        #       name_classA<-"disease"
        #       name_classB<-"control"
        #       nSamples_A<-length(colnames(dfA))
        #       nSamples_B<-length(colnames(dfB))

        #      # list of all possible combinations
        #       listCombinationAntigens<-array(0,dim=c(K,1))
        #      # relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
        #      frequencyCombinationAntigens<-array(0,dim=c(K,2))

        #      index<-1
        #      for (i in 1:length(k)){
        #         temp<- combinations(n_features,k[i],rownames(dfA))
        #         # storage the row index to calculate the relative frequency
        #         row_index_combination<-combinations(n_features,k[i],k)
        #         for (j in 1:dim(temp)[1]){
        #         listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
        #         ## single antigen
        #         if(dim(temp)[2]==1){ ## 1 antigen combination
        #           frequencyCombinationAntigens[index,class_A]<-length(which(dfA[row_index_combination[j,],]>=input$signalthr))    #input$signalthr
        #           frequencyCombinationAntigens[index,class_B]<-length(which(dfB[row_index_combination[j,],]>=input$signalthr))    #input$signalthr
        #         }else{ ## more than 1 antigen (combination)
        #           frequencyCombinationAntigens[index,class_A]<- length(which((colSums(dfA[row_index_combination[j,],]>=input$signalthr))>=input$combithr))   #input$signalthr))>=input$combithr
        #           frequencyCombinationAntigens[index,class_B]<-length(which((colSums(dfB[row_index_combination[j,],]>=input$signalthr))>=input$combithr))     #input$signalthr))>=input$combithr
        #            }
        #           index<-index+1
        #              }
        #              }
        #              #set the row/col names
        #              rownames(frequencyCombinationAntigens)<-listCombinationAntigens
        #              colnames(frequencyCombinationAntigens)<-c(name_classA,name_classB)
        #              names(dimnames(frequencyCombinationAntigens)) <- c("Antigens", "")

        #      # Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
        #     SE_SP<-array(0,dim=c(K,2*2))
        #     SE_SP[,class_A]<- round(frequencyCombinationAntigens[,class_A]*100/nSamples_A,digits=0)
        #     SE_SP[,class_B] <-round(frequencyCombinationAntigens[,class_B]*100/nSamples_B,digits=0)
        #     rownames(SE_SP)<-listCombinationAntigens
        #     colnames(SE_SP)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
        #     SE_SP[,class_A+2]<-100-SE_SP[,class_A]
        #     SE_SP[,class_B+2]<-100-SE_SP[,class_B]
            


        #    uh1<-hist(SE_SP[,1],plot=F)
        #    uh2<-hist(SE_SP[,4],plot=F)


        #    par(mfrow=c(1,2))
        #    plot(uh1, ylim=c(0, 200), col="firebrick3",xlab="", main="Sensitivity",xlim=c(0,100))
        #    text(uh1$mids, uh1$counts+10, label=c(uh1$counts))
        #    plot(uh2, ylim=c(0, 200), col="dodgerblue3",xlab="", main="Specificity",xlim=c(0,100))
        #    text(uh2$mids, uh2$counts+10, label=c(uh2$counts))
        #  })

 ####### Function to visualize error message if the user insert unappropriate thresholds         
         not_value <- function(input1,input2) {
             if (input1=='' | input1 < 1 | input2 < 1 ) {
             "Choose the appropriate thresholds. No zero/negative threshold please!"
               } 
            }

        output$barDistribution <- renderChart2({
          input$runButton
            if(input$signalthr>=1 & input$combithr>=1){
              nbar()
          }else {
             validate(not_value(input$signalthr,input$combithr))
         }})


         # output$viewDistribution <- renderPlot({
         #  if(input$signalthr>=1 & input$combithr>=1){
         #      nview()
         #  }else {
         #     validate(not_value(input$signalthr,input$combithr))
         # }})

 ####### Displat the SE and SP table for all possible combinations 
         se_sptable<-eventReactive(input$runButton,{
          if (is.null(data2()))
               return(NULL)
           else if (input$signalthr>=1 & input$combithr>=1) {
              #remove patients and class columns
              dfA<-(classA2()[,3:dim(classA2())[2]])
              dfB<-(classB2()[,3:dim(classB2())[2]])
              dfA<-t(dfA)
              dfB<-t(dfB)
              n_features<-length(rownames(dfA))
              k<-1:n_features
              K<-2^n_features-1      #total number of combinations
              nAntigen<-K
              class_A<-1
              class_B<-2
              name_classA<-"disease"
              name_classB<-"control"
              nSamples_A<-length(colnames(dfA))
              nSamples_B<-length(colnames(dfB))

             # list of all possible combinations
              listCombinationAntigens<-array(0,dim=c(K,1))
             # relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
             frequencyCombinationAntigens<-array(0,dim=c(K,2))

             index<-1
             for (i in 1:length(k)){
                temp<- combinations(n_features,k[i],rownames(dfA))
                # storage the row index to calculate the relative frequency
                row_index_combination<-combinations(n_features,k[i],k)
                for (j in 1:dim(temp)[1]){
                listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
                ## single antigen
                if(dim(temp)[2]==1){ ## 1 antigen combination
                  frequencyCombinationAntigens[index,class_A]<-length(which(dfA[row_index_combination[j,],]>=input$signalthr))
                  frequencyCombinationAntigens[index,class_B]<-length(which(dfB[row_index_combination[j,],]>=input$signalthr))
                }else{ ## more than 1 antigen (combination)
                  frequencyCombinationAntigens[index,class_A]<- length(which((colSums(dfA[row_index_combination[j,],]>=input$signalthr))>=input$combithr))
                  frequencyCombinationAntigens[index,class_B]<-length(which((colSums(dfB[row_index_combination[j,],]>=input$signalthr))>=input$combithr))
                   }
                  index<-index+1
                     }
                     }
                     #set the row/col names
                     rownames(frequencyCombinationAntigens)<-listCombinationAntigens
                     colnames(frequencyCombinationAntigens)<-c(name_classA,name_classB)
                     names(dimnames(frequencyCombinationAntigens)) <- c("Antigens", "")

             # Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
            SE_SP<-array(0,dim=c(K,2*2))
            SE_SP[,class_A]<- round(frequencyCombinationAntigens[,class_A]*100/nSamples_A,digits=0)
            SE_SP[,class_B] <-round(frequencyCombinationAntigens[,class_B]*100/nSamples_B,digits=0)
            rownames(SE_SP)<-listCombinationAntigens
            colnames(SE_SP)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
            SE_SP[,class_A+2]<-100-SE_SP[,class_A]
            SE_SP[,class_B+2]<-100-SE_SP[,class_B]
            #SE_SP

            if(input$combithr!=n_features){
                SE_SPselected<-SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr),]
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               } 
               #only the combination with all features has been discovered
               else{  
               SE_SPselected<-t(SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr),])
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               rownames(SE_SPselected) <-rownames(SE_SP)[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr)]
               }
            SE_SPselected

          } else{validate(not_value(input$signalthr,input$combithr))}
          })

    output$SE_SPtable<-DT::renderDataTable ({
            se_sptable()
            },colnames = c('Combinations'=1),extensions = c('TableTools','Responsive'),options = list(paging = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25','50','100','All')),searching = TRUE,initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))
            )
 
 ####### Plot the sensitivity and specificity distributions  for Transcriptomics demo data
   nbar_3<-eventReactive(input$runButton_3, {
             #remove patients and class columns
              dfA<-(classA2()[,3:dim(classA2())[2]])
              dfB<-(classB2()[,3:dim(classB2())[2]])
              dfA<-t(dfA)
              dfB<-t(dfB)
              n_features<-length(rownames(dfA))
              k<-1:n_features
              K<-2^n_features-1      #total number of combinations
              nAntigen<-K
              class_A<-1
              class_B<-2
              name_classA<-"disease"
              name_classB<-"control"
              nSamples_A<-length(colnames(dfA))
              nSamples_B<-length(colnames(dfB))

             # list of all possible combinations
              listCombinationAntigens<-array(0,dim=c(K,1))
             # relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
             frequencyCombinationAntigens<-array(0,dim=c(K,2))

             index<-1
             for (i in 1:length(k)){
                temp<- combinations(n_features,k[i],rownames(dfA))
                # storage the row index to calculate the relative frequency
                row_index_combination<-combinations(n_features,k[i],k)
                for (j in 1:dim(temp)[1]){
                listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
                ## single antigen
                if(dim(temp)[2]==1){ ## 1 antigen combination
                  frequencyCombinationAntigens[index,class_A]<-length(which(dfA[row_index_combination[j,],]>=input$signalthr_3))    #input$signalthr
                  frequencyCombinationAntigens[index,class_B]<-length(which(dfB[row_index_combination[j,],]>=input$signalthr_3))    #input$signalthr
                }else{ ## more than 1 antigen (combination)
                  frequencyCombinationAntigens[index,class_A]<- length(which((colSums(dfA[row_index_combination[j,],]>=input$signalthr_3))>=input$combithr_3))   #input$signalthr))>=input$combithr
                  frequencyCombinationAntigens[index,class_B]<-length(which((colSums(dfB[row_index_combination[j,],]>=input$signalthr_3))>=input$combithr_3))     #input$signalthr))>=input$combithr
                   }
                  index<-index+1
                     }
                     }
                     #set the row/col names
                     rownames(frequencyCombinationAntigens)<-listCombinationAntigens
                     colnames(frequencyCombinationAntigens)<-c(name_classA,name_classB)
                     names(dimnames(frequencyCombinationAntigens)) <- c("Antigens", "")

             # Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
            SE_SP<-array(0,dim=c(K,2*2))
            SE_SP[,class_A]<- round(frequencyCombinationAntigens[,class_A]*100/nSamples_A,digits=0)
            SE_SP[,class_B] <-round(frequencyCombinationAntigens[,class_B]*100/nSamples_B,digits=0)
            rownames(SE_SP)<-listCombinationAntigens
            colnames(SE_SP)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
            SE_SP[,class_A+2]<-100-SE_SP[,class_A]
            SE_SP[,class_B+2]<-100-SE_SP[,class_B]



              if(input$combithr_3!=n_features){
                SE_SPselected<-SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_3),]
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               } 
               #only the combination with all features has been discovered
               else{  
               SE_SPselected<-t(SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_3),])
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               rownames(SE_SPselected) <-rownames(SE_SP)[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_3)]
               }
            SE_SPselected
            

            # Highcharts pyramid via rCharts
            breaks_upper<-c(10,20,30,40,50,60,70,80,90,100)
            breaks_lower<-c(0,11,21,31,41,51,61,71,81,91)
            breaks<-cbind(breaks_lower,breaks_upper)
            temp<-array(0,dim=c(10,2))
               for(i in 1:10){
                 temp[i,1]<-sum(SE_SPselected[,1]>=breaks[i,1] & SE_SPselected[,1]<=breaks[i,2])
                  temp[i,2]<-sum(SE_SPselected[,4]>=breaks[i,1] & SE_SPselected[,4]<=breaks[i,2])
                  }
                  Age<-c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100")
                  temp[,1]<-temp[,1]*(-1)
                  Population<-c(temp)
                  Gender<-c(rep("SE",10),rep("SP",10))
                  df <-data.frame(Frequency=Population,Intervals=rep(Age,2),Class=Gender)


                   h1 <- hPlot(
                        y = 'Frequency', 
                        x = 'Intervals', 
                        type = 'bar', 
                        data = df,
                        group = 'Class')
                  h1$plotOptions(series = list(stacking = 'normal', pointPadding = 0, borderWidth = 0))
                  h1$tooltip(formatter = "#! function() { return '<b>'+ this.series.name +', intervals '+ this.point.category +'</b><br/>' + 'Frequency: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);} !#")
                  # to change the bar colors
                  # colors = c('blue', 'red')
                  # h1$colors(colors)
                   if (max(df$Frequency >= 0)) {
                       h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value)) ;} !#"), 
                                 title = list(enabled = TRUE, text = 'Frequency'))
                  } else {
                      h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value)) ;} !#"), 
                                 title = list(enabled = TRUE, text = 'Frequency'))
                   }
                  h1$exporting(enabled=TRUE)
                  return(h1)
})





 ####### Plot the sensitivity and specificity distributions  for OWN data2
   nbar_2<-eventReactive(input$runButton_2, {
             #remove patients and class columns
              dfA<-(classA2()[,3:dim(classA2())[2]])
              dfB<-(classB2()[,3:dim(classB2())[2]])
              dfA<-t(dfA)
              dfB<-t(dfB)
              n_features<-length(rownames(dfA))
              k<-1:n_features
              K<-2^n_features-1      #total number of combinations
              nAntigen<-K
              class_A<-1
              class_B<-2
              name_classA<-"disease"
              name_classB<-"control"
              nSamples_A<-length(colnames(dfA))
              nSamples_B<-length(colnames(dfB))

             # list of all possible combinations
              listCombinationAntigens<-array(0,dim=c(K,1))
             # relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
             frequencyCombinationAntigens<-array(0,dim=c(K,2))

             index<-1
             for (i in 1:length(k)){
                temp<- combinations(n_features,k[i],rownames(dfA))
                # storage the row index to calculate the relative frequency
                row_index_combination<-combinations(n_features,k[i],k)
                for (j in 1:dim(temp)[1]){
                listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
                ## single antigen
                if(dim(temp)[2]==1){ ## 1 antigen combination
                  frequencyCombinationAntigens[index,class_A]<-length(which(dfA[row_index_combination[j,],]>=input$signalthr_2))    #input$signalthr
                  frequencyCombinationAntigens[index,class_B]<-length(which(dfB[row_index_combination[j,],]>=input$signalthr_2))    #input$signalthr
                }else{ ## more than 1 antigen (combination)
                  frequencyCombinationAntigens[index,class_A]<- length(which((colSums(dfA[row_index_combination[j,],]>=input$signalthr_2))>=input$combithr_2))   #input$signalthr))>=input$combithr
                  frequencyCombinationAntigens[index,class_B]<-length(which((colSums(dfB[row_index_combination[j,],]>=input$signalthr_2))>=input$combithr_2))     #input$signalthr))>=input$combithr
                   }
                  index<-index+1
                     }
                     }
                     #set the row/col names
                     rownames(frequencyCombinationAntigens)<-listCombinationAntigens
                     colnames(frequencyCombinationAntigens)<-c(name_classA,name_classB)
                     names(dimnames(frequencyCombinationAntigens)) <- c("Antigens", "")

             # Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
            SE_SP<-array(0,dim=c(K,2*2))
            SE_SP[,class_A]<- round(frequencyCombinationAntigens[,class_A]*100/nSamples_A,digits=0)
            SE_SP[,class_B] <-round(frequencyCombinationAntigens[,class_B]*100/nSamples_B,digits=0)
            rownames(SE_SP)<-listCombinationAntigens
            colnames(SE_SP)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
            SE_SP[,class_A+2]<-100-SE_SP[,class_A]
            SE_SP[,class_B+2]<-100-SE_SP[,class_B]



              if(input$combithr_2!=n_features){
                SE_SPselected<-SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_2),]
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               } 
               #only the combination with all features has been discovered
               else{  
               SE_SPselected<-t(SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_2),])
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               rownames(SE_SPselected) <-rownames(SE_SP)[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_2)]
               }
            SE_SPselected
            

            # Highcharts pyramid via rCharts
            breaks_upper<-c(10,20,30,40,50,60,70,80,90,100)
            breaks_lower<-c(0,11,21,31,41,51,61,71,81,91)
            breaks<-cbind(breaks_lower,breaks_upper)
            temp<-array(0,dim=c(10,2))
               for(i in 1:10){
                 temp[i,1]<-sum(SE_SPselected[,1]>=breaks[i,1] & SE_SPselected[,1]<=breaks[i,2])
                  temp[i,2]<-sum(SE_SPselected[,4]>=breaks[i,1] & SE_SPselected[,4]<=breaks[i,2])
                  }
                  Age<-c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100")
                  temp[,1]<-temp[,1]*(-1)
                  Population<-c(temp)
                  Gender<-c(rep("SE",10),rep("SP",10))
                  df <-data.frame(Frequency=Population,Intervals=rep(Age,2),Class=Gender)


                   h1 <- hPlot(
                        y = 'Frequency', 
                        x = 'Intervals', 
                        type = 'bar', 
                        data = df,
                        group = 'Class')
                  h1$plotOptions(series = list(stacking = 'normal', pointPadding = 0, borderWidth = 0))
                  h1$tooltip(formatter = "#! function() { return '<b>'+ this.series.name +', intervals '+ this.point.category +'</b><br/>' + 'Frequency: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);} !#")
                  # to change the bar colors
                  # colors = c('blue', 'red')
                  # h1$colors(colors)
                   if (max(df$Frequency >= 0)) {
                       h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value)) ;} !#"), 
                                 title = list(enabled = TRUE, text = 'Frequency'))
                  } else {
                      h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value)) ;} !#"), 
                                 title = list(enabled = TRUE, text = 'Frequency'))
                   }
                  h1$exporting(enabled=TRUE)
                  return(h1)
})







        # nview_2<-eventReactive(input$runButton_2, {
        #        #remove patients and class columns
        #       dfA<-(classA2()[,3:dim(classA2())[2]])
        #       dfB<-(classB2()[,3:dim(classB2())[2]])
        #       dfA<-t(dfA)
        #       dfB<-t(dfB)
        #       n_features<-length(rownames(dfA))
        #       k<-1:n_features
        #       K<-2^n_features-1      #total number of combinations
        #       nAntigen<-K
        #       class_A<-1
        #       class_B<-2
        #       name_classA<-"disease"
        #       name_classB<-"control"
        #       nSamples_A<-length(colnames(dfA))
        #       nSamples_B<-length(colnames(dfB))

        #      # list of all possible combinations
        #       listCombinationAntigens<-array(0,dim=c(K,1))
        #      # relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
        #      frequencyCombinationAntigens<-array(0,dim=c(K,2))

        #      index<-1
        #      for (i in 1:length(k)){
        #         temp<- combinations(n_features,k[i],rownames(dfA))
        #         # storage the row index to calculate the relative frequency
        #         row_index_combination<-combinations(n_features,k[i],k)
        #         for (j in 1:dim(temp)[1]){
        #         listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
        #         ## single antigen
        #         if(dim(temp)[2]==1){ ## 1 antigen combination
        #           frequencyCombinationAntigens[index,class_A]<-length(which(dfA[row_index_combination[j,],]>=input$signalthr_2))    #input$signalthr
        #           frequencyCombinationAntigens[index,class_B]<-length(which(dfB[row_index_combination[j,],]>=input$signalthr_2))    #input$signalthr
        #         }else{ ## more than 1 antigen (combination)
        #           frequencyCombinationAntigens[index,class_A]<- length(which((colSums(dfA[row_index_combination[j,],]>=input$signalthr_2))>=input$combithr_2))   #input$signalthr))>=input$combithr
        #           frequencyCombinationAntigens[index,class_B]<-length(which((colSums(dfB[row_index_combination[j,],]>=input$signalthr_2))>=input$combithr_2))     #input$signalthr))>=input$combithr
        #            }
        #           index<-index+1
        #              }
        #              }
        #              #set the row/col names
        #              rownames(frequencyCombinationAntigens)<-listCombinationAntigens
        #              colnames(frequencyCombinationAntigens)<-c(name_classA,name_classB)
        #              names(dimnames(frequencyCombinationAntigens)) <- c("Antigens", "")

        #      # Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
        #     SE_SP<-array(0,dim=c(K,2*2))
        #     SE_SP[,class_A]<- round(frequencyCombinationAntigens[,class_A]*100/nSamples_A,digits=0)
        #     SE_SP[,class_B] <-round(frequencyCombinationAntigens[,class_B]*100/nSamples_B,digits=0)
        #     rownames(SE_SP)<-listCombinationAntigens
        #     colnames(SE_SP)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
        #     SE_SP[,class_A+2]<-100-SE_SP[,class_A]
        #     SE_SP[,class_B+2]<-100-SE_SP[,class_B]
            


        #    uh1<-hist(SE_SP[,1],plot=F)
        #    uh2<-hist(SE_SP[,4],plot=F)


        #    par(mfrow=c(1,2))
        #    plot(uh1, ylim=c(0, 200), col="firebrick3",xlab="", main="Sensitivity",xlim=c(0,100))
        #    text(uh1$mids, uh1$counts+10, label=c(uh1$counts))
        #    plot(uh2, ylim=c(0, 200), col="dodgerblue3",xlab="", main="Specificity",xlim=c(0,100))
        #    text(uh2$mids, uh2$counts+10, label=c(uh2$counts))
        #  })

        output$barDistribution_3 <- renderChart2({
            if(input$signalthr_3>=0 & input$combithr_3>=1){
              nbar_3()
          }else {
             validate(not_value(input$signalthr_3,input$combithr_3))
         }})

       
        output$barDistribution_2 <- renderChart2({
            if(input$signalthr_2>=0 & input$combithr_2>=1){
              nbar_2()
          }else {
             validate(not_value(input$signalthr_2,input$combithr_2))
         }})

         # output$viewDistribution_2 <- renderPlot({
         #  if(input$signalthr_2>=1 & input$combithr_2>=1){
         #      nview_2()
         #  }else {
         #     validate(not_value(input$signalthr_2,input$combithr_2))
         # }})

 ####### Display the SE and SP table for all possible combinations for TRANSCRIPTOMICS demo data
         se_sptable_3<-eventReactive(input$runButton_3,{
               if (is.null(data2()))
               return(NULL)
           else if (input$signalthr_3>=0 & input$combithr_3>=1){
              #remove patients and class columns
              dfA<-(classA2()[,3:dim(classA2())[2]])
              dfB<-(classB2()[,3:dim(classB2())[2]])
              dfA<-t(dfA)
              dfB<-t(dfB)
              n_features<-length(rownames(dfA))
              k<-1:n_features
              K<-2^n_features-1      #total number of combinations
              nAntigen<-K
              class_A<-1
              class_B<-2
              name_classA<-"disease"
              name_classB<-"control"
              nSamples_A<-length(colnames(dfA))
              nSamples_B<-length(colnames(dfB))

             # list of all possible combinations
              listCombinationAntigens<-array(0,dim=c(K,1))
             # relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
             frequencyCombinationAntigens<-array(0,dim=c(K,2))

             index<-1
             for (i in 1:length(k)){
                temp<- combinations(n_features,k[i],rownames(dfA))
                # storage the row index to calculate the relative frequency
                row_index_combination<-combinations(n_features,k[i],k)
                for (j in 1:dim(temp)[1]){
                listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
                ## single antigen
                if(dim(temp)[2]==1){ ## 1 antigen combination
                  frequencyCombinationAntigens[index,class_A]<-length(which(dfA[row_index_combination[j,],]>=input$signalthr_3))
                  frequencyCombinationAntigens[index,class_B]<-length(which(dfB[row_index_combination[j,],]>=input$signalthr_3))
                }else{ ## more than 1 antigen (combination)
                  frequencyCombinationAntigens[index,class_A]<- length(which((colSums(dfA[row_index_combination[j,],]>=input$signalthr_3))>=input$combithr_3))
                  frequencyCombinationAntigens[index,class_B]<-length(which((colSums(dfB[row_index_combination[j,],]>=input$signalthr_3))>=input$combithr_3))
                   }
                  index<-index+1
                     }
                     }
                     #set the row/col names
                     rownames(frequencyCombinationAntigens)<-listCombinationAntigens
                     colnames(frequencyCombinationAntigens)<-c(name_classA,name_classB)
                     names(dimnames(frequencyCombinationAntigens)) <- c("Antigens", "")

             # Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
            SE_SP<-array(0,dim=c(K,2*2))
            SE_SP[,class_A]<- round(frequencyCombinationAntigens[,class_A]*100/nSamples_A,digits=0)
            SE_SP[,class_B] <-round(frequencyCombinationAntigens[,class_B]*100/nSamples_B,digits=0)
            rownames(SE_SP)<-listCombinationAntigens
            colnames(SE_SP)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
            SE_SP[,class_A+2]<-100-SE_SP[,class_A]
            SE_SP[,class_B+2]<-100-SE_SP[,class_B]

               
              if(input$combithr_3!=n_features){
                SE_SPselected<-SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_3),]
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               } 
               #only the combination with all features has been discovered
               else{  
               SE_SPselected<-t(SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_3),])
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               rownames(SE_SPselected) <-rownames(SE_SP)[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_3)]
               }
            SE_SPselected
            
           } else{validate(not_value(input$signalthr_3,input$combithr_3))}
           })




    output$SE_SPtable_3<-DT::renderDataTable ({
            se_sptable_3()
            },colnames = c('Combinations'=1),extensions = c('TableTools','Responsive'),options = list(paging = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25','50','100','All')),searching = TRUE,initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))
            )

 ####### Display the SE and SP table for all possible combinations for OTHER data
         se_sptable_2<-eventReactive(input$runButton_2,{
               if (is.null(data2()))
               return(NULL)
           else if (input$signalthr_2>=0 & input$combithr_2>=1){
              #remove patients and class columns
              dfA<-(classA2()[,3:dim(classA2())[2]])
              dfB<-(classB2()[,3:dim(classB2())[2]])
              dfA<-t(dfA)
              dfB<-t(dfB)
              n_features<-length(rownames(dfA))
              k<-1:n_features
              K<-2^n_features-1      #total number of combinations
              nAntigen<-K
              class_A<-1
              class_B<-2
              name_classA<-"disease"
              name_classB<-"control"
              nSamples_A<-length(colnames(dfA))
              nSamples_B<-length(colnames(dfB))

             # list of all possible combinations
              listCombinationAntigens<-array(0,dim=c(K,1))
             # relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
             frequencyCombinationAntigens<-array(0,dim=c(K,2))

             index<-1
             for (i in 1:length(k)){
                temp<- combinations(n_features,k[i],rownames(dfA))
                # storage the row index to calculate the relative frequency
                row_index_combination<-combinations(n_features,k[i],k)
                for (j in 1:dim(temp)[1]){
                listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
                ## single antigen
                if(dim(temp)[2]==1){ ## 1 antigen combination
                  frequencyCombinationAntigens[index,class_A]<-length(which(dfA[row_index_combination[j,],]>=input$signalthr_2))
                  frequencyCombinationAntigens[index,class_B]<-length(which(dfB[row_index_combination[j,],]>=input$signalthr_2))
                }else{ ## more than 1 antigen (combination)
                  frequencyCombinationAntigens[index,class_A]<- length(which((colSums(dfA[row_index_combination[j,],]>=input$signalthr_2))>=input$combithr_2))
                  frequencyCombinationAntigens[index,class_B]<-length(which((colSums(dfB[row_index_combination[j,],]>=input$signalthr_2))>=input$combithr_2))
                   }
                  index<-index+1
                     }
                     }
                     #set the row/col names
                     rownames(frequencyCombinationAntigens)<-listCombinationAntigens
                     colnames(frequencyCombinationAntigens)<-c(name_classA,name_classB)
                     names(dimnames(frequencyCombinationAntigens)) <- c("Antigens", "")

             # Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
            SE_SP<-array(0,dim=c(K,2*2))
            SE_SP[,class_A]<- round(frequencyCombinationAntigens[,class_A]*100/nSamples_A,digits=0)
            SE_SP[,class_B] <-round(frequencyCombinationAntigens[,class_B]*100/nSamples_B,digits=0)
            rownames(SE_SP)<-listCombinationAntigens
            colnames(SE_SP)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
            SE_SP[,class_A+2]<-100-SE_SP[,class_A]
            SE_SP[,class_B+2]<-100-SE_SP[,class_B]

               
              if(input$combithr_2!=n_features){
                SE_SPselected<-SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_2),]
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               } 
               #only the combination with all features has been discovered
               else{  
               SE_SPselected<-t(SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_2),])
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               rownames(SE_SPselected) <-rownames(SE_SP)[which(str_count(rownames(SE_SP),pattern="M")>=input$combithr_2)]
               }
            SE_SPselected
            
           } else{validate(not_value(input$signalthr_2,input$combithr_2))}
           })

    output$SE_SPtable_2<-DT::renderDataTable ({
            se_sptable_2()
            },colnames = c('Combinations'=1),extensions = c('TableTools','Responsive'),options = list(paging = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25','50','100','All')),searching = TRUE,initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))
            )


 ####### Storage the SE_SP matrix for the selected thresholds
SE_SPmatrix<-reactive({
  input$runButton
  input$runButton_3
  input$runButton_2
  if (input$runButton!='0'){
    signalthr<-input$signalthr
    combithr<-input$combithr
  } 
  if (input$runButton_3!='0'){
    signalthr<-input$signalthr_3
    combithr<-input$combithr_3
  }
  if (input$runButton_2!='0'){
    signalthr<-input$signalthr_2
    combithr<-input$combithr_2
  }
    #remove patients and class columns
              dfA<-(classA2()[,3:dim(classA2())[2]])
              dfB<-(classB2()[,3:dim(classB2())[2]])
              dfA<-t(dfA)
              dfB<-t(dfB)
              n_features<-length(rownames(dfA))
              k<-1:n_features
              K<-2^n_features-1      #total number of combinations
              nAntigen<-K
              class_A<-1
              class_B<-2
              name_classA<-"disease"
              name_classB<-"control"
              nSamples_A<-length(colnames(dfA))
              nSamples_B<-length(colnames(dfB))

             # list of all possible combinations
              listCombinationAntigens<-array(0,dim=c(K,1))
             # relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
             frequencyCombinationAntigens<-array(0,dim=c(K,2))

             index<-1
             for (i in 1:length(k)){
                temp<- combinations(n_features,k[i],rownames(dfA))
                # storage the row index to calculate the relative frequency
                row_index_combination<-combinations(n_features,k[i],k)
                for (j in 1:dim(temp)[1]){
                listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
                ## single antigen
                if(dim(temp)[2]==1){ ## 1 antigen combination
                  frequencyCombinationAntigens[index,class_A]<-length(which(dfA[row_index_combination[j,],]>=signalthr))
                  frequencyCombinationAntigens[index,class_B]<-length(which(dfB[row_index_combination[j,],]>=signalthr))
                }else{ ## more than 1 antigen (combination)
                  frequencyCombinationAntigens[index,class_A]<- length(which((colSums(dfA[row_index_combination[j,],]>=signalthr))>=combithr))
                  frequencyCombinationAntigens[index,class_B]<-length(which((colSums(dfB[row_index_combination[j,],]>=signalthr))>=combithr))
                   }
                  index<-index+1
                     }
                     }
                     #set the row/col names
                     rownames(frequencyCombinationAntigens)<-listCombinationAntigens
                     colnames(frequencyCombinationAntigens)<-c(name_classA,name_classB)
                     names(dimnames(frequencyCombinationAntigens)) <- c("Antigens", "")

             # Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
            SE_SP<-array(0,dim=c(K,2*2))
            SE_SP[,class_A]<- round(frequencyCombinationAntigens[,class_A]*100/nSamples_A,digits=0)
            SE_SP[,class_B] <-round(frequencyCombinationAntigens[,class_B]*100/nSamples_B,digits=0)
            rownames(SE_SP)<-listCombinationAntigens
            colnames(SE_SP)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
            SE_SP[,class_A+2]<-100-SE_SP[,class_A]
            SE_SP[,class_B+2]<-100-SE_SP[,class_B]
              if(combithr!=n_features){
                SE_SPselected<-SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=combithr),]
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               } 
               #only the combination with all features has been discovered
               else{  
               SE_SPselected<-t(SE_SP[which(str_count(rownames(SE_SP),pattern="M")>=combithr),])
               colnames(SE_SPselected)<-c("SE%_disease","SE%_control","SP%_disease","SP%_control")
               rownames(SE_SPselected) <-rownames(SE_SP)[which(str_count(rownames(SE_SP),pattern="M")>=combithr)]
               }
            SE_SPselected
            rownames(SE_SPselected) <-rownames(SE_SP)[which(str_count(rownames(SE_SP),pattern="M")>=combithr)]

           SE_SPselected

  })


# Conditional icon for widget.
# Returns arrow-up icon on true (if true_direction is 'up'), e.g. load time % change > 0

cond_icon <- function(condition, true_direction = "up") {

  if (true_direction == "up") {
    if(condition){
    return(icon(ifelse(condition, "step-forward", "spinner")))
    }else {
      validate(not_value(input$signalthr,input$combithr))
    }
  }
  #return(icon(ifelse(condition, "spinner", "step-forward")))
}
# Conditional color for widget
# Returns 'green' on true, 'red' on false, e.g. api usage % change > 0
#                                               load time % change < 0
cond_color <- function(condition, true_color = "green") {
  if(is.na(condition)){
    return("black")
  }

  colours <- c("green","red")
  return(ifelse(condition, true_color, colours[!colours == true_color]))
}

  
# Next widget
  output$kpi_summary_box_1 <- renderValueBox({
      z_color<-0
      z_icon<-0
      # Proteomics demo data
      if(input$runButton!='0' & input$signalthr>=1 & input$combithr>=1  ){  #input$runButton!='0' &
        z_color<-input$runButton
        z_icon<-input$runButton
      }
      if(input$runButton!='0' &  input$signalthr=='' & input$combithr==''  ){ #input$runButton!='0' &  IN questo ciclo non entra!!!!!
        z_color<-0
        z_icon<-0
      }
      # Transcriptomics demo data 
      if(input$runButton_3!='0' & input$signalthr_3>=1 & input$combithr_3>=1  ){ #input$runButton_3!='0' &
        z_color<-input$runButton_3
        z_icon<-input$runButton_3
      }
      if(input$runButton_3!='0' & input$signalthr_3=='' & input$combithr_3==''  ){ #input$runButton_3!='0' &
        z_color<-0
        z_icon<-0
      }
      # Upload own data 
      if(input$runButton_2!='0' & input$signalthr_2>=1 & input$combithr_2>=1  ){ #input$runButton_2!='0' & 
        z_color<-input$runButton_2
        z_icon<-input$runButton_2
      }
      if(input$runButton_2!='0' & input$signalthr_2=='' & input$combithr_2==''  ){ #input$runButton_2!='0' &
        z_color<-0
        z_icon<-0
      }

    valueBox(
      value = "Next",
      subtitle = "Gold Combinations", 
      color = cond_color(z_color),
      icon=  cond_icon(z_icon)
    )
  })

# Warning widget
   observeEvent(input$do,{
    if( input$runButton==0 | input.runButton_3==0 | input.runButton_2==0){
    newvalue<- "combi"
    updateTabItems(session,"tabs",newvalue)
  }
    })



  #     output$kpi_summary_box_2 <- renderValueBox({
  #   valueBox(
  #     value = sprintf("%s", (190)),
  #     subtitle = sprintf("KPI 2 (%.1f%%)", -0.23),
  #     icon = icon("step-forward"),
  #     color = "red"
  #   )
  # })



  # output$provathr <- renderText({ 
  #     # paste("You have selected", input$combithr,input$signalthr)
  #     # paste("This is the",SE_disease())
  #     # paste("This is the",rownames(SPE_control()))
  #     s1=(rownames(SE_SPmatrix()))
  #     print((s1))
  #   })

  #   output$provathr_2 <- renderText({ 
  #     #paste("You have selected", input$combithr_2,input$signalthr_2)
  #     #paste("This is the",SE_disease())
  #      # paste("This is the",rownames(SPE_control()))
  #        s1=(rownames(SE_SPmatrix()))
  #     print((s1))
  #   })



         
 ##################################
################# Gold combinations tab
##################################        

 ####### Combination bubble chart


####################### ggplot graph 
#       output$plotbubble<- renderPlot({
#           # Return the data and options
#      input1<- SE_disease()
#      input2 <- SPE_control()
#      input3 <-cbind(input1,input2)
#      a=(unique(input3))
#      nCount=array(0,dim=c(dim(a)[1],1))
#      for (i in 1:dim(a)[1]){
#       nCount[i,1]<- length(intersect(which(a[i,1]==input3[,1]), which(a[i,2]==input3[,2])))
#       }
#      IdVart=array(0,dim=c(dim(a)[1],1))
#      for (i in 1:dim(a)[1]){
#          IdVart[i]<-paste0("Int_",i)
#       }
#       df <- data.frame(NameCombination=IdVart,SE_disease=a[,1],SP_control=a[,2],nCount=nCount)
#             g <- ggplot(df, aes(SE_disease, SP_control)) + geom_point(aes(size = nCount), alpha=0.5,colour = "mediumaquamarine") + theme_bw() + xlab("SE") + ylab("SP")+ggtitle("All combination intervals")+ theme_grey(base_size = 18)
#              g + scale_size_continuous(range=c(10,30),name='N of combinations') + geom_text(aes(label = nCount))
#             })
            
            
#      output$mychart <- renderScatterChart({
#           input1<- SE_disease()
#         #input2 <- SPE_control()
#     # Return a data frame. Each column will be a series in the line chart.
    
#     data.frame( #"All Combinations" = input1 + input$sineAmplitude)
#       #Sine = sin(1:100/10 + input$sinePhase * pi/180) * input$sineAmplitude,
# #      Cosine = 0.5 * cos(1:100/10),
# #      "Sine 2" = sin(1:100/10) * 0.25 + 0.5
#        #Specificity=list(sample(50:100,10),sample(50:100,10))
#        Sensitivity=sample(50:100,10),
#        Specificity=sample(50:100,10)
#      )
# ############# per dopo quando carico i miei dati
# #        Specificity=input1*input$sineAmplitude,
# #       Sensitivity=input2*input$sinePhase
# })
  # observeEvent(input$switchtab, {
  #   newtab <- switch(input$tabs,
  #     actionbutton("nexstop",input$gold)
  #   )
  #   updateTabItems(session, "tabs", newtab)
  # }) 

####################### Dynamic Value of SinePhase and SineAmplitude Sliders

observeEvent(input$DataInput,{
  x.dataset.selection<-input$DataInput
  if(x.dataset.selection==4){ #the choice is on "Select"
    val.sine=0
    val.amplitude=0
  }else if(x.dataset.selection==1){ #the choice is on "Proteomics" demo data
    val.sine=40
    val.amplitude=80
  } else if(x.dataset.selection==5){ #the choice is on "Transcriptomics" demo data
    val.sine=70
    val.amplitude=80
  } else {  #the choice is on "Upload" 
    val.sine=40
    val.amplitude=60
  }
    updateSliderInput(session, "sinePhase", value = val.sine)
    updateSliderInput(session,"sineAmplitude",value=val.amplitude)
  })
      #  observeEvent(input$sinePhase,{
      #   if(input$DataInput==4){
      #     val=0
      #   }
      #   else if(input$DataInput=="Load demo data (proteomics)"){
      #     val=40
      #   }
      #   else if(input$DataInput==5){
      #     val=70
      #   }
 
      # #   # Control the value.
      # #   # SE value is 40 when input value is proteomics; 70 when value is transcriptomics.
      #    updateSliderInput(session, "sinePhase", value = val)
      #  })

   
####################### highchart graph 

 ####### Function to visualize error message if the user insert unappropriate thresholds         
         not_value2 <- function(input1) {
             if (length(input1)==0  ) {
             "Choose the appropriate filter. No gold combinations with this filter!"
               } 
            }



 ####### bubble graph with size: the number of marker in the combiantions
     output$highbubble<- renderChart2({
             input$runButton
             input$runButton_3
             input$runButton_2
           
              
             # Highcharts bubble via rCharts
                input1<- SE_SPmatrix()[,1]
                input2 <- SE_SPmatrix()[,4]
                input3 <-cbind(input1,input2)
                nCount<-str_count(rownames(SE_SPmatrix()),pattern="M")
                a=input3
              #  a=(unique(input3))
              # nCount=array(0,dim=c(dim(a)[1],1))
              # #Names=rep("Simple Duple",dim(a)[1])
              # for (i in 1:dim(a)[1]){
              #      nCount[i,1]<- length(intersect(which(a[i,1]==input3[,1]), which(a[i,2]==input3[,2])))
              #  }
               gold = which(a[,1]>=input$sinePhase & a[,2]>=input$sineAmplitude)
             

               Colours<- rep("Under the thresholds",dim(a)[1])
               if(length(gold)!= 0){Colours[gold]<- "Gold"}
               
                #Colours=c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0) 
                # Colours=c("white","gold","white","white","white","white","white","white","white","white","white","white",
                #       "white","white","white","white","white","white","white","white","white","white","white","white","white","gold","white")
                Names<-Colours

           df <-data.frame(SE=a[,1],SP=a[,2],Occurence=nCount,Name=Names,Colour=Colours)  #,Class=Gender
               h1 <- hPlot(
                        y = 'SE', 
                        x = 'SP',
                        group='Colour', 
                        size='Occurence',
                        type = 'bubble', 
                        name='Names',
                        data = df)
                        #group='Class') #ci puo essere anche group
               h1$title(text="Gold combinations")
             
             #check the range the thresholds in order to visualize the appropriate colour
             if(length(gold) != 0){
                h1$colors(c("#ffe976","#4480BB"))   #funziona ma non governo opacità e quindi devo scegliere i colori opportuni #b1e5ff"
             } else if (length(gold)==0)  {
             # h1$colors(c("#4480BB"))           #only white combinations
               validate(not_value2(gold))        #error message
               } 
               
             #h1$colors(list(radialGradient=list(cx=0.5,cy=0.3,r=0.5),stops=list(c(0, 'yellow'))))   #NON VAAAAAAAA
              
              # https://gist.github.com/ramnathv/d640d4e29755667626f7#file-code-r
              # to radialize the color
              # colors = "#! Highcharts.map(Highcharts.getOptions().colors, function(color) {
              #    return {
              #       radialGradient: { cx: 0.4, cy: 0.3, r: 0.5 },
              #          stops: [
              #                  [0, color],
              #              [1,  Highcharts.Color(color).brighten(-0.3).get('rgb')] // darken
              #                 ]
              #            }}) !#"
              #    h1$plotOptions(bubble = list(colors = "#0000FF"))    


              #h1$plotOptions(series= list(fillColor=list(linearGradient=c(0,0,0,300))))
               
               #h1$plotOptions(series = list(colors = ("lightblue"))) # to change the bubble color (it works with one colour!!!)
               #h1$plotOptions(type = "addColorAxis",colorSeries = "Colour",palette = c("lightblue","gold") )
               h1$legend(layout='vertical',title=list(text="Combination Type")) # labelFormat=list("{name1}","{name2}"),labelFormatter="#! function() {return this.series.name;} !#"
            
               h1$exporting(enabled=TRUE)
               h1$chart(zoomType = "xy",plotBorderWidth=1,options3d=list(enabled=TRUE))
               h1$xAxis(min=0,max=100,title = list(text = "Specificity (%)"),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="black",dashStyle="dot",width=2,value=input$sineAmplitude,label=list(align='middle',rotation=0,y=15,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="your chosen specificity"),zIndex=3)))
               h1$yAxis(min=0,max=100,title = list(text = "Sensitivity (%)"),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="black",dashStyle="dot",width=2,value=input$sinePhase,label=list(align='middle',rotation=0,y=15,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="your chosen sensitivity"),zIndex=3)))
               # h1$plotOptions(bubble = list(dataLabels = list(enabled = TRUE, x = 0, 
               #                                formatter="#! function() {
               #                                  return this.point.name;
               #                                } !#", style=list(color= 'black'))))

               h1$plotOptions(bubble=list(minSize=min(nCount)*10,maxSize=max(nCount)*10,sizeBy="width")) # to set the bubble size
              # h1$series(marker=list(fillColor=list(radialGradient=list(cx=0.4,cy=0.3,r=0.5),stops=list(c(0, 'blue'), c(1, 'orange')))))   #marker=list(fillColor=list(radialGradient=list(cx=0.4,cy=0.3,r=0.5),stops=list(c(0, "#003399"), c(1, '#3366AA'))))
               h1$tooltip(crosshairs=c(TRUE,TRUE))
               h1$set(width = 500,height=600)    #to set the plot width
              

                #h1$s?eries(data=df[which(df$SE>=input$sinePhase),which(df$SP>=input$sineAmplitude)],color='#FFEB7F')
               #h1$tooltip(useHTML=TRUE,headerFormat='<table>',pointFormat='<tr><th colspan="2"><h3>{Simple Duple}</h3></th></tr>'+'<tr><th>Sensitivity:</th><td>{point.x}</td></tr>',footerFormat='</table>',followPointer=TRUE)
                # pointFormat='<tr><th colspan="2"><h3>{Simple Duple}</h3></th></tr>' +
                # '<tr><th>Sensitivity:</th><td>{point.x}g</td></tr>' +
                # '<tr><th>Specificity:</th><td>{point.y}g</td></tr>' +
                # '<tr><th>Occurence:</th><td>{point.z}%</td></tr>',footerFormat='</table>', followPointer=TRUE)
                
                return(h1)
                  
      })




 ####### Display the gold table


        #Interactive / Reactive change of value field of numericInput
        output$numericSe<-renderUI({
                   numericInput("innumericSe","Sensitivity (%)",value=input$sinePhase,min=0,max=100)
          })
        output$numericSp<-renderUI({
                 numericInput("innumericSp","Specificity (%)",value=input$sineAmplitude,min=0,max=100)        
          })


        nTable1<-eventReactive(input$submitButton1, {
          input$runButton_2
          input$runButton
              input1<-SE_SPmatrix()[,1]
              input2<-SE_SPmatrix()[,4]
              input3<-cbind(input1,input2)

              gold<-input3[intersect(which(input3[,1]>=input$innumericSe),which(input3[,2]>=input$innumericSp)),]     #input$sethr1,input$spethr1: parameters before the iteractive change
              #symbolgold<-rownames(gold)
              symbolgold<-rownames(SE_SPmatrix())[intersect(which(input3[,1]>=input$innumericSe),which(input3[,2]>=input$innumericSp))]
              start<-which(as.numeric(str_detect(symbolgold,pattern="-"))==1)[1]
              if(length(symbolgold)>1){
                symbolgold[start:dim((gold))[1]]<-paste0("Combo ",as.roman(1:(dim((gold))[1]-start+1)))
                 gold<-cbind(symbolgold,(gold))
               
              }else if (length(symbolgold)==1){
                symbolgold[start:1]<-paste0("Combo ",as.roman(1:(1-start+1)))
                gold<-cbind(symbolgold,t(gold))
              }else{
                column(10,  validate(not_value2(symbolgold)))
               
              }
            colnames(gold)<-c("Symbol","SE%_disease","SP%_control")
            gold
         })


      


    #####GoldTable1 without  styleColorBar
    #       output$Goldtable1 <- DT::renderDataTable({
    #                     nTable1()
    #      },colnames = c('Combinations'=1),extensions = c('TableTools','Responsive'),options = list(paging = TRUE, searching = TRUE, initComplete = JS(
    # "function(settings, json) {",
    # "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    # "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))
    #      )

    #####GoldTable1 with  styleColorBar It is also easy to style the table cells according to their values using the formatStyle() function
          output$Goldtable1 <- DT::renderDataTable({
            df=nTable1()

         dat<- datatable( nTable1(),colnames = c('Combinations'=1),extensions = c('TableTools','Responsive'),options = list(paging = TRUE, searching = TRUE, initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))) %>%
          formatStyle('SE%_disease',background = styleColorBar(range(0,as.numeric(df[,2])), 'lightblue'), backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center') %>%
            formatStyle('SP%_control',background = styleColorBar(range(0,as.numeric(df[,3])), 'lightgray'), backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center')

         })

####### Back widget: back to Combinatorial Analysis
    output$kpi_summary_box_2 <- renderValueBox({
    valueBox(
      value = "Back",
      subtitle = "Combinatorial Analysis",
      icon = icon("step-backward"),
      color = "green"
    )
  })



# ####### Warning widget: click the Submit button
  output$kpi_summary_box_5 <- renderValueBox({
      # 
      if(input$submitButton1=='0'){
        z_color<-"red"
        z_icon<-icon("spinner")
        z_title<-"Click the \"Submit\" button"
      }

    valueBox(
      value = "Next",
      subtitle = z_title, 
      color = z_color,
      icon=  z_icon
    )
  })


# Conditional icon for widget.
# Returns arrow-up icon on true (if true_direction is 'up'), e.g. load time % change > 0
cond_icon2 <- function(condition, true_direction = "up") {

  if (true_direction == "up") {
    if(condition){
    return(icon(ifelse(condition, "step-forward", "spinner")))
    }else{validate(not_value2(condition,condition))}
  }

  #return(icon(ifelse(condition, "spinner", "step-forward")))

}
# Conditional color for widget
# Returns 'green' on true, 'red' on false, e.g. api usage % change > 0
#                                               load time % change < 0
cond_color2 <- function(condition, true_color = "green") {
  if(is.na(condition)){
    return("black")
  }

  colours <- c("green","red")
  return(ifelse(condition, true_color, colours[!colours == true_color]))
}
cond_title2 <- function(condition, true_color = "green") {
  if(is.na(condition)){
    return("black")
  }

  "Next"
}
####### Next widget: next to ROC curve
  output$kpi_summary_box_3 <- renderValueBox({

    valueBox(
      value = "Next",
      subtitle = "ROC analysis",
      icon = icon("step-forward"),
      color ="green"
    )
  })



##################################
################# ROC curves tab
##################################

####### Plot the gold table
         nTable<-eventReactive(input$submitButton, {

              input1<-SE_disease()
              input2<-SPE_control()
              input3<-cbind(input1,input2)
              gold<-input3[intersect(which(input3[,1]>=input$sethr),which(input3[,2]>=input$spethr)),]
             

              gold <-cbind(paste0("Combo ",as.roman(1:dim(gold)[1])), gold)
              colnames(gold)<-c("Symbol","SE%_disease","SP%_control")
               gold

         })

   output$Goldtable <- DT::renderDataTable({

         nTable()

         },colnames = c('Combinations'=1),extensions = 'TableTools',options = list(paging = TRUE, searching = TRUE, initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE))))

####### Notification message  for the user if no gold combinations have been selected
output$Mgoldtable<-renderPrint({
   str<-tags$strong(style="color: mediumvioletred",p("Be careful!"),p(" No gold combinations have been selected yet. (See the box Gold Table in Gold Combinations)"))
      str
    })

   
####### Select the marker combination once the user have been selected the gold combinations
    outVar <- reactive({
      #vars <- as.list(rownames(nTable1()))  #rownames(nTable1())
     input$submitButton1
      vars <- nTable1()[1:dim(nTable1())[1]]
      return(vars)
    })

    outVar2 <- reactive({
      #vars <- as.list(rownames(nTable1()))  #rownames(nTable1())
     input$submitButton1
      vars <- rownames(SE_SPmatrix())
      return(vars)
    })

    output$Mvariable = renderUI({
      input$submitButton1 
      selectInput('variables2', 'Marker', outVar())
    })



####### Data Combo Marker Selection

# opt.cut function to determine the se and sp at the cutoff based on Youden index
opt.cut<-function(perf, pred){
     cut.ind = mapply(FUN=function(x, y, p){
         d = (x - 0)^2 + (y-1)^2
         ind = which(d == min(d))
         c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
           cutoff = p[[ind]])
     }, perf@x.values, perf@y.values, pred@cutoffs)
     }

SelectedComboObj<-eventReactive(input$rocButton, {
      ListColumnMarker=which(as.numeric(str_detect(input$variables2, pattern = colnames(data2())))==1)
       if(length(ListColumnMarker)==1){
              tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
        }else{
               tmpdf <-data2()[,ListColumnMarker]
        }
     if(dim(tmpdf)[1]==1){
      tmpcolumn <-log(tmpdf+1)
      }else{
    tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
    }
    tmpcolumn
   })

SelectedDfObj<-eventReactive(input$rocButton, {
      ListColumnMarker=which(as.numeric(str_detect(input$variables2, pattern = colnames(data2())))==1)
       if(length(ListColumnMarker)==1){
              tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
        }else{
               tmpdf <-data2()[,ListColumnMarker]
        }
     if(dim(tmpdf)[1]==1){
      tmpcolumn <-log(tmpdf+1)
      }else{
    tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
    }
    #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
    dinput<-data2()[,-c(1)]
    dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1)) 
    dinput
   })
# ####### Storage the perfomance values of whole cohort(cutoff,SE,SP,AUC,ACC,ErroRate,TP,FP,TN,FN,PPV,NPV) with ROCR package
# SelectedPerfObj<-eventReactive(input$variables2, {
#   # function to create the variables "log(Markerx+1)" for each marker of the combination
#          StringComposition <- function(x) paste("log(",x,"+1)",sep="")
#       matchNames=cbind(rownames(nTable1()),nTable1()[,1])
#          pos=which(matchNames[,2]==input$variables2)
#          ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2
#       #ListColumnMarker=which(as.numeric(str_detect(input$variables2, pattern = colnames(data2())))==1)
#     #    if(length(ListColumnMarker)==1){
#     #           tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
#     #     }else{
#     #            tmpdf <-data2()[,ListColumnMarker]
#     #     }
#     #  if(dim(tmpdf)[1]==1){
#     #   tmpcolumn <-log(tmpdf+1)
#     #   }else{
#     # tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))

#     # }

#     vars<- str_split(matchNames[pos,1],"-")
#     fla <- as.formula(paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+"))))
#     #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
#     dinput<-data2()
#     dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1)) 
#     glm.combo<-glm(fla,data=dinput, family="binomial")
#     pred.combo<-prediction(glm.combo$fitted.values,dinput$Class) 
#     roc.perf = performance(pred.combo, measure = "tpr", x.measure = "fpr")
#     #extract se and spe at the optimal cutoff
#     opthr<-opt.cut(roc.perf,pred.combo)
#     #determine the position in the slot of the optimal cutoff
#     posopthr<-which(opthr[3,1]==pred.combo@cutoffs[[1]])

#     #retrieve the AUC value(single value in the slot)
#     perf.auc <- performance(pred.combo, "auc")
#     AUC <- round(as.numeric(perf.auc@y.values),3)
#     #retrieve the ACC values (all values in the slot)
#     perf.acc <- performance(pred.combo, "acc")
#     ACC <- round(as.numeric(perf.acc@y.values[[1]][posopthr]),3)
#     #retrieve the ERR values (all values in the slot)
#     perf.err <- performance(pred.combo, "err")
#     ERR <- round(as.numeric(perf.err@y.values[[1]][posopthr]),3)
#     #retrieve TP,FP,TN,FN
#     TP <- as.numeric(pred.combo@tp[[1]][posopthr])
#     FP <- as.numeric(pred.combo@fp[[1]][posopthr])
#     TN <- as.numeric(pred.combo@tn[[1]][posopthr])
#     FN <- as.numeric(pred.combo@fn[[1]][posopthr])
#     #retrieve PPV and NPV
#     perf.ppv <- performance(pred.combo, "ppv")
#     PPV<- round(as.numeric(perf.ppv@y.values[[1]][posopthr]),3)
#     perf.npv <- performance(pred.combo, "npv")
#     NPV<- round(as.numeric(perf.npv@y.values[[1]][posopthr]),3)
#     perfwhole <- cbind(round(t(opthr[3:1]),3),AUC,ACC,ERR,TP,FP,TN,FN,PPV,NPV) #flip the opthr array
#     colnames(perfwhole)<-c("CutOff","SE","SP","AUC","ACC","ERR","TP","FP","TN","FN","PPV","NPV")
#     perfwhole
# })

####### Storage the perfomance values of whole cohort(cutoff,SE,SP,AUC,ACC,ErroRate,TP,FP,TN,FN,PPV,NPV) with pROC package
SelectedPerfObj<-eventReactive(input$variables2, {
               # function to create the variables "log(Markerx+1)" for each marker of the combination
         StringComposition <- function(x) paste("log(",x,"+1)",sep="")
    #input$rocButton
         matchNames=cbind(rownames(nTable1()),nTable1()[,1])
         pos=which(matchNames[,2]==input$variables2)
         ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2
          

    #    if(length(ListColumnMarker)==1){
    #           tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
    #     }else{
    #            tmpdf <-data2()[,ListColumnMarker]
    #     }
    #  if(dim(tmpdf)[1]==1){
    #   tmpcolumn <-log(tmpdf+1)
    #   }else{
    # tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
    # }
     #vars<- str_split(matchNames[pos,1],"-")
     if(str_count(matchNames[pos,1],"M")==1){ #single marker in the combination
             #get the dataframe
             dinput<-data2()[,-c(1)]
              #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
              dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))

                         
                glm.single<-glm(Class~log(dinput[,ListColumnMarker-1]+1),data=dinput, family="binomial") 
                 #rocobj<-roc(dinput$Class,dinput[,ListColumnMarker-1],levels=c("0","1")) w/o logistic regression and probabilities in ptedictions range from 0 to max value of selected marker
                 rocobj<-roc(dinput$Class,glm.single$fitted.values,levels=c("0","1"))
                 optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
            


              #  glm.single<-glm(Class~dinput[,ListColumnMarker-1],data=dinput, family="binomial")  
              # #rocobj<-roc(dinput$Class,dinput[,ListColumnMarker-1],levels=c("0","1")) w/o logistic regression and probabilities in ptedictions range from 0 to max value of selected marker
              # rocobj<-roc(dinput$Class,glm.single$fitted.values,levels=c("0","1"))
             
              # optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

    }else{  # panel marker
        vars<- str_split(matchNames[pos,1],"-")
         fla <- as.formula(paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+"))))
          dinput<-data2()[,-c(1)]
        dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
        glm.combo<-glm(fla,data=dinput, family="binomial")  
        rocobj<-roc(dinput$Class,glm.combo$fitted.values,levels=c("0","1"))
        optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
    }
     AUC <- round(rocobj$auc[1],3)
     ACC <- round(optcoordinates[[4]],3)
     ERR <- round((optcoordinates[[8]]+optcoordinates[[7]])/dim(data2())[1],3)    #(FP+FN)/P+N
     TP  <- round(optcoordinates[[6]],3)
     FP  <- round(optcoordinates[[8]],3)
     TN  <- round(optcoordinates[[5]],3)
     FN  <- round(optcoordinates[[7]],3)
     PPV <- round(optcoordinates[[10]],3)
     NPV <- round(optcoordinates[[9]],3)
    perfwhole <- cbind(round(optcoordinates[[1]],3),round(optcoordinates[[3]],3),round(optcoordinates[[2]],3),AUC,ACC,ERR,TP,FP,TN,FN,PPV,NPV)
    colnames(perfwhole)<-c("CutOff","SE","SP","AUC","ACC","ERR","TP","FP","TN","FN","PPV","NPV")
    perfwhole
})



# ####### Storage the perfomance values of 10 fold CV cohort(cutoff,SE,SP,AUC,ACC,ErroRate,TP,FP,TN,FN,PPV,NPV)  with ROCR package
# SelectedCVPerfObj<-eventReactive(input$variables2, {

#     matchNames=cbind(rownames(nTable1()),nTable1()[,1])
#          pos=which(matchNames[,2]==input$variables2)
#          ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  
#      # ListColumnMarker=which(as.numeric(str_detect(input$variables2, pattern = colnames(data())))==1)
#        if(length(ListColumnMarker)==1){
#               tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
#         }else{
#                tmpdf <-data2()[,ListColumnMarker]
#         }
#      if(dim(tmpdf)[1]==1){
#       tmpcolumn <-log(tmpdf+1)
#       }else{
#     tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
#     }
#     #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
#     dinput<-data2()
#     dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1)) 
#     df=dinput[,-c(1)]
#     #function to extract performances
#     mycvAUC <- function (predictions, labels, label.ordering = NULL, folds = NULL) 
#     {     
#         #  clean <- .process_input(predictions = predictions, labels = labels, 
#         # label.ordering = label.ordering, folds = NULL, ids = NULL, 
#         # confidence = NULL)
#         pred <- ROCR::prediction(predictions, labels)
#         perf <- ROCR::performance(pred, "tpr", "fpr")
#         #extract se and spe at the optimal cutoff
#         opthr<-opt.cut(perf,pred)
#         #determine the position in the slot of the optimal cutoff
#         posopthr<-which(opthr[3,1]==pred@cutoffs[[1]])
        
#         #retrieve the ACC values (all values in the slot)
#         perf.acc <- ROCR::performance(pred, "acc", x.measure="cutoff")
#         ACC <- round(as.numeric(perf.acc@y.values[[1]][posopthr]),3)

#         perf.se <- ROCR::performance(pred, "sens", x.measure="cutoff")
#         perf.sp <- ROCR::performance(pred, "spec", x.measure="cutoff")
#          #retrieve the ERR values (all values in the slot)
#         perf.err <- ROCR::performance(pred, "err", x.measure="cutoff")
#         ERR <- round(as.numeric(perf.err@y.values[[1]][posopthr]),3)

#         #retrieve TP,FP,TN,FN
#         TP <- as.numeric(pred@tp[[1]][posopthr])
#         FP <- as.numeric(pred@fp[[1]][posopthr])
#         TN <- as.numeric(pred@tn[[1]][posopthr])
#         FN <- as.numeric(pred@fn[[1]][posopthr])
#         #retrieve PPV and NPV
#         perf.ppv <- performance(pred, "ppv")
#         PPV<- round(as.numeric(perf.ppv@y.values[[1]][posopthr]),3)
#         perf.npv <- performance(pred, "npv")
#         NPV<- round(as.numeric(perf.npv@y.values[[1]][posopthr]),3)
        
#         fold_auc <- as.numeric(ROCR::performance(pred, measure = "auc", 
#         x.measure = "cutoff")@y.values)
#          cvauc <- mean(fold_auc)
#          return(list(fold.Cutoff= round(opthr[3,1],3),fold.SE=round(opthr[1,1],3),fold.SP=round(opthr[2,1],3), fold.AUC = round(fold_auc,3),fold.ACC=ACC,fold.ERR=ERR,fold.TP=TP,fold.FP=FP,
#                 fold.TN=TN,fold.FN=FN,fold.PPV=PPV,fold.NPV=NPV))
#     }


#     # function to calculate 10fold CV ROC curves
#     PredictionScore <- function(data, V=10){
#          # function to create the variables "log(Markerx+1)" for each marker of the combination
#          StringComposition <- function(x) paste("log(",x,"+1)",sep="")
         
#          #Create CV folds (stratify by outcome)
#         cvFolds <- function(Y, V){ 
#         set.seed(1) 
#             Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
#             Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
#             folds <- vector("list", length=V)
#             for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}   
#             return(folds)
#         }
#         #Train/test glm for each fold
#         doFit <- function(v, folds, data){ 
#         set.seed(1)
#              #vars<- str_split(input$variables2,"-")
#              vars<- str_split(matchNames[pos,1],"-")
#              fla <- paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+")))
#              as.formula(fla)
#              fit <- glm(fla, data=data[-folds[[v]],], family=binomial)
#              pred <- predict.glm(fit, newdata=data[folds[[v]],], type="response")
#              return(pred)
#         }
   
        
#         folds <- cvFolds(Y=data$Class, V=V)  #Create folds
#            # doesn't work predictions <- unlist(sapply(seq(V), doFit, folds=folds, data=data))  #CV train/predict
#         predictions <- (lapply(seq(V), doFit, folds=folds, data=data))
#         predictions<-unlist(predictions)
    
#         #predictions<-doFit(1,folds,data)
#         #predictions[unlist(folds)] <- predictions  #Re-order pred values
#         predictions<- predictions[sort(as.numeric(names(predictions)),index.return=T)$ix]
#         #return(predictions)
#         out<- mycvAUC(predictions=predictions, labels=data$Class)
#         return(out)

#      }  

#      perftest<- PredictionScore (data=df,V=10)
    
#     perftest

# }) 

####### Storage the perfomance values of 10 fold CV cohort(cutoff,SE,SP,AUC,ACC,ErroRate,TP,FP,TN,FN,PPV,NPV)
SelectedCVPerfObj<-eventReactive(input$variables2, {
        # function to create the variables "log(Markerx+1)" for each marker of the combination
         StringComposition <- function(x) paste("log(",x,"+1)",sep="")
         matchNames=cbind(rownames(nTable1()),nTable1()[,1])
         pos=which(matchNames[,2]==input$variables2)
         ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2

   
      #get the dataframe
     dinput<-data2()[,-c(1)]
     #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
      dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
      df=dinput
   
    #function to extract performances
    mycvAUC <- function (predictions, labels, label.ordering = NULL, folds = NULL) 
    {     
        #  clean <- .process_input(predictions = predictions, labels = labels, 
        # label.ordering = label.ordering, folds = NULL, ids = NULL, 
        # confidence = NULL)
        #pred <- ROCR::prediction(predictions, labels)
        #perf <- ROCR::performance(pred, "tpr", "fpr")
        perf<-roc(labels,predictions,levels=c("0","1"))
        optcoordinates<-coords(perf, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
        AUC <- round(perf$auc[1],3)
        ACC <- round(optcoordinates[[4]],3)
        ERR <- round((optcoordinates[[8]]+optcoordinates[[7]])/dim(data2())[1],3)    #(FP+FN)/P+N
        TP  <- round(optcoordinates[[6]],3)
        FP  <- round(optcoordinates[[8]],3)
        TN  <- round(optcoordinates[[5]],3)
        FN  <- round(optcoordinates[[7]],3)
        PPV <- round(optcoordinates[[10]],3)
       NPV <- round(optcoordinates[[9]],3)
       
        # #extract se and spe at the optimal cutoff
        # opthr<-opt.cut(perf,pred)
        # #determine the position in the slot of the optimal cutoff
        # posopthr<-which(opthr[3,1]==pred@cutoffs[[1]])
        
        # #retrieve the ACC values (all values in the slot)
        # perf.acc <- ROCR::performance(pred, "acc", x.measure="cutoff")
        # ACC <- round(as.numeric(perf.acc@y.values[[1]][posopthr]),3)

        # perf.se <- ROCR::performance(pred, "sens", x.measure="cutoff")
        # perf.sp <- ROCR::performance(pred, "spec", x.measure="cutoff")
        #  #retrieve the ERR values (all values in the slot)
        # perf.err <- ROCR::performance(pred, "err", x.measure="cutoff")
        # ERR <- round(as.numeric(perf.err@y.values[[1]][posopthr]),3)

        # #retrieve TP,FP,TN,FN
        # TP <- as.numeric(pred@tp[[1]][posopthr])
        # FP <- as.numeric(pred@fp[[1]][posopthr])
        # TN <- as.numeric(pred@tn[[1]][posopthr])
        # FN <- as.numeric(pred@fn[[1]][posopthr])
        # #retrieve PPV and NPV
        # perf.ppv <- performance(pred, "ppv")
        # PPV<- round(as.numeric(perf.ppv@y.values[[1]][posopthr]),3)
        # perf.npv <- performance(pred, "npv")
        # NPV<- round(as.numeric(perf.npv@y.values[[1]][posopthr]),3)
        
        # fold_auc <- as.numeric(ROCR::performance(pred, measure = "auc", 
        # x.measure = "cutoff")@y.values)
        #  cvauc <- mean(fold_auc)
        return(list(fold.Cutoff= round(optcoordinates[[1]],3),fold.SE=round(optcoordinates[[3]],3),fold.SP=round(optcoordinates[[2]],3), fold.AUC = AUC,fold.ACC=ACC,fold.ERR=ERR,fold.TP=TP,fold.FP=FP,
                fold.TN=TN,fold.FN=FN,fold.PPV=PPV,fold.NPV=NPV))
    }


    # function to calculate 10fold CV ROC curves
    PredictionScore <- function(data, V=10){
         # function to create the variables "log(Markerx+1)" for each marker of the combination
         StringComposition <- function(x) paste("log(",x,"+1)",sep="")
         
         #Create CV folds (stratify by outcome)
        cvFolds <- function(Y, V){ 
        set.seed(1) 
            Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
            Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
            folds <- vector("list", length=V)
            for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}   
            return(folds)
        }
        #Train/test glm for each fold
        doFit <- function(v, folds, data){ 
        set.seed(1)
             #vars<- str_split(input$variables2,"-")
             vars<- str_split(matchNames[pos,1],"-")
             fla <- paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+")))
             as.formula(fla)
             fit <- glm(fla, data=data[-folds[[v]],], family=binomial)
             pred <- predict.glm(fit, newdata=data[folds[[v]],], type="response")
             return(pred)
        }
   
        
        folds <- cvFolds(Y=data$Class, V=V)  #Create folds
           # doesn't work predictions <- unlist(sapply(seq(V), doFit, folds=folds, data=data))  #CV train/predict
        predictions <- (lapply(seq(V), doFit, folds=folds, data=data))
        predictions<-unlist(predictions)
    
        #predictions<-doFit(1,folds,data)
        #predictions[unlist(folds)] <- predictions  #Re-order pred values
        predictions<- predictions[sort(as.numeric(names(predictions)),index.return=T)$ix]
        #return(predictions)
        out<- mycvAUC(predictions=predictions, labels=data$Class)
        return(out)

     }  

     perftest<- PredictionScore (data=df,V=10)
    
    perftest

}) 



####### Display the ROC curve
   ROCobj<-eventReactive(input$rocButton, {
         ListColumnMarker=which(as.numeric(str_detect(input$variables2, pattern = colnames(data2())))==1)
       if(length(ListColumnMarker)==1){
              tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
        }else{
               tmpdf <-data2()[,ListColumnMarker]
        }
     if(dim(tmpdf)[1]==1){
      tmpcolumn <-log(tmpdf+1)
      }else{
    tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
    }
    #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
    dinput<-data2()[,-c(1)]
    dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))

      glm.ag1<-glm(Class~tmpcolumn,data=dinput, family="binomial")          #glm(Class~tmpcolumn,data=data(), family="binomial")
      #pROC package
     #rocobj3.glm<-roc(data()$Class,glm.ag1$fitted.values,levels=c("A","B"))
      #plot(rocobj3.glm, print.auc=TRUE, print.thres.best.method="youden",axes=T,col="green",specificities=seq(0, 100, 5))
      #ROCR package
      #!!!Be careful, the class label must be NUMERIC and not letteral!!!!!
      
     pred<-prediction(glm.ag1$fitted.values,dinput$Class)                #prediction(glm.ag1$fitted.values,data()$Class)
     # SE and SP
     roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
     #AUC
     perf.auc <- performance(pred, "auc")
     #extract SE,SP,cutoff and AUC
     perf.auc.areas <- slot(perf.auc, "y.values")
     valAUC <- c("AUC", round(perf.auc.areas[[1]], 3))
     valAUC <- sprintf("%-9s", valAUC)
     
     opt.cut = function(perf, pred){
     cut.ind = mapply(FUN=function(x, y, p){
         d = (x - 0)^2 + (y-1)^2
         ind = which(d == min(d))
         c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
           cutoff = p[[ind]])
     }, perf@x.values, perf@y.values, pred@cutoffs)
     }
      valperf<- round(opt.cut(roc.perf, pred),3)
      valperfSE <-c("SE",valperf[1,1])
      valperfSE <- sprintf("%-9s", valperfSE)
      valperfSP <-c("SP",valperf[2,1])
      valperfSP <- sprintf("%-9s", valperfSP)
      valperfcutoff <-c("@cutoff",valperf[3,1])
      valperfcutoff <- sprintf("%-9s", valperfcutoff)
     
     #par(bg="lightgray", mai=c(1.2,1.5,1,1))
     par(mar=c(5.1,12,4.1,12))

     plot(roc.perf,xlab="1-Specificity", ylab="Sensitivity",axes=F,colorize=TRUE,box.lty=7, box.lwd=5,
  box.col='black' ,lwd=5,main=input$variables2, colorkey.relwidth=0.5, xaxis.cex.axis=1.5,
  xaxis.col='darkblue', xaxis.col.axis="darkblue", yaxis.col='darkblue', yaxis.cex.axis=1.5,
 , yaxis.las=1, xaxis.lwd=2, yaxis.lwd=2,yaxis.col.axis="darkblue", cex.lab=2,cex.main=2)  #yaxis.at=c(0,0.5,0.8,0.85,0.9,1)
  box(which = "plot", lty = "solid", col="red")
     grid.at <- seq(0, 1, 0.1)
     abline(v=grid.at, lty=3, col="lightgrey")
     abline(h=grid.at, lty=3, col="lightgrey")
     abline(a=0, b= 1,lty=3)
     abline(v=1-valperf[2,1],lty=4,lwd=2,col="cornflowerblue")
     abline(h=valperf[1,1],lty=4,lwd=2,col="cornflowerblue")
     roclegend <-paste(valAUC, valperfSE,valperfSP,valperfcutoff, sep="")       #paste(feats, vals, sep="");
     legend("bottomright",legend = roclegend,inset = .05,title=input$variables2,bg='aliceblue')
  })
         
     output$ROCcurve <-renderPlot ({
      (ROCobj())
     })

################################################################
# ####### Display the ROC curve 1  (with ROCR package) # #######
################################################################
#    ROCobj1<-eventReactive(input$variables2, {
#      # function to create the variables "log(Markerx+1)" for each marker of the combination
#          StringComposition <- function(x) paste("log(",x,"+1)",sep="")
#     #input$rocButton
#          matchNames=cbind(rownames(nTable1()),nTable1()[,1])
#          pos=which(matchNames[,2]==input$variables2)
#          ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2
          

#     #    if(length(ListColumnMarker)==1){
#     #           tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
#     #     }else{
#     #            tmpdf <-data2()[,ListColumnMarker]
#     #     }
#     #  if(dim(tmpdf)[1]==1){
#     #   tmpcolumn <-log(tmpdf+1)
#     #   }else{
#     # tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
#     # }
#      vars<- str_split(matchNames[pos,1],"-")
#     fla <- as.formula(paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+"))))
#     #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
#     dinput<-data2()[,-c(1)]
#     dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))

#       glm.ag1<-glm(fla,data=dinput, family="binomial")          #glm(Class~tmpcolumn,data=data(), family="binomial")
#       #pROC package
#      #rocobj3.glm<-roc(data()$Class,glm.ag1$fitted.values,levels=c("A","B"))
#       #plot(rocobj3.glm, print.auc=TRUE, print.thres.best.method="youden",axes=T,col="green",specificities=seq(0, 100, 5))
#       #ROCR package
#       #!!!Be careful, the class label must be NUMERIC and not letteral!!!!!
      
#      pred<-prediction(glm.ag1$fitted.values,dinput$Class)                #prediction(glm.ag1$fitted.values,data()$Class)
#      # SE and SP
#      roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#      #AUC
#      perf.auc <- performance(pred, "auc")
#      #extract SE,SP,cutoff and AUC
#      perf.auc.areas <- slot(perf.auc, "y.values")
#      valAUC <- c("AUC", round(perf.auc.areas[[1]], 3))
#      valAUC <- sprintf("%-9s", valAUC)
     
#      opt.cut = function(perf, pred){
#      cut.ind = mapply(FUN=function(x, y, p){
#          d = (x - 0)^2 + (y-1)^2
#          ind = which(d == min(d))
#          c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
#            cutoff = p[[ind]])
#      }, perf@x.values, perf@y.values, pred@cutoffs)
#      }
#       valperf<- round(opt.cut(roc.perf, pred),3)
#       valperfSE <-c("SE",valperf[1,1])
#       valperfSE <- sprintf("%-9s", valperfSE)
#       valperfSP <-c("SP",valperf[2,1])
#       valperfSP <- sprintf("%-9s", valperfSP)
#       valperfcutoff <-c("Opt Cutoff @ ",valperf[3,1])
#       valperfcutoff <- sprintf("%-9s", valperfcutoff)
# #       drill_function <- "#! function() {
# #     var drilldown = this.drilldown;
# #     function setChart(name, categories, data, color) {
# #     chart.xAxis[0].setCategories(categories, false);
# #     chart.series[0].remove(false);
# #     chart.addSeries({
# #       name: name,
# #       data: data,
# #       color: color || 'black'
# #     }, false);
# #     chart.redraw();
# #     };
# #     if (drilldown) { // drill down
# #         setChart(drilldown.name, drilldown.categories, drilldown.data, drilldown.color);
# #     } else { // restore
# #         setChart(name, categories, data);
# #     }
# # } !#"




# #            df <-data.frame(SE=roc.perf@y.values[[1]],SP=roc.perf@x.values[[1]])  #,Class=Gender
# #                h1 <- hPlot(
# #                         y = 'SE', 
# #                         x = 'SP', 
# #                         type = 'line', 
# #                         data = df)
# #                h1$title(text=input$variables2,style=list(fontSize=32,zIndex=3,color="#ffe34c",fontWeight="bold",textShadow="0 0 6px contrast, 0 0 3px contrast"))  #,useHTML=TRUE+'background-color'="black",border='2px solid black'
# #                h1$tooltip(crosshairs=c(TRUE,TRUE),shared=TRUE)
# #                h1$set(width = 700,height=600) 
# #                h1$exporting(enabled=TRUE)
# #                h1$plotOptions(series = list(shadow = TRUE,lineWidth=3,color="#7dd4b6"))
# #                # h1$plotOptions(series=list(cursor="pointer",point=list(events=list(click=' function (e) {
# #                #                  hs.htmlExpand(null, {
# #                #                      pageOrigin: {
# #                #                          x: e.pageX || e.clientX,
# #                #                          y: e.pageY || e.clientY
# #                #                      },
# #                #                      headingText: this.series.name,
                                   
# #                #                      width: 200
# #                #                  });
# #                #              }'))))
# # #h1$plotOptions(line= list(cursor = 'pointer', point = list(events = list(click = drill_function))))
# #    #questo funziona h1$tooltip(formatter = "#! function() { return '<b>'+ this.series.name +', intervals '+ this.point.category +'</b><br/>' + 'Frequency: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);} !#")
# #     #h1$tooltip(shared=TRUE,useHTML=TRUE,headerFormat='<small>{point.key}</small><table>',  pointFormat='<tr><td style="color: {series.color}">{series.name}: </td>' +
# #                 #'<td style="text-align: right"><b>{point.y} EUR</b></td></tr>',footerFormat= '</table>',valueDecimals=2)
# #                h1$chart(zoomType = "xy",plotBorderWidth=1,options3d=list(enabled=TRUE),lineWidth=3,plotShadow=TRUE)
# #                h1$xAxis(min=-0.03,max=1.1,title = list(text = "1-Specificity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(1-valperf[2,1]),label=list(align='middle',rotation=0,y=15,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="optimal cutoff"),zIndex=3)))
# #                h1$yAxis(min=-0,max=1.1,title = list(text = "Sensitivity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(valperf[1,1]),label=list(align='middle',rotation=0,y=15,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="optimal cutoff"))))
# #      #abline(v=1-valperf[2,1],lty=4,lwd=2,col="cornflowerblue")
# #      #abline(h=valperf[1,1],lty=4,lwd=2,col="cornflowerblue")
# #      #par(bg="lightgray", mai=c(1.2,1.5,1,1))


#       h1 <-rCharts::Highcharts$new()
#       #create  list to storage values of ROC curves
#       plot_data_fitted<-vector("list",length(roc.perf@x.values[[1]]))
#       #initialize the list plot_data_fitted
#       for (i in 1:length(roc.perf@x.values[[1]])){
#          plot_data_fitted[[i]]<-list(x=roc.perf@x.values[[1]][i],y=roc.perf@y.values[[1]][i],z=perf.auc@y.values[[1]][i])
#       }
#       #create diagonal line
#       plot_diagonal = list(
#           list(x = -1, y = -1),
#       list(x = 2, y = 2)
#      )
#       #create optimal cutoff point
#     opt_cutoff=list(
#             list(x=as.numeric(1-valperf[2,1]),y=as.numeric(valperf[1,1]))
#    )
 

#   h1$series(name = input$variables2,

#             type = "line",
#             data = plot_data_fitted)
#    h1$series(name=valperfcutoff,
#          type="scatter",
#          data=opt_cutoff,
#          marker=list(symbol='url(https://www.highcharts.com/samples/graphics/sun.png)')
#          #tooltip=list(formatter="#! function() { return '<b>'+ this.series.name +'<br/>' + 'optimal cutoff: ' + Highcharts.numberFormat((this.point.category), 2) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 2) +'<br/>'+ 'AUC: '+ Highcharts.numberFormat((1-(this.point.x)), 2);} !#")
#          )


#  h1$series(name = "diagonal line",
#             type = "line",
#             data = plot_diagonal,dashStyle="ShortDot",lineWidth=1,color="black")
#    h1$title(text=input$variables2,style=list(fontSize=32,zIndex=3,color="#ffe34c",fontWeight="bold",textShadow="0 0 6px contrast, 0 0 3px contrast"))  #,useHTML=TRUE+'background-color'="black",border='2px solid black'
#                 h1$plotOptions(line= list(shadow = TRUE,lineWidth=3,color="#7dd4b6"))
#                h1$tooltip(crosshairs=c(TRUE,TRUE),shared=TRUE)
#                h1$set(width = 700,height=700) 
#                h1$exporting(enabled=TRUE)
#                h1$chart(zoomType = "xy",options3d=list(enabled=TRUE),lineWidth=3,plotShadow=TRUE) # to plot the border plotBorderWidth=1,
#                 h1$set(width = 500,height=500,dom="ROCobj1") 
#                h1$xAxis(min=-0.03,max=1,title = list(text = "1-Specificity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(1-valperf[2,1]),zIndex=3)))
#                h1$yAxis(min=-0,max=1,title = list(text = "Sensitivity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(valperf[1,1]))))
#                #if I want to add the optimal cutoff label
#                # h1$xAxis(min=-0.03,max=1,title = list(text = "1-Specificity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(1-valperf[2,1]),label=list(align='left',rotation=0,x=220,y=as.numeric(valperf[1,1])+50,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="optimal cutoff"),zIndex=3)))
#                # h1$yAxis(min=-0,max=1,title = list(text = "Sensitivity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(valperf[1,1]),label=list(align='middle',rotation=0,x=as.numeric(1-valperf[2,1])+100,y=280,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="optimal cutoff"))))

#               h1$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'SE: ' + Highcharts.numberFormat((this.point.y), 2) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 2);} !#")
  
#       return(h1)
#   })


#   output$ROCcurve1 <-renderChart2 ({
#       #input$rocButton
#       (ROCobj1())
#      })

####### Display the ROC curve 2 (with pROC package)

   ROCobj2<-eventReactive(input$variables2, {
     # function to create the variables "log(Markerx+1)" for each marker of the combination
         StringComposition <- function(x) paste("log(",x,"+1)",sep="")
    #input$rocButton
         matchNames=cbind(rownames(nTable1()),nTable1()[,1])
         pos=which(matchNames[,2]==input$variables2)
         ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2
          

    #    if(length(ListColumnMarker)==1){
    #           tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
    #     }else{
    #            tmpdf <-data2()[,ListColumnMarker]
    #     }
    #  if(dim(tmpdf)[1]==1){
    #   tmpcolumn <-log(tmpdf+1)
    #   }else{
    # tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
    # }
     #vars<- str_split(matchNames[pos,1],"-")
     if(str_count(matchNames[pos,1],"M")==1){ #single marker in the combination
             #get the dataframe
             dinput<-data2()[,-c(1)]
              #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
              dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
              
             
                glm.single<-glm(Class~log(dinput[,ListColumnMarker-1]+1),data=dinput, family="binomial") 
                 #rocobj<-roc(dinput$Class,dinput[,ListColumnMarker-1],levels=c("0","1")) w/o logistic regression and probabilities in ptedictions range from 0 to max value of selected marker
                 rocobj<-roc(dinput$Class,glm.single$fitted.values,levels=c("0","1"))
                 optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
             
    }else{  # panel marker
        vars<- str_split(matchNames[pos,1],"-")
         fla <- as.formula(paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+"))))
          dinput<-data2()[,-c(1)]
        dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
        glm.combo<-glm(fla,data=dinput, family="binomial")  
        rocobj<-roc(dinput$Class,glm.combo$fitted.values,levels=c("0","1"))
        optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

    }



      valperfcutoff <-paste0("Opt Cutoff @ ", round(optcoordinates[[1]],3))
      valperfcutoff <- sprintf("%-3s", valperfcutoff)



      h1 <-rCharts::Highcharts$new()
      #create  list to storage values of ROC curves
      plot_data_fitted<-vector("list",length(rocobj$sensitivities))
      #initialize the list plot_data_fitted
      for (i in 1:length(rocobj$sensitivities)){
         plot_data_fitted[[i]]<-list(x=(1-rocobj$specificities[i]),y=rocobj$sensitivities[i],z=rocobj$auc[[1]])
      }
      #create diagonal line
      plot_diagonal = list(
          list(x = -1, y = -1),
      list(x = 2, y = 2)
     )

    #create optimal cutoff point
    opt_cutoff=list(
            list(x=as.numeric(1-optcoordinates[[2]]),y=as.numeric(optcoordinates[[3]]))
   )


 

  h1$series(name = input$variables2,

            type = "line",
            data = plot_data_fitted)
   h1$series(name=valperfcutoff,
         type="scatter",
         data=opt_cutoff,
         marker=list(symbol='url(https://www.highcharts.com/samples/graphics/sun.png)')
         #tooltip=list(formatter="#! function() { return '<b>'+ this.series.name +'<br/>' + 'optimal cutoff: ' + Highcharts.numberFormat((this.point.category), 2) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 2) +'<br/>'+ 'AUC: '+ Highcharts.numberFormat((1-(this.point.x)), 2);} !#")
         )


 h1$series(name = "diagonal line",
            type = "line",
            data = plot_diagonal,dashStyle="ShortDot",lineWidth=1,color="black")
   h1$title(text=input$variables2,style=list(fontSize=32,zIndex=3,color="#ffe34c",fontWeight="bold",textShadow="0 0 6px contrast, 0 0 3px contrast"))  #,useHTML=TRUE+'background-color'="black",border='2px solid black'
                h1$plotOptions(line= list(shadow = TRUE,lineWidth=3,color="#7dd4b6"))
               h1$tooltip(crosshairs=c(TRUE,TRUE),shared=TRUE)
               h1$set(width = 700,height=700) 
               h1$exporting(enabled=TRUE)
               h1$chart(zoomType = "xy",options3d=list(enabled=TRUE),lineWidth=3,plotShadow=TRUE) # to plot the border plotBorderWidth=1,
                h1$set(width = 300,height=500,dom="ROCobj2") 
               h1$xAxis(min=-0.03,max=1,title = list(text = "1-Specificity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(1-optcoordinates[[2]]),zIndex=3)))
               h1$yAxis(min=-0,max=1,title = list(text = "Sensitivity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(optcoordinates[[3]]))))
               #if I want to add the optimal cutoff label
               # h1$xAxis(min=-0.03,max=1,title = list(text = "1-Specificity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(1-valperf[2,1]),label=list(align='left',rotation=0,x=220,y=as.numeric(valperf[1,1])+50,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="optimal cutoff"),zIndex=3)))
               # h1$yAxis(min=-0,max=1,title = list(text = "Sensitivity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(valperf[1,1]),label=list(align='middle',rotation=0,x=as.numeric(1-valperf[2,1])+100,y=280,style=list(fontStyle='italic',fontWeight='bold',color='#BE51BF'),text="optimal cutoff"))))

              h1$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'SE: ' + Highcharts.numberFormat((this.point.y), 3) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 3);} !#")
            
      return(h1)
  })


  output$ROCcurve2 <-renderChart2 ({
      #input$rocButton
      (ROCobj2())
     })





  ####### Display as datatable (ROCtable1) the performances of the whole cohort 
         rocTable<-eventReactive(input$variables2, {
              # to retrieve the Combiantions name/column
              matchNames=cbind(rownames(nTable1()),nTable1()[,1])
              pos=which(matchNames[,2]==input$variables2)

              df=SelectedPerfObj()
              #perf=data.frame(matrix(NA, nrow=1, ncol=4)) 
              perf=data.frame(matrix(NA, nrow=1, ncol=5)) 
              colnames(perf)<-c("Symbol","AUC","SE %","SP %","Opt Cutoff")
              rownames(perf)<-c(matchNames[pos,1])
              perf[1,1:5]<-cbind(input$variables2,df[1,4],df[1,2],df[1,3],df[1,1])
              perf
         })

   output$ROCtable1 <- DT::renderDataTable({
           rocTable()
         },colnames = c('Combinations'=1),extensions = c('TableTools','Responsive'),options = list(paging = FALSE, searching = FALSE, initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))
         )

    # output$provathr_2 <- renderText({ 
    #   #paste("You have selected", input$combithr_2,input$signalthr_2)
    #   #paste("This is the",SE_disease())
    #    # paste("This is the",rownames(SPE_control()))
    #      s1=((nTable1()[,1]))
    #   print((s1))
    # })





####### Display the class prediction
    Predobj<-eventReactive(input$rocButton, {
           ListColumnMarker=which(as.numeric(str_detect(input$variables2, pattern = colnames(data2())))==1)
       if(length(ListColumnMarker)==1){
              tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
        }else{
               tmpdf <-data2()[,ListColumnMarker]
        }
     if(dim(tmpdf)[1]==1){
      tmpcolumn <-log(tmpdf+1)
      }else{
    tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
    }
    #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
    dinput<-data2()
    dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))

      glm.ag1<-glm(Class~tmpcolumn,data=dinput, family="binomial")
      pred<-prediction(glm.ag1$fitted.values,dinput$Class)
      roc.perf = performance(pred, measure = "tpr", x.measure = "fpr") 
      
         #function to calculate the optimal cutoff (source: da recuperare presente nel web !!!ACKNOWLE!!!!!)
           opt.cut = function(perf, pred){
                      cut.ind = mapply(FUN=function(x, y, p){
                      d = (x - 0)^2 + (y-1)^2
                      ind = which(d == min(d))
                      c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
                       cutoff = p[[ind]])
                      }, perf@x.values, perf@y.values, pred@cutoffs)
          }
         valperf<- round(opt.cut(roc.perf, pred),3)     # third row is the optimal cutoff
      
     #create the dataframe with Class/Predictions/Prediction Type
     predictiondf<-data.frame(class=dinput$Class,pred=NA)
     predictiondf$pred<-pred@predictions[[1]]
     #function to calculate the sample prediciton type (source: http://www.r-bloggers.com/illustrated-guide-to-roc-and-auc/ !!!ACKNOWLE!!!!!)
     plot_pred_type_distribution=function(df, threshold) {
                       v <- rep(NA, nrow(df))
                       v <- ifelse(df$pred >= threshold & df$class == 1, "TP", v)
                       v <- ifelse(df$pred >= threshold & df$class == 0, "FP", v)
                       v <- ifelse(df$pred < threshold & df$class == 1, "FN", v)
                       v <- ifelse(df$pred < threshold & df$class == 0, "TN", v)
                       df$pred_type <- v
                       ggplot(data=df, aes(x=class, y=pred)) +
                       geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) +
                       geom_jitter(aes(color=pred_type), alpha=0.6) +
                       geom_hline(yintercept=threshold, color="red", alpha=0.6) +
                       scale_color_discrete(name = "type") +
                       labs(title=sprintf("Cutoff at %.3f", threshold))
           }
      plot_pred_type_distribution(predictiondf,valperf[3,1])
    
    
     
    })

    output$ClassPred <-renderPlot ({
      (Predobj())
     })

# ####### Display the class prediction 1  with ROCR package
#     Predobj1<-eventReactive(input$variables2, {
#           matchNames=cbind(rownames(nTable1()),nTable1()[,1])
#          pos=which(matchNames[,2]==input$variables2)
#          ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2
#            #ListColumnMarker=which(as.numeric(str_detect(input$variables2, pattern = colnames(data())))==1)
#        if(length(ListColumnMarker)==1){
#               tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
#         }else{
#                tmpdf <-data2()[,ListColumnMarker]
#         }
#      if(dim(tmpdf)[1]==1){
#       tmpcolumn <-log(tmpdf+1)
#       }else{
#     tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
#     }
#     #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
#     dinput<-data2()
#     dinput$Class<-factor(c(rep("disease",nrow(classA2())),rep("control",nrow(classB2()))),levels=c("control","disease"))

#       glm.ag1<-glm(Class~tmpcolumn,data=dinput, family="binomial")
#       pred<-prediction(glm.ag1$fitted.values,dinput$Class)
#       roc.perf = performance(pred, measure = "tpr", x.measure = "fpr") 
      
#          #function to calculate the optimal cutoff (source: da recuperare presente nel web !!!ACKNOWLE!!!!!)
#            opt.cut = function(perf, pred){
#                       cut.ind = mapply(FUN=function(x, y, p){
#                       d = (x - 0)^2 + (y-1)^2
#                       ind = which(d == min(d))
#                       c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
#                        cutoff = p[[ind]])
#                       }, perf@x.values, perf@y.values, pred@cutoffs)
#           }
#          valperf<- round(opt.cut(roc.perf, pred),3)     # third row is the optimal cutoff
      
#      #create the dataframe with Class/Predictions/Prediction Type
#      predictiondf<-data.frame(Class=dinput$Class,Prediction_Probabilities=NA)
#      predictiondf$Prediction_Probabilities<-pred@predictions[[1]]
#      #function to calculate the sample prediciton type (source: http://www.r-bloggers.com/illustrated-guide-to-roc-and-auc/ !!!ACKNOWLE!!!!!)
#      plot_pred_type_distribution=function(df, threshold) {
#                        v <- rep(NA, nrow(df))
#                        v <- ifelse(df$Prediction_Probabilities >= threshold & df$Class == "disease", "TP", v)
#                        v <- ifelse(df$Prediction_Probabilities >= threshold & df$Class == "control", "FP", v)
#                        v <- ifelse(df$Prediction_Probabilities < threshold & df$Class == "disease", "FN", v)
#                        v <- ifelse(df$Prediction_Probabilities < threshold & df$Class == "control", "TN", v)
#                        df$pred_type <- v
#                        ggplot(data=df, aes(x=Class, y=Prediction_Probabilities)) +
#                        geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) +
#                        geom_jitter(aes(color=pred_type), alpha=0.6) +
#                        geom_hline(yintercept=threshold, color="red", alpha=0.6) +
#                        scale_color_discrete(name = "type") +
#                        labs(title=sprintf("Cutoff at %.3f", threshold))
#            }
#       plot_pred_type_distribution(predictiondf,valperf[3,1])  
#     })

####### Display the class prediction 1
    Predobj1<-eventReactive(input$variables2, {
      
         # function to create the variables "log(Markerx+1)" for each marker of the combination
         StringComposition <- function(x) paste("log(",x,"+1)",sep="")
    #input$rocButton
         matchNames=cbind(rownames(nTable1()),nTable1()[,1])
         pos=which(matchNames[,2]==input$variables2)
         ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2
          

    #    if(length(ListColumnMarker)==1){
    #           tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
    #     }else{
    #            tmpdf <-data2()[,ListColumnMarker]
    #     }
    #  if(dim(tmpdf)[1]==1){
    #   tmpcolumn <-log(tmpdf+1)
    #   }else{
    # tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
    # }
     #vars<- str_split(matchNames[pos,1],"-")
     if(str_count(matchNames[pos,1],"M")==1){ #single marker in the combination
             #get the dataframe
             dinput<-data2()[,-c(1)]
              #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
              dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
               
                glm.single<-glm(Class~log(dinput[,ListColumnMarker-1]+1),data=dinput, family="binomial") 
                 #rocobj<-roc(dinput$Class,dinput[,ListColumnMarker-1],levels=c("0","1")) w/o logistic regression and probabilities in ptedictions range from 0 to max value of selected marker
                 rocobj<-roc(dinput$Class,glm.single$fitted.values,levels=c("0","1"))
                 optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
            

              # glm.single<-glm(Class~dinput[,ListColumnMarker-1],data=dinput, family="binomial")  
              # #rocobj<-roc(dinput$Class,dinput[,ListColumnMarker-1],levels=c("0","1"))
              # rocobj<-roc(dinput$Class,glm.single$fitted.values,levels=c("0","1"))
              # optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

    }else{  # panel marker
        vars<- str_split(matchNames[pos,1],"-")
         fla <- as.formula(paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+"))))
          dinput<-data2()[,-c(1)]
        dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
        glm.combo<-glm(fla,data=dinput, family="binomial")  
        rocobj<-roc(dinput$Class,glm.combo$fitted.values,levels=c("0","1"))
        optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

    }



      valperfcutoff <-paste0("Opt Cutoff @ ", round(optcoordinates[[1]],3))
      valperfcutoff <- sprintf("%-3s", valperfcutoff)






    #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
    dinput<-data2()
    dinput$Class<-factor(c(rep("disease",nrow(classA2())),rep("control",nrow(classB2()))),levels=c("control","disease"))
     
     #create the dataframe with Class/Predictions/Prediction Type
     predictiondf<-data.frame(Class=dinput$Class,Prediction_Probabilities=NA)
     predictiondf$Prediction_Probabilities<-c(rocobj$cases,rocobj$controls)
     #function to calculate the sample prediciton type (source: http://www.r-bloggers.com/illustrated-guide-to-roc-and-auc/ !!!ACKNOWLE!!!!!)
     plot_pred_type_distribution=function(df, threshold) {
                       v <- rep(NA, nrow(df))
                       v <- ifelse(df$Prediction_Probabilities >= threshold & df$Class == "disease", "TP", v)
                       v <- ifelse(df$Prediction_Probabilities >= threshold & df$Class == "control", "FP", v)
                       v <- ifelse(df$Prediction_Probabilities < threshold & df$Class == "disease", "FN", v)
                       v <- ifelse(df$Prediction_Probabilities < threshold & df$Class == "control", "TN", v)
                       df$pred_type <- v
                       f <- factor(c("FN","FP","TN","TP"))   #impose the colour labels
                       ggplot(data=df, aes(x=Class, y=Prediction_Probabilities,colour=pred_type)) +
                       geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) +
                       geom_jitter() + 
                       #geom_point() +            
                       scale_colour_discrete(drop=TRUE, limits = levels(f)) +
                       geom_hline(yintercept=threshold, color="red", alpha=0.6) +
                       #scale_color_discrete(name = "type") +
                       labs(title=sprintf("Cutoff at %.3f", threshold))
           }
      plot_pred_type_distribution(predictiondf,optcoordinates[[1]])  
    })



    output$ClassPred1<-renderPlot ({
      (Predobj1())
     })

   Pieobj<-eventReactive(input$variables2, {
      #calculate the real percentages
      ntot=dim(data2())[1]
      nA=dim(classA2())[1]
      nB=dim(classB2())[1]
      percentA=round((nA*100)/ntot,2)
      percentB=round((nB*100)/ntot,2)

     #calculate the prdicted percentages
      #retrieve the TP,FN,TN,FP values
      df=SelectedPerfObj()
      percentTP=round((df[1,7]*100)/ntot,2)
      percentFN=round((df[1,10]*100)/ntot,2)
      percentTN=round((df[1,9]*100)/ntot,2)
      percentFP=round((df[1,8]*100)/ntot,2)



      h1 <-rCharts::Highcharts$new()
       h1 <- Highcharts$new()
     h1$chart(type = "pie",zoomType="xy")
     h1$title(text=input$variables2)  #,style=list(fontSize=32,zIndex=3,color="#ffe34c",fontWeight="bold",textShadow="0 0 6px contrast, 0 0 3px contrast"
      #h1$series(data=list(list(name="AIH",y=24,drilldown=list(name="AIH",categories=c('TP','FN'),data=list(21,3))),list(name="HD",y=76,drilldown=list(name="HD",categories=c('TN','FP'),data=list(50,26)))))
       h1$series(list(list(name="real",data=list(list(name="Class A",y=percentA,color="#E96F8D"),list(name="Class B",y=percentB,color="#6F8DE9")),size="52%",dataLabels=list(formatter="#! function() { return this.point.name;} !#",color="#ffffff",distance=-35,style=list(fontWeight="bold"))),list(name="predicted",data=list(list(name="FN",y=as.numeric(percentFN),color="#FF7276"),list(name="TP",y=as.numeric(percentTP),color="#BF9BDE"),list(name="TN",y=as.numeric(percentTN),color="#2DCCD3"),list(name="FP",y=as.numeric(percentFP),color="#6BA539")),size="80%",innerSize="60%")))
       #h1$drilldown(series=list(list(id="AIH",data=list(c("TP",21),c("FN",3))),list(id="HD",data=list(c("TN",50),c("FP",26)))))
       #h1$series(name="predicted",data=list(list(list(name="TP",y=21,color="#E96F8D"),list(name="FN",y=3,color="#E96F8D")),list(list(name="TN",y=50,color="#6F8DE9"),list(name="FP",y=26,color="#6F8DE9"))),size="80%",innerSize="60%",type="pie")
     
      #h1$colors(c("#E96F8D","#6F8DE9"))
              # h1$series(name=input$profilevariable)
               h1$exporting(enabled=TRUE)
               #h1$chart(zoomType = "xy",plotBorderWidth=1,options3d=list(enabled=TRUE))
                h1$plotOptions(pie = list(center=c("50%","50%")))
                # h1$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'SE: ' + Highcharts.numberFormat((this.point.y), 3) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 3);} !#")
        h1$tooltip(pointFormat = '{series.name}: <b>{point.percentage:.1f}%</b>')
        #Highcharts.numberFormat((this.point.y), 3)
         h1$set(width = 400,height=400)    #to set the plot width
      h1
  
      return(h1)
  })



      output$pie <-renderChart2 ({
      #input$rocButton
      (Pieobj())
     })

# ####### Display the classification as heatmap


#   output$heatmap<-renderChart2 ({
#               #  a<-(c(0,0,10))
#               # b<-(c(0,1,19))
#               # c<-(c(1,0,92))
#               #  d<-(c(1,1,58))
#              # x<-as.list(c(0,0,0,1))
#              # y<-as.list(c(0,1,0,1))
#              # value<-as.list(c(10,19,92,58))
#              # prova=as.list(x=x,y=y,value=value)
#              #  prova=matrix(cbind(a,b,c,d),ncol=3,byrow=TRUE)
#              #  colnames(prova) <- c("x","y","value")

#               dat <- rjson::fromJSON(file="heatmap-data.json")
# dat <- lapply(dat, function(x) {
#   x[sapply(x, is.null)] <- NA
#   unlist(x)
# })
# dat <- matrix(dat$data, ncol=3, byrow=TRUE)
# colnames(dat) <- c("x","y","value")
#                map <- Highcharts$new()
#         map$chart(zoomType = "x", type = 'heatmap')
#         map$credits(text = "Created with rCharts and Highcharts", href = "http://rcharts.io")
#        map$title(text='Sales per employee per weekday')

#     map$series(name = 'Sales per employee',
#          data =toJSON(dat),
#          color = "#cccccc",
#          dataLabels = list(
#            enabled = TRUE,
#            color = 'black',
#            style = list(
#               textShadow = 'none',
#               HcTextStroke = NULL
#            )
#          )
#          )
         


#      map$xAxis(categories = c('Alexander', 'Marie', 'Maximilian', 'Sophia', 'Lukas', 
#       'Maria', 'Leon', 'Anna', 'Tim', 'Laura'))

#     map$yAxis(categories = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
#         title=list(text = ""))
        
#     map$addParams(colorAxis = 
#       list(
#           min = 0,
#           minColor='#FFFFFF',
#           maxColor='#7cb5ec'
#       )
#     )

#     map$legend(align='right',
#          layout='vertical',
#          margin=0,
#          verticalAlign='top',
#          y=25,
#          symbolHeight=320)
           
#     # custom tooltip
#     map$tooltip(formatter = "#! function() { return '<b>' + this.series.xAxis.categories[this.point.x] + '</b> sold <br><b>' +
#                     this.point.value + '</b> items on <br><b>' + this.series.yAxis.categories[this.point.y] + '</b>'; } !#")

        
#     # set width and height of the plot and attach it to the DOM
#     map$addParams(height = 400, width=1000, dom="heatmap")
    
#     # save heatmap as HTML page heatmap.html for debugging
#     #map$save(destfile = 'heatmap.html')
#     map$addAssets(js = 
#    c("https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js",
#      "https://code.highcharts.com/highcharts.js",
#      "https://code.highcharts.com/highcharts-more.js",
#      "https://code.highcharts.com/modules/exporting.js",
#      "https://code.highcharts.com/modules/heatmap.js"
#      )
# )
    
#     # print map
#     #print(map)
# print(map)
# })



####### Display the comparison plot: comparison between the whole dataset and the cross-validated dataset
    Compobj<-eventReactive(input$rocButton, {
                    ListColumnMarker=which(as.numeric(str_detect(input$variables2, pattern = colnames(data2())))==1)
       if(length(ListColumnMarker)==1){
              tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
        }else{
               tmpdf <-data2()[,ListColumnMarker]
        }
     if(dim(tmpdf)[1]==1){
      tmpcolumn <-log(tmpdf+1)
      }else{
    tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
    }
    #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
    dinput<-data2()[,-c(1)]
    dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))

      glm.ag1<-glm(Class~tmpcolumn,data=dinput, family="binomial")          #glm(Class~tmpcolumn,data=data(), family="binomial")
      
     pred<-prediction(glm.ag1$fitted.values,dinput$Class)                #prediction(glm.ag1$fitted.values,data()$Class)
     # SE and SP
     roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
     #AUC
     perf.auc <- performance(pred, "auc")
     #extract SE,SP,cutoff and AUC
     perf.auc.areas <- slot(perf.auc, "y.values")
     valAUC <- c("AUC", round(perf.auc.areas[[1]], 3))
     valAUC <- sprintf("%-9s", valAUC)
     
     opt.cut = function(perf, pred){
     cut.ind = mapply(FUN=function(x, y, p){
         d = (x - 0)^2 + (y-1)^2
         ind = which(d == min(d))
         c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
           cutoff = p[[ind]])
     }, perf@x.values, perf@y.values, pred@cutoffs)
     }
      valperf<- round(opt.cut(roc.perf, pred),3)
      valperfSE <-c("SE",valperf[1,1])
      valperfSE <- sprintf("%-9s", valperfSE)
      valperfSP <-c("SP",valperf[2,1])
      valperfSP <- sprintf("%-9s", valperfSP)
      valperfcutoff <-c("@cutoff",valperf[3,1])
      valperfcutoff <- sprintf("%-9s", valperfcutoff)
     
     #par(bg="lightgray", mai=c(1.2,1.5,1,1))
     #par(mar=c(5.1,12,4.1,12))
      par(oma=c(2,2,2,2))
      par(mar=c(3.8, 3.8, 2, 3))  #bottom,left,top,right
     plot(roc.perf,xlab="1-Specificity", ylab="Sensitivity",axes=F,col="seagreen",box.lty=7, box.lwd=5,
  box.col='black' ,lwd=5,main=input$variables2, colorkey.relwidth=0.5, xaxis.cex.axis=0.8,
  xaxis.col='darkblue', xaxis.col.axis="darkblue", yaxis.col='darkblue', yaxis.cex.axis=0.8,
 , yaxis.las=1, xaxis.lwd=2, yaxis.lwd=2,yaxis.col.axis="darkblue", cex.lab=1,cex.main=1)  #yaxis.at=c(0,0.5,0.8,0.85,0.9,1)
  box(which = "plot", lty = "solid", col="red")
     grid.at <- seq(0, 1, 0.1)
     abline(v=grid.at, lty=3, col="lightgrey")
     abline(h=grid.at, lty=3, col="lightgrey")
     abline(a=0, b= 1,lty=3)
     abline(v=1-valperf[2,1],lty=4,lwd=2,col="cornflowerblue")
     abline(h=valperf[1,1],lty=4,lwd=2,col="cornflowerblue")
    # roclegend <-paste(valAUC, valperfSE,valperfSP,valperfcutoff, sep="")       #paste(feats, vals, sep="");
    # legend("bottomright",legend = roclegend,inset = .05,title=input$variables2,bg='aliceblue')


      df=dinput
        #function to extract performances
    mycvAUC <- function (predictions, labels, label.ordering = NULL, folds = NULL) 
    {     
        pred <- ROCR::prediction(predictions, labels)
        perf <- ROCR::performance(pred, "tpr", "fpr")
        return(perf)
    }


    # function to calculate 10fold CV ROC curves
    PredictionScore <- function(data, V=10){
         # function to create the variables "log(Markerx+1)" for each marker of the combination
         StringComposition <- function(x) paste("log(",x,"+1)",sep="")
         
         #Create CV folds (stratify by outcome)
        cvFolds <- function(Y, V){ 
        set.seed(1) 
            Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
            Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
            folds <- vector("list", length=V)
            for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}   
            return(folds)
        }
        #Train/test glm for each fold
        doFit <- function(v, folds, data){ 
        set.seed(1)
             vars<- str_split(input$variables2,"-")
             fla <- paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+")))
             as.formula(fla)
             fit <- glm(fla, data=data[-folds[[v]],], family=binomial)
             pred <- predict.glm(fit, newdata=data[folds[[v]],], type="response")
             return(pred)
        }
   
        
        folds <- cvFolds(Y=data$Class, V=V)  #Create folds
        predictions <- unlist(sapply(seq(V), doFit, folds=folds, data=data))  #CV train/predict
        #predictions<-doFit(1,folds,data)
        #predictions[unlist(folds)] <- predictions  #Re-order pred values
        predictions<- predictions[sort(as.numeric(names(predictions)),index.return=T)$ix]
        #return(predictions)
        out<- mycvAUC(predictions=predictions, labels=data$Class)
        return(out)
     }  

     perftest<- PredictionScore (data=df,V=10)
     plot(perftest,add=TRUE,col="tomato4",lwd=5,lty="dotted")

    })


   output$ComparisonPlot <- renderPlot ({
      Compobj()
     })
   
####### Display the comparison plot 1: comparison between the whole dataset and the cross-validated dataset

 #    Compobj1<-eventReactive(input$variables2, {
 #      #input$rocButton
 #      #input$variables2
 #       matchNames=cbind(rownames(nTable1()),nTable1()[,1])
 #         pos=which(matchNames[,2]==input$variables2)
 #                    ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)
 #       if(length(ListColumnMarker)==1){
 #              tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
 #        }else{
 #               tmpdf <-data2()[,ListColumnMarker]
 #        }
 #     if(dim(tmpdf)[1]==1){
 #      tmpcolumn <-log(tmpdf+1)
 #      }else{
 #    tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
 #    }
 #    #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
 #    dinput<-data2()[,-c(1)]
 #    dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))

 #      glm.ag1<-glm(Class~tmpcolumn,data=dinput, family="binomial")          #glm(Class~tmpcolumn,data=data(), family="binomial")
      
 #     pred<-prediction(glm.ag1$fitted.values,dinput$Class)                #prediction(glm.ag1$fitted.values,data()$Class)
 #     # SE and SP
 #     roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
 #     #AUC
 #     perf.auc <- performance(pred, "auc")
 #     #extract SE,SP,cutoff and AUC
 #     perf.auc.areas <- slot(perf.auc, "y.values")
 #     valAUC <- c("AUC", round(perf.auc.areas[[1]], 3))
 #     valAUC <- sprintf("%-9s", valAUC)
     
 #     opt.cut = function(perf, pred){
 #     cut.ind = mapply(FUN=function(x, y, p){
 #         d = (x - 0)^2 + (y-1)^2
 #         ind = which(d == min(d))
 #         c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
 #           cutoff = p[[ind]])
 #     }, perf@x.values, perf@y.values, pred@cutoffs)
 #     }
 #      valperf<- round(opt.cut(roc.perf, pred),3)
 #      valperfSE <-c("SE",valperf[1,1])
 #      valperfSE <- sprintf("%-9s", valperfSE)
 #      valperfSP <-c("SP",valperf[2,1])
 #      valperfSP <- sprintf("%-9s", valperfSP)
 #      valperfcutoff <-c("Opt Cutoff @ ",valperf[3,1])
 #      valperfcutoff <- sprintf("%-9s", valperfcutoff)


 #       df=dinput
 #        #function to extract performances
 #    mycvAUC <- function (predictions, labels, label.ordering = NULL, folds = NULL) 
 #    {     
 #        pred <- ROCR::prediction(predictions, labels)
 #        perf <- ROCR::performance(pred, "tpr", "fpr")
 #        return(perf)
 #    }


 #    # function to calculate 10fold CV ROC curves
 #    PredictionScore <- function(data, V=10){
 #         # function to create the variables "log(Markerx+1)" for each marker of the combination
 #         StringComposition <- function(x) paste("log(",x,"+1)",sep="")
         
 #         #Create CV folds (stratify by outcome)
 #        cvFolds <- function(Y, V){ 
 #        set.seed(1) 
 #            Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
 #            Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
 #            folds <- vector("list", length=V)
 #            for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}   
 #            return(folds)
 #        }
 #        #Train/test glm for each fold
 #        doFit <- function(v, folds, data){ 
 #        set.seed(1)
 #             vars<- str_split(matchNames[pos,1],"-")
 #             fla <- paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+")))
 #             as.formula(fla)
 #             fit <- glm(fla, data=data[-folds[[v]],], family=binomial)
 #             pred <- predict.glm(fit, newdata=data[folds[[v]],], type="response")
 #             return(pred)
 #        }
   
        
 #        folds <- cvFolds(Y=data$Class, V=V)  #Create folds
 #        # doesn't work predictions <- unlist(sapply(seq(V), doFit, folds=folds, data=data))  #CV train/predict
 #        predictions <- (lapply(seq(V), doFit, folds=folds, data=data))
 #        predictions<-unlist(predictions)
 #        #predictions<-doFit(1,folds,data)
 #        #predictions[unlist(folds)] <- predictions  #Re-order pred values
 #        predictions<- predictions[sort(as.numeric(names(predictions)),index.return=T)$ix]
 #        #return(predictions)
 #        out<- mycvAUC(predictions=predictions, labels=data$Class)
 #        return(out)
 #     }  

 #     perftest<- PredictionScore (data=df,V=10)
   




 #            h2 <-rCharts::Highcharts$new()

 #      #create empty list to storage values of ROC curves
 #      plot_data_fitted2<-vector("list",length(roc.perf@x.values[[1]]))
 #      #initialize the list plot_data_fitted
 #      for (i in 1:length(roc.perf@x.values[[1]])){
 #         plot_data_fitted2[[i]]<-list(x=roc.perf@x.values[[1]][i],y=roc.perf@y.values[[1]][i],z=perf.auc@y.values[[1]][i])
 #      }
 #      #create diagonal line
 #      plot_diagonal = list(
 #          list(x = -1, y = -1),
 #      list(x = 2, y = 2)
 #     )
 #     #create  the CC data
 #      plot_data_CV<-vector("list",length(perftest@x.values[[1]]))
 #       #initialize the list plot_data_fitted
 #      for (i in 1:length(perftest@x.values[[1]])){
 #         plot_data_CV[[i]]<-list(x=perftest@x.values[[1]][i],y=perftest@y.values[[1]][i])
 #      }

 #      #create optimal cutoff point
 #    opt_cutoff=list(
 #            list(x=as.numeric(1-valperf[2,1]),y=as.numeric(valperf[1,1]))
 #   )
 

 #  h2$series(name = input$variables2,

 #            type = "line",
 #            data = plot_data_fitted2)
 #  h2$series(name = paste0(" 10-fold CV, ",input$variables2),

 #            type = "line",
 #            data = plot_data_CV,dashStyle="ShortDot",color="gray")


 #   h2$series(name=valperfcutoff,
 #         type="scatter",
 #         data=opt_cutoff,
 #         marker=list(symbol='url(https://www.highcharts.com/samples/graphics/sun.png)')
 #         #tooltip=list(formatter="#! function() { return '<b>'+ this.series.name +'<br/>' + 'optimal cutoff: ' + Highcharts.numberFormat((this.point.category), 2) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 2) +'<br/>'+ 'AUC: '+ Highcharts.numberFormat((1-(this.point.x)), 2);} !#")
 #         )


 # h2$series(name = "diagonal line",
 #            type = "line",
 #            data = plot_diagonal,dashStyle="ShortDot",lineWidth=1,color="black")
 #   h2$title(text=input$variables2,style=list(fontSize=32,zIndex=3,color="#ffe34c",fontWeight="bold",textShadow="0 0 6px contrast, 0 0 3px contrast"))  #,useHTML=TRUE+'background-color'="black",border='2px solid black'
 #                h2$plotOptions(line= list(shadow = TRUE,lineWidth=3,color="#7dd4b6"))
 #               h2$tooltip(crosshairs=c(TRUE,TRUE),shared=TRUE)
 #               h2$set(width = 600,height=700) 
 #               h2$exporting(enabled=TRUE)
 #               h2$chart(zoomType = "xy",options3d=list(enabled=TRUE),lineWidth=3,plotShadow=TRUE) # to plot the border plotBorderWidth=1,
 #                h2$set(width = 500,height=500,dom=" Compobj1")  
 #                ###BE CAREFUL!!!! the dom must be the same of the declared object name otherwise two highcharts plot don't work on the same page or the
 #                ###two highcharts are visualized together
 #               h2$xAxis(min=-0.03,max=1,title = list(text = "1-Specificity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(1-valperf[2,1]),zIndex=3)))
 #               h2$yAxis(min=-0,max=1,title = list(text = "Sensitivity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(valperf[1,1]))))
 #              h2$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'SE: ' + Highcharts.numberFormat((this.point.y), 2) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 2);} !#") 
 #      return(h2)
 #    })


Compobj1<-eventReactive(input$variables2, {

     # function to create the variables "log(Markerx+1)" for each marker of the combination
         StringComposition <- function(x) paste("log(",x,"+1)",sep="")
    #input$rocButton
         matchNames=cbind(rownames(nTable1()),nTable1()[,1])
         pos=which(matchNames[,2]==input$variables2)
         ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2


    #    if(length(ListColumnMarker)==1){
    #           tmpdf <-array(data=data2()[,ListColumnMarker],dim=c(dim(data2())[1],length(ListColumnMarker)))
    #     }else{
    #            tmpdf <-data2()[,ListColumnMarker]
    #     }
    #  if(dim(tmpdf)[1]==1){
    #   tmpcolumn <-log(tmpdf+1)
    #   }else{
    # tmpcolumn<-rowSums(apply(tmpdf,2,function(x){log(x+1)})) #(sapply(1:ncol(tmpdf),function(x){(tmpdf[,x])}))
    # }
     #vars<- str_split(matchNames[pos,1],"-")
     if(str_count(matchNames[pos,1],"M")==1){ #single marker in the combination
             #get the dataframe
             dinput<-data2()[,-c(1)]
              #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
              dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))


                glm.single<-glm(Class~log(dinput[,ListColumnMarker-1]+1),data=dinput, family="binomial")
                 #rocobj<-roc(dinput$Class,dinput[,ListColumnMarker-1],levels=c("0","1")) w/o logistic regression and probabilities in ptedictions range from 0 to max value of selected marker
                 rocobj<-roc(dinput$Class,glm.single$fitted.values,levels=c("0","1"))
                 optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

    }else{  # panel marker
        vars<- str_split(matchNames[pos,1],"-")
         fla <- as.formula(paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+"))))
          dinput<-data2()[,-c(1)]
        dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
        glm.combo<-glm(fla,data=dinput, family="binomial")
        rocobj<-roc(dinput$Class,glm.combo$fitted.values,levels=c("0","1"))
        optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

    }



      valperfcutoff <-paste0("Opt Cutoff @ ", round(optcoordinates[[1]],3))
      valperfcutoff <- sprintf("%-3s", valperfcutoff)
      
                   #get the dataframe
             dinput<-data2()[,-c(1)]
              #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
              dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
              
df=dinput
    #function to extract performances
    mycvAUC <- function (predictions, labels, label.ordering = NULL, folds = NULL)
    {
        #pred <- ROCR::prediction(predictions, labels)
        #perf <- ROCR::performance(pred, "tpr", "fpr")
        perf<-roc(labels,predictions,levels=c("0","1"))
        return(perf)
    }


    # function to calculate 10fold CV ROC curves
    PredictionScore <- function(data, V=10){
         # function to create the variables "log(Markerx+1)" for each marker of the combination
         StringComposition <- function(x) paste("log(",x,"+1)",sep="")

         #Create CV folds (stratify by outcome)
        cvFolds <- function(Y, V){
        set.seed(1)
            Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
            Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
            folds <- vector("list", length=V)
            for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}
            return(folds)
        }
        #Train/test glm for each fold
        doFit <- function(v, folds, data){
        set.seed(1)
             vars<- str_split(matchNames[pos,1],"-")
             fla <- paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+")))
             as.formula(fla)
             fit <- glm(fla, data=data[-folds[[v]],], family=binomial)
             pred <- predict.glm(fit, newdata=data[folds[[v]],], type="response")
             return(pred)
        }


        folds <- cvFolds(Y=data$Class, V=V)  #Create folds
        # doesn't work predictions <- unlist(sapply(seq(V), doFit, folds=folds, data=data))  #CV train/predict
        predictions <- (lapply(seq(V), doFit, folds=folds, data=data))
        predictions<-unlist(predictions)
        #predictions<-doFit(1,folds,data)
        #predictions[unlist(folds)] <- predictions  #Re-order pred values
        predictions<- predictions[sort(as.numeric(names(predictions)),index.return=T)$ix]
        #return(predictions)
        out<- mycvAUC(predictions=predictions, labels=data$Class)
        return(out)
     }

     perftest<- PredictionScore (data=df,V=10)
     
     

       h2 <-rCharts::Highcharts$new()

      #create empty list to storage values of ROC curves
      plot_data_fitted2<-vector("list",length(rocobj$sensitivities))
      #initialize the list plot_data_fitted
      for (i in 1:length(rocobj$sensitivities)){
         plot_data_fitted2[[i]]<-list(x=(1-rocobj$specificities[i]),y=rocobj$sensitivities[i],z=rocobj$auc[[1]])
      }
      #create diagonal line
      plot_diagonal = list(
          list(x = -1, y = -1),
      list(x = 2, y = 2)
     )
     #create  the CV data
      plot_data_CV<-vector("list",length(perftest$sensitivities))
       #initialize the list plot_data_fitted
      for (i in 1:length(perftest$sensitivities)){
         plot_data_CV[[i]]<-list(x=(1-perftest$specificities[i]),y=perftest$sensitivities[i])
      }

      #create optimal cutoff point
    opt_cutoff=list(
         list( x=as.numeric(1-optcoordinates[[2]]),y=as.numeric(optcoordinates[[3]]))
   )


  h2$series(name = input$variables2,
            type = "line",
            data = plot_data_fitted2)

  h2$series(name = paste0(" 10-fold CV, ",input$variables2),
            type = "line",
            data = plot_data_CV,dashStyle="ShortDot",color="gray")


   h2$series(name=valperfcutoff,
         type="scatter",
         data=opt_cutoff,
         marker=list(symbol='url(https://www.highcharts.com/samples/graphics/sun.png)')
         #tooltip=list(formatter="#! function() { return '<b>'+ this.series.name +'<br/>' + 'optimal cutoff: ' + Highcharts.numberFormat((this.point.category), 2) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 2) +'<br/>'+ 'AUC: '+ Highcharts.numberFormat((1-(this.point.x)), 2);} !#")
         )


 h2$series(name = "diagonal line",
            type = "line",
            data = plot_diagonal,dashStyle="ShortDot",lineWidth=1,color="black")
   h2$title(text=input$variables2,style=list(fontSize=32,zIndex=3,color="#ffe34c",fontWeight="bold",textShadow="0 0 6px contrast, 0 0 3px contrast"))  #,useHTML=TRUE+'background-color'="black",border='2px solid black'
                h2$plotOptions(line= list(shadow = TRUE,lineWidth=3,color="#7dd4b6"))
               h2$tooltip(crosshairs=c(TRUE,TRUE),shared=TRUE)
               h2$set(width = 600,height=700)
               h2$exporting(enabled=TRUE)
               h2$chart(zoomType = "xy",options3d=list(enabled=TRUE),lineWidth=3,plotShadow=TRUE) # to plot the border plotBorderWidth=1,
                h2$set(width = 500,height=500,dom=" Compobj1")
                ###BE CAREFUL!!!! the dom must be the same of the declared object name otherwise two highcharts plot don't work on the same page or the
                ###two highcharts are visualized together
               h2$xAxis(min=-0.03,max=1,title = list(text = "1-Specificity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(1-optcoordinates[[2]]),zIndex=3)))
               h2$yAxis(min=-0,max=1,title = list(text = "Sensitivity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE,plotLines=list(list(color="#7c9bd4",dashStyle="dashdot",width=2,value=as.numeric(optcoordinates[[3]]))))
              h2$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'SE: ' + Highcharts.numberFormat((this.point.y), 3) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 3);} !#")
      return(h2)
    })



   output$Chart2<-renderChart2 ({
    #input$rocbutton
    input$variables2
      Compobj1()
     })


####### Display as datatable (Perftable) the performances of the whole cohort and cross-validated dataset
        pPerfTable<-eventReactive(input$variables2, {
              df=SelectedPerfObj()
              dfcv=SelectedCVPerfObj()
              perf=data.frame(matrix(NA, nrow=2, ncol=5))
              colnames(perf)<-c("ACC","Error Rate","SE","SP","AUC")
              rownames(perf)<-c("Whole Cohort","10-fold CV")
              #perf[1,1:5]<-cbind(df[1,5],df[1,6],df[1,3],df[1,2],df[1,4]) before with ROCR
              perf[1,1:5]<-cbind(df[1,5],df[1,6],df[1,2],df[1,3],df[1,4])
              perf[2,1:5]<-cbind(dfcv$fold.ACC[[1]],dfcv$fold.ERR[[1]],dfcv$fold.SE[[1]],dfcv$fold.SP[[1]],dfcv$fold.AUC[[1]])
              perf
         })

     output$Perftable <- DT::renderDataTable({
           pPerfTable()
         },colnames = c('Dataset Type'=1),extensions = c('TableTools','Responsive'),options = list(paging = FALSE, searching = FALSE, initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))
         )


#  if you want to print the performance values (e.g. cross-validated)
     # output$prova1<-renderPrint({
     #      s=SelectedCVPerfObj()
     #      print(s)
     #      print(input$variables2)
     #    }) 

####### Back widget: back to "Gold combinations"
    output$kpi_summary_box_4 <- renderValueBox({
    valueBox(
      value = "Back",
      subtitle = "Gold combinations",
      icon = icon("step-backward"),
      color = "green"
    )
  })



##################################
################# Multiple ROC curves
##################################
   
####### Select the marker combination once the user have been selected the gold combinations
    outVar3<- reactive({
       vars <- nTable1()[1:dim(nTable1())[1]]
      return(vars)
    })

 
    output$Mvariable2 = renderUI({
      selectInput('variables3', 'Marker', choices=outVar3(),multiple=TRUE,selected=outVar3()[1])
    })



   multipleROCobj<-eventReactive(input$variables3, {
     # function to create the variables "log(Markerx+1)" for each marker of the combination
        StringComposition <- function(x) paste0("log(",x,"+1)",sep="")
      h1 <-rCharts::Highcharts$new()
      h1$chart(type = "line", height = 600,width=500,zoomType = "xy",options3d=list(enabled=TRUE),lineWidth=3,plotShadow=TRUE)
      h1$title(text="Multiple ROC curves",style=list(fontSize=32,zIndex=3,color="black",fontWeight="bold",textShadow="0 0 6px contrast, 0 0 3px contrast")) 
      h1$plotOptions(line= list(shadow = TRUE,lineWidth=3))
      h1$tooltip(crosshairs=c(TRUE,TRUE),shared=TRUE)
      h1$exporting(enabled=TRUE)
      #h1$chart(zoomType = "xy",options3d=list(enabled=TRUE),lineWidth=3,plotShadow=TRUE)
      
      for(i in 1:length(input$variables3)){
  
         matchNames=cbind(rownames(nTable1()),nTable1()[,1])
         pos=which(matchNames[,2]==input$variables3[i])
         ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2
        if(str_count(matchNames[pos,1],"M")==1){ #single marker in the combination
             #get the dataframe
             dinput<-data2()[,-c(1)]
              #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
              dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))


                glm.single<-glm(Class~log(dinput[,ListColumnMarker-1]+1),data=dinput, family="binomial")
                 #rocobj<-roc(dinput$Class,dinput[,ListColumnMarker-1],levels=c("0","1")) w/o logistic regression and probabilities in ptedictions range from 0 to max value of selected marker
                 rocobj<-roc(dinput$Class,glm.single$fitted.values,levels=c("0","1"))
                 optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

       }else{  # panel marker
        vars<- str_split(matchNames[pos,1],"-")
         fla <- as.formula(paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+"))))
          dinput<-data2()[,-c(1)]
        dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
        glm.combo<-glm(fla,data=dinput, family="binomial")
        rocobj<-roc(dinput$Class,glm.combo$fitted.values,levels=c("0","1"))
        optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

       }



      valperfcutoff <-paste0("Opt Cutoff @ ", round(optcoordinates[[1]],3))
      valperfcutoff <- sprintf("%-3s", valperfcutoff)
         #create  list to storage values of ROC curves
      plot_data_fitted<-vector("list",length(rocobj$sensitivities))
          #initialize the list plot_data_fitted
      for (j in 1:length(rocobj$sensitivities)){
         plot_data_fitted[[j]]<-list(x=(1-rocobj$specificities[j]),y=rocobj$sensitivities[j],z=rocobj$auc[[1]])
      }

      h1$series(name=input$variables3[i],data=plot_data_fitted)

    } #closing for cicle

       h1$legend(
           align = 'right', 
           verticalAlign = 'middle', 
           layout = 'vertical'
       )
       h1$xAxis(min=-0.03,max=1,title = list(text = "1-Specificity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE)
       h1$yAxis(min=-0,max=1,title = list(text = "Sensitivity ",style=list(fontSize=18,zIndex=3,color="black")),gridLineWidth=1,startOntick=FALSE,endOnTick=FALSE)
       
       h1$tooltip(formatter = "#! function() { return '<em><b>'+ this.series.name +'<br/>' + 'SE: ' + Highcharts.numberFormat((this.point.y), 3) +'<br/>'+ 'SP: '+ Highcharts.numberFormat((1-(this.point.x)), 3);} !#")
   
      return(h1)
  })



  output$multipleROC <-renderChart2 ({
      (multipleROCobj())
     })

  ####### Display as datatable (ROCtable1) the performances of the whole cohort 
  rocmultipleTable<-eventReactive(input$variables3, {
           # function to create the variables "log(Markerx+1)" for each marker of the combination
         StringComposition <- function(x) paste0("log(",x,"+1)",sep="")
            
              
               perf=data.frame(matrix(NA, nrow=length(input$variables3), ncol=4)) 
              colnames(perf)<-c("AUC","SE %","SP %","Opt Cutoff")
              rownames(perf)<-c(input$variables3)
                
      for (i in 1:length(input$variables3)){
                      matchNames=cbind(rownames(nTable1()),nTable1()[,1])
                     pos=which(matchNames[,2]==input$variables3[i])
                    ListColumnMarker=which(as.numeric(str_detect(matchNames[pos,1], pattern = colnames(data2())))==1)  #input$variables2
  
                        if(str_count(matchNames[pos,1],"M")==1){ #single marker in the combination
             #get the dataframe
             dinput<-data2()[,-c(1)]
              #transfotm the class label in numeric code (0 for control classB2 and 1 for disease classA2)
              dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))


                glm.single<-glm(Class~log(dinput[,ListColumnMarker-1]+1),data=dinput, family="binomial")
                 #rocobj<-roc(dinput$Class,dinput[,ListColumnMarker-1],levels=c("0","1")) w/o logistic regression and probabilities in ptedictions range from 0 to max value of selected marker
                 rocobj<-roc(dinput$Class,glm.single$fitted.values,levels=c("0","1"))
                 optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))



              #  glm.single<-glm(Class~dinput[,ListColumnMarker-1],data=dinput, family="binomial")
              # #rocobj<-roc(dinput$Class,dinput[,ListColumnMarker-1],levels=c("0","1")) w/o logistic regression and probabilities in ptedictions range from 0 to max value of selected marker
              # rocobj<-roc(dinput$Class,glm.single$fitted.values,levels=c("0","1"))

              # optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

    }else{  # panel marker
        vars<- str_split(matchNames[pos,1],"-")
         fla <- as.formula(paste("Class ~", paste(paste(sapply(vars,StringComposition),collapse="+"))))
          dinput<-data2()[,-c(1)]
        dinput$Class<-factor(c(rep(1,nrow(classA2())),rep(0,nrow(classB2()))),levels=c(0,1))
        glm.combo<-glm(fla,data=dinput, family="binomial")
        rocobj<-roc(dinput$Class,glm.combo$fitted.values,levels=c("0","1"))
        optcoordinates<-coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
    }
     AUC <- round(rocobj$auc[1],3)
     ACC <- round(optcoordinates[[4]],3)
     ERR <- round((optcoordinates[[8]]+optcoordinates[[7]])/dim(data2())[1],3)    #(FP+FN)/P+N
     TP  <- round(optcoordinates[[6]],3)
     FP  <- round(optcoordinates[[8]],3)
     TN  <- round(optcoordinates[[5]],3)
     FN  <- round(optcoordinates[[7]],3)
     PPV <- round(optcoordinates[[10]],3)
     NPV <- round(optcoordinates[[9]],3)
    perfwhole <- cbind(round(optcoordinates[[1]],3),round(optcoordinates[[3]],3),round(optcoordinates[[2]],3),AUC,ACC,ERR,TP,FP,TN,FN,PPV,NPV)
    colnames(perfwhole)<-c("CutOff","SE","SP","AUC","ACC","ERR","TP","FP","TN","FN","PPV","NPV")
    #rownames(perf)[i]<-c(matchNames[pos,1])
     perf[i,1:4]<-cbind(perfwhole[1,4],perfwhole[1,2],perfwhole[1,3],perfwhole[1,1])


     }
     perf



         })

   output$ROCmultipletable <- DT::renderDataTable({
           rocmultipleTable()
         },colnames = c('Symbol'=1),extensions = c('TableTools','Responsive'),options = list(paging = FALSE, searching = FALSE, initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),dom = 'T<"clear">lfrtip',tableTools = list(sSwfPath = copySWF('www',pdf=TRUE),aButtons=list('copy','csv','pdf')))
         )










    # output$provathr_2 <- renderText({ 
    #   #paste("You have selected", input$combithr_2,input$signalthr_2)
    #   #paste("This is the",SE_disease())
    #    # paste("This is the",rownames(SPE_control()))
    #      s1=((nTable1()[,1]))
    #   print((s1))
    # })





# ##################################
# ################# Interaction map tab
# ##################################


# ####### Select the marker combination
#     # outVar <- reactive({
#     # vars <- as.list(rownames(nTable()))
#     #   return(vars)
#     # })

#     # output$Mvariable = renderUI({
#     #   selectInput('variables2', 'Variables', outVar())
#     # })
#         QueryName <- eventReactive(input$NetworkButton, {
#             return(input$MarkerName)
#       })




#       # StringDb <- reactive({
#       #       stringdb=STRINGdb$new(version="10",species=9606,score_threshold=803)
#       #       stringquery=stringdb$mp(QueryName())
#       #       return(stringquery)
#       # })

#      # QueryName <-reactive({
#      #        return(input$MarkerName)
#      #  })

#      # StringQuery <- eventReactive(input$NetworkButton, {
#      #     stringdb=StringDb()
#      #     stringquery=stringdb$mp(QueryName())
#      #     return(stringquery)
#      #     })

#       NetworkDetails <- eventReactive(input$NetworkButton, {
#            str1<-p(str_c("Your input: ",QueryName(),colllapse=" "))
#            HTML(paste(str1,sep='<p>'))

#        })    
#        #  Print data details the number of samples and features
#      output$uploadNetworkDetails=renderPrint({
#              NetworkDetails()
#      #str1<-p(str_c("Your input: ",QueryName(),colllapse=" ")) #str_c("A total of ",  length(nclass), " groups (", nclass[1], ",", nclass[2], ") were detected."))
#     #str2<-p(str_c("A total of ",  length(nclass), " groups (", nclass[1], ",", nclass[2], ") were detected."))
#      #str3<-p(str_c("All data values are numeric......"))
#      #str4<-p(str_c("A total of xx missing values were detected"))
#      #HTML(paste(str1,sep='<p>'))
#      })     


#       output$networkPlot <- renderPrint({
#         MisNodes<- data.frame(name=c("CHAD","ITGA3","ITGA2","ITGA6","ITGAV","ITGA1","ITGB1","ITGA5","ITGA11","ITGA9","ITGA2B"),group=c(1,2,3,4,5,6,7,8,9,10,11))
#     source <- c(1,1,0,0,0,0,0,0,0,0,0,4,4,10,10,9,9,5,7,7,2,6,6)
#     target <- c(0,6,2,3,4,5,6,7,8,9,10,5,6,6,3,6,3,6,6,3,6,8,3)
#     value <- c(1,1,1,15,15,15,15,15,15,15,15,15,1,1,1,1,1,1,1,1,1,1,1,1)
#      #value <- c( 820, 994, 806, 801, 801, 804, 803, 810, 803, 801, 810, 922, 994, 978, 835, 998, 820, 997, 999, 842, 998, 969, 999)


#         MisLinks<-data.frame(source=source,target=target,value=value)
#         d3ForceNetwork(Nodes = MisNodes, 
#                         Links = MisLinks,  
#                         Source = "source", Target = "target", Value="value",
#                          NodeID = "name", 
#                         Group = "group", width = 400, height = 400, 
#                         standAlone = FALSE,
#                         parentElement = '#networkPlot',zoom=TRUE,linkDistance = 150,opacity = 0.8) #,linkWidth =5
#     })
   

#    output$network <- renderVisNetwork({
    
#     input$goButton
#     isolate({
#       nb = input$nb
#       nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
#                           enjeux = sample(c("Faible", "Moyen","Fort"), 10, replace = TRUE),
#                           group = sample(1:10, nb, replace = TRUE), value = 1:nb,
#                           title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
      
#       edges <- data.frame(from = trunc(runif(nb)*(nb-1))+1,
#                           to = trunc(runif(nb)*(nb-1))+1,
#                           value = rnorm(nb, 10), label = paste("Edge", 1:nb),
#                           title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))
      
#       # custom navigation
#       visNetwork(nodes, edges) %>% 
#         visPhysics(stabilization = FALSE) %>%
#         visInteraction(navigationButtons = TRUE) %>%
#         visOptions(manipulation = TRUE,
#                    highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "enjeux") %>%
#         visLegend(enabled =  input$legend2)
#     })
    
#   })


#   output$test <- renderText({
#     paste(input$network_selected, input$network_selectedBy)
#   })
     

     
    
#   output$datatest <- renderText({
     
#        s1=QueryName()
      
       
#         print(s1)

#   })


##################################
################# Download tab
##################################

####### Download proteomics demo data in csv format
output$downloadDemoData<-downloadHandler(
       #if (input$DataInput == 1 | input$DataInput== 2) {
             
    
            filename<-function(){paste('CombiROCDemoData.csv')},
             content<-function(file){
             write.csv(data(),file,row.names=FALSE)}
      #}
    )




##################################
################# Tutorial tab
##################################



  # output$downloadreport <- downloadHandler(
  #   filename = function() {
  #     paste('my-report', sep = '.', switch(
  #       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
  #     ))
  #   },

  #   content = function(file) {
  #     src <- normalizePath('report.Rmd')

  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'report.Rmd')

  #     library(rmarkdown)
  #     out <- render('report.Rmd', switch(
  #       input$format,
  #       PDF = pdf_document(), HTML = html_document(), Word = word_document()
  #     ))
  #     file.rename(out, file)
  #   }
  # )


  output$downloadreport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
#filename <- 'report.pdf',
 content <- function(file) {
  file.copy('www/links.pdf', file)
},

#       out <- rmarkdown::render('report.Rmd', switch(
#         input$format,
#         PDF = pdf_document(), HTML = html_document(), Word = word_document()
#       ))
#       file.rename(out, file)
#   #rmarkdown::render('report.Rmd')

# },

contentType = 'application/pdf')


  
  

  # output$downloadBoxPlotPDF<-downloadHandler(
  #   filename<-function(){paste('BoxPlot.pdf')},
  #   content<-function(file){
  #     pdf(file,width=input$plotwidth/72,height=input$plotheight/72)
  #     MakePlot(classA(),'A')
  #     MakePlot(classB(),'B')
  #     dev.off()
  #   },
  #   contentType='application/pdf'
  #   )
   
 ####### Bar chart
 #data table
# output$nCount<- DT::renderDataTable ({
# input1<- SE_disease()
#     input2 <- SPE_control()
#     input <-cbind(input1,input2)
#     colnames(input)<-c("SE_disease","SP_control")
#     a=(unique(input))
#     nCount=array(0,dim=c(dim(a)[1],1))
#        for (i in 1:dim(a)[1]){
#      nCount[i,1]<- length(intersect(which(a[i,1]==input[,1]), which(a[i,2]==input[,2])))
#      }
#my<-cbind(a,nCount)
#IdVart=array(0,dim=c(dim(a)[1],1))
#for (i in 1:dim(a)[1]){
#IdVart[i]<-paste0("Int_",i)
#}
#my <-cbind(IdVart,my)
#Col<-sample(colors(),19)
#my<-cbind(my,Col)
# names(my)<-c("IdVart","SE_disease","SP_control","nCount","Col")
# my
#  })



#output$bubbles<- renderGvis({
#input1<- SE_disease()
#     input2 <- SPE_control()
#     input <-cbind(input1,input2)
#     colnames(input)<-c("SE_disease","SP_control")
#     a=(unique(input))
#     nCount=array(0,dim=c(dim(a)[1],1))
#        for (i in 1:dim(a)[1]){
#      nCount[i,1]<- length(intersect(which(a[i,1]==input[,1]), which(a[i,2]==input[,2])))
#      }
#my<-cbind(a,nCount)
#IdVart=array(0,dim=c(dim(a)[1],1))
#for (i in 1:dim(a)[1]){
#IdVart[i]<-paste0("Int_",i)
#}
#my <-cbind(IdVart,my)
#Col<-sample(colors(),19)
#my<-cbind(my,Col)
# names(my)<-c("IdVart","SE_disease","SP_control","nCount","Col")
#
#Bubble<-(gvisBubbleChart(my, idvar="IdVart", xvar="SE_disease", yvar="SP_control",colorvar="Col", sizevar="nCount",options=list( width=100, height=100,legend="none",bubble.opacity=1,title="SE vs SP",hAxis="{title:'SE %', titleTextStyle:{color:'blue',fontName:'SourceSansPro',fontSize:22}}",vAxis="{title:'SP %', titleTextStyle:{color:'blue',fontName:'SansSerif',fontSize:22}}",explorer = list(),tooltip="{textStyle: {color: 'blue',showColorCode: false}}",bubble="{opacity: '0.4', stroke:'gray', textStyle: {color:'black',fontName:'SourceSansPro' , fontSize:'6' }}")))
#return(Bubble)
#http://www.analyticsforfun.com/2015/08/playing-with-r-shiny-dashboard-and.html
# }



#  output$bar <- renderGvis({
#    # Return the data and options
#     input1<- SE_disease()
#     input2 <- SPE_control()
#     input3 <-cbind(input1,input2)
#     a=(unique(input3))
#     nCount=array(0,dim=c(dim(a)[1],1))
#     for (i in 1:dim(a)[1]){
#      nCount[i,1]<- length(intersect(which(a[i,1]==input3[,1]), which(a[i,2]==input3[,2])))
#      }
#     IdVart=array(0,dim=c(dim(a)[1],1))
#     for (i in 1:dim(a)[1]){
#         IdVart[i]<-paste0("Int_",i)
#      }
#      Col<-sample(colors(),19)
#      df <- data.frame(NameCombination=IdVart,SE_disease=a[,1],SP_control=a[,2],nCount=nCount,Col=Col)
#      xlim <- reactive({list(min = input$se,max =100) })
#      ylim <- list(min = input$spe,max =100)
#
#       gvisBubbleChart(df, idvar="NameCombination", xvar="SE_disease", yvar="SP_control",colorvar="Col", sizevar="nCount",options=list( width=1000, height=1000,legend="none",sortBubblesBySize=T,bubble.opacity=1,title="SE vs SP",hAxis="[{viewWindowMode:'explicit',viewWindow:{min:[input$se],max:100}, title:'SE %', titleTextStyle:{color:'blue',fontName:'SourceSansPro',fontSize:22}}]",vAxis="{title:'SP %', titleTextStyle:{color:'blue',fontName:'SansSerif',fontSize:22}}",explorer = list(),tooltip="{textStyle: {color: 'blue',showColorCode: false}}",bubble="{opacity: '0.4', stroke:'gray', textStyle: {color:'black',fontName:'SourceSansPro' , fontSize:'12' }}"))
#
#})


	
	
 #	output$trendPlot <- renderGraph({
#
#		# build graph with ggplot syntax
#		# pull x and y variable info
#		p <- ggplot(dataset(),
#								aes_string(x = input$x,
#													 y = input$y)) +
#				 geom_point()
#
#		# if statements for possible color/facet row/facet column variables
#		if (input$color != 'None')
#				p <- p + aes_string(color=input$color)
#
#		facets <- paste(input$facet_row, '~', input$facet_col)
#		if (facets != '. ~ .')
#				p <- p + facet_grid(facets)
#
#		# use gg2list() to convert from ggplot->plotly
#		gg <- gg2list(p)
#
#		# make edits with plotly syntax
#		gg$layout <- list(legend = list(
#															x = 1,
#															y = 1,
#															bgcolor = "transparent"))
#
#		# Send this message up to the browser client, which will get fed through to
#		# Plotly's javascript graphing library embedded inside the graph
#		return(list(
#			list(
#				id = "trendPlot",
#				task = "newPlot",
#				data = gg$data,
#				layout= gg$layout
#			)
#		))
#
#	})


    
    
    
  #  list(
#      data = my,
#      options = list(
#        title = sprintf(
#          "prova")
#
#      )
#    )

 
 
 
   
#  output$distributionPlot <- renderPlot({
#    if (is.null(data()))
#      return(NULL)
#    if (input$panelType=='0') {
#
#     DistrPlot()
#    } else
#      validate(
#        need(try(condPlot != ""), "Make sure your network is completely directed in order to view the paramater infographics...")
#      )
#  })
       
       
       
       
       
       
       
#
})



