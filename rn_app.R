library(shiny)
library(bslib)
library(shinyWidgets)
library(ggplot2)





ui=fluidPage(
  titlePanel("Hybridisation: curse or cure?"),  # Add a title panel
  sidebarLayout(  # Make the layout a sidebarLayout
    
    sidebarPanel(
      card(
        card_header("Goal"),
        tags$p("There are about 1000 fish left in a river. Due to climate change and rise in temperature , the population of fish has been slowly declining. Recently, an invasive species has been reported in the same river. You are in charge of removing the invasive species. In principle, removing most of them should be enough to avoid them replacing the local species. Once the capture operation has been conducted, how many invasive fish are present in the river?  ")
      ),tags$br(),
      card(
        layout_column_wrap(width=1/2,actionButton("image", "Default"),actionButton("scheme", "Scheme"))
      ),tags$br(),
      card(
        card_header("Player 1"),
        sliderTextInput(inputId = "mig1",
                        label= "Number of invasive individuals:",  
                        choices = c(0,10,20,50,100),selected = 0)
      ),
      tags$br(),
      card(
        card_header("Player 2"),
        sliderTextInput(inputId = "mig2",
                        label= "Number of invasive individuals:",  
                        choices = c(0,10,20,50,100),selected = 0)
      ),
      tags$br(),
      card(
        card_header("Ready"),
        actionButton("action", "Play")
      ),tags$br(),
	  card(
        card_header("Check population"),
        numericInput(inputId = "gen",
                        label= "Generation to look at:",9999,min=1,max=9999)
      ),tags$br(),
      card(
        card_header("Population state"),
        actionButton("pie", "Display")
      ),tags$br(),
      card(
        card_header("Reset"),
        actionButton("reset", "Reset")
      ),tags$br(),
	  card(
	    card_header("Multiple cases"),
	    actionButton("cheat", "More information"),tags$br(),
	    card(
	      sliderTextInput(inputId = "migcm",
	                      label= "Number of invasive individuals:",  
	                      choices = c(0,10,20,50,100),selected = 0)
	    )
	  )
      ),# Inside the sidebarLayout, add a sidebarPanel
    mainPanel(
      layout_column_wrap(width=1/3,card(card_header("Local environment"),imageOutput("river")),card(card_header("Local fish"),imageOutput("fish1")),card(card_header("Invasive fish"),imageOutput("fish2"))),
      layout_column_wrap(width=1/2,card(plotOutput("traj1")),card(plotOutput("traj2"))),
	  layout_column_wrap(width=1/2,card(plotOutput("pie1")),card(plotOutput("pie2")))
    )  # Inside the sidebarLayout, add a mainPanel
  )
)

server <- function(input, output) {
	output$traj1=NULL
	output$traj2=NULL
	output$pie1=NULL
	output$pie2=NULL
	
	output$river=renderImage(list(src = "./river.png",width = 400,height = 200,contentType ='image/png'),deleteFile = FALSE)
	output$fish1=renderImage(list(src = "./fish2.png",width = 300,height = 200,contentType ='image/png'),deleteFile = FALSE)
	output$fish2=renderImage(list(src = "./fish1.png",width = 300,height = 200,contentType ='image/png'),deleteFile = FALSE)

	observeEvent(input$image,{
	  output$fish1=renderImage(list(src = "./fish2.png",width = 300,height = 200,contentType ='image/png'),deleteFile = FALSE)
	output$fish2=renderImage(list(src = "./fish1.png",width = 300,height = 200,contentType ='image/png'),deleteFile = FALSE)})
	
	observeEvent(input$scheme,{
	  output$fish1=renderImage(list(src = "./fish2_scheme.png",width = 300,height = 200,contentType ='image/png'),deleteFile = FALSE)
	  output$fish2=renderImage(list(src = "./fish1_scheme.png",width = 300,height = 200,contentType ='image/png'),deleteFile = FALSE)
	})
	
  caseP1=eventReactive(input$action,{
    # player 1 load data
  mig_rate1=reactive({req(input$mig1)
  input$mig1})
  folder1=paste("mig",mig_rate1(),sep="")
  i=sample(seq(0,9),1)
  print(paste(folder1,"/freq",i,".txt",sep=""))
  case1=read.table(paste(folder1,"/freq",i,".txt",sep=""))
  case1$time=seq(2,length(case1$V1)+1)
  rbind(c(1000,2000,2*mig_rate1(),2000,2*mig_rate1(),1),case1)
  })
  
  caseP2=eventReactive(input$action,{ #player 2 laod data
  mig_rate2=reactive({req(input$mig2)
  input$mig2})
  folder2=paste("mig",mig_rate2(),sep="")
  i=sample(seq(0,9),1)
  case2=read.table(paste(folder2,"/freq",i,".txt",sep=""))
  case2$time=seq(2,length(case2$V1)+1)
  rbind(c(1000,2000,2*mig_rate2(),2000,2*mig_rate2(),1),case2)
  })
  
  caseCM=eventReactive(input$cheat,{
    # player 1 load data
    mig_ratecm=reactive({req(input$migcm)
      input$migcm})
    folder1=paste("mig",mig_ratecm(),sep="")
    case_all=NULL
    for(i in 0:9){
    print(paste(folder1,"/freq",i,".txt",sep=""))
    case1=read.table(paste(folder1,"/freq",i,".txt",sep=""))
    case1$time=seq(2,length(case1$V1)+1)
    case1=rbind(c(1000,2000,2*mig_ratecm(),2000,2*mig_ratecm(),1),case1)
    case1$rep=i
    case_all=rbind(case_all,case1)
    }
    case_all
  })
  
  gen=reactive({input$gen})
  
  observeEvent(input$action,{
    case1=caseP1()
	case2=caseP2()
    output$traj1=renderPlot(ggplot(case1,aes(x=time,y=V1))+geom_line()+title("Player 1")+xlab("Time")+ylab("Total number of fish in the river"))
    output$traj2=renderPlot(ggplot(case2,aes(x=time,y=V1))+geom_line()+title("Player 2")+xlab("Time")+ylab("Total number of fish in the river"))
  })
  
  observeEvent(input$cheat,{
    case1=caseCM()
    output$traj1=renderPlot(ggplot(case1[case1$rep<5,],aes(x=time,y=V1,col=as.factor(rep)))+geom_line()+xlab("Time")+ylab("Total number of fish in the river")+scale_color_manual(values=c("black","purple","blue","orange","red"))+theme(legend.position = "none"))
    output$traj2=renderPlot(ggplot(case1[case1$rep>4,],aes(x=time,y=V1,col=as.factor(rep)))+geom_line()+xlab("Time")+ylab("Total number of fish in the river")+scale_color_manual(values=c("black","purple","blue","orange","red"))+theme(legend.position = "none"))
  })
  
  observeEvent(input$pie,{
      case1=caseP1()
	case2=caseP2()
	if (length(case1$V1)>=gen()){
	a=unlist(case1[gen(),])
  if(a[1]>0){
    pie_data=data.frame(gene=sapply(rep(seq(4),each=2),function(x) paste("Gene ",x)),allele=c("Invasive","Local","Local","Invasive","Invasive","Local","Local","Invasive"),freq=c(a[1]-a[2]/2,a[2]/2,a[1]-a[3]/2,a[3]/2,a[1]-a[4]/2,a[4]/2,a[1]-a[5]/2,a[5]/2)) # will need to edit this to reflect the code.

    output$pie1=renderPlot(ggplot(pie_data,aes(x="",y=freq,fill=allele))+geom_bar(stat="identity", width=1)+coord_polar("y", start=0) +facet_wrap(~gene)+theme_void()+title("Player 1")+scale_fill_manual(values=c("orange","purple")))
  } else {
    output$pie1=NULL
  }
  }
  if (length(case2$V1)>=gen()){
  b=unlist(case2[gen(),])
  if(b[1]>0){
    pie_data2=data.frame(gene=sapply(rep(seq(4),each=2),function(x) paste("Gene ",x)),allele=c("Invasive","Local","Local","Invasive","Invasive","Local","Local","Invasive"),freq=c(b[1]-b[2]/2,b[2]/2,b[1]-b[3]/2,b[3]/2,b[1]-b[4]/2,b[4]/2,b[1]-b[5]/2,b[5]/2)) # will need to edit this to reflect the code.
    
    output$pie2=renderPlot(ggplot(pie_data2,aes(x="",y=freq,fill=allele))+geom_bar(stat="identity", width=1)+coord_polar("y", start=0) +facet_wrap(~gene)+theme_void()+title("Player 2")+scale_fill_manual(values=c("orange","purple")))
  } else {
    output$pie2=NULL
  }
  }
  })
  
  observeEvent(input$reset, {
	output$traj1=NULL
	output$traj2=NULL
	output$pie1=NULL
	output$pie2=NULL

  })
}

  shinyApp(ui = ui, server = server)
