options(shiny.maxRequestSize=30*1024^2)

source("pairwise_intersect.R")

#sever code
shinyServer(function(input, output, session) {
  
  library(RColorBrewer)
  library(htmlwidgets)
  library(gplots)
  library(dendextend)

  
  #====================================================#
  ## Venn module ####
  #====================================================#
  venn_type <- reactive({
    return(input$venn_type)
  })
  
  doWeights <- reactive({
    return(input$doWeights)
  })
  
  doEuler <- reactive({
    return(input$doEuler)
  })
  
  venn_size <- reactive({
    return(input$venn_size)
  })
  
  venn_lwd <- reactive({
    return(as.numeric(input$venn_lwd))
  })
  
  venn_labelsize <- reactive({
    return(as.numeric(input$venn_labelsize))
  })
  
  venn_color_type <- reactive({
    return(input$venn_color_type)
  })
  
  
  venn_cex <- reactive({
    return(as.numeric(input$venn_cex))
  })
  
  venn_lty <- reactive({
    return(as.numeric(input$venn_lty))
  })
  
  set1_color <- reactive({
    return(input$set1_color)
  })
  set2_color <- reactive({
    return(input$set2_color)
  })
  set3_color <- reactive({
    return(input$set3_color)
  })
  set4_color <- reactive({
    return(input$set4_color)
  })
  set5_color <- reactive({
    return(input$set5_color)
  })
  set6_color <- reactive({
    return(input$set6_color)
  })
  
  
 
  venn_data <- reactive({
    inFile <- input$file_venn
    #string <- input$venn_comb
    string <- ""
    
    if(is.null(inFile) == F)
    {
      #data <- read.csv(input$file_venn$datapath, header = input$header_venn, sep = input$sep_venn)
      data <- read_delim(input$file_venn$datapath, input$sep_venn , escape_double = FALSE, trim_ws = TRUE, col_names = input$header_venn)
      data <- lapply(data, function(x) x[!is.na(x)])
      return(data)
    }else{
      if (string != "")
       {
         string <- gsub("\n", "", string)
         if(string != ""){
           string <- as.list(unlist(strsplit(string, ",")))
           names <- lapply(string, function(x){x <- unlist(strsplit(x, "=")); x <- x[1]})
           names <- unlist(lapply(names, function(x){x <- gsub(" ", "", x)}))
           vennData <- as.numeric(unlist(lapply(string, function(x){x <- unlist(strsplit(x,"=")); x <- x[2]})))
           names(vennData) <- names
           return(vennData)
         }else{
           return(NULL)
         }
       }
       else{
      #data <- read.csv('data/Whyte_et_al_2013_SEs_genes.csv', header = TRUE, sep = ',')
      data <- read_delim('data/Whyte_et_al_2013_SEs_genes.csv', ",", escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)
      return(lapply(data, function(x) x[!is.na(x)]))
      }
    }
  })
  
  set_names <- reactive({
    #names <- colnames(venn_data())
    names <- names(venn_data())
    #names <- sort(names)
    return(names)
  })
  
  output$venn_sets <- renderUI({
    venn_sets <- selectInput('venn_sets', label = "Select sets",
                         choices = as.character(set_names()),
                         multiple = T, selectize = T, selected = as.character(set_names()[1:5]))
    return(venn_sets)
  })
  
  venn_selected_names <- reactive({
    venn_selected_names <- as.character(c(input$venn_sets))
  })
  
  venn_data_filtered <- reactive({
    
    data <- venn_data()
    if(is.null(input$venn_sets)){
      
    }else{
      data <- data[c(venn_selected_names())]
      return(data)
    }
    return(data)
  })

  venn_combinations <- reactive({
    #string <- input$venn_comb
    string <- ""
    data <- venn_data_filtered()
    if (string !=""){
      return(data)
    }else
    {
      return(Venn(data))
    }
  })
  
  get_venn_gp <- reactive({
    venn_gp <- VennThemes(compute.Venn(venn_combinations()))
    venn_gp$SetText <- lapply(venn_gp$SetText,function(x) {x$fontsize<-venn_labelsize(); return(x)})
    venn_gp$FaceText <- lapply(venn_gp$FaceText,function(x) {x$cex<-venn_cex(); return(x)})
    venn_gp$Set <- lapply(venn_gp$Set,function(x) {x$lwd<-venn_lwd(); return(x)})
    venn_gp$Set <- lapply(venn_gp$Set,function(x) {x$lty<-venn_lty(); return(x)})
    
    if (venn_color_type () == 'custom'){
      venn_gp$Set$Set1$col <- set1_color()
      venn_gp$Set$Set2$col <- set2_color()
      venn_gp$Set$Set3$col <- set3_color()
      venn_gp$Set$Set4$col <- set4_color()
      venn_gp$Set$Set5$col <- set5_color()
      venn_gp$Set$Set6$col <- set6_color()
      
      venn_gp$SetText$Set1$col <- set1_color()
      venn_gp$SetText$Set2$col <- set2_color()
      venn_gp$SetText$Set3$col <- set3_color()
      venn_gp$SetText$Set4$col <- set4_color()
      venn_gp$SetText$Set5$col <- set5_color()
      venn_gp$SetText$Set6$col <- set6_color()
 
    }
    
    return(venn_gp)
  })
  
  data_size <- reactive({
      return(length(venn_data_filtered()))
  })

  get_venn_type <- reactive({
    if (venn_type() == 'Classical'){
      if(data_size() < 4)
        return("circles")
      else
        return("ellipses")
    }else if (venn_type() == 'ChowRuskey' && data_size() < 3){
      return("circles")
    }
      else{
      return(venn_type())
    }
  })

  output$vennPlot <- renderPlot({
    plot(compute.Venn(venn_combinations(), doWeights = doWeights(), doEuler = doEuler(), type = get_venn_type()),
    gp = get_venn_gp(),
    show = list(Universe = FALSE)
    )
      },
      width = venn_size,
      height = venn_size,
      outputArgs = list()
  )
  
  output$VennDown <- downloadHandler(
    filename = function(){
      paste("Venn_diagram", tolower(input$filetype_venn), sep =".")
    }, 
    content = function(file){
      width  <- venn_size()
      height <- venn_size()
      #width  <- session$clientData$output_plot_width
      #height <- ((session$clientData$output_plot_height)*1)
      #pixelratio <- session$clientData$pixelratio
      pixelratio <- 2
      
      if(input$filetype_venn == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio, units = "px", res=72*pixelratio)
      else if(input$filetype_venn == "SVG")
        svg(file, width=8, height=8)
      else if(input$filetype_venn == "TIFF")
        tiff(file, width=width*pixelratio, height=height*pixelratio, units = "px")
      else
        pdf(file, width = 8, height = 8)
      
      plot(venn_combinations(),
           doWeights = doWeights(),
           type = get_venn_type(),
           doEuler = doEuler(),
           show = list(Universe = FALSE)
           #venn_type()
      )
      dev.off()
    }
  )
  
  #====================================================#
  ## UpSet module ####
  #====================================================#
  #Some of the code for upset module is taken from
  #https://github.com/hms-dbmi/UpSetR-shiny

  output$plot_text <- renderUI({
    if(is.null(My_data()) == T){
      h5("There is no data entered. Please upload your data to draw UpSet plot here!")
    }
    else{
      HTML(" ")
    }
  })
  
  My_dat <- reactive({  
    inFile <- input$file1
    input_type = input$upset_input_type
    if (is.null(inFile) == T){
      
      My_dat<- fromExpression(c('H3K4me2&H3K4me3'=16321,'H3K4me2&H3K4me3&H3K27me3'=5756,'H3K27me3'=25174,'H3K4me3&H3K27me3'=15539,'H3K4me3'=32964,'H3K4me2&H3K27me3'=19039,'H3K4me2'=60299,'H3K27ac&H3K4me2&H3K4me3&H3K27me3'=7235,'H3K27ac&H3K4me2&H3K4me3'=17505,'H3K27ac&H3K4me2'=21347,'H3K27ac&H3K4me2&H3K27me3'=1698,'H3K27ac&H3K4me3'=8134,'H3K27ac&H3K4me3&H3K27me3'=295,'H3K27ac&H3K27me3'=7605,'H3K27ac'=42164))
      #read.csv("data/ENCODE_hESC_HMs.txt", header = TRUE, sep = '\t')
      return(My_dat)
    }
    else if(is.null(inFile) == F && input_type == 'binary'){
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
      }else if (is.null(inFile) == F && input_type == 'list'){
        #My_dat <- fromList(convertColsToList(read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)))
        #removed NAs
        #My_dat <- fromList(lapply(as.list(read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)), function(x) x[!is.na(x)]))
        My_dat <- read_delim(inFile$datapath, input$sep , escape_double = FALSE, trim_ws = TRUE, col_names = input$header)
        My_dat <- fromList(lapply(as.list(My_dat), function(x) x[!is.na(x)]))
      
        return(My_dat)
    }else{
      return(NULL)
    }
  })
  
  venneulerData <- reactive({
    string <- input$upset_comb
    string <- gsub("\n", "", string)
    if(string != ""){
      string <- as.list(unlist(strsplit(string, ",")))
      names <- lapply(string, function(x){x <- unlist(strsplit(x, "=")); x <- x[1]})
      names <- unlist(lapply(names, function(x){x <- gsub(" ", "", x)}))
      values <- as.numeric(unlist(lapply(string, function(x){x <- unlist(strsplit(x,"=")); x <- x[2]})))
      names(values) <- names
      venneuler <- fromExpression(values)
      return(venneuler)
    }
  })
  
  My_data <- reactive({
    string <- input$upset_comb
    if(string != ""){
      My_data <- venneulerData()
    }
    else {
      My_data <- My_dat()
      }
    return(My_data)
  })
  
  FindStartEnd <- function(data){
    startend <- c()
    for(i in 1:ncol(data)){
      column <- data[, i]
      column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1" && (length(column) == 2))){
        startend[1] <- i
        break
      }
      else{
        next
      }
    }
    for(i in ncol(data):1){
      column <- data[ ,i]
      column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1") && (length(column) == 2)){
        startend[2] <- i
        break
      }
      else{
        next
      }
    }
    return(startend)
  }
  
  startEnd <- reactive({
    startEnd <- FindStartEnd(My_data())
  })
  
  setSizes <- reactive({
    if(is.null(My_data()) != T){
      sizes <- colSums(My_data()[startEnd()[1]:startEnd()[2]])
      sizes <- sizes[order(sizes, decreasing = T)]
      
      names <- names(sizes); sizes <- as.numeric(sizes);
      maxchar <- max(nchar(names))
      total <- list()
      for(i in 1:length(names)){
        spaces <- as.integer((maxchar - nchar(names[i]))+1)
        spaces <- paste(rep(" ", each=spaces), collapse = "")
        total[[i]] <- paste(paste(names[i], ":", sep=""), spaces, sizes[i], "\n", sep="")
      }
      total <- unlist(total)
      total <- paste(total, collapse = " ")
      return(total)
    }
    else{
      return(NULL)
    }
  })
  
  output$setsizes <- renderText({
    if(is.null(setSizes()) != T){
      paste("---Set Sizes---\n", setSizes())
    }
    else{
      paste("---Set Sizes---\n", "\n No Data Entered")
    }
  })
  
  Specific_sets <- reactive({
    Specific_sets <- as.character(c(input$upset_sets))
  })
  
  output$sets <- renderUI({
    if(is.null(My_data()) == T){
      sets <-  selectInput('upset_sets', label="Select at least two sets ",
                           choices = NULL,
                           multiple=TRUE, selectize=TRUE, selected = Specific_sets())
    }
    else{
      data <- My_data()[startEnd()[1]:startEnd()[2]]
      topfive <- colSums(data)
      topfive <- as.character(head(names(topfive[order(topfive, decreasing = T)]), 5))
      sets <- selectInput('upset_sets', label="Select sets ",
                          choices = as.character(colnames(My_data()[ , startEnd()[1]:startEnd()[2]])),
                          multiple=TRUE, selectize=TRUE, selected = topfive)
    }
    return(sets)
  })
  
  
  mat_prop <- reactive({
    mat_prop <- input$mbratio
  })
  upset_width <- reactive({
    return(input$upset_width)
  })
  upset_height <- reactive({
    return(input$upset_height)
  })
  
  bar_prop <- reactive({
    bar_prop <- (1 - input$mbratio)
  })
  
  orderdat <- reactive({
    orderdat <- as.character(input$order)
    if(orderdat == "degree"){
      orderdat <- c("degree")
    }
    else if(orderdat == "freq"){
      orderdat <- "freq"
    }
    return(orderdat)
  })
  
  show_numbers <- reactive({
    show_numbers <- input$show_numbers
    if(show_numbers){
      show_numbers <- "yes"
      return(show_numbers)
    }
    else{
      show_numbers <- FALSE
      return(show_numbers)
    }
    
  })
  
  main_bar_color <- reactive({
    mbcolor <- input$mbcolor
    return(mbcolor)
  })
  sets_bar_color <- reactive({
    sbcolor <- input$sbcolor
    return(sbcolor)
  })

  
  decrease <- reactive({
    decrease <- as.character(input$decreasing)
    if(decrease == "inc"){
      decrease <- FALSE
    }
    else if(decrease == "dec"){
      decrease <- TRUE
    }
    return(decrease)
  })
  
  number_angle <- reactive({
    angle <- input$angle
    return(angle)
  })
  
  line_size <- reactive({
    line_size <- input$linesize
    return(line_size)
  })
  
  emptyIntersects <- reactive({
    if(isTRUE(input$empty)){choice <- "on"
    return(choice)
    }
    else{
      return(NULL)
    }
  })
  
  scale.intersections <- reactive({
    return(input$scale.intersections)
  })
  
  scale.sets <- reactive({
    return(input$scale.sets)
  })
  
  keep.order <- reactive({
    return(input$keep.order)
  })
  
  # A plot of fixed size
  output$plot1 <- renderPlot({
    
    if(length(My_data()) == 0){stop()}
    if(length(Specific_sets()) == 1){
      stop()
    }
    upset(data = My_data(), 
          nintersects = input$nintersections,
          point.size = input$pointsize,
          line.size = line_size(),
          sets = Specific_sets(),
          order.by = orderdat(),
          main.bar.color= main_bar_color(),
          sets.bar.color= sets_bar_color(),
          decreasing = c(decrease()),
          show.numbers = show_numbers(),
          number.angles = number_angle(),
          scale.intersections = scale.intersections(),
          scale.sets = scale.sets(),
          keep.order = keep.order(),
          mb.ratio = c(as.double(bar_prop()), as.double(mat_prop())),
          empty.intersections = emptyIntersects(),
          text.scale = c(input$intersection_title_scale, input$intersection_ticks_scale,
                         input$set_title_scale, input$set_ticks_scale, input$names_scale,
                         input$intersection_size_numbers_scale))},
    #width  <- session$clientData$output_plot_width
    #height <- ((session$clientData$output_plot_height)*1.7)
    width = upset_width,
    height = upset_height
    )
  
  #outputOptions(output, "plot", suspendWhenHidden = FALSE)
  
  # observe({
  #   if(pushed$B != 0 && length(pushed$B) == 1){
  #     updateTabsetPanel(session, "main_panel", "upset_plot")
  #   }
  # })
  
  output$UpSetDown <- downloadHandler(
    
    filename = function(){
      paste("UpSet_plot", tolower(input$filetype), sep =".")
    }, 
    content = function(file){
      width <- upset_width()
      height <- upset_height()
      pixelratio <- 2
      
      #width  <- session$clientData$output_plot_width
      #height <- ((session$clientData$output_plot_height)*2)
      #pixelratio <- session$clientData$pixelratio
      if(input$filetype == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype == "SVG")
        svg(file, width = width/100, height = height/100)
      else if(input$filetype == "TIFF")
        tiff(file, width=width*pixelratio, height=height*pixelratio, units = "px")
      else
        pdf(file, width = width/100, height = height/100, onefile=FALSE)
      
      upset(data = My_data(), 
            nintersects = input$nintersections,
            point.size = input$pointsize,
            line.size = line_size(),
            
            sets = Specific_sets(),
            order.by = orderdat(),
            main.bar.color= main_bar_color(),
            sets.bar.color= sets_bar_color(),
            decreasing = c(decrease()),
            number.angles = number_angle(),
            show.numbers = show_numbers(),
            scale.intersections = scale.intersections(),
            scale.sets = scale.sets(),
            keep.order = keep.order(),
            mb.ratio = c(as.double(bar_prop()), as.double(mat_prop())),
            empty.intersections = emptyIntersects(),
            text.scale = c(input$intersection_title_scale, input$intersection_ticks_scale,
                           input$set_title_scale, input$set_ticks_scale, input$names_scale,
                           input$intersection_size_numbers_scale))
      
      dev.off()
    }
  )
  
  #====================================================#
  ## Pairwise module ####
  #====================================================#
  output$plot_text_p <- renderUI({
    if(is.null(pairwiseMatrix()) == T){
      h5("There is no data entered. Please upload your data to draw pairwise heatmap here!")
    }
    else{
      HTML(" ")
    }
  })
  
  corplot_method <- reactive({
    return(input$corp_method)
  })
  
  corplot_type <- reactive({
    return(input$corp_type)
  })
  
  corplot_order <- reactive({
    return(input$corp_order)
  })
  
  corplot_diag <- reactive({
    return(input$corp_diag)
  })
  
  corplot_tl.col <- reactive({
    return(input$tl_col)
  })
  
  corplot_title <- reactive({
    return(input$corp_title)
  })
  
  heatmap_size <- reactive({
    return(input$heatmap_size)
  })
  
  addrect <- reactive({
    return(input$addrect)
  })
  
  rect_col <- reactive({
    return(input$rect_col)
  })
  
  hclust_method <- reactive({
    return(input$hclust_method)
  })

  
  tl_pos <- reactive({
    if (corplot_type() =="lower" && input$tl_pos != 'n')
    {
      return('ld')
      
    }else if(corplot_type() =="upper" && input$tl_pos != 'n'){
      return('td')
    }else{
    return(input$tl_pos)
    }
  })
  
  cl_pos <- reactive({
    if (corplot_type() =="lower" && input$cl.pos != 'n')
    {
      return('b')
      
    }else if(corplot_type() =="upper" && input$cl.pos != 'n'){
      return('r')
    }else{
      return(input$cl.pos)
    }
  })
  
  addgrid_col <- reactive({
    return(input$addgrid_col)
  })
  
  tl_srt <- reactive({
    return(input$tl.srt)
  })
  
  tl_cex <- reactive({
    return(input$tl.cex)
  })
  
  cl_cex <- reactive({
    return(input$cl.cex)
  })
  
  lower_colour <- reactive({
    return(input$lower_colour)
  })
  middle_colour <- reactive({
    return(input$middle_colour)
  })
  higher_colour <- reactive({
    return(input$higher_colour)
  })
  
  heamap_colors <- reactive({
    if(input$color_type == 'custom'){
      colors = colorRampPalette(c(lower_colour(), middle_colour(), higher_colour()))(100)
    }else{
      colors = colorRampPalette(brewer.pal(9,input$color_type))(100)
    }
    return(colors)
  })
  
  

  dendrogram <- reactive({
    return(input$dendrogram)
  })
  symm <- reactive({
    return(input$symm)
  })
  key <- reactive({
    return(input$key)
  })
  keysize <- reactive({
    return(input$keysize)
  })
  key.title <- reactive({
    return(input$key.title)
  })
  key.xlab <- reactive({
    return(input$key.xlab)
  })
  key.ylab <- reactive({
    return(input$key.ylab)
  })

  distance <- reactive({
    if(input$distance == 'none'){
      distance= as.dist(pairwiseMatrix())    
    }else{
      distance= dist(pairwiseMatrix(), method =input$distance)    
    }
    return(distance)
  })
  
  is_correlation<- reactive({
    isCor <- input$corp_cor
    if(isCor == 'non'){
      return(FALSE)
    }else
    {
      return(TRUE)
    }
  })

  pairwiseMatrix <- reactive({
    inFile <- input$file_p
    isCor <- input$corp_cor
    input_type <- input$pairwise_input_type
    if (is.null(inFile)){
      
      myMatrix <- as.matrix(read.table("data/frac_pairwise_matrix.txt"))
      if(isCor != 'non'){
        myMatrix <- cor(myMatrix, method=isCor)
      }
      return (myMatrix)
    }else{
      if(input_type == 'matrix'){
        
        myMatrix <- as.matrix(read.table(inFile$datapath))
        
      }else{
        #myMatrix <- as.matrix(pairwise_intersect(read.csv(inFile$datapath, header = input$header_p, sep=input$sep_p)))
        myMatrix <- as.matrix(pairwise_intersect(lapply(as.list(read_delim(inFile$datapath, input$sep_p , escape_double = FALSE, trim_ws = TRUE, col_names = input$header_p)), function(x) x[!is.na(x)])))
        #myMatrix <- as.matrix(pairwise_intersect(lapply(as.list(read.csv(inFile$datapath, header = input$header_p, sep=input$sep_p)), function(x) x[!is.na(x)])))
      }

      if(isCor != 'non'){
        myMatrix <- cor(myMatrix, method=isCor)
      }
    return(myMatrix)
    }
  })
  
  min_limit <- reactive({
    isCor <- input$corp_cor
    if(isCor == 'non'){
      return(0)
    }else
    {
      return(-1)
    }
  })
  
  max_limit <- reactive({
    isCor <- input$corp_cor
    input_type <- input$pairwise_input_type
    
    if(isCor == 'non' && input_type == 'list'){
      return(as.integer(max(pairwiseMatrix())))
    }else
    {
      return(1)
    }
  })

  output$pairwiseTable = DT::renderDataTable(
    round(pairwiseMatrix(),2), options = list(
      lengthChange = TRUE
      )
  )
  
  heatmap2_plot <- reactive({
    hcluster <- hclust(distance(), method =hclust_method())
    dend1 <- as.dendrogram(hcluster)
    # get some colors
    dend1 <- color_branches(dend1, k = addrect())
    col_labels <- get_leaves_branches_col(dend1)
    # order of the data!    
    col_labels <- col_labels[order(order.dendrogram(dend1))]
    
    plt <- heatmap.2(pairwiseMatrix(),
              scale = "none",
              #dendrogram = "both",
              col = heamap_colors(),
              cexRow = tl_cex(),
              cexCol = tl_cex(),
              #srtRow = tl_srt(),
              srtCol = tl_srt(),
              Rowv = dend1, 
              Colv = dend1, 
              main = corplot_title(),
              dendrogram = dendrogram(),
              symm = symm(),
              #revC = TRUE,
              key = key(),
              keysize = keysize(),
              key.title = key.title(),
              key.xlab =  key.xlab(),
              key.ylab = key.ylab(),
              key.par = list(cex=cl_cex()),
              sepwidth = c(0.05, 0.05),  # width of the borders
              mar=c(6,6),
              sepcolor = addgrid_col(),
              colsep =1:ncol(pairwiseMatrix()),
              rowsep =1:nrow(pairwiseMatrix()),
              #offsetRow = 0.1,
              #offsetCol = 0.1,
              trace="none",
              #RowSideColors = col_labels, #colored strips        
              colRow = col_labels,
              ColSideColors = col_labels, #colored strips        
              colCol = col_labels
    )
   return(plt)
  })
  
  output$heatmap2_plot_out <- renderPlot(
    heatmap2_plot(),
                                     
  width= heatmap_size,
  height= heatmap_size
  )
  
  output$Heatmap2PlotDown <- downloadHandler(
    filename = function(){
      paste("Pairwise_heatmap2", tolower(input$filetype_heatmap), sep =".")
    }, 
    content = function(file){
      width  <- heatmap_size()
      height <- heatmap_size()
      #width  <- session$clientData$output_plot_width
      #height <- ((session$clientData$output_plot_height)*2)
      pixelratio <- 2
      if(input$filetype_heatmap == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_heatmap == "SVG")
        svg(file, width=12, height=12)
      else if(input$filetype_heatmap == "TIFF")
        tiff(file, width=width*pixelratio, height=height*pixelratio, units = "px")
      else
        pdf(file, width = 12, height = 12)
      
      hcluster <- hclust(distance(), method =hclust_method())
      dend1 <- as.dendrogram(hcluster)
      # get some colors
      dend1 <- color_branches(dend1, k = addrect())
      col_labels <- get_leaves_branches_col(dend1)
      # order of the data!    
      col_labels <- col_labels[order(order.dendrogram(dend1))]
      
      heatmap.2(pairwiseMatrix(),
                scale = "none",
                #dendrogram = "both",
                col = heamap_colors(),
                cexRow = tl_cex(),
                cexCol = tl_cex(),
                #srtRow = tl_srt(),
                srtCol = tl_srt(),
                Rowv = dend1, 
                Colv = dend1, 
                main = corplot_title(),
                dendrogram = dendrogram(),
                symm = symm(),
                #revC = TRUE,
                key = key(),
                keysize = keysize(),
                key.title = key.title(),
                key.xlab =  key.xlab(),
                key.ylab = key.ylab(),
                key.par = list(cex=cl_cex()),
                sepwidth = c(0.05, 0.05),  # width of the borders
                mar=c(6,6),
                sepcolor = addgrid_col(),
                colsep =1:ncol(pairwiseMatrix()),
                rowsep =1:nrow(pairwiseMatrix()),
                #offsetRow = 0.1,
                #offsetCol = 0.1,
                trace="none",
                #RowSideColors = col_labels, #colored strips        
                colRow = col_labels,
                ColSideColors = col_labels, #colored strips        
                colCol = col_labels
      )
      dev.off()
    }
  )
  
  d3HM_plot <- reactive({
    hcluster = hclust(distance(), method =hclust_method())
    dend1 <- as.dendrogram(hcluster)
    
    # get some colors
    #cols_branches <- c("darkred", "forestgreen", "orange", "blue")
    #cols_branches <- brewer.pal(addrect(), "Set1")
    
    # Set the colors of 4 branches
    #dend1 <- color_branches(dend1, k = addrect(), col = cols_branches[1:addrect()])
    dend1 <- color_branches(dend1, k = addrect())
    
    col_labels <- get_leaves_branches_col(dend1)
    # But due to the way heatmap.2 works - we need to fix it to be in the 
    # order of the data!    
    col_labels <- col_labels[order(order.dendrogram(dend1))]
    d3heatmap(pairwiseMatrix(),
              show_grid = TRUE,
              scale = "none",
              dendrogram = dendrogram(),
              anim_duration = 0,
              k_row = addrect(),
              k_col = addrect(),
              Rowv = dend1, 
              Colv = dend1,
              symm = symm(),
              #revC = FALSE,
              #hclustfun = function(x) hclust(x,method = hclust_method()),
              #distfun = function(x) dist(x,method = hclust_method()),
              colors = heamap_colors(),
              xaxis_font_size = "12px",
              yaxis_font_size = "12px"
              #xaxis_height = 8,
              #yaxis_width = 8
    )
  })
  
  output$d3HM <- renderD3heatmap(d3HM_plot())
  
  output$HeatmapHTMLDown <- downloadHandler(
    filename = function(){
    paste("Interactive_pairwise_heatmap", "html", sep =".")
  }, 
    content = function(file){
    saveWidget(d3HM_plot(), file)
  }
  )
  
  output$corrplotHM <- renderPlot({
    corrplot(pairwiseMatrix(),
             method = corplot_method(),
             title = corplot_title(),
             tl.col= corplot_tl.col(),
             cl.lim=c(min_limit(),max_limit()),
             is.corr = is_correlation(),
             diag = corplot_diag(),
             order = corplot_order(),
             hclust.method = hclust_method(),
             type = corplot_type(),
             addrect = addrect(),
             tl.pos = tl_pos(),
             cl.pos = cl_pos(),
             rect.col = rect_col(),
             addgrid.col= addgrid_col(),
             tl.cex = tl_cex(),
             cl.cex = cl_cex(),
             tl.srt = tl_srt(),
             mar=c(0,0,2,2),
             col = heamap_colors()
             #addCoef.col = "red",
             #col = col1(300)
             #col = list(color = brewer.pal(20, "RdBu"))
             
             )},
  width= heatmap_size,
  height= heatmap_size
  )
  
  output$HeatmapCSVDown <- downloadHandler(
    filename = function(){
    paste("Pairwise_matrix", "csv", sep =".")
  }, 
   content = function(file){
    write.csv(pairwiseMatrix(), file)
  }
  )
  
  
  output$HeatmapDown <- downloadHandler(
    filename = function(){
      paste("Pairwise_heatmap", tolower(input$filetype_heatmap), sep =".")
    }, 
    content = function(file){
      width  <- heatmap_size()
      height <- heatmap_size()
      #width  <- session$clientData$output_plot_width
      #height <- ((session$clientData$output_plot_height)*2)
      pixelratio <- 2
      if(input$filetype_heatmap == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_heatmap == "SVG")
        svg(file, width=12, height=12)
      else if(input$filetype_heatmap == "TIFF")
        tiff(file, width=width*pixelratio, height=height*pixelratio, units = "px")
      else
        pdf(file, width = 12, height = 12)
      
      corrplot(pairwiseMatrix(),
               method = corplot_method(),
               title = corplot_title(),
               tl.col= corplot_tl.col(),
               cl.lim=c(min_limit(),max_limit()),
               is.corr = is_correlation(),
               diag = corplot_diag(),
               order = corplot_order(),
               hclust.method = hclust_method(),
               type = corplot_type(),
               addrect = addrect(),
               tl.pos = tl_pos(),
               cl.pos = cl_pos(),
               rect.col = rect_col(),
               addgrid.col= addgrid_col(),
               tl.cex = tl_cex(),
               cl.cex = cl_cex(),
               tl.srt = tl_srt(),
               mar=c(0,0,2,1),
               #addCoef.col = "red",
               col = heamap_colors()
               #col = col3(100)
      )
      dev.off()
    }
  )
  
}
)
