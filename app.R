# Dependencies
library(shiny)
library(dplyr)
library(readxl)
library(shinyjqui)
library(shinyWidgets)
library(heatmaply)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel('Correlation matrices on demand'),
  sidebarLayout(
    sidebarPanel(
      fileInput("input_file", "Upload Excel File (.xlsx)", accept = ".xlsx"),
      fileInput("meta_file", "Upload Excel File (.xlsx)", accept = ".xlsx"),
      tags$hr(),
      colorPickr("deg_color", "DEGs Color", selected = "orange"),
      colorPickr("bg_color", "Background Color", selected = "grey")
    ),
    mainPanel(
      jqui_resizable(
        plotlyOutput("cor_mat", height = "600px", width = "100%")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  input_data <- reactive({
    req(input$input_file)
    read_excel(input$input_file$datapath)
  })
  
  metadata <- reactive({
    req(input$meta_file)
    read_excel(input$meta_file$datapath)
    
  })
  
  output$cor_mat <- renderPlotly({
    print('Ok')
    req(input_data())

    gradient_col <- ggplot2::scale_fill_gradient2(
      low = "blue", high = "red", 
      midpoint = 0, limits = c(-1, 1)
    )

    
    meta <- metadata() %>% 
      mutate(condition = group) %>% 
      mutate(samples = sample)
    
    # Input matrix
    print('Dist running')
    normalized_counts_d_r <- dist(t(input_data() %>% 
                                      select(meta$samples)),
                                  diag = TRUE,
                                  upper = TRUE) %>% 
      as.matrix() %>% 
      cor()
    print('Dist OK')
    # Colnames and rownames pasted with its condition
    colnames(normalized_counts_d_r) <- paste(meta$condition, meta$samples, sep = "-")
    rownames(normalized_counts_d_r) <- colnames(normalized_counts_d_r)
    sorted_normalized_counts_d_r <- normalized_counts_d_r[order(rownames(normalized_counts_d_r)), order(colnames(normalized_counts_d_r), decreasing = FALSE)]
    
    # Relative height and width to the number of samples
    normalized_counts_d_r_h = nrow(sorted_normalized_counts_d_r) * 0.45
    normalized_counts_d_r_w = ncol(sorted_normalized_counts_d_r) * 0.55    
    
    # Side annotations
    side_annot <- sub("-.*", "", colnames(sorted_normalized_counts_d_r))
    
    side_annot_top <- sub("-.*", "", rownames(sorted_normalized_counts_d_r))
    
    # Construct the heatmap
    normalized_counts_d_r_heatmap <- heatmaply(
      sorted_normalized_counts_d_r,
      scale_fill_gradient_fun = gradient_col,
      col_side_colors = data.frame("Condition" = side_annot_top),
      row_side_colors = data.frame("Condition" = side_annot),
      limits = c(-1,1),
      Rowv = FALSE,
      Colv = FALSE,
      showticklabels = TRUE,
      row_side_palette = colorRampPalette(c('#0C4B8E', '#BF382A'), bias=1),
      col_side_palette = colorRampPalette(c('#0C4B8E', '#BF382A'), bias=1),
      key.title = 'corr',
      colorbar_len = 0.01,
      side_color_colorbar_len = 0.01,
      subplot_heights = c(0.05,0.95),
      subplot_widths = c(0.95,0.05),
      fontsize_row = 7,
      fontsize_col = 7) 
    
    # Remove the duplicated labels caused by two sided annotations and remove the label
    s <- subplot(normalized_counts_d_r_heatmap, margin = .01, titleY = TRUE) %>% 
      config(displayModeBar = FALSE)
    s$x$data[[1]]$showlegend <- FALSE
    s$x$data[[11]]$showlegend <- FALSE
    #s$x$layout$annotations[[2]]$text <- ''
    #s$x$layout$annotations[[2]]$y <- 0.9
    #s$x$layout$annotations[[2]]$x <- 1.026
    print('plot returned')
    return(s)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
