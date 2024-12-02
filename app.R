# app.R
# Pastikan untuk menambahkan library plotly di bagian awal script
library(plotly)

# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(tidyr)
library(leaflet)
library(readr)
library(stringr)

# Data loading function with proper error handling
load_house_data <- function() {
  tryCatch({
    # Use the exact file name from your directory
    data_path <- "data_new.csv"
    
    if (!file.exists(data_path)) {
      stop("Data file not found at: ", data_path)
    }
    
    data <- read_csv(data_path)
    
    # Data preprocessing
    data <- data %>%
      na.omit() %>%
      mutate(
        price_billion = price_in_rp / 1e9,  # Convert to billions
        price_per_sqm = price_in_rp / building_size_m2,
        location = factor(str_to_title(city)),  # Capitalize location names
        floors = floors,
        furnishing = factor(furnishing),
        lat = lat,
        long = long,
        district = district,
        address = address,
        carports = carports,
        electricity = electricity,
        property_condition = property_condition,
        building_orientation = building_orientation,
        garages = garages
      ) %>%
      filter(
        price_billion > 0,
        building_size_m2 > 0
      )
    
    return(data)
  }, error = function(e) {
    message("Error loading data: ", e$message)
    # Create sample data if file not found
    message("Creating sample data instead...")
    
    # Generate sample data
    set.seed(123)  # for reproducibility
    sample_data <- data.frame(
      location = rep(c("Jakarta", "Bogor", "Depok", "Tangerang", "Bekasi"), each=20),
      price = runif(100, 500000000, 5000000000),
      building_size_m2 = runif(100, 36, 500),
      land_size_m2 = runif(100, 60, 1000)
    ) %>%
      mutate(
        price_billion = price / 1e9,
        price_per_sqm = price / building_size_m2,
        location = factor(location)
      )
    
    return(sample_data)
  })
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Analisis Harga Rumah Jabodetabek"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analisis Harga", tabName = "price_analysis", icon = icon("chart-line")),
      menuItem("Sebaran Harga Rumah", tabName = "price_distribution", icon = icon("map-marker")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      selectInput("city_filter_overview", "Pilih Kota:", choices = NULL)
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #dfd7d0;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("avg_price", width = 3),
                valueBoxOutput("med_price", width = 3),
                valueBoxOutput("total_listings", width = 3),
                valueBoxOutput("avg_size", width = 3)
              ),
              fluidRow(
                box(
                  title = "Distribusi Harga Rumah",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("price_dist"),
                  width = 8
                ),
                box(
                  title = "Statistik per Wilayah",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("region_stats"),
                  width = 4
                ),
                box(
                  title = "Distribusi Rata-rata Luas Rumah",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("volume_dist"),
                  width = 6
                ),
                box(
                  title = "Proporsi Furnishing",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("furnishing_pie"),
                  width = 6
                )
              )
      ),
      
      # Price Distribution Tab
      tabItem(tabName = "price_distribution",
              fluidRow(
                box(
                  title = "Peta Sebaran Harga Rumah",
                  status = "primary",
                  solidHeader = TRUE,
                  leafletOutput("price_distribution_map"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = "Statistik Harga per Kecamatan",
                  status = "primary",
                  solidHeader = TRUE,
                  uiOutput("region_dropdown"),
                  plotOutput("district_price_stats"),
                  width = 6
                ),
                box(
                  title = "Boxplot Harga per Kecamatan",
                  status = "primary", 
                  solidHeader = TRUE,
                  plotOutput("district_price_boxplot"),
                  width = 6
                )
              )
      ),
      
      # Price Analysis Tab
      tabItem(tabName = "price_analysis",
              fluidRow(
                box(
                  title = "Kontrol",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  uiOutput("region_selector"),
                  uiOutput("price_range_slider"),
                  uiOutput("size_input")
                ),
                box(
                  title = "Hubungan Harga dan Luas Bangunan",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("price_size_scatter"),
                  width = 9
                )
              ),
              fluidRow(
                box(
                  title = "Harga per Meter Persegi per Wilayah",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("price_per_sqm"),
                  width = 12
                )
              )
      ),
      
      # Data Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Dataset Rumah Jabodetabek",
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("house_data"),
                  width = 12
                )
              )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Load data
  houses_data <- tryCatch({
    load_house_data()
  }, error = function(e) {
    showNotification(
      "Error loading data. Please check your internet connection.",
      type = "error"
    )
    return(NULL)
  })
  
  # Observer untuk mengisi pilihan kota
  observe({
    req(houses_data)
    cities <- c("Semua", unique(houses_data$location))
    updateSelectInput(session, "city_filter_overview", choices = cities)
  })
  
  # Fungsi untuk memfilter data berdasarkan kota di overview
  filtered_data_overview <- reactive({
    req(houses_data, input$city_filter_overview)
    
    data <- houses_data
    
    # Filter berdasarkan kota jika tidak dipilih "Semua"
    if (input$city_filter_overview != "Semua") {
      data <- data %>% filter(location == input$city_filter_overview)
    }
    
    return(data)
  })
  
  # Dynamic UI elements
  output$region_selector <- renderUI({
    req(houses_data)
    regions <- c("Semua", unique(houses_data$location))
    selectInput("region", "Pilih Wilayah:", choices = regions)
  })
  
  output$price_range_slider <- renderUI({
    req(houses_data)
    max_price <- ceiling(max(houses_data$price_billion))
    sliderInput("price_range", "Range Harga (Miliar):",
                min = 0, max = max_price, value = c(0, max_price))
  })
  
  output$size_input <- renderUI({
    req(houses_data)
    numericInput("min_size", "Luas Minimum (m²):",
                 value = 0, min = 0)
  })
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(houses_data, input$region, input$price_range, input$min_size)
    
    data <- houses_data
    
    if (input$region != "Semua") {
      data <- data %>% filter(location == input$region)
    }
    
    data %>%
      filter(
        price_billion >= input$price_range[1],
        price_billion <= input$price_range[2],
        building_size_m2 >= input$min_size
      )
  })
  
  # Value Boxes
  output$avg_price <- renderValueBox({
    req(filtered_data())
    avg <- mean(filtered_data()$price_billion)
    valueBox(
      paste0("Rp ", round(avg, 2), " M"),
      "Rata-rata Harga",
      icon = icon("money-bill"),
      color = "green"
    )
  })
  
  output$med_price <- renderValueBox({
    req(filtered_data())
    med <- median(filtered_data()$price_billion)
    valueBox(
      paste0("Rp ", round(med, 2), " M"),
      "Median Harga",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$total_listings <- renderValueBox({
    req(filtered_data())
    valueBox(
      nrow(filtered_data()),
      "Total Listing",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$avg_size <- renderValueBox({
    req(filtered_data())
    avg <- mean(filtered_data()$building_size_m2)
    valueBox(
      paste0(round(avg, 0), " m²"),
      "Rata-rata Luas Bangunan",
      icon = icon("home"),
      color = "red"
    )
  })
  
  # Plots
  output$price_dist <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = price_billion)) +
      geom_histogram(fill = "red", bins = 30) +
      theme_minimal() +
      labs(
        title = "Distribusi Harga Rumah",
        x = "Harga (Miliar Rupiah)",
        y = "Jumlah"
      )
  })
  
  output$volume_dist <- renderPlot({
    req(filtered_data())
    
    # Gradient warna
    color_gradient <- scale_fill_gradient(
      low = "#f19314",   # Warna biru muda
      high = "#e712ea"   # Warna biru tua
    )
    
    # Alternatif: Menggunakan warna bertingkat
    ggplot(filtered_data(), aes(x = building_size_m2, fill = ..count..)) +
      geom_histogram(
        bins = 30, 
        color = "white",   # Border putih untuk memisahkan batang
        alpha = 0.7        # Sedikit transparansi
      ) +
      scale_x_log10() +   # Log-scale untuk mengatasi skewness
      color_gradient +    # Gradient warna berdasarkan jumlah
      labs(
        title = "Distribusi Luas Rumah di Jabodetabek", 
        x = "Luas Rumah (m2, log-scale)", 
        y = "Jumlah"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(color = "darkgrey"),
        legend.position = "right"
      )
  })
  
  # Plot Statistik per Wilayah
  output$region_stats <- renderPlot({
    req(filtered_data_overview())
    filtered_data_overview() %>%
      group_by(location) %>%
      summarise(avg_price = mean(price_billion)) %>%
      ggplot(aes(x = reorder(location, -avg_price), y = avg_price)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)
      ) +
      labs(
        title = "Rata-rata Harga per Wilayah",
        x = "Wilayah",
        y = "Harga (Miliar)"
      ) +
      coord_cartesian(expand = TRUE)
  })
  
  output$price_size_scatter <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = building_size_m2, y = price_billion)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "Hubungan Harga dan Luas Bangunan",
        x = "Luas Bangunan (m²)",
        y = "Harga (Miliar Rupiah)"
      )
  })
  
  output$price_per_sqm <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = location, y = price_per_sqm)) +
      geom_boxplot(fill = "lightblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Distribusi Harga per Meter Persegi",
        x = "Wilayah",
        y = "Harga per m² (Juta Rupiah)"
      )
  })
  
  # Dalam server function
  output$furnishing_pie <- renderPlotly({
    req(houses_data)
    
    # Hitung proporsi setiap kategori furnishing
    furnishing_data <- houses_data %>%
      count(furnishing) %>%
      mutate(
        Percentage = n / sum(n) * 100,
        label = paste0(furnishing, "\n", round(Percentage, 1), "%")
      )
    
    # Buat pie chart 3D menggunakan plotly
    plot_ly(
      data = furnishing_data,
      type = "pie",
      labels = ~label,
      values = ~Percentage,
      textinfo = "label",
      hoverinfo = "none",
      hole = 0.3,  # Menambahkan efek donat untuk kesan 3D
      marker = list(
        colors = c("#66c2a5", "#fc8d62", "#8da0cb"),  # Pilihan warna yang berbeda
        line = list(color = "#FFFFFF", width = 2)
      )
    ) %>%
      layout(
        title = list(
          text = "Proporsi Furnishing",
          font = list(size = 16)
        ),
        showlegend = TRUE,
        margin = list(t = 50, b = 50, l = 50, r = 50)
      ) %>%
      # Tambahkan efek 3D
      config(displayModeBar = FALSE)
  })
  
  # Tambahkan fungsi render di dalam server function
  # Peta Sebaran Harga
  output$price_distribution_map <- renderLeaflet({
    req(houses_data)
    
    # Tentukan warna berdasarkan rentang harga
    color_palette <- colorNumeric(
      palette = "YlOrRd", 
      domain = houses_data$price_billion
    )
    
    leaflet(houses_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~long, 
        lat = ~lat,
        radius = 5,
        color = ~color_palette(price_billion),
        fillOpacity = 0.7,
        popup = paste(
          "Lokasi:", houses_data$address, 
          "<br>Harga:", round(houses_data$price_billion, 2), "Miliar",
          "<br>Luas Bangunan:", round(houses_data$building_size_m2, 0), "m²"
        )
      ) %>%
      addLegend(
        pal = color_palette, 
        values = ~price_billion,
        title = "Harga Rumah (Miliar)"
      )
  })
  
  # Statistik Harga per Kecamatan
  output$district_price_stats <- renderPlot({
    req(houses_data)
    
    # Define the cities in Jabodetabek region
    jabodetabek_cities <- c(
      "Jakarta Selatan", "Jakarta Pusat", "Jakarta Barat", 
      "Jakarta Timur", "Jakarta Utara", "Bogor", "Tangerang", 
      "Bekasi", "Depok", "Tangerang Selatan"
    )
    
    # Create a reactive value to store the selected region
    selected_region <- reactiveVal("semua")
    
    # UI code to create the dropdown menu
    output$region_dropdown <- renderUI({
      selectInput("region", "Pilih Wilayah:", 
                  choices = c("Semua", jabodetabek_cities), 
                  selected = selected_region())
    })
    
    # Filter the data based on the selected region
    district_stats <- houses_data %>%
      filter(
        if (input$region == "Semua") TRUE else city %in% input$region
      ) %>%
      group_by(district) %>%
      summarise(
        avg_price = mean(price_billion),
        median_price = median(price_billion)
      ) %>%
      arrange(desc(avg_price))
    
    # Update the selected_region reactive value
    selected_region(input$region)
    
    ggplot(district_stats, aes(x = reorder(district, -avg_price), y = avg_price)) +
      geom_bar(stat = "identity", fill = "coral") +
      theme_minimal() +
      labs(
        title = "Rata-rata Harga Rumah per Kecamatan di Jabodetabek",
        x = "Kecamatan",
        y = "Harga Rata-rata (Miliar)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Boxplot Harga per Kecamatan
  output$district_price_boxplot <- renderPlot({
    req(houses_data)
    
    ggplot(houses_data, aes(x = city, y = price_billion)) +
      geom_boxplot(fill = "skyblue") +
      theme_minimal() +
      labs(
        title = "Distribusi Harga Rumah per Kecamatan",
        x = "Kecamatan",
        y = "Harga (Miliar)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Data Table
  output$house_data <- renderDT({
    req(filtered_data())
    filtered_data() %>%
      select(district, location, price_billion, building_size_m2, land_size_m2, floors, furnishing, garages) %>%
      rename(
        "Distrik" = district,
        "Lokasi" = location,
        "Luas Bangunan (m²)" = building_size_m2,
        "Luas Tanah (m²)" = land_size_m2,
        "Jumlah Lantai" = floors,
        "Status Perabotan" = furnishing,
        "Jumlah Garasi" = garages,
        "Harga (Rp. Miliar)" = price_billion
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)