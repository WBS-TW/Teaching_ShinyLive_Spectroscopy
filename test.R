library(shiny)
library(plotly)
library(httr)
library(jsonlite)
library(bslib)

# Mock function to simulate API call for available compounds
get_available_compounds <- function() {
    # In a real application, this would make an actual API call
    # For demonstration, returning mock data
    list(
        compounds = c("Metan", "Etanol", "Bensen", "Vatten", "Koldioxid"),
        ids = c("methane", "ethanol", "benzene", "water", "co2")
    )
}

# Mock function to simulate API call for spectroscopy data
get_spectroscopy_data <- function(compound_id, spec_type) {
    # Simulate API delay
    Sys.sleep(0.5)
    
    # Mock spectroscopy data based on compound and type
    if (spec_type == "UV-Vis") {
        wavelength <- seq(200, 800, by = 2)
        if (compound_id == "methane") {
            absorbance <- 0.1 + 0.3 * exp(-0.01 * (wavelength - 300)^2) + rnorm(length(wavelength), 0, 0.02)
        } else if (compound_id == "ethanol") {
            absorbance <- 0.2 + 0.5 * exp(-0.008 * (wavelength - 280)^2) + rnorm(length(wavelength), 0, 0.03)
        } else if (compound_id == "benzene") {
            absorbance <- 0.15 + 0.8 * exp(-0.012 * (wavelength - 250)^2) + rnorm(length(wavelength), 0, 0.025)
        } else if (compound_id == "water") {
            absorbance <- 0.05 + 0.2 * exp(-0.015 * (wavelength - 200)^2) + rnorm(length(wavelength), 0, 0.01)
        } else {
            absorbance <- 0.3 + 0.4 * exp(-0.009 * (wavelength - 350)^2) + rnorm(length(wavelength), 0, 0.02)
        }
        return(data.frame(wavelength = wavelength, absorbance = pmax(0, absorbance)))
    } else {
        wavenumber <- seq(500, 4000, by = 10)
        if (compound_id == "methane") {
            transmittance <- 0.8 + 0.15 * sin(wavenumber/200) - 0.6 * exp(-0.0001 * (wavenumber - 2900)^2) + rnorm(length(wavenumber), 0, 0.02)
        } else if (compound_id == "ethanol") {
            transmittance <- 0.7 + 0.2 * sin(wavenumber/300) - 0.5 * exp(-0.0001 * (wavenumber - 3300)^2) - 0.4 * exp(-0.0002 * (wavenumber - 1050)^2) + rnorm(length(wavenumber), 0, 0.03)
        } else if (compound_id == "benzene") {
            transmittance <- 0.75 + 0.18 * sin(wavenumber/250) - 0.7 * exp(-0.0001 * (wavenumber - 3000)^2) - 0.5 * exp(-0.0002 * (wavenumber - 1500)^2) + rnorm(length(wavenumber), 0, 0.025)
        } else if (compound_id == "water") {
            transmittance <- 0.9 + 0.05 * sin(wavenumber/400) - 0.8 * exp(-0.0001 * (wavenumber - 3400)^2) - 0.3 * exp(-0.0003 * (wavenumber - 1600)^2) + rnorm(length(wavenumber), 0, 0.01)
        } else {
            transmittance <- 0.6 + 0.25 * sin(wavenumber/180) - 0.4 * exp(-0.0001 * (wavenumber - 2350)^2) + rnorm(length(wavenumber), 0, 0.02)
        }
        return(data.frame(wavenumber = wavenumber, transmittance = pmax(0.1, pmin(1, transmittance))))
    }
}

# UI
ui <- page_sidebar(
    title = "Spektroskopi - Interaktiv API-datavisning",
    sidebar = sidebar(
        card(
            card_header("Val av spektroskopi"),
            selectInput("specType", "Välj spektroskopityp:",
                        choices = c("UV-Vis", "IR")),
            br(),
            selectInput("compound", "Välj förening:",
                        choices = NULL),  # Will be populated dynamically
            br(),
            actionButton("loadData", "Ladda spektroskopi-data", 
                         class = "btn-primary"),
            br(), br(),
            helpText("Zooma och panorera i grafen för detaljerad analys.")
        )
    ),
    card(
        card_header("Spektrum"),
        plotlyOutput("specPlot", height = "400px")
    ),
    card(
        card_header("Förklaring"),
        verbatimTextOutput("specExplanation")
    ),
    card(
        card_header("API-status"),
        verbatimTextOutput("apiStatus")
    )
)

# Server
server <- function(input, output, session) {
    
    # Reactive values to store data
    values <- reactiveValues(
        compounds_data = NULL,
        spec_data = NULL,
        loading = FALSE,
        api_status = "Initierar..."
    )
    
    # Load available compounds on app start
    observe({
        values$api_status <- "Laddar tillgängliga föreningar från API..."
        
        # Simulate API call
        tryCatch({
            compounds_data <- get_available_compounds()
            values$compounds_data <- compounds_data
            
            # Update compound choices
            choices <- setNames(compounds_data$ids, compounds_data$compounds)
            updateSelectInput(session, "compound", choices = choices)
            
            values$api_status <- paste("API ansluten. Tillgängliga föreningar:", 
                                       length(compounds_data$compounds))
        }, error = function(e) {
            values$api_status <- paste("API-fel:", e$message)
        })
    })
    
    # Load spectroscopy data when button is clicked
    observeEvent(input$loadData, {
        req(input$compound, input$specType)
        
        values$loading <- TRUE
        values$api_status <- paste("Laddar", input$specType, "data för", 
                                   names(which(values$compounds_data$ids == input$compound)))
        
        # Simulate API call for spectroscopy data
        tryCatch({
            spec_data <- get_spectroscopy_data(input$compound, input$specType)
            values$spec_data <- spec_data
            values$loading <- FALSE
            values$api_status <- paste("Data laddad framgångsrikt för", 
                                       names(which(values$compounds_data$ids == input$compound)))
        }, error = function(e) {
            values$loading <- FALSE
            values$api_status <- paste("Fel vid datainläsning:", e$message)
        })
    })
    
    # Render plot
    output$specPlot <- renderPlotly({
        req(values$spec_data)
        
        compound_name <- names(which(values$compounds_data$ids == input$compound))
        
        if (input$specType == "UV-Vis") {
            plot_ly(data = values$spec_data, x = ~wavelength, y = ~absorbance, 
                    type = 'scatter', mode = 'lines',
                    line = list(color = 'blue', width = 2),
                    hovertemplate = "Våglängd: %{x} nm<br>Absorbans: %{y:.3f}<extra></extra>") %>%
                layout(title = paste("UV-Vis-spektrum för", compound_name),
                       xaxis = list(title = "Våglängd (nm)"),
                       yaxis = list(title = "Absorbans"),
                       showlegend = FALSE)
        } else {
            plot_ly(data = values$spec_data, x = ~wavenumber, y = ~transmittance, 
                    type = 'scatter', mode = 'lines',
                    line = list(color = 'red', width = 2),
                    hovertemplate = "Vågtal: %{x} cm⁻¹<br>Transmittans: %{y:.3f}<extra></extra>") %>%
                layout(title = paste("IR-spektrum för", compound_name),
                       xaxis = list(title = "Vågtal (cm⁻¹)"),
                       yaxis = list(title = "Transmittans"),
                       showlegend = FALSE)
        }
    })
    
    # Render explanation
    output$specExplanation <- renderText({
        if (!is.null(values$spec_data) && !is.null(input$compound)) {
            compound_name <- names(which(values$compounds_data$ids == input$compound))
            
            if (input$specType == "UV-Vis") {
                paste("UV-Vis-spektroskopi mäter absorbans av ljus i det ultravioletta och synliga området (200-800 nm).",
                      "Här visas", compound_name, "spektrum som laddats från API:n.",
                      "Toppar i spektrumet indikerar elektroniska övergångar i molekylen.")
            } else {
                paste("IR-spektroskopi detekterar molekylära vibrationer genom att mäta transmittans av infraröd strålning (500-4000 cm⁻¹).",
                      "Här visas", compound_name, "IR-spektrum från API-data.",
                      "Dalar i spektrumet (låg transmittans) indikerar karakteristiska bindningsvibrationer.")
            }
        } else {
            "Välj en förening och klicka på 'Ladda spektroskopi-data' för att se spektrum och förklaring."
        }
    })
    
    # Render API status
    output$apiStatus <- renderText({
        if (values$loading) {
            "Status: Laddar data från API..."
        } else {
            paste("Status:", values$api_status)
        }
    })
}

shinyApp(ui = ui, server = server)
