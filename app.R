# Fair Mitigation Explorer Webapp ----------------------------------------------
# Setu Pelz (TISS, ECE, IIASA, pelz@iiasa.ac.at)
#
# Enables interactive analysis of the work presented in "Fairness considerations 
# in Global Mitigation Investments", by Shonali Pachauri, Setu Pelz, 
# Christoph Bertram, Silvie Kreibiehl, Narasimha D. Rao, Youba Sokona and Keywan 
# Riahi. Science (2022), 10.1126/science.adf0067.

# LOAD NECESSARY PACKAGES ------------------------------------------------------

# install.packages("pacman")
library(pacman)

p_load(dplyr, ggplot2, shiny, DT, ggrepel, tidyr, shinycssloaders, shinythemes,
       networkD3, patchwork, readr, readxl, forcats)

# LOAD DATA --------------------------------------------------------------------

# Standard region ordering for figures and setting name abbreviations
rgnorder <- tibble(
  region = c("South-East Asia and developing Pacific", "Middle East",
             "Asia-Pacific Developed", "Africa", "Southern Asia", 
             "Latin America and Caribbean", "Eastern Europe and West-Central Asia", 
             "Eastern Asia", "Europe", "North America"),
  regionshort = c("SAP", "MEA", "APD", "AFR", "SAS", "LAC", "EEA", "EAS", "EUR", "NAM"))

# Set standard equity principle indicator names and order
equitynames <- 
  tibble(indicator = c("1850 CO2FFI", "1990 CO2FFI", 
                       "GDP per Capita (2019)", "Capital stock per Capita (2019)",
                       "DLS deprivation (2015)", "Climate risk (2030)"),
         indicatorunits = c("1850-2019 CO2FFI (Gt)", "1990-2019 CO2FFI (Gt)", 
                            "GDP per cap. (2019) ($ PPP 2017)", "Capital stock per cap. (2019) ($ PPP 2017)",
                            "DLS deprivation (2015) (avg. % deprived)", "Climate risk (2030) (% at risk)"),
         indicatorshort = paste0(c(rep(c("R", "C", "N"), each = 2)), rep(c(1:2), 3)),
         principle = c(rep(rep(c("Responsibility", "Capability", "Needs"), each = 2))),
         principleshort = c(rep(c("R", "C", "N"), each = 2)))

# Set original equal weights across principles
tbl_weights <- data.frame(principleshort = c("R", "C", "N"),
                          weights = c(1/3,1/3,1/3))

# Read in necessary datasets (see replication archive for creation)
equity_shares <- read_csv("equity_shares.csv") %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort))

equity_raw <- read_csv("equity_raw.csv") %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort))

popgdp <- read_csv("popgdp_r10.csv") %>% 
  mutate(gdpshare = gdp2017ppp / sum(gdp2017ppp))

# # Debugging input tibble
# input <- tibble(res = "1850 CO2FFI", cap = "GDP per Capita (2019)", 
# ned = "DLS deprivation (2015)", highlow = "CE_low")

# APP UI -----------------------------------------------------------------------

# Introduction tab
intro_tab <- tabPanel(
  title = "Introduction",
  h3("Welcome"),
  hr(),
  fluidRow(
  column(7,
  HTML("<p>Welcome to the webapp accompanying 'Fairness considerations in global mitigation 
  investments' by Shonali Pachauri, Setu Pelz, Christoph Bertram, Silvie Kreibiehl, 
    Narasimha D. Rao, Youba Sokona and Keywan Riahi. Published in Science (2022), 
    <a href='https://science.org/doi/10.1126/science.adf0067'target = 'blank'>10.1126/science.adf0067</a>.</p>"),
  HTML("<p>This webapp enables users to systematically explore 'fair' regional contributions to 
    near-term regional mitigation investment needs (2020-2030) derived from global 
    cost-effective mitigation pathways assessed by Working Group III within the 
    <a href='https://www.ipcc.ch/report/sixth-assessment-report-working-group-3/', 
    target = 'blank'>IPCC AR6</a>.</p>"),
  p("Users can select from a range of indicators reflecting different 
    considerations of equity and assign these weights. The tool then determines 
    corresponding 'fair-share' regional contributions to the regional 'cost-effective' 
    investment needs defined within Figure TS.25 in the Technical Summary of the WGIII contribtion to the IPCC AR6."),
  p("Users should select considerations and set desired weights starting from top 
  to bottom such that these sum to 100 (or as near as possible). 
  Weights are relative such that user inputs are proportionally transformed into 
    weights between 0-100, summing to 100 across all three considerations."),
  p("Users should also select between the lower- and upper-bounds of near-term 
    global climate mitigation investment needs as defined within Figure TS.25 in 
    the Technical Summary of the IPCC WGIII AR6 report."),
  p("Please note that regions are defined as per the IPCC WGIII Report as follows: 
    \nSAP - South-East Asia and developing Pacific, \nMEA - Middle East, 
    \nAPD - Asia-Pacific Developed, \nAFR - Africa, \nSAS - Southern Asia, 
    \nLAC - Latin America and Caribbean, \nEEA - Eastern Europe and West-Central Asia, 
    \nEAS - Eastern Asia, \nEUR - Europe, \nNAM - North America. \nPlease refer 
    to the manuscript SI for further country-level detail."),
  p("To continue, navigate to the 'Tool' tab and click 'Calculate' after setting 
    the desired parameters. Please note that the webapp is not intended to be 
    viewed on tablets and may not function correctly.")
  ),
  
  column(5,
         img(src='introfig.png', height="500px")
         )
  
  )
)

# Publication tab
publication_tab <- tabPanel(
  title = "Publication",
  h3("Fairness considerations in global mitigation investments"),
  hr(),
  column(12, fluidRow(
    HTML("<p>Publication: <a href='https://science.org/doi/10.1126/science.adf0067'target = 'blank'>https://science.org/doi/10.1126/science.adf0067</a></p>"),
    HTML("<p>Replication archive: <a href='https://doi.org/10.5281/zenodo.7308573'target = 'blank'>https://doi.org/10.5281/zenodo.7308573</a></p>"),
    p("Authors: Shonali Pachauri¹, Setu Pelz¹, Christoph Bertram², Silvie Kreibiehl³, 
    Narasimha D. Rao¹,⁴, Youba Sokona⁵,⁶,  Keywan Riahi¹ "),
    p("Affiliations: ¹International Institute for Applied Systems Analysis, 
    ²Potsdam Institute for Climate Impact Research, ³Germanwatch e.V., ⁴Yale School of the Environment, 
    ⁵South Centre, ⁶University College London"),
    p("We will provide an abstract at time of publication."))
  )
)

# Related tools tab
related_tab <- tabPanel(
  title = "Other tools",
  h3("Other climate equity tools"),
  hr(),
  column(6, fluidRow(
    p("A vast climate equity literature has developed to inform discussions of 
      carbon budget sharing and equitable emissions reductions, applying several 
      different equity approaches and principles. We provide here a list 
      of other available climate equity tools, noting that these efforts are 
      generally distinct from our work on fairness in near-term mitigation 
      investment finance contributions. This list does not indicate endorsement nor
      comprehensiveness."),
    tags$ul(
      tags$li(a("https://calculator.climateequityreference.org/", href="https://calculator.climateequityreference.org/", target="_blank")), 
      tags$li(a("https://climateequitymonitor.in", href="https://climateequitymonitor.in", target="_blank")), 
      tags$li(a("https://climateactiontracker.org/countries/rating-system/", href="https://climateactiontracker.org/countries/rating-system/", target="_blank")),
      tags$li(a("https://paris-equity-check.org/multi-equity-map.html", href="https://paris-equity-check.org/multi-equity-map.html", target="_blank")), 
      tags$li(a("http://www.ccalc.ethz.ch", href="http://www.ccalc.ethz.ch", target="_blank"))
    )))
  )

# Webapp tab
tool_tab <- tabPanel(
  
  title = "Tool", fluid = TRUE, value = "tool",
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$head(
        tags$style(".shiny-plot-output{height:70vh !important;}"),
        tags$style(type='text/css', ".selectize-input { font-size: 14px; line-height: 14px;} .selectize-dropdown { font-size: 14px; line-height: 14px; }")),
      span(strong("How would you determine 'fair' inter-regional mitigation finance contributions?"),
           div(style = "display:inline-block; font-size:16px, title-size:20px",
               title = "Select from the indicators for each of the equity considerations and weight these such that weights approximately sum to 100. Selected considerations are combined using selected weights and used to distribute regional contributions to cost-effective mitigation investment needs taken from the WGIII AR6 Technical Summary Figure 25.",
               icon("info-circle"))),
      div(style = "margin-top:-15px"),
      hr(),
      div(style = "margin-top:-15px"),
      selectInput(
        inputId = "res",
        label = 
          span("Responsibility",
               div(style = "display:inline-block; font-size:16px",
                   title = "Responsibility: selecting higher values here indicates that you believe regions with larger historical emissions should carry a proportionally larger share of contributions to finance global mitigation investment. We quantify this consideration using historical emissions across two time periods, 1850-2019 and 1990-2019, and focus on CO2-FFI emissions as the dominant long-lived climate forcer given the historical nature of the analysis.",
                   icon("info-circle"))),
        choices = list(
          `R1 - 1850-2019 CO2FFI` = "1850 CO2FFI",
          `R2 - 1990-2019 CO2FFI` = "1990 CO2FFI"
        )
      ),
      div(style = "margin-top:-15px"),
      sliderInput(
        inputId = "slider1",
        label = NULL,
        min = 0L,
        max = 100L,
        value = 30L,
        width = '100%',
        step = 10L
      ),
      div(style = "margin-top:-15px"),
      hr(),
      div(style = "margin-top:-15px"),
      selectInput(
        inputId = "cap",
        label = 
          span("Capability",
               div(style = "display:inline-block; font-size:16px",
                   title = "Capability: selecting higher values here indicates that you believe  wealthier and thus more capable regions should carry a proportionally larger share of contributions to finance global mitigation. We quantify this consideration using per-capita GDP and per-capita capital stock. For both of these indicators we take the value for the year 2019, the year before the decade of analysis (2020-2030)",
                   icon("info-circle"))),
        choices = list(
          `C1 - GDP per capita (2019)` = "GDP per Capita (2019)",
          `C2 - Capital stock per capita (2019)` = "Capital stock per Capita (2019)"
        )
      ),
      div(style = "margin-top:-15px"),
      uiOutput(outputId = "slider2"),
      div(style = "margin-top:-15px"),
      hr(),
      div(style = "margin-top:-15px"),
      selectInput(
        inputId = "ned",
        label = 
          span("Need",
               div(style = "display:inline-block; font-size:16px",
                   title = "Need: selecting higher values here indicates that you believe regions with higher deprivation or climate risk should contribute a proportionally smaller share to finance global mitigation. We quantify this consideration firstly using the degree of multi-dimensional deprivation in access to Decent Living Standards (DLS) (Rao & Min, 2018; Kikstra et al., 2021); and secondly, as acute multi-sector climate risk (Byers et al., 2018).",
              icon("info-circle"))),
        choices = list(
          `N1 - DLS deprivation (2015)` = "DLS deprivation (2015)",
          `N2 - Climate risk (2030)` = "Climate risk (2030)"
        )
      ),
      div(style = "margin-top:-15px"),
      uiOutput(outputId = "slider3"),
      div(style = "margin-top:-15px"),
      hr(),
      div(style = "margin-top:-15px"),
      selectInput(
        inputId = "highlow",
        label = 
          span(
            "AR6 TS.25 cost-effective investment need bounds",
            div(style = "display:inline-block; width = 20px",
                title = 
                "Select from the lower- and upper-bound of the estimated regional cost-effective annual mitigation investment needs in the decade 2020-2030, provided in Figure 25 of the IPCC WGIII AR6 Technical Summary.",
                icon("info-circle"))),
        choices = list(
          Lower = "CE_low",
          Upper = "CE_high"
        )
      ),
      div(style = "margin-top:-15px"),
      actionButton(
        inputId = "do",
        label = "Calculate"
      )
    ),
    
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel("'Fair-share' contributions", 
                 fluidRow(
                   h4("WGIII AR6 regional cost-effective investment needs and 'fair-share' contributions 2020-2030 in Billion USD"),
                   column(8, plotOutput("fig_r10cont", height = "70%")),
                   column(4, plotOutput("fig_r10cont_shares")),
                   hr(),
                   helpText("Regional cost-effective investment needs and 'fair-share' regional contributions 2020-2030 in Billion USD. IPCC R10 defined as follows: SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America."),
                   hr(),
                   h4("Regional cost-effective investment needs and 'fair-share' contributions 2020-2030 in Billion USD"),
                   DTOutput("tbl_r10cont"),
                   hr(),
                 )),
        tabPanel("'Fair-share' contributions as share of regional GDP", 
                 fluidRow(
                   h4("WGIII AR6 regional cost-effective investment needs and 'fair-share' contributions 2020-2030, as share of regional GDP"),
                   column(8, plotOutput("fig_r10cont_sharegdp", height = "70%")),
                   column(4, plotOutput("fig_r10cont_shares_sharegdp")),
                   hr(),
                   helpText("Regional cost-effective investment needs and 'fair-share' regional contributions 2020-2030 as a share of the regional GDP in 2019. IPCC R10 defined as follows: SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America."),
                   h4("Regional cost-effective investment needs and 'fair-share' contributions 2020-2030 as share of regional GDP in 2019"),
                   DTOutput("tbl_r10cont_sharegdp"),
                   hr()
                 )),
        tabPanel("Sankey diagram", 
                 fluidRow(
                   h4("Within-region and inter-regional contributions to global cost-effective mitigation investment needs"),
                   p("This diagram describes annual within-region and inter-regional contributions necessary to align regional cost-effective needs with 'fair-share' contributions in the decade 2020-2030, in billion USD PPP 2015. Flows are directional from left to right. Hover your cursor over flows and nodes for further detail."),
                   sankeyNetworkOutput("sankey"),
                   div(style = "margin-top:-20px"),
                   helpText("IPCC R10 defined as follows: SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America.")
                 )), 
        tabPanel("Indicator aggregates", 
                 h4("Equity consideration indicator values, distinct 'fair-share' contributions and weighted 'fair-share' contributions"),
                 tabsetPanel(
                   tabPanel("Indicators", plotOutput("equitydataplot_a")),
                   tabPanel("Indicator allocations", plotOutput("equitydataplot_b")),
                   tabPanel("Weighted aggregate allocation", plotOutput("equitydataplot_c"))),
                 helpText("Visualisation of individual indicators, corresponding indicator allocations and weighted allocation aggregate used to determine 'fair-share' regional finance contributions to global cost-effective mitigation investment needs. See paper for methods used to transform original indicator aggregates into the shares shown here. IPCC R10 defined as follows: SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America.")
                 )
      ) #ends tabsetpanel
    ) # ends mainpanel
  ) # ends sidebarlayout
  ) # ends tool tab

# App UI setup
ui <- 
  navbarPage("Fair Mitigation Finance Explorer", theme = shinytheme("lumen"),
  id = "tabs", # must give id here to add/remove tabs in server
  collapsible = TRUE,
  intro_tab, tool_tab, publication_tab, related_tab
)

# APP SERVER -------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Set dynamic sliders
  output$slider2 <- renderUI ({
    sliderInput("slider2", label = NULL, min = 0,  max = 100 - input$slider1, value = 30, step = 10, width = '100%')  
  })
  output$slider3 <- renderUI ({
    sliderInput("slider3", label = NULL, min = 0,  max = 100 - input$slider1 - input$slider2, value = 30, step = 10, width = '100%')  
  })
  
  # Analysis method following "calculate"
  observeEvent(input$do, {
    
    # Adjust weights following user input
    tbl_weights <<- tbl_weights <- 
      data.frame(principleshort = c("R", "C", "N"),
                 weights = c(input$slider1, input$slider2, input$slider3)) %>%
      mutate(weights = weights / sum(weights))
    
    # Set investment bounds and select principles following user input
    equity_shares_select <- equity_shares %>% 
      select(regionshort, CE = input$highlow, Recent = Recent,
             input$res, input$cap, input$ned)
    
    # Weight principles by user input weights
    equity_shares_select[,4] <- equity_shares_select[,4] * tbl_weights[1,2]
    equity_shares_select[,5] <- equity_shares_select[,5] * tbl_weights[2,2]
    equity_shares_select[,6] <- equity_shares_select[,6] * tbl_weights[3,2]
    
    # Aggregate weight
    equity_shares_select <- equity_shares_select %>% 
      mutate(finalweight = .[[4]] + .[[5]] + .[[6]])
    
    # Determine regional annual mitigation investment contributions ------------
    regionalmitigation <-  equity_shares_select %>% 
      transmute(regionshort = regionshort, 
                CE = CE, # cost-effective needs
                Recent = Recent, # estimated recent within-region contributions
                FS = sum(CE) * finalweight, # total 'fair-share' contribution
                FSCEdiff = FS - CE) %>% # CE - FS
      mutate(FSIRC_inflow = - ifelse(FSCEdiff < 0, FSCEdiff, 0), # FS gap
             FSIRC_outflow = ifelse(FSCEdiff > 0, FSCEdiff, 0), # FS debt
             FS_inregion = CE - FSIRC_inflow) # FS within-region (including recent)
    
    # Regional annual mitigation investment contributions ----------------------
    fig_r10cont <- regionalmitigation %>% 
     select(regionshort, CE, FS, Recent, FS_inregion, FSIRC_inflow, FSIRC_outflow) %>% 
     mutate(across(c(-regionshort), ~ . )) %>% 
     pivot_longer(-c(regionshort, CE, FS, Recent)) %>% 
     mutate(name = factor(name, levels = c("FS_inregion", "FSIRC_inflow", "FSIRC_outflow", "Recent"),
                          labels = c("Within-region contributions to mitigation needs", 
                                     "Mitigation needs met by inter-regional contributions", 
                                     "Inter-regional contributions to mitigation needs outside the region", 
                                     "Recent average within-region mitigation investments"))) %>% 
     arrange(regionshort) %>% 
     ggplot(aes(x = regionshort)) +
      geom_col(aes( y = value, fill = fct_rev(name)), width = 0.8, alpha = 1, position = "stack") +
      geom_point(aes(y = CE, shape = "AR6 WGIII regional cost-effective mitigation investment needs"), size = 3,
                data = . %>% filter(name == "Within-region contributions to mitigation needs")) +
      geom_col(aes(y = Recent), fill = "darkgrey", show.legend = F, width = 0.6,
              data = . %>% filter(name == "Within-region contributions to mitigation needs")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), position = "left") +
      scale_fill_manual(values = c("darkgrey", "#DDAA33", "#BB5566","#4477AA"), drop = F) +
      labs(x = NULL, y = expression("Billion USD PPP 2015 \u00D7 year "^{-1}),
          fill = NULL,
          shape = NULL) +
     theme_minimal() +
     theme(
       legend.position = c(0.04,1.05), 
       legend.justification = c(0, 1),
       legend.margin = unit(0.01, "cm"),
       axis.ticks.x = element_line(size = 0.5),
       axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
       plot.caption = element_text(hjust = 0, size = 8),
       plot.margin = margin(0.7,.1,.1,.1, "cm")) +
     guides(
       fill = guide_legend(nrow = 4, reverse = T),
       shape = guide_legend(order = 1))

   fig_weights <- tbl_weights %>%
     left_join(equitynames %>% filter(indicator %in% c(input$res, input$cap, input$ned))) %>% 
     mutate(principleshort = factor(principleshort, levels = c("R", "C", "N"), 
                                    label  = paste0(indicatorshort)),
            weights = round(weights * 100)) %>% 
     ggplot(aes(principleshort, weights, label = round(weights, 2))) +
     geom_col(fill = "#9ebcda") +
     geom_label(aes(y = weights / 2)) +
     geom_text(x = +Inf, y = c(100,90,80), hjust = 1.01, vjust = 1.5,
              aes(label = paste0(indicatorshort, " - ", indicator))) +
     scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
     theme_bw() +
     labs(x = NULL, y = "Weight", subtitle = "Weighted equity considerations")
   
   fig_r10contsum <- fig_r10cont$data %>% 
     pivot_wider(names_from = name, values_from = value) %>% 
     mutate(across(-regionshort, ~round(.,2))) %>% 
     summarise("Total AR6 global\nmitigation needs" = sum(CE),
               "Total 'fair' inter-\nregional contributions" = sum(`Inter-regional contributions to mitigation needs outside the region`)) %>% 
     pivot_longer(everything()) %>% 
     mutate(name = factor(name, levels = c("Total AR6 global\nmitigation needs", "Total 'fair' inter-\nregional contributions"))) %>% 
     ggplot(aes(name, value / 1000, label = round(value / 1000, 2))) +
     geom_col(fill = "#9ebcda") +
     geom_label() +
     scale_y_continuous(breaks = scales::pretty_breaks()) +
     theme_bw() +
     labs(x = NULL, y = expression("Trillion USD PPP 2015 \u00D7 year "^{-1}), 
          subtitle = "Global inter-regional contributions")
    
    output$tbl_r10cont <- renderDT(fig_r10cont$data %>% 
                                     pivot_wider(names_from = name, values_from = value) %>% 
                                     mutate(across(-regionshort, ~round(.,2))) %>% 
                                     select(Region = regionshort, 
                                            `Recent average investments` = Recent,
                                            `WGIII AR6 investment needs` = CE, `'Fair' regional contributions` = FS, 
                                            `.. Within-region` = `Within-region contributions to mitigation needs`, 
                                            `.. Inter-regional` = `Inter-regional contributions to mitigation needs outside the region`
                                     ),
                                     rownames = FALSE, options = list(dom = 't'))
    
    output$fig_r10cont <- renderPlot(fig_r10cont, res = 110)
    output$fig_r10cont_shares <- renderPlot({fig_weights/fig_r10contsum}, res = 80)
    
    # Regional annual mitigation investment contributions as share of GDP ------
    fig_r10cont_sharegdp <- regionalmitigation %>% 
      select(regionshort, CE, FS, Recent, FS_inregion, FSIRC_inflow, FSIRC_outflow) %>% 
      left_join(popgdp %>% select(regionshort, gdp2017ppp)) %>% 
      mutate(across(-c(regionshort,gdp2017ppp), ~ . / (gdp2017ppp / 1e9)),
             regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>%
      select(-gdp2017ppp) %>% 
      pivot_longer(-c(regionshort, CE, FS, Recent)) %>% 
      mutate(name = factor(name, levels = c("FS_inregion", "FSIRC_inflow", "FSIRC_outflow", "Recent"),
                           labels = c("Within-region contributions to mitigation needs", 
                                      "Mitigation needs met by inter-regional contributions", 
                                      "Inter-regional contributions to mitigation needs outside the region", 
                                      "Recent average within-region mitigation investments"))) %>% 
      arrange(regionshort) %>% 
      ggplot(aes(x = regionshort)) +
      geom_col(aes( y = value, fill = fct_rev(name)), width = 0.8, alpha = 1, position = "stack") +
      geom_point(aes(y = CE, shape = "AR6 WGIII regional cost-effective mitigation investment needs"), size = 3,
                 data = . %>% filter(name == "Within-region contributions to mitigation needs")) +
      geom_col(aes(y = Recent), fill = "darkgrey", show.legend = F, width = 0.6,
               data = . %>% filter(name == "Within-region contributions to mitigation needs")) +
      scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0,ifelse(input$highlow == "CE_low", 0.04,0.08)), labels = scales::percent_format(accuracy = 1), 
                         position = "left") +
      scale_fill_manual(values = c("darkgrey", "#DDAA33", "#BB5566","#4477AA"), drop = F) +
      labs(x = NULL, y = "Share of regional GDP (2019)",
           fill = NULL,
           shape = NULL) +
      theme_minimal() +
      theme(
        legend.position = c(0.04,1.05), 
        legend.justification = c(0, 1),
        legend.margin = unit(0.01, "cm"),
        axis.ticks.x = element_line(size = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        plot.caption = element_text(hjust = 0, size = 8),
        plot.margin = margin(0.7,.1,.1,.1, "cm")) +
      guides(
        fill = guide_legend(nrow = 4, reverse = T),
        shape = guide_legend(order = 1))
    
    fig_r10contsum_sharegdp <- fig_r10cont$data %>% 
      pivot_wider(names_from = name, values_from = value) %>% 
      mutate(across(-regionshort, ~round(. ,2))) %>% 
      summarise("Total AR6 global\nmitigation needs" = sum(CE) / sum(popgdp$gdp2017ppp / 1e9),
                "Total 'fair' inter-\nregional contributions" = sum(`Inter-regional contributions to mitigation needs outside the region`) / sum(popgdp$gdp2017ppp / 1e9)) %>% 
      pivot_longer(everything()) %>% 
      mutate(name = factor(name, levels = c("Total AR6 global\nmitigation needs", 
                                            "Total 'fair' inter-\nregional contributions"))) %>% 
      ggplot(aes(name, value, label = scales::percent(value, accuracy = .01))) +
      geom_col(fill = "#9ebcda") +
      geom_label() +
      scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::percent_format(accuracy = .1)) +
      theme_bw() +
      labs(x = NULL, y = "Share of global GDP (2019)", subtitle = "Global inter-regional contributions")
      
    output$tbl_r10cont_sharegdp <- renderDT(fig_r10cont_sharegdp$data %>% 
                                    pivot_wider(names_from = name, values_from = value) %>% 
                                      mutate(across(-regionshort, ~scales::percent(.,.01))) %>% 
                                      select(Region = regionshort, 
                                             `Recent average investments` = Recent,
                                             `WGIII AR6 investment needs` = CE, `'Fair' regional contributions` = FS, 
                                             `.. Within-region` = `Within-region contributions to mitigation needs`, 
                                             `.. Inter-regional` = `Inter-regional contributions to mitigation needs outside the region`
                                      ),
                                    rownames = FALSE, options = list(dom = 't'))
    
    output$fig_r10cont_sharegdp <- renderPlot(fig_r10cont_sharegdp, res = 110)
    output$fig_r10cont_shares_sharegdp <- renderPlot({fig_weights/fig_r10contsum_sharegdp}, res = 80)
    
    # Indicator and aggregate plots --------------------------------------------
    a <- equity_raw %>% 
      select(regionshort, input$res, input$cap, input$ned) %>% 
      pivot_longer(-regionshort, names_to = "indicator") %>% 
      left_join(equitynames) %>% 
      left_join(tbl_weights) %>% 
      mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort),
             indicator = factor(indicator, levels = equitynames$indicator,
                                labels = paste0(equitynames$indicatorshort, " - ", equitynames$indicatorunits)),
             indicatorused = ifelse(weights > 0,"y","n"),
             indicatorused = factor(indicatorused, levels = c("y","n"))) %>% 
      ggplot(aes(regionshort, value, fill = indicatorused, drop = F)) +
      geom_col(show.legend = F, width = .7) +
      geom_text(aes(x = regionshort, y = value, label = regionshort), 
                colour = "black", angle = 90, hjust = 0, size = 3) +
      facet_wrap(~indicator, ncol = 3, scales = "free_y") +
      scale_y_continuous(breaks = scales::pretty_breaks(),
                         expand = expansion(mult = c(0, 0.1))) +
      scale_fill_manual(values = c("#756bb1", "#bdbdbd")) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(x = NULL, y = NULL,
           title = "Regional indicators in original data units")
    
    b <- equity_shares %>% 
      select(regionshort, input$res,input$cap, input$ned) %>% 
      pivot_longer(-regionshort, names_to = "indicator") %>% 
      left_join(equitynames) %>% 
      left_join(tbl_weights) %>% 
      mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort),
             indicator = factor(indicator, levels = equitynames$indicator, 
                                labels = paste0(equitynames$indicatorshort, " - ", equitynames$indicator)),
             indicatorused = ifelse(weights > 0,"y","n"),
             indicatorused = factor(indicatorused, levels = c("y","n"))) %>% 
      ggplot(aes(regionshort, value, fill = indicatorused, drop = F)) +
      geom_col(show.legend = F, width = .7) +
      geom_text(aes(x = regionshort, y = value, label = regionshort), 
                colour = "black", angle = 90, hjust = 0, size = 3) +
      facet_wrap(~indicator, ncol = 3) +
      scale_fill_manual(values = c("#756bb1", "#bdbdbd")) +
      scale_y_continuous(breaks = scales::pretty_breaks(),
                         labels = scales::percent_format(),
                         limits = c(0,0.4),
                         expand = expansion(mult = c(0, 0.1))) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(x = NULL, y = "Contribution to global mitigation needs",
           title = "Corresponding regional indicator allocations")
    
    c <- equity_shares_select %>% 
      mutate(regionshort = factor(regionshort, levels = c(rgnorder$regionshort))) %>% 
      ggplot(aes(regionshort, finalweight)) +
      geom_col(show.legend = F, width = .7, fill = "#756bb1") +
      geom_text(aes(x = regionshort, y = finalweight, label = regionshort), 
                colour = "black", angle = 90, hjust = 0, size = 3) +
      scale_y_continuous(breaks = scales::pretty_breaks(),
                         labels = scales::percent_format(),
                         limits = c(0,0.4),
                         expand = expansion(mult = c(0, 0.1))) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(x = NULL, y = "Contribution to global mitigation needs",
           title = "Weighted aggregate allocation of regional contributions to global cost-effective investment needs")
    
    output$equitydataplot_a <- renderPlot(a, res = 90)
    output$equitydataplot_b <- renderPlot(b, res = 90)
    output$equitydataplot_c <- renderPlot(c, res = 90)
    
    
    # Sankey diagram -----------------------------------------------------------
    equity_invest_sankey <- equity_shares_select %>% 
      transmute(regionshort = regionshort, 
                CE = CE,
                FSCEdiff = sum(CE) * finalweight - CE)
    
    # Define regional contributions
    equity_invest_sankey <- equity_invest_sankey %>% 
      mutate(
        value_equityinflows = ifelse(FSCEdiff < 0, FSCEdiff, 0),
        value_equityoutflows = ifelse(FSCEdiff > 0, FSCEdiff, 0),
        value_intraregion = CE + value_equityinflows)
    
    # Prepare sankey plot
    links <- equity_invest_sankey %>% 
      mutate(value_equityinflows = - value_equityinflows) %>% 
      pivot_longer(-c(regionshort)) %>% 
      mutate(
        source = case_when(
          name == "value_intraregion" ~ paste0(as.character(regionshort), " "),
          name == "value_equityoutflows" ~ paste0(as.character(regionshort), " "),
          name == "value_equityinflows" ~ "Inter-regional fund (IRF) "),
        target = case_when(
          name == "value_intraregion" ~ "Within-region ",
          name == "value_equityoutflows" ~ "Inter-regional ",
          name == "value_equityinflows" ~ paste0("IRF to ", paste0(as.character(regionshort), " "))),
        group = case_when(
          name == "value_intraregion" ~ paste0(as.character(regionshort), " "),
          name == "value_equityoutflows" ~ paste0(as.character(regionshort), " "),
          name == "value_equityinflows" ~ "Inter-regional ")
      ) %>% 
      select(source, target, value, group) %>% 
      na.omit() %>% 
      filter(value > 0) %>% 
      mutate(value = round(value)) %>% 
      data.frame()
    
    nodes <- data.frame(
      name=c(as.character(links$source), 
             as.character(links$target)) %>% unique()
    )
    
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    
    sankey <- sankeyNetwork(Links = links, Nodes = nodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name", LinkGroup = "group",
                            units = "B USD / year", 
                            colourScale = JS("d3.scaleOrdinal(['#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928', '#969696']);"),
                            margin = list("left" = 80),
                            sinksRight = F)
    
    javascript_string <- 
      'function(el, x){
    d3.select(el).selectAll(".node text")
      .text(d => d.name + " (" + d.value + " B USD / year)")
      .style("font-size", "18px");
    }'
    
    sankey <- htmlwidgets::onRender(x = sankey, jsCode = javascript_string)
    
    output$sankey <- renderSankeyNetwork({sankey})
    
  }) # ends observe: do
} # ends server code

# Run the application 
shinyApp(ui = ui, server = server)
