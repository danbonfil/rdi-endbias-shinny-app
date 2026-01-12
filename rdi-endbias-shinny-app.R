# Clear workspace
rm(list = ls(all = TRUE))

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(tidyr)
library(readr)

## -------------------------------------------
## Retrieve RDI data from FRED API
## -------------------------------------------
api_key <- "b613f51921dab64aeae0bb2fe2894ea7"
series_id <- "A229RX0"
today <- format(Sys.Date(), "%Y-%m-%d")

url <- paste0(
  "https://api.stlouisfed.org/fred/series/observations?",
  "series_id=", series_id,
  "&api_key=", api_key,
  "&file_type=json",
  "&observation_start=1960-01-01",
  "&observation_end=", today,
  "&frequency=a"
)

data <- fromJSON(content(GET(url), "text"))

## -------------------------------------------
## Original data (1940–1960) from CSV
## -------------------------------------------
odata <- read.csv("https://www.dropbox.com/s/r5qsxerj04jgu7y/RDI_40_09.csv?dl=1")

df_o <- odata %>%
  as.data.frame() %>%
  # assume one row per year starting 1940
  mutate(
    date    = seq(1940, length.out = nrow(.)),      # year as numeric
    usvalue = as.numeric(usvalue),                  # level
    value   = 100 * (log(usvalue) - log(dplyr::lag(usvalue)))
  ) %>%
  mutate(
    value = if_else(is.na(value), 0, value)         # first year growth = 0
  ) %>%
  filter(date >= 1940, date <= 1960) %>%
  select(date, usvalue, value)

## -------------------------------------------
## FRED data (1960–today)
## -------------------------------------------
df_fred <- data$observations %>%
  as.data.frame() %>%
  mutate(
    date    = as.numeric(format(as.Date(date), "%Y")),  # year as numeric
    usvalue = as.numeric(value),                        # level from FRED "value"
    value   = 100 * (log(usvalue) - log(dplyr::lag(usvalue)))
  ) %>%
  mutate(
    value = if_else(is.na(value), 0, value)             # first obs in series = 0
  ) %>%
  filter(date >= 1960) %>%
  select(date, usvalue, value)

## -------------------------------------------
## Combine and save full series
## -------------------------------------------
df <- bind_rows(df_o, df_fred) %>%
  arrange(date) %>%
  distinct(date, .keep_all = TRUE)

write.csv(df, "RDI_40_24.csv", row.names = FALSE)

#### SHINY APP ####

# Read the CSV file
df <- read.csv("RDI_40_24.csv")

# Process the data (FIX: keep date as numeric year; derive year from it directly)
df <- df %>%
  mutate(
    date = as.numeric(date),           # ensure numeric year
    year = date,
    group = (year - 1941) %/% 4 + 1    # 4-year groups starting in 1941
  ) %>%
  filter(year >= 1940) %>%
  group_by(group) %>%
  mutate(cumgrowth = cumsum(coalesce(value, 0))) %>%
  ungroup() %>%
  rename(growth = value) %>%
  select(date, year, usvalue, growth, cumgrowth)

# Function to get president info
get_president_info <- function(year) {
  case_when(
    year %in% 1941:1944 ~ list(presnum = 32, term = "3th Term", party = "Democrat"),
    year %in% 1945:1948 ~ list(presnum = 32, term = "4th Term", party = "Democrat"),
    year %in% 1949:1952 ~ list(presnum = 33, term = "Second Term", party = "Democrat"),
    year %in% 1953:1956 ~ list(presnum = 34, term = "First Term", party = "Republican"),
    year %in% 1957:1960 ~ list(presnum = 34, term = "Second Term", party = "Republican"),
    year %in% 1961:1964 ~ list(presnum = 35, term = "Single Term", party = "Democrat"),
    year %in% 1965:1968 ~ list(presnum = 36, term = "Single Term", party = "Democrat"),
    year %in% 1969:1972 ~ list(presnum = 37, term = "1st Term", party = "Republican"),
    year %in% 1973:1976 ~ list(presnum = 37, term = "2nd Term*", party = "Republican"),
    year %in% 1974:1976 ~ list(presnum = 38, term = "Single Term", party = "Republican"),
    year %in% 1977:1980 ~ list(presnum = 39, term = "Single Term", party = "Democrat"),
    year %in% 1981:1984 ~ list(presnum = 40, term = "1st Term", party = "Republican"),
    year %in% 1985:1988 ~ list(presnum = 40, term = "2nd Term", party = "Republican"),
    year %in% 1989:1992 ~ list(presnum = 41, term = "Single Term", party = "Republican"),
    year %in% 1993:1996 ~ list(presnum = 42, term = "1st Term", party = "Democrat"),
    year %in% 1997:2000 ~ list(presnum = 42, term = "2nd Term", party = "Democrat"),
    year %in% 2001:2004 ~ list(presnum = 43, term = "1st Term", party = "Republican"),
    year %in% 2005:2008 ~ list(presnum = 43, term = "2nd Term", party = "Republican"),
    year %in% 2009:2012 ~ list(presnum = 44, term = "1st Term", party = "Democrat"),
    year %in% 2013:2016 ~ list(presnum = 44, term = "2nd Term", party = "Democrat"),
    year %in% 2017:2020 ~ list(presnum = 45, term = "1st Term", party = "Republican"),
    year %in% 2021:2024 ~ list(presnum = 46, term = "Single Term", party = "Democrat"),
    TRUE ~ list(presnum = NA, term = NA, party = NA)
  )
}

# Add president info
df <- df %>%
  rowwise() %>%
  mutate(presinfo = list(get_president_info(year))) %>%
  unnest_wider(presinfo)

# President metadata (names and Dropbox image links)
presidents_df <- data.frame(
  presnum = 32:46,
  presname = c(
    "Franklin D. Roosevelt", "Harry S. Truman", "Dwight D. Eisenhower", "John F. Kennedy",
    "Lyndon B. Johnson", "Richard Nixon", "Gerald R. Ford", "Jimmy Carter", "Ronald Reagan",
    "George H. W. Bush", "Bill Clinton", "George W. Bush", "Barack Obama", "Donald Trump", "Joe Biden"
  ),
  presphoto = c(
    "https://www.dropbox.com/scl/fi/47wv9nqvpp7q8tev9ur39/32_franklin_d_roosevelt_resize-700-530.jpg?rlkey=ln84pq27kqf6c4hot8q45yxvm&st=6mrg5llj&dl=1",
    "https://www.dropbox.com/scl/fi/p8kb4w1n1ogm10ep2qjyo/33_harry_s_truman_resize-700-530.jpg?rlkey=1ht78werjuqeijb7dld65acwp&st=vxalse7t&dl=1",
    "https://www.dropbox.com/scl/fi/xogiudb9h6ulncpmq0nsm/34_dwight_d_eisenhower_resize-700-530.jpg?rlkey=7ghd8lmxf26ivnp5wrrmjvwl2&st=jj8f0q6y&dl=1",
    "https://www.dropbox.com/scl/fi/pbbbh88xc7ss3rvlu2uzs/35_john_f_kennedy_resize-700-530.jpg?rlkey=jnjjx8v9dc9gmgb1ctc14y2e8&st=xy2vp845&dl=1",
    "https://www.dropbox.com/scl/fi/4ebrvikm8a1d8t4wdjrlc/36_lyndon_johnson_resize-700-530.jpg?rlkey=gu4ddynsrk7hq9jviztgem3pn&st=dzqd4gqd&dl=1",
    "https://www.dropbox.com/scl/fi/xqu5fkntht356rnx07og5/37_richard_nixon_resize-700-530.jpg?rlkey=mpeajfphhw6jej1k5r76lcivy&st=yh1r17ax&dl=1",
    "https://www.dropbox.com/scl/fi/ebnvn65aufy3pcd4v8slm/38_gerald_ford_resize-700-530.jpg?rlkey=qvmniqgmq0c0ch4a4774qyj8i&st=gawa9nog&dl=1",
    "https://www.dropbox.com/scl/fi/lqaa3zrot8uficj3gioac/39_jimmy_carter_resize-700-530.jpg?rlkey=2u4wxnoj5hvsgzsgjcth63axq&st=y9fu11qr&dl=1",
    "https://www.dropbox.com/scl/fi/yhlekxliphktg3b7d3ctc/40_ronald_reagan_resize-700-530.jpg?rlkey=ze53p46izjey3kqavq3why0zq&st=8igfyt33&dl=1",
    "https://www.dropbox.com/scl/fi/668gbzy8i8p3cr2y3prl9/41_george_hw_bush_resize-700-530.jpg?rlkey=vem1s6x9r6pxvmv1rc0grodq7&st=6y93bsx7&dl=1",
    "https://www.dropbox.com/scl/fi/7j2f0kvb2lixj6z6ex46y/42_bill_clinton_resize-700-530.jpg?rlkey=8mq7r5yx10a1ycjjwq8rk1n18&st=uffkb1xb&dl=1",
    "https://www.dropbox.com/scl/fi/0xbbduduzvtgj8gav31bo/43_george_w_bush_resize-700-530.jpg?rlkey=rrvk9w7rway61r0oepahdh8da&st=paee8fin&dl=1",
    "https://www.dropbox.com/scl/fi/dgn7ifpislde4cw5053r1/44_barack_obama_resize-700-530.jpg?rlkey=kcr8zjs1m1bzw72qlcrxi2mdk&st=1c9yysft&dl=1",
    "https://www.dropbox.com/scl/fi/fjuwoas83x3t1mu9imryq/45_donald_trump_resize-700-530.jpg?rlkey=6duj658rkhqbxom0c4g07bn5l&st=q9dktekt&dl=1",
    "https://www.dropbox.com/scl/fi/3xkcnf4s07vmw6946379z/46_joseph_r_biden_resize-700-530.jpg?rlkey=9j29gvvt808dbx2q4mlmr96vl&st=hmdp0fxg&dl=1"
  ),
  stringsAsFactors = FALSE
)

# Merge and finalize shiny_df with president info by year
shiny_df_tmp <- merge(df, presidents_df, by = "presnum", all.x = TRUE) %>%
  filter(!is.na(presnum)) %>%
  mutate(presterm = paste(presname, term, sep = " - "))

shiny_df <- df %>%
  left_join(
    shiny_df_tmp %>%
      arrange(year) %>%
      distinct(year, .keep_all = TRUE) %>%
      select(year, presname, presphoto, presterm),
    by = "year"
  ) %>%
  filter(!is.na(presname) & !is.na(presphoto) & !is.na(presterm))

### UI & SERVER
ui <- navbarPage(
  title = "RDI Growth",
  theme = shinytheme("flatly"),
  tabPanel(
    "Fred Data",
    icon = icon("chart-line"),
    fluidRow(
      column(
        width = 6,
        div(
          style = "text-align: center;",
          uiOutput("presidentImage"),
          uiOutput("averageGrowth"),
          uiOutput("cumulativeGrowth"),
          div(
            style = "display: inline-block; width: 200px;",
            numericInput(
              "yearSelect",
              "Select General Election Year",
              value = 2020,
              min = 1944,
              max = 2024,
              step = 4
            )
          )
        )
      ),
      column(
        width = 6,
        plotOutput("yearlyPlot")
      )
    ),
    fluidRow(
      column(
        width = 6,
        plotOutput("rankingPlot")
      ),
      column(
        width = 6,
        plotOutput("cumulativePlot")
      )
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    shiny_df %>%
      filter(year >= input$yearSelect - 3, year <= input$yearSelect) %>%
      select(date, year, usvalue, growth, cumgrowth, presnum, presname, presphoto)
  })
  
  output$yearlyPlot <- renderPlot({
    start_year <- input$yearSelect - 3
    end_year <- input$yearSelect
    
    ggplot(filteredData(), aes(x = year, y = growth, group = 1)) +
      geom_line(color = "steelblue", size = 1.5) +
      geom_point(color = "steelblue", size = 3) +
      geom_text(aes(label = round(growth, 1)), size = 5, nudge_y = 0.5) +
      labs(x = "Year", y = "% Change in Personal Income",
           title = paste0("Yearly Personal Income Growth for ", start_year, "-", end_year)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0, margin = margin(b = 20), size = 20),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 18))
  })
  
  output$cumulativePlot <- renderPlot({
    start_year <- input$yearSelect - 3
    end_year <- input$yearSelect
    
    ggplot(filteredData(), aes(x = year, y = cumgrowth, group = 1)) +
      geom_line(color = "steelblue", size = 1.5) +
      geom_point(color = "steelblue", size = 3) +
      geom_text(aes(label = round(cumgrowth, 1)), size = 5, nudge_y = 0.5) +
      labs(x = "Year", y = "% Change in Personal Income",
           title = paste0("Cumulative Personal Income Growth for ", start_year, "-", end_year),
           subtitle = "Each year adds the growth of all previous years, so Year 4 shows the total growth from Years 1-4") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0, margin = margin(b = 20), size = 20),
            plot.subtitle = element_text(hjust = 0, margin = margin(b = 20), size = 14, vjust = 0),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 18))
  })
  
  output$presidentImage <- renderUI({
    if (input$yearSelect) {
      president <- unique(filteredData()$presname)
      president_image <- unique(filteredData()$presphoto)
      
      tagList(
        tags$h3(paste("President:", president)),
        tags$img(src = president_image, width = "150px", height = "150px")
      )
    }
  })
  
  output$averageGrowth <- renderUI({
    if (input$yearSelect) {
      avg_growth <- mean(filteredData()$growth, na.rm = TRUE)
      tagList(
        tags$h4(paste("Average Growth:", round(avg_growth, 1), "%"))
      )
    }
  })
  
  output$cumulativeGrowth <- renderUI({
    if (input$yearSelect) {
      cum_growth <- tail(filteredData()$cumgrowth, 1)
      tagList(
        tags$h4(paste("Cumulative Growth:", round(cum_growth, 1), "%"))
      )
    }
  })
  
  output$rankingPlot <- renderPlot({
    cum_growth_ranking <- shiny_df %>%
      group_by(presnum, presterm, party) %>%
      summarise(total_cumgrowth = max(cumgrowth, na.rm = TRUE), .groups = "drop") %>%
      arrange(presnum) %>%
      mutate(rank = row_number())
    
    ggplot(cum_growth_ranking, aes(x = reorder(presterm, presnum), y = total_cumgrowth, fill = party)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(total_cumgrowth, 1)), hjust = -0.3, size = 5) +
      scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
      labs(x = "President", y = "Cumulative Growth (%)", title = "Cumulative Growth Comparison of Presidents") +
      coord_flip() +
      theme_classic() +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, hjust = 0.5))
  })
}

shinyApp(ui, server)