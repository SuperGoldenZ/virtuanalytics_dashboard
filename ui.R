library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)
library(shinyfullscreen)
library(DT) # Load DT package for interactive tables

source("R/analytics.R")
source("R/analytics_character.R")

generate_character_tab <- function(character) {
    tabPanel(
        uiOutput(paste0(character, "Button")),
        fluidRow(
            tags$p(paste(count_character_matches(data, character), " total matches"))
        ),
        fluidRow(
            column(
                3,
                DT::dataTableOutput(paste0(tolower(character), "_matches_list")),
            ),
            column(
                4,
                DT::dataTableOutput(paste0(tolower(character), "_wins_per_stage_table"))
                # DT::dataTableOutput(paste0(tolower(character), "_match_wins_per_stage_lookup_table")),
                # DT::dataTableOutput(paste0(tolower(character), "_wins_per_stage_lookup_table"))
            ),
            column(
                4,
                DT::dataTableOutput(paste0(tolower(character), "_wins_per_character_table")),
                plotOutput(paste0(tolower(character), "_win_probability_per_round"))
            )
        )
    )
}

# Define UI
ui <- fluidPage(
    tags$head(
        tags$title("VirtuAnalytics"),
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Brush+Script+MT&display=swap');

      h1 {
        font-family: 'Caveat Brush', cursive;
        font-size: 50px;
        background: -webkit-linear-gradient(top, white, yellow, red, gold);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        text-align: left;
        background-color: #421301;
      }

        .two-columns .checkbox {
        }

        .scroll-checkbox-group {
            height: 250px;  /* Set desired height */
            overflow-y: auto; /* Enable vertical scrolling */
            border: 1px solid #ccc; /* Optional styling */
        }

        .dataTable {
        font-size: 12px;  /* Change this value to adjust the font size */
      }

      /*body {
        background-color: #9c7c70;
      }*/
    "))
    ),
    navbarPage(
        "VirtuaAnalytics",
        header = tagList(
            div(
                radioButtons(
                    "language",
                    label = NULL, # No label to minimize space
                    choices = c("English", "日本語"),
                    selected = "English",
                    inline = TRUE
                ),
                style = "position: relative; float: right; margin-right: 20px;margin-top: -50px; z-index: 9999; "
            )
        ),
        tabPanel(
            uiOutput("VideoSearch"),
            fluidRow(
                column(
                    4,
                    fluidRow(
                        column(
                            3,
                            fluidRow(
                                actionButton("select_all_stages", uiOutput("SelectAllStages"), style = "padding:4px; font-size:80%"),
                                actionButton("clear_all_stages", uiOutput("ClearAllStages"), style = "padding:4px; font-size:80%")
                            ),
                            checkboxGroupInput("stages", uiOutput("Stages"),
                                choices = stages,
                                selected = stages
                            ) %>% div(class = "scroll-checkbox-group")
                        ),
                        column(
                            3,
                            fluidRow(
                                actionButton("select_all_ranks", uiOutput("SelectAllRanks"), style = "padding:4px; font-size:80%"),
                                actionButton("clear_all_ranks", uiOutput("ClearAllRanks"), style = "padding:4px; font-size:80%"),
                            ),
                            checkboxGroupInput("ranks", uiOutput("Ranks"),
                                choices = ranks,
                                selected = ranks
                            ),
                        ),
                        column(
                            3,
                            # Select All / Clear All buttons for characters
                            # actionButton("select_all_characters", uiOutput("SelectAllCharacters")),
                            actionButton("clear_all_characters", uiOutput("ClearAllCharacters"), style = "padding:4px; font-size:80%"),
                            checkboxGroupInput("characters", uiOutput("Characters"),
                                choices = characters
                            ) %>% div(class = "scroll-checkbox-group")
                        )
                    ),
                    # h2(uiOutput("MatchList")),
                    DT::dataTableOutput("youtube_videos_table")
                ),
                column(
                    8,
                    fluidRow(
                        column(6, plotOutput("rankDistPlot")),
                        column(4, DT::dataTableOutput("win_rate_per_rank"))
                    ),
                    fluidRow(
                        column(6, plotOutput("stageDistPlot")),
                        column(4, DT::dataTableOutput("time_remaining_per_stage")),
                    ),
                    fluidRow(
                        column(
                            6, plotOutput("win_method_piechart"),
                            DT::dataTableOutput("how_rounds_end_per_stage")
                        ),
                        column(
                            4,
                            DT::dataTableOutput("win_methods_by_character"),
                            DT::dataTableOutput("loss_methods_by_character")
                        )
                    ),
                    fluidRow(
                        column(6, plotOutput("characterDistPlot")),
                        column(
                            4,
                            DT::dataTableOutput("win_rate_table"),
                            DT::dataTableOutput("character_matchup_table")
                        )
                    ),
                    fullscreen_those(items = list("characterDistPlot", "rankDistPlot", "stageDistPlot")),
                )
            ),
        ),

        # lapply(characters, generate_character_tab),
        generate_character_tab("Akira"),
        generate_character_tab("Aoi"),
        generate_character_tab("Brad"),
        generate_character_tab("Eileen"),
        generate_character_tab("Blaze"),
        generate_character_tab("Goh"),
        generate_character_tab("Jean"),
        generate_character_tab("Jacky"),
        generate_character_tab("Jeffry"),
        generate_character_tab("Kage"),
        generate_character_tab("Lau"),
        generate_character_tab("LeiFei"),
        generate_character_tab("Lion"),
        generate_character_tab("Pai"),
        generate_character_tab("Sarah"),
        generate_character_tab("Shun"),
        generate_character_tab("Taka"),
        generate_character_tab("Vanessa"),
        generate_character_tab("Wolf"),
        tabPanel(
            "About",
            tags$p(
                "VirtuAnalytics v1.2.2"
            ),
            tags$hr(),
            tags$p(
                "Video scraped teathered via  ",
                tags$a(href = "https://r10.to/hNXlYV", "Rakuten Mobile", target = "_blank_vf")
            ),
            tags$p(
                "Compiled by ",
                tags$a(href = "https://x.com/SuperGolden", "@SuperGolden", target = "_blank_twitter")
            ),
            tags$p(
                "Source code: ",
                tags$a(href = "https://github.com/SuperGoldenZ/vf_analytics", "GitHub", target = "_blank_github")
            ),
            tags$p(
                "Made with ",
                tags$a(href = "https://github.com/rstudio/shiny", "R & Shiny", target = "_blank_shiny")
            ),
            tags$p(
                "License: ",
                tags$a(href = "https://github.com/rstudio/shiny/blob/main/LICENSE", "License", target = "_blank_shiny")
            ),
            tags$p(
                "Data scraped from: ",
                tags$a(href = "https://www.youtube.com/@vfans", "@vfans", target = "_blank_vf")
            )
        )
    )
)
