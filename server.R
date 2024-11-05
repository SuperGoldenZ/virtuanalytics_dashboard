library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)
library(forcats)
library(DT) # Load DT package for interactive tables

source("ui.R")
if (!exists("win_rate_per_rank")) source("R/analytics.R")
if (!exists("character_names")) source("R/analytics_character.R")
source("R/analytics_rounds.R")

dict <- list(
    "Ranks" = c("English" = "Ranks", "日本語" = "段位"),
    "Stages" = c("English" = "Stages", "日本語" = "ステージ"),
    "Characters" = c("English" = "Characters", "日本語" = "キャラクター"),
    "Select All" = c("English" = "Select All", "日本語" = "すべて選択"),
    "Clear All" = c("English" = "Clear All", "日本語" = "すべてクリア"),
    "Match List" = c("English" = "Match List", "日本語" = "試合一覧"),
    "Akira" = c("English" = "Akira", "日本語" = "晶"),
    "Pai" = c("English" = "Pai", "日本語" = "パイ"),
    "Lau" = c("English" = "Lau", "日本語" = "ラウ"),
    "Wolf" = c("English" = "Wolf", "日本語" = "ウルフ"),
    "Jeffry" = c("English" = "Jeffry", "日本語" = "ジェフリー"),
    "Kage" = c("English" = "Kage", "日本語" = "影"),
    "Sarah" = c("English" = "Sarah", "日本語" = "サラ"),
    "Jacky" = c("English" = "Jacky", "日本語" = "ジャッキー"),
    "Shun" = c("English" = "Shun", "日本語" = "舜 帝"),
    "Lion" = c("English" = "Lion", "日本語" = "リオン"),
    "Aoi" = c("English" = "Aoi", "日本語" = "葵"),
    "LeiFei" = c("English" = "Lei-Fei", "日本語" = "雷 飛"),
    "Vanessa" = c("English" = "Vanessa", "日本語" = "ベネッサ"),
    "Brad" = c("English" = "Brad", "日本語" = "ブラッド"),
    "Goh" = c("English" = "Goh", "日本語" = "剛"),
    "Eileen" = c("English" = "Eileen", "日本語" = "アイリーン"),
    "Blaze" = c("English" = "Blaze", "日本語" = "エル・ブレイズ"),
    "Taka" = c("English" = "Taka", "日本語" = "鷹嵐"),
    "Jean" = c("English" = "Jean", "日本語" = "ジャン"),
    "Video Search" = c("English" = "Video Search", "日本語" = "検索"),
    "matches" = c("English" = " matches", "日本語" = "件"),
    "Rank Distribution" = c("English" = "Rank Distribution", "日本語" = "段位"),
    "Character Distribution" = c("English" = "Character Distribution", "日本語" = "キャラクター"),
    "Stage Distribution" = c("English" = "Stage Distribution", "日本語" = "ステージ"),
    "Link" = c("English" = "View", "日本語" = "再生")
)

create_character_tables <- function(output, data, character_name) {
    l_character_name <- tolower(character_name)

    # Wins per Character Table
    output[[paste0(l_character_name, "_wins_per_character_table")]] <- DT::renderDataTable({
        datatable(character_matchup_win_table(data, character_name), options = list(
            pageLength = 20,
            paging = FALSE,
            searching = FALSE
        )) %>%
            formatPercentage("Win %", digits = 0) %>%
            formatRound("p_value", digits = 3) %>%
            formatStyle(
                "p_value",
                backgroundColor = styleInterval(c(0.05), c("yellow", ""))
            )
    })

    output[[paste0(l_character_name, "_wins_per_character_and_stage_table")]] <- DT::renderDataTable({
        datatable(character_stage_matchup_win_table(data, character_name), options = list(
            pageLength = 20,
            paging = FALSE,
            searching = FALSE
        )) %>%
            formatPercentage("Win %", digits = 0) # %>%
        # formatRound("p_value", digits = 3) %>%
        # formatStyle(
        # "p_value",
        # backgroundColor = styleInterval(c(0.05), c("yellow", ""))
        # )
    })

    # Wins per Stage Table
    output[[paste0(l_character_name, "_wins_per_stage_table")]] <- DT::renderDataTable({
        datatable(win_percentages_per_character(data, character_name), options = list(
            pageLength = 20,
            paging = FALSE,
            searching = FALSE
        )) %>%
            formatPercentage("Win %", digits = 0) %>%
            formatRound("p_value", digits = 3) %>%
            formatStyle(
                "p_value",
                backgroundColor = styleInterval(c(0.05), c("yellow", ""))
            )
    })

    output[[paste0(l_character_name, "_matches_list")]] <- DT::renderDataTable({
        datatable(matches_list[[character_name]], escape = FALSE, options = list(lengthChange = FALSE, searching = TRUE))
    })

    # output[[paste0(l_character_name, "_win_probability_per_round")]] <- DT::renderDataTable({
    # win_probability_per_round(data, character_name)
    # })

    output[[paste0(l_character_name, "_win_probability_per_round")]] <- renderPlot({
        df <- win_probability_per_round(data, character_name) %>%
            filter(cumulative_wins < 3) %>%
            filter((round_number - cumulative_wins) < 3)

        ggplot(df, aes(x = round_number + 1, y = wp, color = as.factor(cumulative_wins))) +
            xlim(0, 6) +
            scale_x_continuous(breaks = c(1, 2, 3, 4, 5)) +
            scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.25))) + # Set y-axis limits and top margin
            geom_point(size = 3) +
            geom_text(aes(label = paste(cumulative_wins, "-", (round_number - cumulative_wins), "\n", round(wp * 100, 0), "%"), vjust = -0.5, size = 2)) +
            guides(fill = "none") +
            labs(title = "Match Win Probability by Round", x = "Round Number", y = "Match Win Probability") +
            theme_minimal() +
            theme(
                legend.position = "none",
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 14)
            )
    })

    return(1)

    # Match Wins per Stage Lookup Table
    output[[paste0(l_character_name, "_match_wins_per_stage_lookup_table")]] <- DT::renderDataTable({
        char_data <- matches_won_per_stage_per_character(data, character_name)

        datatable(char_data, options = list(
            dom = "t",
            paging = FALSE,
            searching = FALSE
        )) %>%
            formatRound(columns = c(1:ncol(char_data)), digits = 3) %>%
            formatStyle(
                columns = names(char_data),
                backgroundColor = styleInterval(c(0.05), c("yellow", ""))
            )
    })

    # Wins per Stage Lookup Table
    output[[paste0(l_character_name, "_wins_per_stage_lookup_table")]] <- DT::renderDataTable({
        char_data <- rounds_won_per_stage_per_character_lookup(data, character_name)

        datatable(char_data, options = list(
            dom = "t", # Only show the table body, no footer
            paging = FALSE, # Disable pagination
            searching = FALSE # Disable the search box
        )) %>%
            formatRound(columns = c(1:ncol(char_data)), digits = 3) %>%
            formatStyle(
                columns = names(char_data),
                backgroundColor = styleInterval(c(0.05), c("yellow", "")) # Highlight cells <= 0.05
            )
    })
}

# Define server logic
server <- function(input, output, session) {
    query <- reactive({
        parseQueryString(session$clientData$url_search)
    })

    output$paramValue <- renderText({
        # Check if 'param' exists in the query string
        param_value <- query()[["japanese"]]

        if (!is.null(param_value)) {
            print("should be Japanese")
        }
    })

    # Select All / Clear All buttons for ranks
    observeEvent(input$select_all_ranks, {
        updateCheckboxGroupInput(session, "ranks", selected = ranks)
    })

    observeEvent(input$clear_all_ranks, {
        updateCheckboxGroupInput(session, "ranks", selected = character(0))
    })

    observe({
        # Check the number of selected options
        if (length(input$characters) > 2) {
            # Show a warning message
            # output$warning <- renderText("You can only select a maximum of 2 options.")
            # Update input to keep only the first two selections
            updateCheckboxGroupInput(session, "characters", selected = input$options[1:2])
        } else {
            output$warning <- renderText("") # Clear the warning if valid
        }
    })

    # Select All / Clear All buttons for characters
    observeEvent(input$select_all_characters, {
        updateCheckboxGroupInput(session, "characters", selected = characters)
    })

    observeEvent(input$clear_all_characters, {
        updateCheckboxGroupInput(session, "characters", selected = character(0))
    })

    # Select All / Clear All buttons for stages
    observeEvent(input$select_all_stages, {
        updateCheckboxGroupInput(session, "stages", selected = stages)
    })

    observeEvent(input$clear_all_stages, {
        updateCheckboxGroupInput(session, "stages", selected = character(0))
    })

    # Reactive data filtering based on selected ranks
    filtered_data <- reactive({
        checked_count <- length(input$characters)
        if (checked_count == 0) {
            data_combined %>%
                filter(player_rank %in% input$ranks) %>%
                filter(stage %in% input$stages)
        } else if (checked_count == 1) {
            data_combined %>%
                filter(player_rank %in% input$ranks) %>%
                filter(character %in% input$characters) %>%
                filter(stage %in% input$stages)
        } else if (checked_count == 2) {
            data_combined %>%
                filter(player_rank %in% input$ranks) %>%
                # filter(character %in% input$characters) %>%
                filter(stage %in% input$stages)
        }
    })

    youtube_video_data <- reactive({
        checked_count <- length(input$characters)

        # return all data at first which is already available for faster loading
        if (checked_count == 0 & length(input$ranks) == 7) {
            youtube_video_data_all
        } else if (checked_count == 1) {
            match_data %>%
                filter(Player.1.Rank %in% input$ranks | Player.2.Rank %in% input$ranks) %>%
                filter(Player.1.Character %in% input$characters & Player.2.Character %in% input$characters) %>%
                filter(Stage %in% input$stages) %>%
                mutate(Stage = Stage, Desc = paste("Lv", Player.1.Rank, " ", Player.1.Character, " vs Lv", Player.2.Rank, " ", Player.2.Character), Link = Youtube.Link) %>%
                select(Stage, Desc, Link)
        } else if (checked_count == 2) {
            match_data %>%
                filter(Player.1.Rank %in% input$ranks | Player.2.Rank %in% input$ranks) %>%
                filter(
                    (Player.1.Character == input$characters[[1]] & Player.2.Character == input$characters[[2]]) |
                        (Player.1.Character == input$characters[[2]] & Player.2.Character == input$characters[[1]])
                ) %>%
                filter(Stage %in% input$stages) %>%
                mutate(Stage = Stage, Desc = paste("Lv", Player.1.Rank, " ", Player.1.Character, " vs Lv", Player.2.Rank, " ", Player.2.Character), Link = Youtube.Link) %>%
                select(Stage, Desc, Link)
        } else if (checked_count == 0) {
            match_data %>%
                filter(Player.1.Rank %in% input$ranks | Player.2.Rank %in% input$ranks) %>%
                filter(Stage %in% input$stages) %>%
                mutate(Stage = Stage, Desc = paste("Lv", Player.1.Rank, " ", Player.1.Character, " vs Lv", Player.2.Rank, " ", Player.2.Character), Link = Youtube.Link) %>%
                select(Stage, Desc, Link)
        }
        # colnames(match_data)[colnames(match_data) == "Link"] <- dict[["Link"]][[input$language]]
    })

    # Calculate total number of samples
    total_samples <- reactive({
        nrow(filtered_data()) # Get the number of rows in the data
    })

    selected_language <- reactive({
        input$language
    })

    output$MatchList <- renderText(dict[["Match List"]][[selected_language()]])
    output$VideoSearch <- renderText(dict[["Video Search"]][[selected_language()]])
    output$Ranks <- renderText(dict[["Ranks"]][[selected_language()]])
    output$Stages <- renderText(dict[["Stages"]][[selected_language()]])
    output$Characters <- renderText(dict[["Characters"]][[selected_language()]])
    output$SelectAllStages <- renderText(dict[["Select All"]][[selected_language()]])
    output$ClearAllStages <- renderText(dict[["Clear All"]][[selected_language()]])
    output$SelectAllCharacters <- renderText(dict[["Select All"]][[selected_language()]])
    output$ClearAllCharacters <- renderText(dict[["Clear All"]][[selected_language()]])
    output$SelectAllRanks <- renderText(dict[["Select All"]][[selected_language()]])
    output$ClearAllRanks <- renderText(dict[["Clear All"]][[selected_language()]])

    output$AkiraButton <- renderText(dict[["Akira"]][[selected_language()]])
    output$BlazeButton <- renderText(dict[["Blaze"]][[selected_language()]])
    output$EileenButton <- renderText(dict[["Eileen"]][[selected_language()]])
    output$PaiButton <- renderText(dict[["Pai"]][[selected_language()]])
    output$LauButton <- renderText(dict[["Lau"]][[selected_language()]])
    output$WolfButton <- renderText(dict[["Wolf"]][[selected_language()]])
    output$JeffryButton <- renderText(dict[["Jeffry"]][[selected_language()]])
    output$KageButton <- renderText(dict[["Kage"]][[selected_language()]])
    output$SarahButton <- renderText(dict[["Sarah"]][[selected_language()]])
    output$JackyButton <- renderText(dict[["Jacky"]][[selected_language()]])
    output$ShunButton <- renderText(dict[["Shun"]][[selected_language()]])
    output$LionButton <- renderText(dict[["Lion"]][[selected_language()]])
    output$AoiButton <- renderText(dict[["Aoi"]][[selected_language()]])
    output$LeiFeiButton <- renderText(dict[["LeiFei"]][[selected_language()]])
    output$VanessaButton <- renderText(dict[["Vanessa"]][[selected_language()]])
    output$BradButton <- renderText(dict[["Brad"]][[selected_language()]])
    output$GohButton <- renderText(dict[["Goh"]][[selected_language()]])
    output$TakaButton <- renderText(dict[["Taka"]][[selected_language()]])
    output$JeanButton <- renderText(dict[["Jean"]][[selected_language()]])


    # Rank distribution plot
    output$rankDistPlot <- renderPlot({
        rank_counts <- filtered_data() %>%
            select(player_rank) %>%
            pivot_longer(cols = everything(), names_to = "Player", values_to = "Rank") %>%
            count(Rank)
        rank_counts$Rank <- factor(rank_counts$Rank)

        ggplot(rank_counts, aes(x = Rank, y = n, fill = Rank)) +
            geom_bar(stat = "identity") +
            labs(
                title = paste(
                    dict[["Rank Distribution"]][[input$language]], "(", comma(total_samples() / 4),
                    dict[["matches"]][[input$language]], ")"
                ),
                x = dict[["Ranks"]][[input$language]], y = dict[["matches"]][[input$language]]
            ) +
            theme_minimal() +
            scale_x_discrete(limits = rank_counts$Rank) +
            theme(
                plot.margin = margin(b = 15),
                plot.title = element_text(size = 20, face = "bold"), # Title font size
                axis.title.x = element_text(size = 16), # X-axis label font size
                axis.title.y = element_text(size = 16), # Y-axis label font size
                axis.text.x = element_text(size = 14), # X-axis tick label font size
                axis.text.y = element_text(size = 14),
                legend.title = element_text(size = 16), # Legend title font size
                legend.text = element_text(size = 14),
                legend.position = "none"
            )
    })

    # Character distribution plot
    output$stageDistPlot <- renderPlot({
        stage_counts <- filtered_data() %>%
            select(stage) %>%
            pivot_longer(cols = everything(), values_to = "Stage") %>%
            count(Stage)
        stage_counts <- stage_counts %>%
            mutate(Stage = fct_reorder(Stage, n, .desc = TRUE))
        ggplot(stage_counts, aes(x = Stage, y = n, fill = Stage)) +
            geom_bar(stat = "identity") +
            labs(title = paste(dict[["Stage Distribution"]][[input$language]], "(", comma(total_samples() / 4), dict[["matches"]][[input$language]], ")"), x = "Stage", y = "Count") +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 14), plot.margin = margin(b = 15),
                plot.title = element_text(size = 20, face = "bold"), # Title font size
                axis.title.y = element_text(size = 16), # Y-axis label font size
                axis.text.y = element_text(size = 14),
                legend.title = element_text(size = 0), # Legend title font size
                legend.text = element_text(size = 10),
                legend.position = "none"
            )
    })


    # Character distribution plot
    output$characterDistPlot <- renderPlot({
        character_counts <- filtered_data() %>%
            select(character) %>%
            # mutate(character = sapply(character, function(char) dict[[char]][[input$language]])) %>%
            pivot_longer(cols = everything(), names_to = "Player", values_to = "Character") %>%
            count(Character)
        character_counts <- character_counts %>%
            mutate(Character = fct_reorder(Character, n, .desc = TRUE))
        ggplot(character_counts, aes(x = Character, y = n, fill = Character)) +
            geom_bar(stat = "identity") +
            labs(title = paste(dict[["Character Distribution"]][[input$language]], "(", comma(total_samples() / 4), dict[["matches"]][[input$language]], ")"), x = "Character", y = "Count") +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 14), plot.margin = margin(b = 15),
                plot.title = element_text(size = 20, face = "bold"), # Title font size
                axis.title.x = element_text(size = 16), # X-axis label font size
                axis.title.y = element_text(size = 16), # Y-axis label font size
                axis.text.y = element_text(size = 14),
                legend.title = element_text(size = 0), # Legend title font size
                legend.text = element_text(size = 10),
                legend.position = "none"
            )
    })


    # Calculate and render overall win rates per character
    output$win_rate_table <- renderTable(win_percentage_table)

    output$character_matchup_table <- renderTable(character_matchup)
    output$youtube_videos_table <- DT::renderDataTable({
        datatable(youtube_video_data(), escape = FALSE, options = list(lengthChange = FALSE, searching = FALSE))
    })

    output$win_rate_table <- DT::renderDataTable({
        datatable(win_percentage_table, options = list(pageLength = 20, paging = FALSE, searching = FALSE, dom = "t")) %>% formatPercentage("Win_Percentage", digits = 0)
    })
    output$character_matchup_table <- DT::renderDataTable({
        datatable(character_matchup, options = list(paging = FALSE, searching = FALSE)) %>% formatPercentage("Win_Percentage", digits = 0)
    })

    output$time_remaining_per_stage <- DT::renderDataTable({
        datatable(time_remaining_per_stage_data, options = list(paging = FALSE, searching = FALSE)) %>%
            formatRound("Average_Time_Per_Round", digits = 1) %>%
            formatRound("p_value", digits = 3) %>%
            formatStyle(
                "p_value",
                backgroundColor = styleInterval(c(0.05), c("yellow", ""))
            )
    })

    output$win_rate_per_rank <- DT::renderDataTable({
        datatable(win_rate_per_rank_data, options = list(paging = TRUE, searching = FALSE)) %>%
            formatPercentage("win_percentage", digits = 0)
    })

    create_character_tables(output, data, "Akira")
    create_character_tables(output, data, "Aoi")
    create_character_tables(output, data, "Brad")
    create_character_tables(output, data, "Eileen")
    create_character_tables(output, data, "Blaze")
    create_character_tables(output, data, "Goh")
    create_character_tables(output, data, "Jean")
    create_character_tables(output, data, "Jacky")
    create_character_tables(output, data, "Jeffry")
    create_character_tables(output, data, "Kage")
    create_character_tables(output, data, "Lau")
    create_character_tables(output, data, "LeiFei")
    create_character_tables(output, data, "Lion")
    create_character_tables(output, data, "Pai")
    create_character_tables(output, data, "Sarah")
    create_character_tables(output, data, "Shun")
    create_character_tables(output, data, "Taka")
    create_character_tables(output, data, "Vanessa")
    create_character_tables(output, data, "Wolf")

    output$win_method_piechart <- renderPlot({
        # Create pie chart
        ggplot(rounds_won_piechart_data(data), aes(x = "", y = percentage, fill = How.Round.Ended)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y") +
            labs(title = "Win Method Distribution", fill = "How.Round.Ended") +
            theme_void() + # Clean up theme for pie chart appearance
            geom_text(aes(label = paste0(round(percentage), "%")),
                position = position_stack(vjust = 0.5)
            )
    })

    output$win_methods_by_character <- DT::renderDataTable({
        datatable(how_rounds_won_data(data), options = list(paging = FALSE, searching = FALSE)) %>%
            formatPercentage("KO", digits = 1) %>%
            formatPercentage("EX", digits = 1) %>%
            formatPercentage("RO", digits = 1)
    })

    output$loss_methods_by_character <- DT::renderDataTable({
        datatable(how_rounds_lost_data(data), options = list(paging = FALSE, searching = FALSE)) %>%
            formatPercentage("KO", digits = 1) %>%
            formatPercentage("EX", digits = 1) %>%
            formatPercentage("RO", digits = 1)
    })

    output$how_rounds_end_per_stage <- DT::renderDataTable({
        datatable(how_rounds_end_per_stage(data), options = list(paging = FALSE, searching = FALSE)) %>%
            formatPercentage("KO", digits = 1) %>%
            formatPercentage("EX", digits = 1) %>%
            formatPercentage("RO", digits = 1)
    })

    localized_characters <- reactive({
        lang <- input$language # Get the selected language
        dict$characters[[lang]] # Return character names based on selected language
    })
}
