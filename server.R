library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)
library(forcats)
library(DT) # Load DT package for interactive tables

if (!exists("ui")) source("ui.R")
if (!exists("win_rate_per_rank")) source("R/analytics.R")
if (!exists("character_stage_matchup_win_table")) source("R/analytics_character_functions.R")
if (!exists("character_names")) source("R/analytics_character.R")
if (!exists("how_rounds_lost_data")) source("R/analytics_rounds.R")

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
        dt <- character_matchup_win_table(data, character_name, character_matchup_win_table_data, FALSE, TRUE)
        dt <- dt %>% setNames(c("Main", "vs", "# Matches", "# Wins", "Win %", "p-value"))
        dt <- dt %>%
            mutate(Icon = paste("<img src = 'images/", `Main`, ".jpg' height='45px' style='margin-top: 2px; margin-bottom: 2px;margin-right: 2px;'/>VS<img src = 'images/", `vs`, ".jpg' height='45px' style='margin-left: 2px;margin-top: 2px; margin-bottom: 2px;'/>", sep = ""), .after = `vs`)

        datatable(dt, escape = FALSE, options = list(
            pageLength = 20,
            paging = FALSE,
            searching = FALSE
        )) %>%
            formatPercentage("Win %", digits = 0) %>%
            formatRound("p-value", digits = 3) %>%
            formatStyle(
                "p-value",
                backgroundColor = styleInterval(c(0.05), c("yellow", ""))
            ) %>%
            formatStyle(
                columns = names(dt),
                fontSize = "24px"
            )
    })

    output[[paste0(l_character_name, "_wins_per_character_table_same_rank")]] <- DT::renderDataTable({
        dt <- character_matchup_win_table(data, character_name, character_matchup_win_table_data, FALSE, FALSE, character_matchup_win_table_data_same_rank)
        dt <- dt %>% setNames(c("Main", "vs", "# Matches", "# Wins", "Win %", "p-value"))
        dt <- dt %>%
            mutate(Icon = paste("<img src = 'images/", `Main`, ".jpg' height='45px' style='margin-top: 2px; margin-bottom: 2px;margin-right: 2px;'/>VS<img src = 'images/", `vs`, ".jpg' height='45px' style='margin-left: 2px;margin-top: 2px; margin-bottom: 2px;'/>", sep = ""), .after = `vs`)

        datatable(dt, escape = FALSE, options = list(
            pageLength = 20,
            paging = FALSE,
            searching = FALSE
        )) %>%
            formatPercentage("Win %", digits = 0) %>%
            formatRound("p-value", digits = 3) %>%
            formatStyle(
                "p-value",
                backgroundColor = styleInterval(c(0.05), c("yellow", ""))
            ) %>%
            formatStyle(
                columns = names(dt),
                fontSize = "24px"
            )
    })

    output[[paste0(l_character_name, "_wins_per_character_and_stage_table")]] <- DT::renderDataTable({
        datatable(character_stage_matchup_win_table(data, character_name, character_stage_matchup_win_table_lookup), options = list(
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
        df <- win_percentages_per_character(data, character_name, win_percentages_per_character_lookup)
        df <- df %>% setNames(c("Stage\nType", "# Matches", "# Won", "Win %", "p-value"))

        datatable(df, options = list(
            pageLength = 20,
            paging = FALSE,
            searching = FALSE
        )) %>%
            formatPercentage("Win %", digits = 0) %>%
            formatRound("p-value", digits = 3) %>%
            formatStyle(
                "p-value",
                backgroundColor = styleInterval(c(0.05), c("yellow", ""))
            ) %>%
            formatStyle(
                columns = names(df),
                fontSize = "24px"
            )
    })

    output[[paste0(l_character_name, "_matches_list")]] <- DT::renderDataTable({
        datatable(matches_list[[character_name]], escape = FALSE, options = list(lengthChange = FALSE, searching = TRUE))
    })

    output[[paste0(l_character_name, "_win_probability_per_round")]] <- renderPlot({
        df <- win_probability_per_round(data, character_name, win_probability_per_round_lookup) %>%
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
            data_combined
            # data_combined %>%
            # filter(player_rank %in% input$ranks) %>%
            # filter(stage %in% input$stages)
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
            matches %>%
                select(
                    Date, Stage,
                    Player.1.Ringname, Player.1.Rank, Player.1.Character,
                    Player.2.Ringname, Player.2.Rank, Player.2.Character,
                    Video.URL
                )
        } else if (checked_count == 1) {
            matches %>%
                filter(Player.1.Rank %in% input$ranks | Player.2.Rank %in% input$ranks) %>%
                filter(Player.1.Character %in% input$characters | Player.2.Character %in% input$characters) %>%
                filter(Stage %in% input$stages) %>%
                mutate(Stage = Stage, Desc = paste("Lv", Player.1.Rank, " ", Player.1.Character, " vs Lv", Player.2.Rank, " ", Player.2.Character), Link = Video.URL) %>%
                select(
                    Date, Stage,
                    Player.1.Ringname, Player.1.Rank, Player.1.Character,
                    Player.2.Ringname, Player.2.Rank, Player.2.Character,
                    Video.URL
                )
        } else if (checked_count == 2) {
            matches %>%
                filter(Player.1.Rank %in% input$ranks | Player.2.Rank %in% input$ranks) %>%
                filter(
                    (Player.1.Character == input$characters[[1]] & Player.2.Character == input$characters[[2]]) |
                        (Player.1.Character == input$characters[[2]] & Player.2.Character == input$characters[[1]])
                ) %>%
                filter(Stage %in% input$stages) %>%
                mutate(Stage = Stage, Desc = paste("Lv", Player.1.Rank, " ", Player.1.Character, " vs Lv", Player.2.Rank, " ", Player.2.Character), Link = Video.URL) %>%
                select(
                    Date, Stage,
                    Player.1.Ringname, Player.1.Rank, Player.1.Character,
                    Player.2.Ringname, Player.2.Rank, Player.2.Character,
                    Video.URL
                )
        } else if (checked_count == 0) {
            matches %>%
                filter(Player.1.Rank %in% input$ranks | Player.2.Rank %in% input$ranks) %>%
                filter(Stage %in% input$stages) %>%
                mutate(Stage = Stage, Desc = paste("Lv", Player.1.Rank, " ", Player.1.Character, " vs Lv", Player.2.Rank, " ", Player.2.Character), Link = Video.URL) %>%
                select(
                    Date, Stage,
                    Player.1.Ringname, Player.1.Rank, Player.1.Character,
                    Player.2.Ringname, Player.2.Rank, Player.2.Character,
                    Video.URL
                )
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
    # output$VideoSearch <- renderText(dict[["Video Search"]][[selected_language()]])
    output$VideoSearch <- renderText("Video<br/>Search")
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


    output$rank_win_rate_plot <- renderPlot({
        ggplot(win_rate_plot_data, aes(x = `vs Rank`, y = `Win %`, color = as.factor(`Target_Rank`))) +
            geom_line(size = 3) +
            labs(color = "Player Rank") +
            scale_x_discrete(limits = win_rate_plot_data$`vs Rank`) +
            scale_color_brewer(palette = "Blues") +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 14), plot.margin = margin(b = 15),
                plot.title = element_text(size = 20, face = "bold"), # Title font size
                axis.title.x = element_text(size = 16), # X-axis label font size
                axis.title.y = element_text(size = 16), # Y-axis label font size
                axis.text.y = element_text(size = 14),
                legend.title = element_text(size = 16), # Legend title font size
                legend.text = element_text(size = 14),
                panel.background = element_rect(fill = "gray40", color = NA)
            )
    })


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

    output$rankDistPlotStatic <- renderPlot({
        ggplot(rank_counts_static, aes(x = Rank, y = n, fill = Rank)) +
            geom_bar(stat = "identity") +
            labs(
                title = paste(
                    dict[["Rank Distribution"]][[input$language]], "(", comma(total_samples() / 4),
                    dict[["matches"]][[input$language]], ")"
                ),
                x = dict[["Ranks"]][[input$language]], y = dict[["matches"]][[input$language]]
            ) +
            theme_minimal() +
            scale_x_discrete(limits = rank_counts_static$Rank) +
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

    output$timeRemainingDistPlot <- renderPlot({
        # , fill = Time.Seconds
        ggplot(time_counts, aes(x = Time.Seconds, y = n, fill = Time.Seconds)) +
            geom_bar(stat = "identity") +
            labs(title = "Time Per Round", x = "Time Per Round", y = "Round Count") +
            theme_minimal() +
            geom_text(aes(x = 5.0, y = 2500, label = paste("Fastest:", min_point)), hjust = 0, vjust = -1, color = "black", size = 8) +
            geom_text(aes(x = 5.0, y = 2250, label = paste("Longest:", max_point)), hjust = 0, vjust = -1, color = "black", size = 8) +
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

    # Stage distribution plot
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

    output$characterDistPie <- renderPlot({
        character_counts <- filtered_data() %>%
            select(character) %>%
            # mutate(character = sapply(character, function(char) dict[[char]][[input$language]])) %>%
            pivot_longer(cols = everything(), names_to = "Player", values_to = "Character") %>%
            count(Character)
        character_counts <- character_counts %>%
            mutate(Character = fct_reorder(Character, n, .desc = TRUE))

        # character_counts$percentage <- character_counts$value / sum(data$value) * 100

        ggplot(character_counts, aes(x = "", y = n, fill = Character)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0) +
            geom_text(
                aes(label = ifelse(n / sum(n) > 0.0,
                    paste0(Character, "\n", round((n / sum(n)) * 100, 1), "%"), ""
                )),
                position = position_stack(vjust = 0.5), size = 4
            ) + # Only show labels for slices >5%
            theme_void() +
            theme(
                plot.title = element_text(size = 20, face = "bold"), # Title font size
                legend.title = element_text(size = 0), # Legend title font size
                legend.text = element_text(size = 10),
                legend.position = "none"
            )
    })

    # Calculate and render overall win rates per character
    observeEvent(input$win_rate_sig, {
        output$win_rate_table <- DT::renderDataTable(
            {
                # win_percentage_table <- mutate(win_percentage_table, `Win %` <- Win_Percentage)

                win_percentage_table <- win_percentage_table %>%
                    mutate(Icon = paste("<img src = 'images/", Character, ".jpg' height='45px'/>", sep = ""), .after = Character)

                if (input$win_rate_sig == FALSE) {
                    win_percentage_table$p_value <- NULL
                    win_percentage_table <- win_percentage_table %>% setNames(c("Character", "Icon", "# Matches", "# Wins", "Win %"))
                } else {
                    win_percentage_table <- win_percentage_table %>% setNames(c("Character", "Icon", "# Matches", "# Wins", "Win %", "p-value"))
                }

                t <- datatable(win_percentage_table, escape = FALSE, options = list(pageLength = 20, paging = FALSE, lengthChange = FALSE, searching = FALSE)) %>%
                    formatPercentage("Win %", digits = 0) %>%
                    formatStyle(
                        columns = names(win_percentage_table),
                        fontSize = "24px"
                    ) %>%
                    formatCurrency(columns = c("# Wins", "# Matches"), currency = "", interval = 3, mark = ",", digits = 0)

                if (input$win_rate_sig) {
                    t <- t %>%
                        formatRound("p-value", digits = 3) %>%
                        formatStyle(
                            "p-value",
                            backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                        )
                }
                return(t)
            },
            class = "custom-dt-table"
        )
    })

    observeEvent(input$win_rate_same_sig, {
        output$win_rate_same_rank_table <- DT::renderDataTable({
            # win_percentage_table <- mutate(win_percentage_table, `Win %` <- Win_Percentage)



            win_percentage_same_rank_table <- win_percentage_same_rank_table %>%
                mutate(Icon = paste("<img src = 'images/", Character, ".jpg' height='45px'/>", sep = ""), .after = Character)

            if (input$win_rate_same_sig == FALSE) {
                win_percentage_same_rank_table$p_value <- NULL

                win_percentage_same_rank_table <- win_percentage_same_rank_table %>% setNames(c("Character", "Icon", "# Matches", "# Wins", "Win %"))
            } else {
                win_percentage_same_rank_table <- win_percentage_same_rank_table %>% setNames(c("Character", "Icon", "# Matches", "# Wins", "Win %", "p-value"))
            }

            t <- datatable(win_percentage_same_rank_table, escape = FALSE, options = list(pageLength = 20, paging = FALSE, lengthChange = FALSE, searching = FALSE)) %>%
                formatPercentage("Win %", digits = 0) %>%
                formatStyle(
                    columns = names(win_percentage_same_rank_table),
                    fontSize = "24px"
                )


            if (input$win_rate_same_sig) {
                t <- t %>%
                    formatRound("p-value", digits = 3) %>%
                    formatStyle(
                        "p-value",
                        backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                    )
            }
            return(t)
        })
    })

    output$character_matchup_table <- DT::renderDataTable({
        character_matchup <- character_matchup %>%
            mutate(Icon = paste("<img src = 'images/", `Main\nCharacter`, ".jpg' height='45px' style='margin-top: 2px; margin-bottom: 2px;margin-right: 2px;'/>VS<img src = 'images/", `vs\nCharacter`, ".jpg' height='45px' style='margin-left: 2px;margin-top: 2px; margin-bottom: 2px;'/>", sep = ""), .after = `vs\nCharacter`)
        # mutate(`p-value` = NA)

        # for (i in 1:nrow(character_matchup)) {
        # Access individual columns within the current row
        # main_character_name <- character_matchup$`Main\nCharacter`[i]
        # vs_character_name <- character_matchup$`vs\nCharacter`[i]
        # name <- df$name[i]
        # score <- df$score[i]
        # print(paste(main_character_name, vs_character_name, matches_won_per_matchup(character_matchup, main_character_name, vs_character_name)))
        # }

        datatable(character_matchup, escape = FALSE, options = list(paging = FALSE, searching = FALSE)) %>%
            formatPercentage("Win_Percentage", digits = 0) %>%
            formatStyle(
                columns = names(character_matchup),
                fontSize = "24px"
            )
    })

    observeEvent(input$time_remaining_sig, {
        df <- time_remaining_per_stage_data
        if (input$time_remaining_sig) {
            df <- df[, c(1, 2, 3, 4, 5)]
            df <- df %>% setNames(c("Stage Type", "Avg. Time / Round", "95th %tile", "Fastest", "p-value"))
        } else {
            df <- df[, c(1, 2, 3, 4)]
            df <- df %>% setNames(c("Stage Type", "Avg. Time / Round", "95th %tile", "Fastest"))
        }



        output$time_remaining_per_stage <- DT::renderDataTable({
            t <- datatable(df, options = list(paging = FALSE, searching = FALSE)) %>%
                formatRound("Avg. Time / Round", digits = 1) %>%
                formatRound("95th %tile", digits = 1) %>%
                formatStyle(
                    columns = names(df),
                    fontSize = "24px"
                )

            if (input$time_remaining_sig) {
                t <- t %>%
                    formatRound("p-value", digits = 3) %>%
                    formatStyle(
                        "p-value",
                        backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                    )
            }
            return(t)
        })
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

    observeEvent(input$win_method_sig, {
        output$win_methods_by_character <- DT::renderDataTable({
            rd <- how_rounds_won_data(data)
            print(colnames(rd))
            if (input$win_method_sig) {
                rd <- rd[, c(1, 6, 3, 7, 4, 8, 2, 9, 5)]
            } else {
                rd <- rd[, c(1, 6, 3, 4, 2, 5)]
            }


            t <- datatable(rd, options = list(paging = FALSE, searching = FALSE)) %>%
                formatPercentage("KO", digits = 1) %>%
                formatPercentage("EX", digits = 1) %>%
                formatPercentage("RO", digits = 1) %>%
                formatPercentage("TO", digits = 2)
            if (input$win_method_sig) {
                t <- t %>%
                    formatRound("ro_p_value", digits = 3) %>%
                    formatRound("ex_p_value", digits = 3) %>%
                    formatRound("ko_p_value", digits = 3) %>%
                    formatStyle(
                        "ro_p_value",
                        backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                    ) %>%
                    formatStyle(
                        "ko_p_value",
                        backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                    ) %>%
                    formatStyle(
                        "ex_p_value",
                        backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                    )
            }
            return(t)
        })
    })


    observeEvent(input$loss_method_sig, {
        output$loss_methods_by_character <- DT::renderDataTable({
            rd <- how_rounds_lost_data(data)

            if (input$loss_method_sig) {
                rd <- rd[, c(1, 6, 3, 7, 4, 8, 2, 9, 5)]
            } else {
                rd <- rd[, c(1, 6, 3, 4, 2, 5)]
            }

            t <- datatable(rd, options = list(paging = FALSE, searching = FALSE)) %>%
                formatPercentage("KO", digits = 1) %>%
                formatPercentage("EX", digits = 1) %>%
                formatPercentage("RO", digits = 1) %>%
                formatPercentage("TO", digits = 2)

            if (input$loss_method_sig) {
                t <- t %>%
                    formatRound("ro_p_value", digits = 3) %>%
                    formatRound("ex_p_value", digits = 3) %>%
                    formatRound("ko_p_value", digits = 3) %>%
                    formatStyle(
                        "ro_p_value",
                        backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                    ) %>%
                    formatStyle(
                        "ko_p_value",
                        backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                    ) %>%
                    formatStyle(
                        "ex_p_value",
                        backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                    )
            }

            return(t)
        })
    })

    output$how_rounds_end_per_stage <- DT::renderDataTable({
        datatable(how_rounds_end_per_stage(data), options = list(paging = FALSE, searching = FALSE)) %>%
            formatPercentage("KO", digits = 1) %>%
            formatPercentage("EX", digits = 1) %>%
            formatPercentage("RO", digits = 1) %>%
            formatPercentage("TO", digits = 2)
    })

    localized_characters <- reactive({
        lang <- input$language # Get the selected language
        dict$characters[[lang]] # Return character names based on selected language
    })

    output$all_matchups <- renderText("All matchups (players maybe different ranks)")
    output$same_rank_matchups <- renderText("Players are same rank")

    output$all_matchups_rounds <- renderText("All matchups (players maybe different ranks)")
    output$same_rank_matchups_rounds <- renderText("Players are same rank")


    observeEvent(input$rounds_won_sig, {
        rounds_won_per_character_any_rank <- rounds_won_per_character_any_rank %>%
            mutate(Icon = paste("<img src = 'images/", Character, ".jpg' height='45px'/>", sep = ""), .after = Character)

        if (input$rounds_won_sig == FALSE) {
            rounds_won_per_character_any_rank$p_value <- NULL
            rounds_won_per_character_any_rank <- rounds_won_per_character_any_rank %>% setNames(c("Character", "Icon", "# Matches", "# Rnd. Won", "# Rnd. Won / Match"))
        } else {
            rounds_won_per_character_any_rank <- rounds_won_per_character_any_rank %>% setNames(c("Character", "Icon", "# Matches", "# Rnd. Won", "# Rnd. Won / Match", "p-value"))
        }

        output$rounds_per_match_table <- DT::renderDataTable(
            {
                t <- datatable(rounds_won_per_character_any_rank, escape = FALSE, options = list(paging = FALSE, searching = FALSE)) %>%
                    formatRound("# Rnd. Won / Match", 3) %>%
                    formatStyle(
                        columns = names(rounds_won_per_character_any_rank),
                        fontSize = "24px"
                    ) %>%
                    formatCurrency(columns = c("# Matches", "# Rnd. Won"), currency = "", interval = 3, mark = ",", digits = 0) %>%
                    formatStyle(columns = c("# Matches", "# Rnd. Won", "# Rnd. Won / Match"), textAlign = "right")

                if (input$rounds_won_sig) {
                    t <- t %>%
                        formatRound("p-value", 3) %>%
                        formatStyle(
                            "p-value",
                            backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                        )
                }

                return(t)
            },
            class = "custom-dt-table"
        )
    })

    observeEvent(input$rounds_won_same_rank_sig, {
        rounds_won_per_character_same_rank <- rounds_won_per_character_same_rank %>%
            mutate(Icon = paste("<img src = 'images/", Character, ".jpg' height='45px'/>", sep = ""), .after = Character)

        if (input$rounds_won_same_rank_sig == FALSE) {
            rounds_won_per_character_same_rank$p_value <- NULL
            rounds_won_per_character_same_rank <- rounds_won_per_character_same_rank %>% setNames(c("Character", "Icon", "# Matches", "# Rnd. Won", "# Rnd. Won / Match"))
        } else {
            rounds_won_per_character_same_rank <- rounds_won_per_character_same_rank %>% setNames(c("Character", "Icon", "# Matches", "# Rnd. Won", "# Rnd. Won / Match", "p-value"))
        }

        output$rounds_per_match_same_rank_table <- DT::renderDataTable(
            {
                t <- datatable(rounds_won_per_character_same_rank, escape = FALSE, options = list(paging = FALSE, searching = FALSE)) %>%
                    formatRound("# Rnd. Won / Match", 3) %>%
                    formatStyle(
                        columns = names(rounds_won_per_character_same_rank),
                        fontSize = "24px"
                    ) %>%
                    formatCurrency(columns = c("# Matches", "# Rnd. Won"), currency = "", interval = 3, mark = ",", digits = 0) %>%
                    formatStyle(columns = c("# Matches", "# Rnd. Won", "# Rnd. Won / Match"), textAlign = "right")

                if (input$rounds_won_same_rank_sig) {
                    t <- t %>%
                        formatRound("p-value", 3) %>%
                        formatStyle(
                            "p-value",
                            backgroundColor = styleInterval(c(0.05), c("yellow", ""))
                        )
                }
                return(t)
            },
            class = "custom-dt-table"
        )

        output$unique_players <- renderText({
            paste(comma(number_unique_players(youtube_video_data())), " unique players total")
        })

        output$youtube_videos_table <- DT::renderDataTable({
            dt <- youtube_video_data()
            dt <- dt %>%
                mutate(Player.1.Character = paste0("<img src = 'images/", `Player.1.Character`, ".jpg' height='45px' style='margin-top: 2px; margin-bottom: 2px;margin-right: 2px;'/><br/>", `Player.1.Character`)) %>%
                mutate(Player.2.Character = paste0("<img src = 'images/", `Player.2.Character`, ".jpg' height='45px' style='margin-top: 2px; margin-bottom: 2px;margin-right: 2px;'/><br/>", `Player.2.Character`))

            datatable(dt, escape = FALSE, options = list(lengthChange = FALSE, searching = TRUE, pageLength = 100, searchHighlight = TRUE)) %>%
                formatStyle(
                    columns = names(dt),
                    fontSize = "24px"
                )
        })

        output$matches_per_year_bar_chart <- renderPlot({
            data <- youtube_video_data()
            data <- data %>%
                filter(grepl("^\\d{4}/\\d{2}/\\d{2}$", Date))

            # Preprocess data
            yearly_data <- data %>%
                mutate(Year = year(Date)) %>% # Extract year
                count(Year) # Count matches per year

            # Create bar chart
            ggplot(yearly_data, aes(x = Year, y = n)) +
                geom_bar(stat = "identity", fill = "skyblue") +
                labs(
                    title = "Total Matches Per Year",
                    x = "Year",
                    y = "Number of Matches"
                ) +
                theme_minimal()
        })

        output$unique_players_bar_chart <- renderPlot({
            # Clean and preprocess the data
            clean_data <- youtube_video_data() %>%
                filter(grepl("^\\d{4}/\\d{2}/\\d{2}$", Date)) %>% # Keep valid dates
                mutate(
                    Date = as.Date(Date, format = "%Y/%m/%d"), # Convert to Date format
                    Year = year(Date) # Extract year
                ) %>%
                select(Year, `Player.1.Ringname`, `Player.2.Ringname`) %>%
                pivot_longer(cols = ends_with("Ringname"), values_to = "Player") %>% # Combine player columns
                distinct(Year, Player) # Identify unique players per year

            # Count unique players per year
            player_counts <- clean_data %>%
                count(Year, name = "UniquePlayers")

            # Create bar chart
            ggplot(player_counts, aes(x = Year, y = UniquePlayers)) +
                geom_bar(stat = "identity", fill = "steelblue") +
                labs(
                    title = "Unique Players Per Year",
                    x = "Year",
                    y = "Number of Unique Players"
                ) +
                theme_minimal()
        })

        output$unique_players_per_character_chart <- renderPlot({
            combined <- youtube_video_data() %>%
                select(Date, Player.1.Ringname, Player.1.Character) %>%
                rename(ringname = Player.1.Ringname, character = Player.1.Character) %>%
                bind_rows(
                    youtube_video_data() %>%
                        select(Date, Player.2.Ringname, Player.2.Character) %>%
                        rename(ringname = Player.2.Ringname, character = Player.2.Character)
                )

            combined <- combined %>%
                filter(grepl("^\\d{4}/\\d{2}/\\d{2}$", Date)) %>% # Keep valid dates
                mutate(
                    Date = as.Date(Date, format = "%Y/%m/%d"), # Convert to Date format
                    Year = year(Date) # Extract year
                )

            unique_per_character <- combined %>%
                group_by(Year, character) %>%
                summarise(total_unique_players = n_distinct(ringname), .groups = "drop")

            ggplot(unique_per_character, aes(x = Year, y = total_unique_players, color = character)) +
                geom_line(size = 1) +
                geom_point(size = 2) +
                labs(
                    title = "Unique Players Per Character Per Year",
                    x = "Year",
                    y = "total_unique_players",
                    color = "character"
                ) +
                theme_minimal() +
                theme(legend.position = "bottom")
        })

        output$unique_players_per_rank_chart <- renderPlot({
            combined <- youtube_video_data() %>%
                select(Player.1.Ringname, Player.1.Rank) %>%
                rename(ringname = Player.1.Ringname, rank = Player.1.Rank) %>%
                bind_rows(
                    youtube_video_data() %>%
                        select(Player.2.Ringname, Player.2.Rank) %>%
                        rename(ringname = Player.2.Ringname, rank = Player.2.Rank)
                )

            unique_per_rank <- combined %>%
                group_by(rank) %>%
                summarise(unique_players = n_distinct(ringname), .groups = "drop")

            ggplot(unique_per_rank, aes(x = rank, y = unique_players)) +
                geom_bar(stat = "identity", fill = "steelblue") +
                labs(
                    title = "Unique Players Per Rank",
                    x = "Rank",
                    y = "Number of Unique Players"
                ) +
                theme_minimal()
        })

        shun_data <- reactive({
            shun_matches %>%
                filter(`Player.1.Character` == "Shun" | `Player.2.Character` == "Shun") %>%
                filter(Stage != "Island") %>%
                filter(Stage != "Waterfalls") %>%
                filter(Stage != "River") %>%
                filter(Stage != "Training Room") %>%
                mutate(
                    drinks = ifelse(`Player.1.Character` == "Shun", as.numeric(`Shun.Drinks.1P`), -1) +
                        ifelse(`Player.2.Character` == "Shun", as.numeric(`Shun.Drinks.2P`), -1)
                ) %>%
                filter(round_number > 0) %>% # Exclude invalid rounds
                select(round_number, Shun.Drinks.1P, Shun.Drinks.2P) %>%
                pivot_longer(
                    cols = c(Shun.Drinks.1P, Shun.Drinks.2P),
                    names_to = "Player",
                    values_to = "Drinks"
                ) %>%
                filter(!is.na(Drinks)) %>%
                filter(Drinks >= 0)
        })

        output$shun_drinks_per_round <- renderPlot({
            ggplot(shun_data(), aes(x = round_number, y = Drinks, color = Player)) +
                geom_point(size = 3) +
                scale_color_manual(values = c("Shun.Drinks.1P" = "blue", "Shun.Drinks.2P" = "red")) +
                labs(
                    x = "Round Number",
                    y = "Number of Drinks (DP)",
                    title = "Scatter Plot of Shun's Drinks by Player and Round",
                    color = "Player"
                ) +
                theme_minimal()
        })
    })

    shun_data_round2 <- reactive({
        shun_matches %>%
            filter(`Player.1.Character` == "Shun" | `Player.2.Character` == "Shun") %>%
            mutate(
                `Shun.Drinks.1P` = ifelse(`Player.1.Character` == "Shun", as.numeric(`Shun.Drinks.1P`), NA),
                `Shun.Drinks.2P` = ifelse(`Player.2.Character` == "Shun", as.numeric(`Shun.Drinks.2P`), NA)
            ) %>%
            filter(round_number == 2) %>% # Filter for round 2 only
            pivot_longer(
                cols = c(`Shun.Drinks.1P`, `Shun.Drinks.2P`),
                names_to = "Player",
                values_to = "Drinks"
            ) %>%
            filter(!is.na(Drinks)) %>% # Exclude rows with NA drinks
            filter(Drinks >= 0) %>% # Exclude rows with NA drinks
            filter(Stage != "Island") %>%
            filter(Stage != "Waterfalls") %>%
            filter(Stage != "River") %>%
            filter(Stage != "Training Room") %>%
            group_by(Drinks) %>% # Group by Drinks
            summarize(Count = n(), .groups = "drop") # Count occurrences of each Drinks value
    })

    shun_data_round3 <- reactive({
        shun_matches %>%
            filter(`Player.1.Character` == "Shun" | `Player.2.Character` == "Shun") %>%
            mutate(
                `Shun.Drinks.1P` = ifelse(`Player.1.Character` == "Shun", as.numeric(`Shun.Drinks.1P`), NA),
                `Shun.Drinks.2P` = ifelse(`Player.2.Character` == "Shun", as.numeric(`Shun.Drinks.2P`), NA)
            ) %>%
            filter(round_number == 3) %>% # Filter for round 2 only
            pivot_longer(
                cols = c(`Shun.Drinks.1P`, `Shun.Drinks.2P`),
                names_to = "Player",
                values_to = "Drinks"
            ) %>%
            filter(!is.na(Drinks)) %>% # Exclude rows with NA drinks
            filter(Drinks >= 0) %>% # Exclude rows with NA drinks
            filter(Stage != "Island") %>%
            filter(Stage != "Waterfalls") %>%
            filter(Stage != "River") %>%
            filter(Stage != "Training Room") %>%
            group_by(Drinks) %>% # Group by Drinks
            summarize(Count = n(), .groups = "drop") # Count occurrences of each Drinks value
    })

    shun_data_round4 <- reactive({
        shun_matches %>%
            filter(`Player.1.Character` == "Shun" | `Player.2.Character` == "Shun") %>%
            mutate(
                `Shun.Drinks.1P` = ifelse(`Player.1.Character` == "Shun", as.numeric(`Shun.Drinks.1P`), NA),
                `Shun.Drinks.2P` = ifelse(`Player.2.Character` == "Shun", as.numeric(`Shun.Drinks.2P`), NA)
            ) %>%
            filter(round_number == 4) %>% # Filter for round 2 only
            pivot_longer(
                cols = c(`Shun.Drinks.1P`, `Shun.Drinks.2P`),
                names_to = "Player",
                values_to = "Drinks"
            ) %>%
            filter(!is.na(Drinks)) %>% # Exclude rows with NA drinks
            filter(Drinks >= 0) %>% # Exclude rows with NA drinks
            filter(Stage != "Island") %>%
            filter(Stage != "Waterfalls") %>%
            filter(Stage != "River") %>%
            filter(Stage != "Training Room") %>%
            group_by(Drinks) %>% # Group by Drinks
            summarize(Count = n(), .groups = "drop") # Count occurrences of each Drinks value
    })

    shun_data_round5 <- reactive({
        shun_matches %>%
            filter(`Player.1.Character` == "Shun" | `Player.2.Character` == "Shun") %>%
            mutate(
                `Shun.Drinks.1P` = ifelse(`Player.1.Character` == "Shun", as.numeric(`Shun.Drinks.1P`), NA),
                `Shun.Drinks.2P` = ifelse(`Player.2.Character` == "Shun", as.numeric(`Shun.Drinks.2P`), NA)
            ) %>%
            filter(round_number == 5) %>% # Filter for round 2 only
            pivot_longer(
                cols = c(`Shun.Drinks.1P`, `Shun.Drinks.2P`),
                names_to = "Player",
                values_to = "Drinks"
            ) %>%
            filter(!is.na(Drinks)) %>% # Exclude rows with NA drinks
            filter(Drinks >= 0) %>% # Exclude rows with NA drinks
            filter(Stage != "Island") %>%
            filter(Stage != "Waterfalls") %>%
            filter(Stage != "River") %>%
            filter(Stage != "Training Room") %>%
            group_by(Drinks) %>% # Group by Drinks
            summarize(Count = n(), .groups = "drop") # Count occurrences of each Drinks value
    })

    output$shun_distribution_round2 <- renderPlot({
        stats <- shun_data_round2() %>%
            summarize(
                mean_drinks = mean(Drinks, na.rm = TRUE),
                median_drinks = median(Drinks, na.rm = TRUE),
                mode_drinks = Drinks[which.max(Count)] # Find the mode
            )

        ggplot(shun_data_round2(), aes(x = Drinks, y = Count)) +
            geom_bar(stat = "identity", fill = "skyblue", color = "black") +
            scale_x_continuous(
                breaks = seq(0, max(shun_data_round2()$Drinks, na.rm = TRUE), by = 5) # Custom tick intervals
            ) +
            labs(
                x = "Number of Drinks (DP)",
                y = "Number of Rounds",
                title = "Distribution of Shun's Starting Drinks (Round 2)"
            ) +
            annotate(
                "text",
                x = max(shun_data_round2()$Drinks, na.rm = TRUE) * 0.01,
                y = 350,
                label = paste0(
                    "Mean: ", round(stats$mean_drinks, 2), ", ",
                    "Median: ", stats$median_drinks, ", ",
                    "Mode: ", stats$mode_drinks
                ),
                hjust = 0,
                size = 5,
                color = "black"
            ) +
            theme_minimal()
    })


    output$shun_distribution_round3 <- renderPlot({
        stats <- shun_data_round3() %>%
            summarize(
                mean_drinks = mean(Drinks, na.rm = TRUE),
                median_drinks = median(Drinks, na.rm = TRUE),
                mode_drinks = Drinks[which.max(Count)] # Find the mode
            )

        ggplot(shun_data_round3(), aes(x = Drinks, y = Count)) +
            geom_bar(stat = "identity", fill = "skyblue", color = "black") +
            scale_x_continuous(
                breaks = seq(0, max(shun_data_round2()$Drinks, na.rm = TRUE), by = 5) # Custom tick intervals
            ) +
            labs(
                x = "Number of Drinks (DP)",
                y = "Number of Rounds",
                title = "Distribution of Shun's Starting Drinks (Round 3)"
            ) +
            annotate(
                "text",
                x = max(shun_data_round2()$Drinks, na.rm = TRUE) * 0.01,
                y = 200,
                label = paste0(
                    "Mean: ", round(stats$mean_drinks, 2), ", ",
                    "Median: ", stats$median_drinks, ", ",
                    "Mode: ", stats$mode_drinks
                ),
                hjust = 0,
                size = 5,
                color = "black"
            ) +
            theme_minimal()
    })

    output$shun_distribution_round4 <- renderPlot({
        stats <- shun_data_round4() %>%
            summarize(
                mean_drinks = mean(Drinks, na.rm = TRUE),
                median_drinks = median(Drinks, na.rm = TRUE),
                mode_drinks = Drinks[which.max(Count)] # Find the mode
            )

        ggplot(shun_data_round4(), aes(x = Drinks, y = Count)) +
            geom_bar(stat = "identity", fill = "skyblue", color = "black") +
            scale_x_continuous(
                breaks = seq(0, max(shun_data_round2()$Drinks, na.rm = TRUE), by = 5) # Custom tick intervals
            ) +
            labs(
                x = "Number of Drinks (DP)",
                y = "Number of Rounds",
                title = "Distribution of Shun's Starting Drinks (Round 4)"
            ) +
            annotate(
                "text",
                x = max(shun_data_round2()$Drinks, na.rm = TRUE) * 0.01,
                y = 75,
                label = paste0(
                    "Mean: ", round(stats$mean_drinks, 2), ", ",
                    "Median: ", stats$median_drinks, ", ",
                    "Mode: ", stats$mode_drinks
                ),
                hjust = 0,
                size = 5,
                color = "black"
            ) +
            theme_minimal()
    })

    output$shun_distribution_round5 <- renderPlot({
        stats <- shun_data_round5() %>%
            summarize(
                mean_drinks = mean(Drinks, na.rm = TRUE),
                median_drinks = median(Drinks, na.rm = TRUE),
                mode_drinks = Drinks[which.max(Count)] # Find the mode
            )

        ggplot(shun_data_round5(), aes(x = Drinks, y = Count)) +
            geom_bar(stat = "identity", fill = "skyblue", color = "black") +
            scale_x_continuous(
                breaks = seq(0, max(shun_data_round2()$Drinks, na.rm = TRUE), by = 5) # Custom tick intervals
            ) +
            labs(
                x = "Number of Drinks (DP)",
                y = "Number of Rounds",
                title = "Distribution of Shun's Starting Drinks (Round 5)"
            ) +
            annotate(
                "text",
                x = max(shun_data_round2()$Drinks, na.rm = TRUE) * 0.01,
                y = 40,
                label = paste0(
                    "Mean: ", round(stats$mean_drinks, 2), ", ",
                    "Median: ", stats$median_drinks, ", ",
                    "Mode: ", stats$mode_drinks
                ),
                hjust = 0,
                size = 5,
                color = "black"
            ) +
            theme_minimal()
    })

    unique_players_string <- reactive({
        paste("Unique Players", unique_players_count(data))
    })

    output$matches_per_player <- DT::renderDataTable({
        datatable(matches_per_player(data))
    })

    output$shun_win_line_chart <- renderPlot({
        ggplot(shun_line_data, aes(x = Drinks, y = Win_Percentage)) +
            geom_line(color = "blue", size = 1) +
            geom_point(color = "red", size = 3) +
            labs(
                x = "Number of Drinks (DP)",
                y = "Win Percentage (%)",
                title = "Shun's Win Percentage by Number of Drinks"
            ) +
            scale_x_continuous(breaks = seq(0, max(shun_line_data$Drinks, na.rm = TRUE), by = 5)) +
            scale_y_continuous(limits = c(0, 100)) + # Ensure y-axis shows percentages clearly
            theme_minimal()
    })
}
