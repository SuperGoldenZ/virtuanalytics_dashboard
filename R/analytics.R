library(dplyr)
library(tidyr)

stage_types <- data.frame(
    Stage = c(
        "Deep Mountain", "Palace", "City", "Ruins", "Arena", "Waterfalls",
        "Broken House", "Grassland", "Aurora", "Island", "Statues",
        "Terrace", "Snow Mountain", "Training Room", "Shrine", "Temple",
        "River", "Sumo Ring", "Genesis", "Great Wall"
    ),
    Stage_Type = c(
        "Rectangle", "Rectangle", "Full Fence", "Full Fence", "Octagon",
        "Octagon", "Breakable Full Fence", "Breakable Full Fence",
        "Breakable Half Fence", "Breakable Half Fence", "Half Fence",
        "Half Fence", "Full Fence and Open", "Full Fence and Open",
        "Low Fence", "Low Fence", "Open", "Open", "Single Wall",
        "Single Wall"
    )
)

win_percentages_per_character <- function(data, character_name) {
    stage_type_lookup <- data.frame(
        Stage = c(
            "Deep Mountain", "Palace", "City", "Ruins", "Arena", "Waterfalls",
            "Broken House", "Grassland", "Aurora", "Island", "Statues",
            "Terrace", "Snow Mountain", "Training Room", "Shrine", "Temple",
            "River", "Sumo Ring", "Genesis", "Great Wall"
        ),
        Stage.Type = c(
            "Rectangle", "Rectangle", "Full Fence", "Full Fence", "Octagon", "Octagon",
            "Breakable Full Fence", "Breakable Full Fence", "Breakable Half Fence",
            "Breakable Half Fence", "Half Fence", "Half Fence", "Full Fence and Open",
            "Full Fence and Open", "Low Fence", "Low Fence", "Open", "Open",
            "Single Wall", "Single Wall"
        )
    )

    # Add Stage Type to data based on the lookup
    stage_data <- data %>%
        left_join(stage_type_lookup, by = "Stage")

    # Filter for matches where Blaze is one of the players, but not both
    blaze_data <- stage_data %>%
        filter((Player.1.Character == character_name & Player.2.Character != character_name) |
            (Player.2.Character == character_name & Player.1.Character != character_name))

    # Determine the winner of the match (the character that won the last round)
    match_winners <- blaze_data %>%
        group_by(Match.ID) %>%
        filter(round_number == max(round_number)) %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        select(Match.ID, Winner_Character, Stage.Type)

    # Total number of matches Blaze played in each stage category
    blaze_played_in_stage <- blaze_data %>%
        group_by(Match.ID, Stage.Type) %>%
        summarise(Total_Matches = n_distinct(Match.ID), .groups = "drop") %>%
        group_by(Stage.Type) %>%
        summarise(Total_Matches = sum(Total_Matches), .groups = "drop")

    # Total number of matches Blaze won in each stage category
    blaze_wins <- match_winners %>%
        filter(Winner_Character == character_name) %>%
        group_by(Stage.Type) %>%
        summarise(Matches_Won = n(), .groups = "drop")

    # Merge total matches and matches won, and calculate win percentage
    blaze_win_percentage <- blaze_played_in_stage %>%
        left_join(blaze_wins, by = "Stage.Type") %>%
        mutate(
            Matches_Won = ifelse(is.na(Matches_Won), 0, Matches_Won), # Handle cases with no wins
            Win_Percentage = (Matches_Won / Total_Matches) # Calculate win percentage
        ) %>%
        arrange(desc(Win_Percentage)) # Sort by win percentage in descending order

    blaze_win_percentage$p_value <- NA

    # Rename columns for clarity
    blaze_win_percentage <- blaze_win_percentage %>%
        rename(
            Stage.Category = Stage.Type,
            Matches.Won = Matches_Won,
            Total.Matches = Total_Matches,
            Win.Percentage = Win_Percentage
        )

    for (stage in unique(data$Stage)) {
        rounds_won_specific <- rounds_won_per_stage_per_character(data, character_name, stage)
        rounds_won_other <- rounds_won_per_other_stages_per_character(data, character_name, stage)

        t_test_result <- t.test(rounds_won_specific, rounds_won_other)

        blaze_win_percentage$p_value[blaze_win_percentage$Stage.Category == get_stage_type(stage, stage_type_lookup)] <- t_test_result$p.value
    }

    blaze_win_percentage <- blaze_win_percentage %>%
        rename(
            `Win %` = Win.Percentage
        )
    return(blaze_win_percentage)
}

get_stage_type <- function(stage_name, lookup_table) {
    result <- lookup_table %>%
        filter(Stage == stage_name) %>%
        select(Stage.Type) %>%
        pull() # Extract the value as a vector

    return(result)
}

matches_won_per_stage_per_character <- function(data, character_name) {
    stage_type_lookup <- data.frame(
        Stage = c(
            "Deep Mountain", "Palace", "City", "Ruins", "Arena", "Waterfalls",
            "Broken House", "Grassland", "Aurora", "Island", "Statues",
            "Terrace", "Snow Mountain", "Training Room", "Shrine", "Temple",
            "River", "Sumo Ring", "Genesis", "Great Wall"
        ),
        Stage.Type = c(
            "Rectangle", "Rectangle", "Full Fence", "Full Fence", "Octagon", "Octagon",
            "Breakable Full Fence", "Breakable Full Fence", "Breakable Half Fence",
            "Breakable Half Fence", "Half Fence", "Half Fence", "Full Fence and Open",
            "Full Fence and Open", "Low Fence", "Low Fence", "Open", "Open",
            "Single Wall", "Single Wall"
        )
    )

    # Add Stage Type to data based on the lookup
    stage_data <- data %>%
        left_join(stage_type_lookup, by = "Stage")

    # Filter for matches where Blaze is one of the players, but not both
    blaze_data <- stage_data %>%
        filter((Player.1.Character == character_name & Player.2.Character != character_name) |
            (Player.2.Character == character_name & Player.1.Character != character_name))

    # Determine the winner of the match (the character that won the last round)
    match_winners <- blaze_data %>%
        group_by(Match.ID) %>%
        filter(round_number == max(round_number)) %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Winner_Character = ifelse(Winner_Character != character_name, 0, 1)) %>%
        select(Match.ID, Winner_Character, Stage.Type)

    return(compare_stage_types_winning_character(match_winners))
}

rounds_won_per_stage_per_character <- function(data, character_name, stage_name) {
    stage_type_lookup <- data.frame(
        Stage = c(
            "Deep Mountain", "Palace", "City", "Ruins", "Arena", "Waterfalls",
            "Broken House", "Grassland", "Aurora", "Island", "Statues",
            "Terrace", "Snow Mountain", "Training Room", "Shrine", "Temple",
            "River", "Sumo Ring", "Genesis", "Great Wall"
        ),
        Stage.Type = c(
            "Rectangle", "Rectangle", "Full Fence", "Full Fence", "Octagon", "Octagon",
            "Breakable Full Fence", "Breakable Full Fence", "Breakable Half Fence",
            "Breakable Half Fence", "Half Fence", "Half Fence", "Full Fence and Open",
            "Full Fence and Open", "Low Fence", "Low Fence", "Open", "Open",
            "Single Wall", "Single Wall"
        )
    )

    # Add Stage Type to data based on the lookup
    stage_data <- data %>%
        left_join(stage_type_lookup, by = "Stage") %>%
        filter(Stage.Type == get_stage_type(stage_name, stage_type_lookup))

    # Filter for matches where Blaze is one of the players, but not both
    character_wins <- stage_data %>%
        filter((Player.1.Character == character_name & Player.2.Character != character_name) |
            (Player.2.Character == character_name & Player.1.Character != character_name))
    # filter((Player.1.Character == character_name & Winning.Player.Number == 1) |
    # (Player.2.Character == character_name & Winning.Player.Number == 2))

    round_winners <- character_wins %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character))

    round_winners <- round_winners %>%
        mutate(Main_Character_Won = ifelse(Winner_Character == character_name, 1, 0))

    rounds_won_summary <- round_winners %>%
        pull(Main_Character_Won)


    # rounds_won_summary <- character_wins %>%
    # group_by(Match.ID, Stage) %>%
    # summarise(Rounds.Won = n(), .groups = "drop") %>%
    # arrange(desc(Rounds.Won)) %>%
    # pull(Rounds.Won)

    return(rounds_won_summary)
}

compare_stage_types <- function(data) {
    # Get unique stage types
    stage_types <- unique(data$Stage.Type)

    # Initialize a matrix to store p-values
    p_value_matrix <- matrix(NA,
        nrow = length(stage_types), ncol = length(stage_types),
        dimnames = list(stage_types, stage_types)
    )

    # Loop through each pair of stage types
    for (i in seq_along(stage_types)) {
        for (j in seq_along(stage_types)) {
            if (i != j) {
                # Filter Rounds.Won for each stage type
                rounds_won_i <- data$Rounds.Won[data$Stage.Type == stage_types[i]]
                rounds_won_j <- data$Rounds.Won[data$Stage.Type == stage_types[j]]

                # Perform t-test
                t_test_result <- t.test(rounds_won_i, rounds_won_j)

                # Store p-value in the matrix
                p_value_matrix[i, j] <- t_test_result$p.value
            }
        }
    }

    # Convert matrix to data frame for easier viewing in a table
    p_value_table <- as.data.frame(p_value_matrix)
    return(p_value_table)
}

compare_stage_types_winning_character <- function(data) {
    # Get unique stage types
    stage_types <- unique(data$Stage.Type)

    # Initialize a matrix to store p-values
    p_value_matrix <- matrix(NA,
        nrow = length(stage_types), ncol = length(stage_types),
        dimnames = list(stage_types, stage_types)
    )

    # Loop through each pair of stage types
    for (i in seq_along(stage_types)) {
        for (j in seq_along(stage_types)) {
            if (i != j) {
                # Filter Rounds.Won for each stage type
                rounds_won_i <- data$Winner_Character[data$Stage.Type == stage_types[i]]
                rounds_won_j <- data$Winner_Character[data$Stage.Type == stage_types[j]]

                # Perform t-test
                t_test_result <- t.test(rounds_won_i, rounds_won_j)

                # Store p-value in the matrix
                p_value_matrix[i, j] <- t_test_result$p.value
            }
        }
    }

    # Convert matrix to data frame for easier viewing in a table
    p_value_table <- as.data.frame(p_value_matrix)
    return(p_value_table)
}

compare_stage_match_types <- function(data) {
    # Get unique stage types
    stage_types <- unique(data$Stage.Type)

    # Initialize a matrix to store p-values
    p_value_matrix <- matrix(NA,
        nrow = length(stage_types), ncol = length(stage_types),
        dimnames = list(stage_types, stage_types)
    )

    # Loop through each pair of stage types
    for (i in seq_along(stage_types)) {
        for (j in seq_along(stage_types)) {
            if (i != j) {
                # Filter Rounds.Won for each stage type
                rounds_won_i <- data$Rounds.Won[data$Stage.Type == stage_types[i]]
                rounds_won_j <- data$Rounds.Won[data$Stage.Type == stage_types[j]]

                # Perform t-test
                t_test_result <- t.test(rounds_won_i, rounds_won_j)

                # Store p-value in the matrix
                p_value_matrix[i, j] <- t_test_result$p.value
            }
        }
    }

    # Convert matrix to data frame for easier viewing in a table
    p_value_table <- as.data.frame(p_value_matrix)
    return(p_value_table)
}

rounds_won_per_other_stages_per_character <- function(data, character_name, stage_name) {
    stage_type_lookup <- data.frame(
        Stage = c(
            "Deep Mountain", "Palace", "City", "Ruins", "Arena", "Waterfalls",
            "Broken House", "Grassland", "Aurora", "Island", "Statues",
            "Terrace", "Snow Mountain", "Training Room", "Shrine", "Temple",
            "River", "Sumo Ring", "Genesis", "Great Wall"
        ),
        Stage.Type = c(
            "Rectangle", "Rectangle", "Full Fence", "Full Fence", "Octagon", "Octagon",
            "Breakable Full Fence", "Breakable Full Fence", "Breakable Half Fence",
            "Breakable Half Fence", "Half Fence", "Half Fence", "Full Fence and Open",
            "Full Fence and Open", "Low Fence", "Low Fence", "Open", "Open",
            "Single Wall", "Single Wall"
        )
    )

    # Add Stage Type to data based on the lookup
    stage_data <- data %>%
        left_join(stage_type_lookup, by = "Stage") %>%
        filter(Stage.Type != get_stage_type(stage_name, stage_type_lookup))

    # Filter for matches where Blaze is one of the players, but not both
    character_wins <- stage_data %>%
        filter((Player.1.Character == character_name & Player.2.Character != character_name) |
            (Player.2.Character == character_name & Player.1.Character != character_name))

    round_winners <- character_wins %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character))

    round_winners <- round_winners %>%
        mutate(Main_Character_Won = ifelse(Winner_Character == character_name, 1, 0))

    rounds_won_summary <- round_winners %>%
        pull(Main_Character_Won)

    return(rounds_won_summary)
}

rounds_won_per_stage_per_character_lookup <- function(data, character_name) {
    stage_type_lookup <- data.frame(
        Stage = c(
            "Deep Mountain", "Palace", "City", "Ruins", "Arena", "Waterfalls",
            "Broken House", "Grassland", "Aurora", "Island", "Statues",
            "Terrace", "Snow Mountain", "Training Room", "Shrine", "Temple",
            "River", "Sumo Ring", "Genesis", "Great Wall"
        ),
        Stage.Type = c(
            "Rectangle", "Rectangle", "Full Fence", "Full Fence", "Octagon", "Octagon",
            "Breakable Full Fence", "Breakable Full Fence", "Breakable Half Fence",
            "Breakable Half Fence", "Half Fence", "Half Fence", "Full Fence and Open",
            "Full Fence and Open", "Low Fence", "Low Fence", "Open", "Open",
            "Single Wall", "Single Wall"
        )
    )

    # Add Stage Type to data based on the lookup
    stage_data <- data %>%
        left_join(stage_type_lookup, by = "Stage")

    # Filter for matches where Blaze is one of the players, but not both
    character_wins <- stage_data %>%
        filter((Player.1.Character == character_name & Player.2.Character != character_name) |
            (Player.2.Character == character_name & Player.1.Character != character_name)) %>%
        filter((Player.1.Character == character_name & Winning.Player.Number == 1) |
            (Player.2.Character == character_name & Winning.Player.Number == 2))

    temp_data <- character_wins %>%
        group_by(Match.ID, Stage.Type) %>%
        summarise(Rounds.Won = n(), .groups = "drop")

    return(compare_stage_types(temp_data))
}

csv_filename <- "vf_match_data.csv"

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if a file name was provided
# if (length(args) == 0) {
# stop("Error: No CSV file provided. Please provide the CSV file as an argument.")
# }

# Assign the first argument to a variable as the filename
if (length(args) > 0) {
    csv_filename <- args[1]
}

# Check if the file exists
if (!file.exists(csv_filename)) {
    stop(paste("Error: File", csv_filename, "not found. Please check the file path and try again."))
}


# Read the CSV file
data <- read.csv(csv_filename)

match_data <- data %>%
    filter(as.numeric(Player.1.Rank) >= 40 & as.numeric(Player.2.Rank) >= 40 & round_number == 0)

ranks <- sort(unique(as.numeric(c(match_data$Player.1.Rank, match_data$Player.2.Rank))))
characters <- sort(unique(c(match_data$Player.1.Character, match_data$Player.2.Character)))
stages <- sort(unique(c(match_data$Stage)))

# Combine Player 1 and Player 2 ranks and characters into one column each
data_combined <- match_data %>%
    pivot_longer(cols = c(Player.1.Rank, Player.2.Rank), names_to = "Player", values_to = "player_rank") %>%
    pivot_longer(cols = c(Player.1.Character, Player.2.Character), names_to = "PlayerCharacter", values_to = "character") %>%
    pivot_longer(cols = c(Stage), names_to = "Stage", values_to = "stage") %>%
    select(-Player, -PlayerCharacter, -Stage)

match_data$Youtube.Link <- paste0("<a href='", match_data$Youtube.Link, "' target='_blank'>Open</a>")


############################ 3
# Identify the winner for each match by the last round played in that match
match_winners <- data %>%
    group_by(Match.ID) %>%
    filter(round_number == max(round_number)) %>% # Get the last round of each match
    mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
    select(Match.ID, Player.1.Character, Player.2.Character, Winner_Character)

# Calculate total number of matches each character participated in
total_matches <- match_winners %>%
    select(Match.ID, Player.1.Character, Player.2.Character) %>%
    pivot_longer(cols = c(Player.1.Character, Player.2.Character), names_to = "Player", values_to = "Character") %>%
    group_by(Character) %>%
    summarise(Total_Matches = n(), .groups = "drop")

# Calculate total number of wins for each character
win_counts <- match_winners %>%
    group_by(Winner_Character) %>%
    summarise(Total_Wins = n(), .groups = "drop")

# Merge total matches and win counts to calculate win percentage
win_percentage_table <- total_matches %>%
    left_join(win_counts, by = c("Character" = "Winner_Character")) %>%
    mutate(
        Total_Wins = ifelse(is.na(Total_Wins), 0, Total_Wins), # Handle characters with zero wins
        Win_Percentage = (Total_Wins / Total_Matches)
    ) %>%
    arrange(desc(Win_Percentage)) # Sort by win percentage in descending order

#################################
# Filter out matches where both players are the same character
matchup_data <- data %>%
    filter(Player.1.Character != Player.2.Character)

# Identify the winner of the match by the last round
match_winners <- matchup_data %>%
    group_by(Match.ID) %>%
    filter(round_number == max(round_number)) %>%
    mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
    select(Match.ID, Player.1.Character, Player.2.Character, Winner_Character)

# Create two perspectives for every matchup: one with Player 1 as main character, and one with Player 2 as main character
matchups_player1 <- match_winners %>%
    mutate(
        Main_Character = Player.1.Character,
        Opponent_Character = Player.2.Character,
        Main_Winner = (Winner_Character == Player.1.Character)
    )

matchups_player2 <- match_winners %>%
    mutate(
        Main_Character = Player.2.Character,
        Opponent_Character = Player.1.Character,
        Main_Winner = (Winner_Character == Player.2.Character)
    )

# Combine both perspectives into a single dataset
full_matchup_data <- bind_rows(matchups_player1, matchups_player2)

# Summarize the results: count total matches and wins for each character-opponent pairing
character_matchup <- full_matchup_data %>%
    group_by(Main_Character, Opponent_Character) %>%
    summarise(
        Total_Matches = n(),
        Wins_By_Main_Character = sum(Main_Winner),
        Win_Percentage = (Wins_By_Main_Character / Total_Matches),
        .groups = "drop"
    ) %>%
    arrange(desc(Total_Matches)) %>%
    rename(`Main\nCharacter` = `Main_Character`) %>%
    rename(`vs\nCharacter` = `Opponent_Character`) %>%
    rename(`Total\nMatches` = `Total_Matches`) %>%
    rename(`Wins By\nMain Character` = `Wins_By_Main_Character`)

count_character_matches <- function(data, character_name) {
    return(nrow(data %>% filter(round_number == 0) %>% filter((Player.1.Character == character_name & Player.2.Character != character_name) |
        (Player.2.Character == character_name & Player.1.Character != character_name))))
}

time_remaining_per_stage <- function() {
    # Filter rows where "How Round Ended" is NA
    filtered_data <- data %>% filter(round_number > 0)

    # Apply floor function to round down the "Time Remaining" to the nearest integer
    filtered_data <- filtered_data %>%
        mutate(Time.Seconds = floor(45 - Time.Remaining.When.Round.Ended))

    # Merge filtered data with stage types
    merged_data <- filtered_data %>%
        left_join(stage_types, by = "Stage")

    # Calculate average and median Time Remaining for each Stage
    summary_stats <- merged_data %>%
        group_by(Stage_Type) %>%
        summarise(
            Average_Time_Per_Round = mean(Time.Seconds, na.rm = TRUE),
            Median_Time_Per_Round = median(Time.Seconds, na.rm = TRUE),
            NFth = quantile(Time.Seconds, 0.95),
            Fastest = min(Time.Seconds),            
            .groups = "drop"
        ) %>%
        arrange((Average_Time_Per_Round))

    summary_stats$p_value <- NA

    for (stage_type in unique(summary_stats$`Stage_Type`)) {
        # print(time_remaining_per_round(stage_type))

        time_remaining_specific <- time_remaining_per_round(stage_type)
        time_remaining_other <- time_remaining_per_round(stage_type, TRUE)

        t_test_result <- t.test(time_remaining_specific, time_remaining_other)

        summary_stats$p_value[summary_stats$Stage_Type == stage_type] <- t_test_result$p.value
        #print(merged_data)
        #summary_stats$Fastest <- merged_data[merged_data$Stage_Type == stage_type][which.min(Time.Seconds), `Youtube Link`]
    }

    summary_stats$Median_Time_Per_Round <- NULL

    return(
        summary_stats %>%
            rename(`Stage\nType` = `Stage_Type`) %>%
            rename(`95th %tile` = `NFth`) # %>%
        # rename(`Avg. Time\nPer Round` = `Average Time Per Round`)
    )
}

time_remaining_per_round <- function(limit_stage_type, other) {
    # Filter rows where "How Round Ended" is NA
    filtered_data <- data %>% filter(round_number > 0)

    # Apply floor function to round down the "Time Remaining" to the nearest integer
    filtered_data <- filtered_data %>%
        mutate(Time.Seconds = floor(45 - Time.Remaining.When.Round.Ended))

    # Merge filtered data with stage types
    merged_data <- filtered_data %>%
        left_join(stage_types, by = "Stage")

    if (!missing(limit_stage_type) & !missing(other)) {
        merged_data <- merged_data %>%
            filter(`Stage_Type` != limit_stage_type)
    } else if (!missing(limit_stage_type)) {
        merged_data <- merged_data %>%
            filter(`Stage_Type` == limit_stage_type)
    }

    return(merged_data %>% pull(Time.Seconds))
}

win_rate_per_rank <- function() {
    match_winners <- data %>%
        group_by(Match.ID) %>%
        filter(round_number == max(round_number)) %>% # Get the last round of each match
        filter(Player.1.Rank != Player.2.Rank) %>%
        mutate(
            Winner_Rank = ifelse(Winning.Player.Number == 1, Player.1.Rank, Player.2.Rank),
            Loser_Rank = ifelse(Winning.Player.Number == 1, Player.2.Rank, Player.1.Rank)
        ) %>%
        select(Match.ID, Winner_Rank, Loser_Rank)

    win_percentage_lookup <- match_winners %>%
        filter(Winner_Rank > 39) %>%
        group_by(Winner_Rank, Loser_Rank) %>%
        summarise(
            wins = n(), # Count the number of wins
            total_matches = n() + sum(match_winners$Loser_Rank == Winner_Rank & match_winners$Winner_Rank == Loser_Rank), # Total matches involving this rank combination
            win_percentage = wins / total_matches # Calculate win percentage
        ) %>%
        select(Target_Rank = Winner_Rank, Other_Rank = Loser_Rank, win_percentage) %>%
        ungroup()
    return(win_percentage_lookup)
}
