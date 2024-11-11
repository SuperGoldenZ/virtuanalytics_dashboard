library(dplyr)

if (!exists("stage_type_lookup")) source("R/analytics_stage.R")

character_matchup_win_table <- function(data, character_name, character_matchup_win_table_data, rounds_won_vs_other_characters_lookup) {
    if (character_name %in% names(character_matchup_win_table_data)) {
        return(character_matchup_win_table_data[[character_name]])
    }

    # Filter out matches where both players are the same character
    matchup_data <- data %>%
        filter(Player.1.Character != Player.2.Character) %>%
        filter(Player.1.Character == character_name | Player.2.Character == character_name) %>%
        group_by(Match.ID) %>%
        filter(round_number == max(round_number))

    # Identify the winner of the match by the last round
    match_winners <- matchup_data %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character)) %>%
        select(Match.ID, Player.1.Character, Player.2.Character, Winner_Character, Loser_Character)

    # Create two perspectives for every matchup: one with Player 1 as main character, and one with Player 2 as main character
    matchups_player1 <- match_winners %>%
        mutate(
            Main_Character = Player.1.Character,
            Opponent_Character = Player.2.Character,
            Main_Winner = (Winner_Character == character_name)
        )

    matchups_player2 <- match_winners %>%
        mutate(
            Main_Character = Player.2.Character,
            Opponent_Character = Player.1.Character,
            Main_Winner = (Winner_Character == character_name)
        )

    # Combine both perspectives into a single dataset
    full_matchup_data <- bind_rows(matchups_player1, matchups_player2) %>%
        filter(Main_Character == character_name)

    # Summarize the results: count total matches and wins for each character-opponent pairing
    character_matchup <- full_matchup_data %>%
        group_by(Main_Character, Opponent_Character) %>%
        summarise(
            Total.Matches = n(),
            Wins_By_Main_Character = sum(Main_Winner),
            Win.Percentage = (Wins_By_Main_Character / Total.Matches),
            .groups = "drop"
        ) %>%
        arrange(desc(Win.Percentage))

    character_matchup$p_value <- NA

    for (opponent_character_name in unique(character_matchup$Opponent_Character)) {
        rounds_won_specific <- rounds_won_vs_character(data, character_name, opponent_character_name)

        rounds_won_other <- rounds_won_vs_other_characters(data, character_name, opponent_character_name, rounds_won_vs_other_characters_lookup)
        t_test_result <- t.test(rounds_won_specific, rounds_won_other)

        character_matchup$p_value[character_matchup$Opponent_Character == opponent_character_name] <- t_test_result$p.value
    }

    # rename(!!get_new_name() := Win)

    character_matchup <- character_matchup %>%
        rename(
            `Win %` = Win.Percentage,
            `Main\nCharacter` = `Main_Character`,
            `Opponent\nCharacter` = `Opponent_Character`,
            `Total\nMatches` = `Total.Matches`,
            `Wins By\nMain Character` = `Wins_By_Main_Character`
        )

    return(character_matchup)
}

rounds_won_vs_character <- function(data, character_name, opponent_character_name) {
    # Filter for matches where Blaze is one of the players, but not both
    character_wins <- data %>%
        filter((Player.1.Character == character_name & Player.2.Character == opponent_character_name) |
            (Player.2.Character == character_name & Player.1.Character == opponent_character_name))

    round_winners <- character_wins %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character))

    round_winners <- round_winners %>%
        mutate(Main_Character_Won = ifelse(Winner_Character == character_name, 1, 0))

    rounds_won_summary <- round_winners %>%
        pull(Main_Character_Won)

    return(rounds_won_summary)
}

rounds_won_vs_other_characters <- function(data, character_name, opponent_character_name, rounds_won_vs_other_characters_lookup) {
    if (character_name %in% names(rounds_won_vs_other_characters_lookup)) {
        if (opponent_character_name %in% names(rounds_won_vs_other_characters_lookup[[character_name]])) {
            return(rounds_won_vs_other_characters_lookup[[character_name]][[opponent_character_name]])
        }
    }

    # Filter for matches where Blaze is one of the players, but not both
    character_wins <- data %>%
        filter(Player.1.Character != Player.2.Character) %>%
        filter((Player.1.Character == character_name & Player.2.Character != opponent_character_name) |
            (Player.2.Character == character_name & Player.1.Character != opponent_character_name))

    round_winners <- character_wins %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character))

    round_winners <- round_winners %>%
        mutate(Main_Character_Won = ifelse(Winner_Character == character_name, 1, 0))

    rounds_won_summary <- round_winners %>%
        pull(Main_Character_Won)

    return(rounds_won_summary)
}

character_stage_matchup_win_table <- function(data, character_name, character_stage_matchup_win_table_lookup) {
    if (character_name %in% names(character_stage_matchup_win_table_lookup)) {
        return(character_stage_matchup_win_table_lookup[[character_name]])
    }

    with_stages <- add_stage_categories(data)

    # Filter out matches where both players are the same character
    matchup_data <- with_stages %>%
        filter(Player.1.Character != Player.2.Character) %>%
        filter(Player.1.Character == character_name | Player.2.Character == character_name) %>%
        group_by(Match.ID) %>%
        filter(round_number == max(round_number))

    # Identify the winner of the match by the last round
    match_winners <- matchup_data %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character)) %>%
        select(Match.ID, Player.1.Character, Player.2.Character, Winner_Character, Loser_Character, Stage_Type)

    # Create two perspectives for every matchup: one with Player 1 as main character, and one with Player 2 as main character
    matchups_player1 <- match_winners %>%
        mutate(
            Main_Character = Player.1.Character,
            Opponent_Character = Player.2.Character,
            Main_Winner = (Winner_Character == character_name)
        )

    matchups_player2 <- match_winners %>%
        mutate(
            Main_Character = Player.2.Character,
            Opponent_Character = Player.1.Character,
            Main_Winner = (Winner_Character == character_name)
        )

    # Combine both perspectives into a single dataset
    full_matchup_data <- bind_rows(matchups_player1, matchups_player2) %>%
        filter(Main_Character == character_name)

    # Summarize the results: count total matches and wins for each character-opponent pairing
    character_matchup <- full_matchup_data %>%
        group_by(Main_Character, Opponent_Character, Stage_Type) %>%
        summarise(
            Total.Matches = n(),
            Wins_By_Main_Character = sum(Main_Winner),
            Win.Percentage = (Wins_By_Main_Character / Total.Matches),
            .groups = "drop"
        ) %>%
        arrange(desc(Win.Percentage))

    # character_matchup$p_value <- NA
    # for (opponent_character_name in unique(character_matchup$Opponent_Character)) {
    # rounds_won_specific <- rounds_won_vs_character(data, character_name, opponent_character_name)

    # rounds_won_other <- rounds_won_vs_other_characters(data, character_name, opponent_character_name)
    # t_test_result <- t.test(rounds_won_specific, rounds_won_other)

    # character_matchup$p_value[character_matchup$Opponent_Character == opponent_character_name] <- t_test_result$p.value
    # }

    # rename(!!get_new_name() := Win)

    character_matchup <- character_matchup %>%
        rename(
            `Win %` = Win.Percentage,
            `Main\nCharacter` = `Main_Character`,
            `Opponent\nCharacter` = `Opponent_Character`,
            `Total\nMatches` = `Total.Matches`,
            `Wins By\nMain Character` = `Wins_By_Main_Character`
        )

    return(character_matchup)
}

win_probability_per_round <- function(data, character_name, win_probability_per_round_lookup) {
    if (character_name %in% names(win_probability_per_round_lookup)) {
        return(win_probability_per_round_lookup[[character_name]])
    }

    # Filter out matches where Player 1 character is the same as Player 2 character
    filtered_data <- data %>%
        filter(round_number > 0) %>%
        filter(Player.1.Character != Player.2.Character) %>%
        filter(Player.1.Character == character_name | Player.2.Character == character_name)

    with_winners <- filtered_data %>%
        mutate(Round_Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(
            Character_Win = ifelse(`Player.1.Character` == character_name & `Winning.Player.Number` == 1, 1,
                ifelse(`Player.2.Character` == character_name & `Winning.Player.Number` == 2, 1, 0)
            ),
            Character_Loss = ifelse(`Winning.Player.Number` != 0 & Character_Win == 0, 1, 0)
        )

    # df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("round_number", "Previous_Win", "Previous_Loss"))

    # overall_wp <- data.frame()

    total_matches_won <- nrow(with_winners %>%
        group_by(Match.ID) %>%
        filter(round_number == max(round_number)) %>%
        filter(Round_Winner_Character == character_name))

    total_matches_lost <- nrow(with_winners %>%
        group_by(Match.ID) %>%
        filter(round_number == max(round_number)) %>%
        filter(Round_Winner_Character != character_name))

    tmw <- data.frame(
        "round_number" = c(0),
        "cumulative_wins" = c(0),
        "matches_won" = c(total_matches_won),
        "matches_lost" = c(total_matches_lost),
        "total_matches" = c(total_matches_won + total_matches_lost),
        "wp" = c(total_matches_won / (total_matches_won + total_matches_lost))
    )

    for (i in 1:5) {
        # df <- rbind(df, round)

        culmative <- with_winners %>%
            filter(round_number > 0) %>%
            group_by(`Match.ID`) %>%
            mutate(
                Cumulative_Wins = cumsum(Character_Win),
                Cumulative_Losses = cumsum(Character_Loss)
            ) %>%
            select(Match.ID, round_number, Cumulative_Wins, Cumulative_Losses, Round_Winner_Character)

        current_round <- with_winners %>% filter(round_number == i)

        total_matches <- nrow(with_winners %>%
            filter(Match.ID %in% current_round$Match.ID) %>%
            group_by(Match.ID) %>%
            filter(round_number == max(round_number)))

        for (wins in 0:min(3, i)) {
            matches <- culmative %>%
                filter(Cumulative_Wins == wins & round_number == i) %>%
                select(Match.ID)

            matches_other <- culmative %>%
                filter(Cumulative_Wins != wins & round_number == i) %>%
                select(Match.ID)

            total_matches_won <- nrow(with_winners %>%
                group_by(Match.ID) %>%
                filter(round_number == max(round_number)) %>%
                filter(Round_Winner_Character == character_name) %>%
                filter(Match.ID %in% matches$Match.ID))

            total_matches_lost <- nrow(with_winners %>%
                group_by(Match.ID) %>%
                filter(round_number == max(round_number)) %>%
                filter(Round_Winner_Character != character_name) %>% filter(Match.ID %in% matches$Match.ID))

            tmw <- rbind(tmw, data.frame(
                round_number = i,
                cumulative_wins = wins,
                # cumulative_losses = i-wins,
                matches_won = total_matches_won,
                matches_lost = total_matches_lost,
                total_matches = total_matches_won + total_matches_lost,
                wp = total_matches_won / (total_matches_won + total_matches_lost)
            ))
        }
    }

    return(tmw)
}

win_percentages_per_character <- function(data, character_name, win_percentages_per_character_lookup) {
    if (character_name %in% win_percentages_per_character_lookup) {
        return(win_percentages_per_character_lookup[[character_name]])
    }

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

add_p_value_rounds_won <- function(df, all_rounds_data, all_matches) {
    df$p_value <- NA
    for (winner_character_name in unique(df$Character)) {
        rounds_player1 <- all_rounds_data %>%
            filter(Player.1.Rank == Player.2.Rank | all_matches) %>%
            filter(round_number > 0 & `Player.1.Character` == winner_character_name) %>%
            mutate(main_won = ifelse(Winning.Player.Number == 1, 1, 0))
        rounds_player2 <- all_rounds_data %>%
            filter(Player.1.Rank == Player.2.Rank | all_matches) %>%
            filter(round_number > 0 & `Player.2.Character` == winner_character_name) %>%
            mutate(main_won = ifelse(`Winning.Player.Number` == 1, 0, 1))
        rounds_won_specific <- rbind(rounds_player1, rounds_player2)

        other_rounds_player1 <- all_rounds_data %>%
            filter(Player.1.Rank == Player.2.Rank | all_matches) %>%
            filter(round_number > 0 & `Player.1.Character` != winner_character_name) %>%
            mutate(main_won = ifelse(Winning.Player.Number == 1, 1, 0))
        other_rounds_player2 <- all_rounds_data %>%
            filter(Player.1.Rank == Player.2.Rank | all_matches) %>%
            filter(round_number > 0 & `Player.2.Character` != winner_character_name) %>%
            mutate(main_won = ifelse(`Winning.Player.Number` == 1, 0, 1))
        rounds_won_other <- rbind(other_rounds_player1, other_rounds_player2)

        t_test_result <- t.test(rounds_won_specific$main_won, rounds_won_other$main_won)

        df$p_value[df$Character == winner_character_name] <- t_test_result$p.value
    }

    return(df)
}

add_p_value_matches_won <- function(df, all_rounds_data, all_matches) {
    df$p_value <- NA

    for (winner_character_name in unique(df$Character)) {
        rounds_player1 <- all_rounds_data %>%
            filter(Player.1.Rank == Player.2.Rank | all_matches) %>%
            group_by("Match.ID") %>%
            filter(round_number == max(round_number) & `Player.1.Character` == winner_character_name) %>%
            mutate(main_won = ifelse(Winning.Player.Number == 1, 1, 0))
        rounds_player2 <- all_rounds_data %>%
            filter(Player.1.Rank == Player.2.Rank | all_matches) %>%
            group_by("Match.ID") %>%
            filter(round_number == max(round_number) & `Player.2.Character` == winner_character_name) %>%
            mutate(main_won = ifelse(`Winning.Player.Number` == 1, 0, 1))
        rounds_won_specific <- rbind(rounds_player1, rounds_player2)

        other_rounds_player1 <- all_rounds_data %>%
            filter(Player.1.Rank == Player.2.Rank | all_matches) %>%
            group_by("Match.ID") %>%
            filter(round_number == max(round_number) & `Player.1.Character` != winner_character_name) %>%
            mutate(main_won = ifelse(Winning.Player.Number == 1, 1, 0))
        other_rounds_player2 <- all_rounds_data %>%
            filter(Player.1.Rank == Player.2.Rank | all_matches) %>%
            group_by("Match.ID") %>%
            filter(round_number == max(round_number) & `Player.2.Character` != winner_character_name) %>%
            mutate(main_won = ifelse(`Winning.Player.Number` == 1, 0, 1))
        rounds_won_other <- rbind(other_rounds_player1, other_rounds_player2)

        t_test_result <- t.test(rounds_won_specific$main_won, rounds_won_other$main_won)

        df$p_value[df$Character == winner_character_name] <- t_test_result$p.value
    }

    return(df)
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

    round_winners <- character_wins %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character))

    round_winners <- round_winners %>%
        mutate(Main_Character_Won = ifelse(Winner_Character == character_name, 1, 0))

    rounds_won_summary <- round_winners %>%
        pull(Main_Character_Won)

    return(rounds_won_summary)
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

rounds_won_per_character <- function(data, any_rank) {
    columns <- c("Character", "Total_Matches", "Rounds_Won", "Rounds_Won_Per_Match")

    result <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(result) <- columns

    for (player1_character_name in unique(data$Player.1.Character)) {
        rounds_won_per_match_counts <- data %>%
            filter(round_number > 0 & Player.1.Character != Player.2.Character) %>%
            filter(any_rank | Player.1.Rank == Player.2.Rank) %>%
            mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
            mutate(Winner_Character_Round_Won = ifelse(Winning.Player.Number == 1, 1, 0)) %>%
            mutate(Losing_Character = ifelse(Winning.Player.Number != 1, Player.1.Character, Player.2.Character)) %>%
            mutate(Losing_Character_Round_Won = ifelse(Winning.Player.Number != 1, 1, 0))

        match_count <- nrow(rounds_won_per_match_counts %>% filter(Losing_Character == player1_character_name | Winner_Character == player1_character_name) %>% filter(round_number == 1))

        rounds_won_count <- nrow(rounds_won_per_match_counts %>% filter(Winner_Character == player1_character_name))

        rounds_per_match <- (rounds_won_count) / (match_count)
        row <- c(player1_character_name, match_count, rounds_won_count, rounds_per_match)

        result[nrow(result) + 1, ] <- row
    }
    result <- result %>% arrange(desc(Rounds_Won_Per_Match)) # Sort by win percentage in descending order
    return(result)
}
