library(dplyr)
library(tidyr)

stage_type_lookup <- data.frame(
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

rounds_won_piechart_data <- function(data) {
    rounds_summary <- data %>%
        filter(round_number > 0) %>%
        count(How.Round.Ended) %>%
        mutate(percentage = n / sum(n) * 100)

    return(rounds_summary)
}

how_rounds_won_data <- function(data) {
    # Filter out matches where both players are the same character
    matchup_data <- data %>%
        filter(Player.1.Character != Player.2.Character) %>%
        filter(round_number >= 1)

    # Identify the winner of the match by the last round
    rounds <- matchup_data %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character)) %>%
        select(Match.ID, Player.1.Character, Player.2.Character, Winner_Character, Loser_Character, `How.Round.Ended`) %>%
        group_by(Winner_Character, How.Round.Ended) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = How.Round.Ended, values_from = count, values_fill = 0) %>%
        rowwise() %>%
        mutate(
            Total = sum(c_across(c(KO, RO, EX))),
            KO = round((KO / Total), 5),
            RO = round((RO / Total), 5),
            EX = round((EX / Total), 5)
        )

    return(rounds)
}

how_rounds_lost_data <- function(data) {
    # Filter out matches where both players are the same character
    matchup_data <- data %>%
        filter(Player.1.Character != Player.2.Character) %>%
        filter(round_number >= 1)

    # Identify the winner of the match by the last round
    rounds <- matchup_data %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character)) %>%
        select(Match.ID, Player.1.Character, Player.2.Character, Winner_Character, Loser_Character, `How.Round.Ended`) %>%
        group_by(Loser_Character, How.Round.Ended) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = How.Round.Ended, values_from = count, values_fill = 0) %>%
        rowwise() %>%
        mutate(
            Total = sum(c_across(c(KO, RO, EX))),
            KO = round((KO / Total), 5),
            RO = round((RO / Total), 5),
            EX = round((EX / Total), 5)
        )

    return(rounds)
}

how_rounds_end_per_stage <- function(data) {
    # Filter out matches where both players are the same character
    matchup_data <- data %>%
        filter(round_number >= 1)

    stage_data <- matchup_data %>%
        left_join(stage_type_lookup, by = "Stage")

    # Identify the winner of the match by the last round
    rounds <- stage_data %>%
        group_by(Stage_Type, How.Round.Ended) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = How.Round.Ended, values_from = count, values_fill = 0) %>%
        rowwise() %>%
        mutate(
            Total = sum(c_across(c(KO, RO, EX))),
            KO = round((KO / Total), 5),
            RO = round((RO / Total), 5),
            EX = round((EX / Total), 5)
        )

    return(rounds)
}

win_probability_per_round <- function(data, character_name) {
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

            # print(nrow(matches))
            # return(1)

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

    # Identify the winner for each match by the last round played in that match
    match_winners <- filtered_data %>%
        group_by(Match.ID) %>%
        filter(round_number == max(round_number)) %>% # Get the last round of each match
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        select(Match.ID, Winner_Character)

    # Merge match winners and first round winners
    full_data <- match_winners %>%
        left_join(first_round_winners, by = "Match.ID")

    # Filter only matches where the first round winner is not NA
    matches_with_first_round_win <- full_data %>%
        filter(!is.na(First_Round_Winner_Character))

    # Calculate total matches played for each character when they won the first round
    total_matches <- matches_with_first_round_win %>%
        group_by(First_Round_Winner_Character) %>%
        summarise(Total_Matches = n(), .groups = "drop")

    # Calculate number of matches won when the character won the first round
    matches_won <- matches_with_first_round_win %>%
        filter(Winner_Character == First_Round_Winner_Character) %>%
        group_by(First_Round_Winner_Character) %>%
        summarise(Matches_Won = n(), .groups = "drop")

    # Merge total matches and matches won to calculate win percentages
    win_percentage_table <- total_matches %>%
        left_join(matches_won, by = "First_Round_Winner_Character") %>%
        mutate(
            Matches_Won = ifelse(is.na(Matches_Won), 0, Matches_Won), # Handle cases with no wins
            Win_Percentage = (Matches_Won / Total_Matches) * 100 # Calculate win percentage
        ) %>%
        arrange(desc(Win_Percentage)) # Sort by win percentage in descending order

    # Rename columns for clarity
    win_percentage_table <- win_percentage_table %>%
        rename(
            Character = First_Round_Winner_Character,
            Number_of_Matches_Won = Matches_Won,
            Total_Number_of_Matches = Total_Matches,
            Win_Percentage = Win_Percentage
        )

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

        rounds_won_other <- rounds_won_vs_other_characters(data, character_name, opponent_character_name)
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

rounds_won_vs_other_characters <- function(data, character_name, opponent_character_name) {
    # Filter for matches where Blaze is one of the players, but not both
    character_wins <- data %>%
        filter(Player.1.Character != Player.2.Character)

    round_winners <- character_wins %>%
        mutate(Winner_Character = ifelse(Winning.Player.Number == 1, Player.1.Character, Player.2.Character)) %>%
        mutate(Loser_Character = ifelse(Winning.Player.Number == 2, Player.1.Character, Player.2.Character))

    round_winners <- round_winners %>%
        mutate(Main_Character_Won = ifelse(Winner_Character == character_name, 1, 0))

    rounds_won_summary <- round_winners %>%
        pull(Main_Character_Won)
}
