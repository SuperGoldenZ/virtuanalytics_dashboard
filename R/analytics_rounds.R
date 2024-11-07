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
        group_by(Loser_Character, How.Round.Ended)

    rounds_summary <- rounds %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = How.Round.Ended, values_from = count, values_fill = 0) %>%
        rowwise() %>%
        mutate(
            Total = sum(c_across(c(KO, RO, EX))),
            KO = round((KO / Total), 5),
            RO = round((RO / Total), 5),
            EX = round((EX / Total), 5)
        )

    rounds <- rounds %>%
        mutate(is_ko = ifelse(How.Round.Ended == "KO", 1, 0)) %>%
        mutate(is_ro = ifelse(How.Round.Ended == "RO", 1, 0)) %>%
        mutate(is_ex = ifelse(How.Round.Ended == "EX", 1, 0))

    rounds_summary$ko_p_value <- NA
    rounds_summary$ro_p_value <- NA
    rounds_summary$ex_p_value <- NA

    for (loser_character_name in unique(rounds$Loser_Character)) {
        ro_specific <- rounds %>% filter(Loser_Character == loser_character_name)
        ro_specific <- ro_specific$is_ro

        ro_other <- rounds %>% filter(Loser_Character != loser_character_name)
        ro_other <- ro_other$is_ro

        t_test_result <- t.test(ro_specific, ro_other)
        rounds_summary$ro_p_value[rounds_summary$Loser_Character == loser_character_name] <- t_test_result$p.value

        ex_specific <- rounds %>% filter(Loser_Character == loser_character_name)
        ex_specific <- ex_specific$is_ex

        ex_other <- rounds %>% filter(Loser_Character != loser_character_name)
        ex_other <- ex_other$is_ex

        t_test_result <- t.test(ex_specific, ex_other)
        rounds_summary$ex_p_value[rounds_summary$Loser_Character == loser_character_name] <- t_test_result$p.value

        ko_specific <- rounds %>% filter(Loser_Character == loser_character_name)
        ko_specific <- ko_specific$is_ko

        ko_other <- rounds %>% filter(Loser_Character != loser_character_name)
        ko_other <- ko_other$is_ko

        t_test_result <- t.test(ko_specific, ko_other)
        rounds_summary$ko_p_value[rounds_summary$Loser_Character == loser_character_name] <- t_test_result$p.value
    }
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
        group_by(Winner_Character, How.Round.Ended)

    rounds_summary <- rounds %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = How.Round.Ended, values_from = count, values_fill = 0) %>%
        rowwise() %>%
        mutate(
            Total = sum(c_across(c(KO, RO, EX))),
            KO = round((KO / Total), 5),
            RO = round((RO / Total), 5),
            EX = round((EX / Total), 5)
        )

    rounds <- rounds %>%
        mutate(is_ko = ifelse(How.Round.Ended == "KO", 1, 0)) %>%
        mutate(is_ro = ifelse(How.Round.Ended == "RO", 1, 0)) %>%
        mutate(is_ex = ifelse(How.Round.Ended == "EX", 1, 0))

    rounds_summary$ko_p_value <- NA
    rounds_summary$ro_p_value <- NA
    rounds_summary$ex_p_value <- NA

    for (winner_character_name in unique(rounds$Winner_Character)) {
        ro_specific <- rounds %>% filter(Winner_Character == winner_character_name)
        ro_specific <- ro_specific$is_ro

        ro_other <- rounds %>% filter(Winner_Character != winner_character_name)
        ro_other <- ro_other$is_ro

        t_test_result <- t.test(ro_specific, ro_other)
        rounds_summary$ro_p_value[rounds_summary$Winner_Character == winner_character_name] <- t_test_result$p.value

        ex_specific <- rounds %>% filter(Winner_Character == winner_character_name)
        ex_specific <- ex_specific$is_ex

        ex_other <- rounds %>% filter(Winner_Character != winner_character_name)
        ex_other <- ex_other$is_ex

        t_test_result <- t.test(ex_specific, ex_other)
        rounds_summary$ex_p_value[rounds_summary$Winner_Character == winner_character_name] <- t_test_result$p.value

        ko_specific <- rounds %>% filter(Winner_Character == winner_character_name)
        ko_specific <- ko_specific$is_ko

        ko_other <- rounds %>% filter(Winner_Character != winner_character_name)
        ko_other <- ko_other$is_ko

        t_test_result <- t.test(ko_specific, ko_other)
        rounds_summary$ko_p_value[rounds_summary$Winner_Character == winner_character_name] <- t_test_result$p.value
    }
    return(rounds_summary)
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
