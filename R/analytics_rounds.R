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
