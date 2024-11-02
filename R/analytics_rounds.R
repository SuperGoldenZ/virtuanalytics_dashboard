library(dplyr)
library(tidyr)

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
