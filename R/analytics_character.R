library(dplyr)

source("R/analytics_stage.R")

character_names <- list(
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
    "Jean" = c("English" = "Jean", "日本語" = "ジャン")
)

character_matchup_win_table_data <- list()

character_matchup_win_table <- function(data, character_name) {
    if (character_name %in% names(character_matchup_win_table_data)) {
        print("exists!")
        return(character_matchup_win_table_data[[character_name]])
    }

    print(paste("no exist ", character_name))
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

rounds_won_vs_character_lookup <- list()
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

rounds_won_vs_other_characters_lookup <- list()
rounds_won_vs_other_characters <- function(data, character_name, opponent_character_name) {
    if (character_name %in% names(rounds_won_vs_other_characters_lookup)) {
        if (opponent_character_name %in% names(rounds_won_vs_other_characters_lookup[[character_name]])) {
            print("exists in rounds won")
            return(rounds_won_vs_other_characters_lookup[[character_name]][[opponent_character_name]])
        }
    }

    print(paste("does not exist for round ", character_name, " ", opponent_character_name))

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

character_stage_matchup_win_table_lookup <- list()

character_stage_matchup_win_table <- function(data, character_name) {
    if (character_name %in% names(character_stage_matchup_win_table_lookup)) {
        print("stage exists!")
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

if (!exists("matches_list")) {
    matches_list <- list()
    print("init matchhes list")
}

lapply(names(character_names), function(chr) {
    rounds_won_vs_other_characters_lookup[[chr]] <- list()
    rounds_won_vs_character_lookup[[chr]] <- list()

    rounds_won_vs_other_characters_lookup[[chr]][["Akira"]] <- rounds_won_vs_other_characters(data, chr, "Akira")
    rounds_won_vs_other_characters_lookup[[chr]][["Pai"]] <- rounds_won_vs_other_characters(data, chr, "Pai")
    rounds_won_vs_other_characters_lookup[[chr]][["Lau"]] <- rounds_won_vs_other_characters(data, chr, "Lau")
    rounds_won_vs_other_characters_lookup[[chr]][["Wolf"]] <- rounds_won_vs_other_characters(data, chr, "Wolf")
    rounds_won_vs_other_characters_lookup[[chr]][["Jeffry"]] <- rounds_won_vs_other_characters(data, chr, "Jeffry")
    rounds_won_vs_other_characters_lookup[[chr]][["Kage"]] <- rounds_won_vs_other_characters(data, chr, "Kage")
    rounds_won_vs_other_characters_lookup[[chr]][["Sarah"]] <- rounds_won_vs_other_characters(data, chr, "Sarah")
    rounds_won_vs_other_characters_lookup[[chr]][["Jacky"]] <- rounds_won_vs_other_characters(data, chr, "Jacky")
    rounds_won_vs_other_characters_lookup[[chr]][["Shun"]] <- rounds_won_vs_other_characters(data, chr, "Shun")
    rounds_won_vs_other_characters_lookup[[chr]][["Lion"]] <- rounds_won_vs_other_characters(data, chr, "Lion")
    rounds_won_vs_other_characters_lookup[[chr]][["Aoi"]] <- rounds_won_vs_other_characters(data, chr, "Aoi")
    rounds_won_vs_other_characters_lookup[[chr]][["LeiFei"]] <- rounds_won_vs_other_characters(data, chr, "LeiFei")
    rounds_won_vs_other_characters_lookup[[chr]][["Vanessa"]] <- rounds_won_vs_other_characters(data, chr, "Vanessa")
    rounds_won_vs_other_characters_lookup[[chr]][["Brad"]] <- rounds_won_vs_other_characters(data, chr, "Brad")
    rounds_won_vs_other_characters_lookup[[chr]][["Goh"]] <- rounds_won_vs_other_characters(data, chr, "Goh")
    rounds_won_vs_other_characters_lookup[[chr]][["Eileen"]] <- rounds_won_vs_other_characters(data, chr, "Eileen")
    rounds_won_vs_other_characters_lookup[[chr]][["Blaze"]] <- rounds_won_vs_other_characters(data, chr, "Blaze")
    rounds_won_vs_other_characters_lookup[[chr]][["Taka"]] <- rounds_won_vs_other_characters(data, chr, "Taka")
    rounds_won_vs_other_characters_lookup[[chr]][["Jean"]] <- rounds_won_vs_other_characters(data, chr, "Jean")


    character_matchup_win_table_data[[chr]] <- character_matchup_win_table(data, chr)
    character_stage_matchup_win_table_lookup[[chr]] <- character_stage_matchup_win_table(data, chr)
})
