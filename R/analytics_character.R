library(dplyr)

if (!exists("stage_type_lookup")) source("R/analytics_stage.R")
if (!exists("character_matchup_win_table")) source("R/analytics_character_functions.R")

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
character_matchup_win_table_data_same_rank <- list()
rounds_won_vs_character_lookup <- list()
rounds_won_vs_other_characters_lookup <- list()
character_stage_matchup_win_table_lookup <- list()
win_probability_per_round_lookup <- list()
win_percentages_per_character_lookup <- list()
matches_list <- list()

if (file.exists("data/rounds_won_vs_other_characters_lookup.Rda")) {
    rounds_won_vs_other_characters_lookup <- readRDS("data/rounds_won_vs_other_characters_lookup.Rda")
    win_probability_per_round_lookup <- readRDS("data/win_probability_per_round_lookup.Rda")
    character_matchup_win_table_data <- readRDS("data/character_matchup_win_table_data.Rda")
    character_matchup_win_table_data_same_rank <- readRDS("data/character_matchup_win_table_data_same_rank.Rda")
    character_stage_matchup_win_table_lookup <- readRDS("data/character_stage_matchup_win_table_lookup.Rda")
    win_percentages_per_character_lookup <- readRDS("data/win_percentages_per_character_lookup.Rda")
    matches_list <- readRDS("data/matches_list.Rda")
    print("loaded RDAs")
} else {
    print("RDAs not found")
    lapply(names(character_names), function(chr) {
        print(paste("Preloading ", chr))
        rounds_won_vs_other_characters_lookup[[chr]] <<- list()
        rounds_won_vs_character_lookup[[chr]] <<- list()

        rounds_won_vs_other_characters_lookup[[chr]][["Akira"]] <<- rounds_won_vs_other_characters(data, chr, "Akira", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Pai"]] <<- rounds_won_vs_other_characters(data, chr, "Pai", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Lau"]] <<- rounds_won_vs_other_characters(data, chr, "Lau", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Wolf"]] <<- rounds_won_vs_other_characters(data, chr, "Wolf", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Jeffry"]] <<- rounds_won_vs_other_characters(data, chr, "Jeffry", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Kage"]] <<- rounds_won_vs_other_characters(data, chr, "Kage", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Sarah"]] <<- rounds_won_vs_other_characters(data, chr, "Sarah", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Jacky"]] <<- rounds_won_vs_other_characters(data, chr, "Jacky", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Shun"]] <<- rounds_won_vs_other_characters(data, chr, "Shun", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Lion"]] <<- rounds_won_vs_other_characters(data, chr, "Lion", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Aoi"]] <<- rounds_won_vs_other_characters(data, chr, "Aoi", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["LeiFei"]] <<- rounds_won_vs_other_characters(data, chr, "LeiFei", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Vanessa"]] <<- rounds_won_vs_other_characters(data, chr, "Vanessa", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Brad"]] <<- rounds_won_vs_other_characters(data, chr, "Brad", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Goh"]] <<- rounds_won_vs_other_characters(data, chr, "Goh", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Eileen"]] <<- rounds_won_vs_other_characters(data, chr, "Eileen", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Blaze"]] <<- rounds_won_vs_other_characters(data, chr, "Blaze", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Taka"]] <<- rounds_won_vs_other_characters(data, chr, "Taka", rounds_won_vs_other_characters_lookup)
        rounds_won_vs_other_characters_lookup[[chr]][["Jean"]] <<- rounds_won_vs_other_characters(data, chr, "Jean", rounds_won_vs_other_characters_lookup)

        win_probability_per_round_lookup[[chr]] <<- win_probability_per_round(data, chr, win_probability_per_round_lookup)

        print("a")
        character_matchup_win_table_data[[chr]] <<-     character_matchup_win_table(data, chr, character_matchup_win_table_data, rounds_won_vs_other_characters_lookup, TRUE, character_matchup_win_table_data_same_rank)
        character_matchup_win_table_data_same_rank[[chr]] <<- character_matchup_win_table(data, chr, character_matchup_win_table_data, rounds_won_vs_other_characters_lookup, FALSE, character_matchup_win_table_data_same_rank)
        print("b")
        character_stage_matchup_win_table_lookup[[chr]] <<- character_stage_matchup_win_table(data, chr, character_stage_matchup_win_table_lookup)
        print("c")
        win_percentages_per_character_lookup[[chr]] <<- win_percentages_per_character(data, chr, win_percentages_per_character_lookup)
        print("d")
1
        matches_list[[chr]] <<- match_data %>%
            filter(Player.1.Character == chr | Player.2.Character == chr) %>%
            mutate(Stage = Stage, Desc = paste("Lv", Player.1.Rank, " ", Player.1.Character, " vs Lv", Player.2.Rank, " ", Player.2.Character), Link = Youtube.Link) %>%
            select(Stage, Desc, Link)
    })


    saveRDS(rounds_won_vs_other_characters_lookup, "data/rounds_won_vs_other_characters_lookup.Rda")
    saveRDS(win_probability_per_round_lookup, "data/win_probability_per_round_lookup.Rda")
    saveRDS(character_matchup_win_table_data, "data/character_matchup_win_table_data.Rda")
    saveRDS(character_matchup_win_table_data_same_rank, "data/character_matchup_win_table_data_same_rank.Rda")
    saveRDS(character_stage_matchup_win_table_lookup, "data/character_stage_matchup_win_table_lookup.Rda")
    saveRDS(win_percentages_per_character_lookup, "data/win_percentages_per_character_lookup.Rda")
    saveRDS(matches_list, "data/matches_list.Rda")
}
