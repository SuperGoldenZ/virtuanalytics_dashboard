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

add_stage_categories <- function(data) {
    return(data %>%
        left_join(stage_type_lookup, by = "Stage"))
}
