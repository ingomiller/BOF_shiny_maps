library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readxl)
library(DT)
library(lubridate)
library(sf)

# -------------------------------------------------------------------
# 1. Fishing and shark data
# -------------------------------------------------------------------
master_data <- readxl::read_excel(
  "Tagging_Catch_Tissue_MASTERFILE.xlsx",
  sheet = "Elasmo_CATCH_Master"
) |>
  dplyr::mutate(
    Date = as.Date(Date),
    year = lubridate::year(Date),
    Catch_Time = format(Catch_Time, "%H:%M:%S"),
    Release_Time = format(Release_Time, "%H:%M:%S")
  )

# Project is column 20 and needs explicit text import: otherwise later
# project names may be coerced to NA by readxl's type guessing.
fishing_col_types <- rep("guess", 20)
fishing_col_types[c(16, 20)] <- "text"

dpi_fishing <- readxl::read_excel(
  "Tagging_Catch_Tissue_MASTERFILE.xlsx",
  sheet = "Fishing_Master",
  col_types = fishing_col_types
) |>
  dplyr::filter(Project == "DPI_bullsharks")


dpi_fishing |>
  dplyr::count(Gear_Type, Hook_number, sort = TRUE)


river_lookup <- tibble::tibble(
  Location = c(
    "Trinity_Inlet", "Barron_River", "Russel_River",
    "Endeavour_river", "Annan_river"
  ),
  River = c(
    "Trinity Inlet", "Barron River", "Russell River",
    "Endeavour River", "Annan River"
  )
)


# fishing summary

# Convert time-of-day values to fractions of a day
time_fraction <- function(x) {
  
  if (inherits(x, "POSIXt")) {
    return(
      (
        as.numeric(format(x, "%H")) * 3600 +
          as.numeric(format(x, "%M")) * 60 +
          as.numeric(format(x, "%S"))
      ) / 86400
    )
  }
  
  if (is.numeric(x)) {
    return(x %% 1)
  }
  
  stop("Unexpected time column type")
}

dpi_fishing <- dpi_fishing |>
  dplyr::mutate(
    Gear = dplyr::case_when(
      grepl("Drumline", Gear_Type, ignore.case = TRUE) ~ "Drumline",
      grepl("Longline", Gear_Type, ignore.case = TRUE) ~ "Longline",
      grepl("Handline_Rod", Gear_Type, ignore.case = TRUE) ~ "Handline/Rod",
      TRUE ~ NA_character_
    ),
    
    Hooks = dplyr::case_when(
      Gear == "Drumline" ~ 1,
      Gear == "Longline" ~ suppressWarnings(as.numeric(Hook_number)),
      Gear == "Handline/Rod" ~ 1,
      TRUE ~ NA_real_
    ),
    
    Hours = ((time_fraction(Time_Out) -
                time_fraction(Time_In)) %% 1) * 24,
    
    Hook_Hours = Hooks * Hours
  )



# dpi_fishing |>
#   dplyr::filter(
#     is.na(Hook_Hours) | is.na(Gear)
#   ) |>
#   dplyr::select(
#     Date, Location, Gear_Type,
#     Hook_number, Time_In, Time_Out,
#     Hooks, Hours, Hook_Hours
#   )


# dpi_fishing |>
#   dplyr::filter(
#     Gear == "Longline",
#     is.na(Hooks)
#   ) |>
#   dplyr::select(Date, Location, Gear_Type, Hook_number)
# 
# dpi_fishing |>
#   dplyr::filter(is.na(Hook_Hours) | is.na(Gear)) |>
#   dplyr::count(Location, Gear_Type, sort = TRUE)




dpi_fishing.sf <- dpi_fishing |>
  dplyr::left_join(river_lookup, by = "Location") |>
  dplyr::mutate(
    Lat1 = suppressWarnings(as.numeric(Lat1)),
    Lon1 = suppressWarnings(as.numeric(Lon1))
  ) |>
  dplyr::filter(!is.na(Lat1), !is.na(Lon1)) |>
  sf::st_as_sf(
    coords = c("Lon1", "Lat1"),
    crs = 4326,
    remove = FALSE
  )







study_sites <- dpi_fishing |>
  dplyr::filter(!is.na(Location), Location != "") |>
  dplyr::distinct(Location) |>
  dplyr::left_join(river_lookup, by = "Location") |>
  dplyr::arrange(Location)

if (anyNA(study_sites$River)) {
  warning("Some fishing locations are missing a River assignment: ",
          paste(study_sites$Location[is.na(study_sites$River)], collapse = ", "))
}

dpi_sharks <- master_data |>
  dplyr::filter(dplyr::if_any(c(Project, Subproject1, Subproject2),
                              ~ .x == "DPI_bullsharks"))

target_species <- c(
  "Bull_shark", "Pigeye_shark", "Tiger_shark", "Blacktip_complex",
  "Dusky_shark", "Common_blacktip", "Australian_blacktip"
)

dpi_sharks <- dpi_sharks |>
  dplyr::filter(Common_Name %in% target_species) |>
  dplyr::mutate(
    Site_Type = dplyr::if_else(
      Location %in% study_sites$Location,
      "Project study site", "Other capture location",
      missing = "Other capture location"
    )
  ) |>
  dplyr::select(-dplyr::any_of("River")) |>
  dplyr::left_join(river_lookup, by = "Location")

dpi_sharks_sites <- dpi_sharks |>
  dplyr::filter(Site_Type == "Project study site")
dpi_sharks_other <- dpi_sharks |>
  dplyr::filter(Site_Type == "Other capture location")

names(dpi_sharks)


dpi_catches.sf <- dpi_sharks |>
  dplyr::mutate(
    Lat_DD = suppressWarnings(as.numeric(Lat_DD)),
    Lon_DD = suppressWarnings(as.numeric(Lon_DD))
  ) |>
  dplyr::filter(
    !is.na(Lat_DD),
    !is.na(Lon_DD)
  ) |>
  sf::st_as_sf(
    coords = c("Lon_DD", "Lat_DD"),
    crs = 4326,
    remove = FALSE
  )



# dpi_sharks |>
#   dplyr::count(Site_Type)


# -------------------------------------------------------------------
# 2. Receiver data
# -------------------------------------------------------------------
rcr_raw <- readxl::read_xlsx(
  "Receiver_Deployments_IMOS_Working-File_MASTER.xlsx",
  sheet = "Receiver Deployment & Recovery", skip = 1
)

dpi_rcr <- rcr_raw |>
  dplyr::filter(Code == "DPI_bulls" & Recovered == "ACTIVE") |>
  dplyr::mutate(
    River = dplyr::case_when(
      grepl("Trinity", Station, ignore.case = TRUE) ~ "Trinity Inlet",
      grepl("Barron", Station, ignore.case = TRUE) ~ "Barron River",
      grepl("Endeavour", Station, ignore.case = TRUE) ~ "Endeavour River",
      TRUE ~ NA_character_
    )
  )

dpi_rcr.sf <- dpi_rcr |>
  dplyr::mutate(name = Station, Lat_DD = as.numeric(Lat_DD),
                Lon_DD = as.numeric(Lon_DD)) |>
  dplyr::filter(!is.na(Lat_DD), !is.na(Lon_DD)) |>
  sf::st_as_sf(coords = c("Lon_DD", "Lat_DD"), crs = 4326, remove = FALSE)

# -------------------------------------------------------------------
# 3. Queensland watercourse polygons
# -------------------------------------------------------------------
qld_riv.sf <- sf::st_read("Watercourse_areas.shp", quiet = TRUE)
if (is.na(sf::st_crs(qld_riv.sf))) {
  stop("qld_riv.sf has no CRS: supply/verify its .prj file before mapping")
}
qld_riv.sf <- sf::st_transform(qld_riv.sf, 4326)

# Automatically select named river polygons. 'name' is the real
# attribute in the Queensland shapefile. No receiver is required.
river_names <- study_sites$River[!is.na(study_sites$River)]

qld_riv.sf <- qld_riv.sf |>
  dplyr::mutate(Study_River = dplyr::case_when(
    tolower(trimws(name)) %in% tolower(river_names) ~
      river_names[match(tolower(trimws(name)), tolower(river_names))],
    TRUE ~ NA_character_
  ))

river_auto.sf <- qld_riv.sf |>
  dplyr::filter(!is.na(Study_River)) |>
  dplyr::transmute(River = Study_River) |>
  dplyr::group_by(River) |>
  dplyr::summarise(.groups = "drop")

# Existing hand-drawn boundaries, if available, take precedence over
# the automatically matched geometry for the same river. One drawn
# boundary can include all mapped named/unnamed sidearms in that area.
if (file.exists("DPI_study_boundaries.gpkg")) {
  study_boundaries.sf <- sf::st_read("DPI_study_boundaries.gpkg", quiet = TRUE) |>
    sf::st_transform(sf::st_crs(qld_riv.sf)) |>
    sf::st_make_valid()
  
  selected_rivers.sf <- sf::st_filter(qld_riv.sf, study_boundaries.sf)
  selected_rivers.sf <- suppressWarnings(sf::st_intersection(
    sf::st_make_valid(selected_rivers.sf), study_boundaries.sf["River"]
  ))
  
  river_manual.sf <- selected_rivers.sf |>
    dplyr::select(River) |>
    dplyr::group_by(River) |>
    dplyr::summarise(.groups = "drop")
  
  # Preserve automatic rivers that do not have hand-drawn boundaries.
  river_sites.sf <- dplyr::bind_rows(
    river_auto.sf |> dplyr::filter(!River %in% river_manual.sf$River),
    river_manual.sf
  ) |>
    dplyr::group_by(River) |>
    dplyr::summarise(.groups = "drop") |>
    sf::st_transform(4326)
} else {
  study_boundaries.sf <- NULL
  selected_rivers.sf <- NULL
  river_sites.sf <- river_auto.sf |> sf::st_transform(4326)
}

# Verify any unmatched river names before relying on the map:
print(setdiff(river_names, river_sites.sf$River))

# This palette is keyed to the full study site list, not to receivers.
river_pal <- leaflet::colorFactor(
  palette = "viridis",
  domain = sort(unique(river_names)),
  na.color = "#808080"
)

# -------------------------------------------------------------------
# 4. Summary functions
# -------------------------------------------------------------------
fishing_summary <- function(dat, catches) {
  
  missing_effort <- sum(
    is.na(dat$Hook_Hours) | is.na(dat$Gear)
  )
  
  total_hook_hours <- sum(dat$Hook_Hours, na.rm = TRUE)
  
  tibble::tibble(
    Metric = c(
      "Fishing days",
      "Drumline sets",
      "Longline sets",
      "Handline/Rod sets",
      "Total hooks deployed",
      "Total fishing hours",
      "Total hook-hours",
      "CPUE (sharks hook⁻¹ h⁻¹)"
    ),
    Value = c(
      dplyr::n_distinct(dat$Date, na.rm = TRUE),
      sum(dat$Gear == "Drumline", na.rm = TRUE),
      sum(dat$Gear == "Longline", na.rm = TRUE),
      sum(dat$Gear == "Handline/Rod", na.rm = TRUE),
      sum(dat$Hooks, na.rm = TRUE),
      round(sum(dat$Hours, na.rm = TRUE), 1),
      round(total_hook_hours, 1),
      if (missing_effort == 0 && total_hook_hours > 0) {
        round(nrow(catches) / total_hook_hours, 4)
      } else {
        NA_real_
      }
    )
  )
}



# species_summary <- function(dat) {
#   
#   summary <- dat |>
#     dplyr::mutate(
#       TL_cm = suppressWarnings(as.numeric(TL_cm))
#     ) |>
#     dplyr::group_by(Common_Name) |>
#     dplyr::summarise(
#       Caught = dplyr::n(),
#       Acoustic = sum(Acoustic_Tag %in% TRUE),
#       Satellite = sum(Sat_Tag %in% TRUE),
#       Tagged = sum(Acoustic_Tag %in% TRUE | Sat_Tag %in% TRUE),
#       
#       `TL range (cm)` = if (all(is.na(TL_cm))) {
#         NA_character_
#       } else {
#         paste0(
#           min(TL_cm, na.rm = TRUE),
#           "–",
#           max(TL_cm, na.rm = TRUE)
#         )
#       },
#       
#       .groups = "drop"
#     ) |>
#     dplyr::arrange(dplyr::desc(Caught))
#   
#   total <- summary |>
#     dplyr::summarise(
#       Common_Name = "Total",
#       Caught = sum(Caught),
#       Acoustic = sum(Acoustic),
#       Satellite = sum(Satellite),
#       Tagged = sum(Tagged),
#       `TL range (cm)` = NA_character_
#     )
#   
#   dplyr::bind_rows(summary, total)
# }


species_summary <- function(dat) {
  
  summary <- dat |>
    dplyr::mutate(
      TL_cm = suppressWarnings(as.numeric(TL_cm))
    ) |>
    dplyr::group_by(Common_Name) |>
    dplyr::summarise(
      Caught = dplyr::n(),
      Acoustic = sum(Acoustic_Tag %in% TRUE),
      Satellite = sum(Sat_Tag %in% TRUE),
      Tagged = sum(Acoustic_Tag %in% TRUE | Sat_Tag %in% TRUE),
      
      Females = sum(toupper(trimws(as.character(Sex))) %in% c("F", "FEMALE")),
      Males = sum(toupper(trimws(as.character(Sex))) %in% c("M", "MALE")),
      
      `TL range (cm)` = if (all(is.na(TL_cm))) {
        NA_character_
      } else {
        paste0(
          min(TL_cm, na.rm = TRUE),
          "–",
          max(TL_cm, na.rm = TRUE)
        )
      },
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `F/M` = dplyr::if_else(
        Males > 0,
        round(Females / Males, 2),
        NA_real_
      )
    ) |>
    dplyr::select(
      Common_Name, Caught, Acoustic, Satellite, Tagged,
      `F/M`, `TL range (cm)`, Females, Males
    ) |>
    dplyr::arrange(dplyr::desc(Caught))
  
  total <- summary |>
    dplyr::summarise(
      Common_Name = "Total",
      Caught = sum(Caught),
      Acoustic = sum(Acoustic),
      Satellite = sum(Satellite),
      Tagged = sum(Tagged),
      `F/M` = if (sum(Males) > 0) {
        round(sum(Females) / sum(Males), 2)
      } else {
        NA_real_
      },
      `TL range (cm)` = NA_character_,
      Females = sum(Females),
      Males = sum(Males)
    )
  
  dplyr::bind_rows(summary, total) |>
    dplyr::select(-Females, -Males)
}

# Helpers used only for plotting; base R date formatting preserves year.
month_start <- function(x) {
  as.Date(format(as.Date(x), "%Y-%m-01"))
}

study_fishing <- dpi_fishing |>
  dplyr::select(-dplyr::any_of("River")) |>
  dplyr::left_join(river_lookup, by = "Location") |>
  dplyr::filter(!is.na(River))






year_summary <- function(dat) {
  dat |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      Caught = dplyr::n(),
      Tagged = sum(Acoustic_Tag %in% TRUE | Sat_Tag %in% TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(year)
}

river_order <- c(
  "Endeavour River",
  "Annan River",
  "Barron River",
  "Trinity Inlet",
  "Russell River"
)




# -------------------------------------------------------------------
# 5. Shiny dashboard
# -------------------------------------------------------------------
receiver_icons <- leaflet::icons(
  iconUrl = vapply(
    river_pal(dpi_rcr.sf$River),
    function(col) {
      svg <- paste0(
        '<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20">',
        '<path d="M10 2 L19 18 L1 18 Z" fill="', col,
        '" stroke="white" stroke-width="1.5"/>',
        '</svg>'
      )
      
      paste0(
        "data:image/svg+xml,",
        utils::URLencode(svg, reserved = TRUE)
      )
    },
    character(1)
  ),
  iconWidth = 15,
  iconHeight = 15,
  iconAnchorX = 10,
  iconAnchorY = 10
)



river_labels.sf <- river_sites.sf |>
  sf::st_transform(32755) |>
  sf::st_point_on_surface() |>
  sf::st_transform(4326)


catch_svg <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18">',
  '<path d="M3 3 L15 15 M15 3 L3 15" ',
  'stroke="#FF0000" stroke-width="3" stroke-linecap="round"/>',
  '</svg>'
)

catch_icon <- leaflet::makeIcon(
  iconUrl = paste0(
    "data:image/svg+xml,",
    utils::URLencode(catch_svg, reserved = TRUE)
  ),
  iconWidth = 15,
  iconHeight = 15,
  iconAnchorX = 9,
  iconAnchorY = 9
)



## carto API
# usethis::edit_r_environ()


carto_key <- Sys.getenv("CARTO_API_KEY")

# if (!nzchar(carto_key)) {
#   stop("CARTO_API_KEY is missing")
# }
# 
# Sys.getenv("CARTO_API_KEY") |> nzchar()



ui <- shiny::fluidPage(
  
  shiny::tags$head(
    shiny::tags$style(
      shiny::HTML("

        /* ===== General dashboard styling ===== */

        body {
          background-color: #F5F8FB;
          color: #263746;
        }

        .container-fluid {
          padding-left: 22px;
          padding-right: 22px;
        }
        
        
        /* ===== Dashboard header and logo ===== */

.dashboard-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 20px;
  border-bottom: 3px solid #2878A8;
  padding: 10px 0 15px 0;
  margin-bottom: 25px;
}

.dashboard-header h2 {
  color: #123B5D;
  font-weight: 700;
  margin: 0;
  padding: 0;
  border-bottom: none;
}

.dashboard-logo {
  height: 65px;
  width: auto;
  max-width: 35%;
  object-fit: contain;
}

@media print {
  .dashboard-logo {
    height: 50px;
  }
}

        /* Main title */
        h2 {
          color: #123B5D;
          font-weight: 700;
          padding-bottom: 12px;
          border-bottom: 3px solid #2878A8;
          margin-bottom: 25px;
        }

        /* Main section headings */
        h3 {
          color: #18547D;
          font-weight: 600;
          font-size: 21px;
          border-bottom: 2px solid #D6E6F0;
          padding-bottom: 7px;
          margin-top: 24px;
          margin-bottom: 15px;
        }

        /* Table and subsection headings */
        h4 {
          color: #2878A8;
          font-weight: 600;
          font-size: 16px;
          margin-top: 20px;
          margin-bottom: 10px;
        }

        /* ===== River selector and export button ===== */

        .dashboard-controls {
          display: flex;
          align-items: flex-end;
          gap: 12px;
          margin-bottom: 12px;
        }

        .dashboard-river-select {
          flex: 1 1 auto;
          min-width: 0;
        }

        .dashboard-river-select .form-group {
          margin-bottom: 0;
        }

        .dashboard-export {
          flex: 0 0 auto;
        }

        #export_pdf {
          background-color: #2878A8;
          border: 1px solid #2878A8;
          color: #FFFFFF;
          font-weight: 600;
          border-radius: 5px;
          padding: 7px 12px;
          white-space: nowrap;
        }

        #export_pdf:hover {
          background-color: #18547D;
          border-color: #18547D;
          color: #FFFFFF;
        }

        #river_select {
          border-color: #B9D1E0;
          border-radius: 5px;
        }

        #river_select:focus {
          border-color: #2878A8;
          box-shadow: 0 0 5px rgba(40,120,168,0.25);
        }

        /* ===== Tables ===== */

        table.dataTable thead th {
          background-color: #E4EFF6;
          color: #123B5D;
          font-weight: 600;
        }

        table.dataTable tbody tr:hover {
          background-color: #F0F6FA;
        }

        /* ===== Small screens ===== */

        @media screen and (max-width: 600px) {
          .dashboard-controls {
            flex-wrap: wrap;
          }

          .dashboard-river-select {
            flex-basis: 100%;
          }
        }

        /* ===== PDF printing ===== */

        @media print {

          @page {
            size: A4 portrait;
            margin: 10mm;
          }

          body {
            background-color: white !important;
            -webkit-print-color-adjust: exact !important;
            print-color-adjust: exact !important;
          }

          /* Hide controls */
          .dashboard-controls,
          .leaflet-control-layers,
          .leaflet-control-zoom,
          .leaflet-control-scale {
            display: none !important;
          }

          /* Full-width content */
          .container-fluid {
            width: 100% !important;
            max-width: none !important;
            padding: 0 !important;
          }

          /* Stack dashboard columns vertically */
          .row > [class*='col-sm-'] {
            width: 100% !important;
            float: none !important;
          }

          /* Preserve plot colours */
          svg, canvas, img {
            -webkit-print-color-adjust: exact !important;
            print-color-adjust: exact !important;
          }

          /* Avoid splitting plots and tables */
          .shiny-plot-output,
          .plotly,
          table {
            break-inside: avoid;
            page-break-inside: avoid;
          }

          h3, h4 {
            break-after: avoid;
            page-break-after: avoid;
          }
        }

      ")
    )
  ),
  
  shiny::div(
    class = "dashboard-header",
    
    shiny::h2("DPI Bull Shark Project  |  Fieldwork Report"),
    
    shiny::img(
      src = "images/Biopixel Oceans New Logo 1.png",
      class = "dashboard-logo"
    )
  ),
  
  # ===== Map and summary tables =====
  
  shiny::fluidRow(
    shiny::column(
      7,
      leaflet::leafletOutput(
        "receiver_map",
        height = "750px"
      )
    ),
    
    shiny::column(
      5,
      
      shiny::div(
        class = "dashboard-controls",
        
        shiny::div(
          class = "dashboard-river-select",
          
          shiny::selectInput(
            inputId = "river_select",
            label = "Select study river (or click river on map)",
            choices = c(
              "All study rivers" = "All",
              river_order
            ),
            selected = "All",
            width = "100%"
          )
        ),
        
        shiny::div(
          class = "dashboard-export",
          
          shiny::actionButton(
            inputId = "export_pdf",
            label = "Export PDF",
            icon = shiny::icon("file-pdf"),
            onclick = "window.print();"
          )
        )
      ),
      
      shiny::h3(shiny::textOutput("selected_river")),
      
      shiny::h4("Fishing effort"),
      DT::DTOutput("fishing_table"),
      
      shiny::h4("Target-species capture records"),
      shiny::textOutput("capture_count"),
      
      shiny::h4("Species summary"),
      DT::DTOutput("species_table"),
      
      shiny::h4("Year summary"),
      DT::DTOutput("year_table")
    )
  ),
  
  # ===== Fishing timeline and diel capture patterns =====
  
  shiny::fluidRow(
    shiny::column(
      7,
      shiny::h3("Fishing activity timeline"),
      plotly::plotlyOutput(
        "fishing_timeline",
        height = "350px"
      ),
      
      shiny::h4("Shark catches for selected sampling day"),
      shiny::uiOutput("timeline_selection"),
      DT::DTOutput("timeline_catches")
    ),
    
    shiny::column(
      5,
      shiny::h3("Diel capture patterns"),
      shiny::plotOutput(
        "diel_capture_plot",
        height = "350px"
      )
    )
  ),
  
  # ===== Fishing effort and captures over time =====
  
  shiny::fluidRow(
    shiny::column(
      12,
      shiny::h3("Fishing effort and shark catches over time"),
      shiny::plotOutput(
        "effort_catch_plot",
        height = "500px"
      )
    )
  ),
  
  # ===== Species composition and size distribution =====
  
  shiny::fluidRow(
    shiny::column(
      6,
      shiny::h3("Species composition by river"),
      shiny::plotOutput(
        "species_river_plot",
        height = "450px"
      )
    ),
    
    shiny::column(
      6,
      shiny::h3("Size distribution (TL)"),
      shiny::plotOutput(
        "size_distribution_plot",
        height = "450px"
      )
    )
  ),
  
  # ===== CPUE and sampling effort =====
  
  shiny::fluidRow(
    shiny::column(
      6,
      shiny::h3("CPUE by river and gear type"),
      shiny::plotOutput(
        "cpue_river_gear_plot",
        height = "420px"
      )
    ),
    
    shiny::column(
      6,
      shiny::h3("Sampling effort heat map"),
      shiny::plotOutput(
        "effort_heatmap_plot",
        height = "420px"
      )
    )
  ),
  
  # ===== Fishing gear comparison =====
  
  shiny::fluidRow(
    shiny::column(
      12,
      shiny::h3("Fishing gear comparison")
    ),
    
    shiny::column(
      4,
      shiny::plotOutput(
        "gear_effort_plot",
        height = "350px"
      )
    ),
    
    shiny::column(
      4,
      shiny::plotOutput(
        "gear_catches_plot",
        height = "350px"
      )
    ),
    
    shiny::column(
      4,
      shiny::plotOutput(
        "gear_cpue_plot",
        height = "350px"
      )
    )
  )
)











server <- function(input, output, session) {
  selected_river <- shiny::reactiveVal(NULL)
  
  # Dropdown selection updates the active river
  shiny::observeEvent(input$river_select, {
    
    if (input$river_select == "All") {
      selected_river(NULL)
    } else {
      selected_river(input$river_select)
    }
    
  })
  
  # Clicking a river polygon updates the dropdown
  shiny::observeEvent(input$receiver_map_shape_click, {
    
    clicked_river <- input$receiver_map_shape_click$id
    
    if (!is.null(clicked_river) && clicked_river %in% river_order) {
      
      shiny::updateSelectInput(
        session,
        inputId = "river_select",
        selected = clicked_river
      )
      
      selected_river(clicked_river)
    }
    
  })
  
  
  # to zoom intoi slected river on map 
  shiny::observeEvent(selected_river(), {
    
    if (is.null(selected_river())) {
      
      # Show all study rivers
      bounds <- sf::st_bbox(river_sites.sf)
      
    } else {
      
      # Get the selected river polygon
      selected_polygon <- river_sites.sf |>
        dplyr::filter(River == selected_river())
      
      shiny::req(nrow(selected_polygon) > 0)
      
      bounds <- sf::st_bbox(selected_polygon)
    }
    
    leaflet::leafletProxy("receiver_map", session = session) |>
      leaflet::fitBounds(
        lng1 = as.numeric(bounds["xmin"]),
        lat1 = as.numeric(bounds["ymin"]),
        lng2 = as.numeric(bounds["xmax"]),
        lat2 = as.numeric(bounds["ymax"])
      )
    
  }, ignoreNULL = FALSE)
  
  
  
  
  filtered_animals <- shiny::reactive({
    if (is.null(selected_river())) {
      dpi_sharks_sites
    } else {
      dpi_sharks_sites |> dplyr::filter(River == selected_river())
    }
  })
  
  # Assign study-river names once, without duplicating an existing River column.
  fishing_with_river <- dpi_fishing |>
    dplyr::select(-dplyr::any_of("River")) |>
    dplyr::left_join(river_lookup, by = "Location")
  
  filtered_fishing <- shiny::reactive({
    if (is.null(selected_river())) {
      fishing_with_river
    } else {
      fishing_with_river |>
        dplyr::filter(River == selected_river())
    }
  })
  
  output$selected_river <- shiny::renderText({
    if (is.null(selected_river())) "All study rivers" else selected_river()
  })
  output$fishing_table <- DT::renderDT({
    
    DT::datatable(
      fishing_summary(
        filtered_fishing(),
        filtered_animals()
      ),
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE
      )
    )
  })
  
  output$capture_count <- shiny::renderText(nrow(filtered_animals()))
  
  output$species_table <- DT::renderDT({
    DT::datatable(
      species_summary(filtered_animals()),
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        scrollX = TRUE
      )
    ) |>
      DT::formatStyle(
        columns = "Common_Name",
        target = "row",
        fontWeight = DT::styleEqual("Total", "bold")
      )
  })
  
  output$year_table <- DT::renderDT({
    DT::datatable(
      year_summary(filtered_animals()),
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        scrollX = TRUE
      )
    )
  })
  
  
  # One point per study river and fishing date. Point size indicates deployments.
  timeline_data <- shiny::reactive({
    filtered_fishing() |>
      dplyr::filter(!is.na(Date), !is.na(River)) |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::group_by(Date, River) |>
      dplyr::summarise(
        Deployments = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        Key = paste(Date, River, sep = "|"),
        Hover = paste0(
          "River: ", River,
          "<br>Date: ", format(Date, "%d %b %Y"),
          "<br>Fishing deployments: ", Deployments,
          "<br>Click to see shark catches"
        )
      )
  })
  
  # The click selection is stored independently of plot rendering.
  selected_timeline_key <- shiny::reactiveVal(NULL)
  
  # Clear the clicked sampling day when switching rivers.
  shiny::observeEvent(selected_river(), {
    selected_timeline_key(NULL)
  }, ignoreNULL = FALSE)
  
  output$fishing_timeline <- plotly::renderPlotly({
    plot_data <- timeline_data()
    shiny::req(nrow(plot_data) > 0)
    
    # Explicit pixel diameters keep dot sizes visibly different.
    plot_data <- plot_data |>
      dplyr::mutate(
        DotSize = 8 + 18 * sqrt(Deployments / max(Deployments))
      )
    
    plotly::plot_ly(
      data = plot_data,
      x = ~Date,
      y = ~River,
      key = ~Key,
      text = ~Hover,
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(
        size = plot_data$DotSize,
        color = ~river_pal(River),
        opacity = 0.8,
        line = list(color = "white", width = 0.5)
      ),
      source = "fishing_timeline"
    ) |>
      plotly::layout(
        xaxis = list(title = "Sampling date"),
        yaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = rev(river_order)
        ),
        margin = list(l = 130, r = 30, b = 60, t = 20)
      )
  })
  
  # Capture Plotly clicks; each point has a unique date|river key.
  shiny::observeEvent(
    plotly::event_data(
      "plotly_click",
      source = "fishing_timeline",
      priority = "event"
    ),
    {
      click <- plotly::event_data(
        "plotly_click",
        source = "fishing_timeline"
      )
      
      if (!is.null(click$key) && length(click$key) > 0) {
        selected_timeline_key(as.character(click$key[[1]]))
      }
    },
    ignoreNULL = TRUE
  )
  
  selected_sampling <- shiny::reactive({
    shiny::req(selected_timeline_key())
    selection <- timeline_data() |>
      dplyr::filter(Key == selected_timeline_key())
    shiny::req(nrow(selection) == 1)
    selection
  })
  
  # Match shark captures by both study river and capture date.
  timeline_catch_data <- shiny::reactive({
    selected <- selected_sampling()
    filtered_animals() |>
      dplyr::filter(
        River == selected$River[[1]],
        as.Date(Date) == selected$Date[[1]]
      )
  })
  
  output$timeline_selection <- shiny::renderUI({
    if (is.null(selected_timeline_key())) {
      return(shiny::p("Click a timeline dot to view fishing and shark captures for that day."))
    }
    
    selected <- selected_sampling()
    catches <- timeline_catch_data()
    tagged <- sum(
      catches$Acoustic_Tag %in% TRUE |
        catches$Sat_Tag %in% TRUE
    )
    
    shiny::div(
      shiny::h4(paste0(
        selected$River[[1]], " — ",
        format(selected$Date[[1]], "%d %B %Y")
      )),
      shiny::p(
        shiny::strong("Fishing deployments: "),
        selected$Deployments[[1]]
      ),
      shiny::p(
        shiny::strong("Target sharks caught: "),
        nrow(catches)
      ),
      shiny::p(
        shiny::strong("Sharks tagged: "),
        tagged,
        " (",
        sum(catches$Acoustic_Tag %in% TRUE), " acoustic, ",
        sum(catches$Sat_Tag %in% TRUE), " satellite)"
      )
    )
  })
  
  output$timeline_catches <- DT::renderDT({
    shiny::req(selected_timeline_key())
    catches <- timeline_catch_data()
    
    if (nrow(catches) == 0) {
      return(DT::datatable(
        data.frame(Message = "No catch"),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE)
      ))
    }
    
    DT::datatable(
      species_summary(catches),
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      )
    ) |>
      DT::formatStyle(
        columns = "Common_Name",
        target = "row",
        fontWeight = DT::styleEqual("Total", "bold")
      )
  })
  
  
  output$effort_catch_plot <- shiny::renderPlot({
    
    # Monthly fishing effort
    effort_monthly <- filtered_fishing() |>
      dplyr::mutate(
        Month = as.Date(format(as.Date(Date), "%Y-%m-01"))
      ) |>
      dplyr::group_by(Month) |>
      dplyr::summarise(
        Hook_Hours = sum(Hook_Hours, na.rm = TRUE),
        Missing_Effort = sum(is.na(Hook_Hours)),
        .groups = "drop"
      )
    
    # Monthly target-shark captures
    catches_monthly <- filtered_animals() |>
      dplyr::mutate(
        Month = as.Date(format(as.Date(Date), "%Y-%m-01"))
      ) |>
      dplyr::count(Month, name = "Catches")
    
    # Combine effort and catches, retaining months with either
    monthly <- dplyr::full_join(
      effort_monthly,
      catches_monthly,
      by = "Month"
    ) |>
      dplyr::arrange(Month) |>
      dplyr::mutate(
        Hook_Hours = dplyr::coalesce(Hook_Hours, 0),
        Catches = dplyr::coalesce(Catches, 0L),
        Missing_Effort = dplyr::coalesce(Missing_Effort, 0L)
      )
    
    shiny::req(nrow(monthly) > 0)
    
    # Two aligned panels
    plot_data <- monthly |>
      dplyr::select(Month, Hook_Hours, Catches) |>
      tidyr::pivot_longer(
        cols = c(Hook_Hours, Catches),
        names_to = "Metric",
        values_to = "Value"
      ) |>
      dplyr::mutate(
        Metric = factor(
          Metric,
          levels = c("Hook_Hours", "Catches"),
          labels = c("Fishing effort (hook-hours)", "Target sharks caught")
        )
      )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = Month, y = Value)
    ) +
      ggplot2::geom_col(
        fill = "#0072B2",
        width = 23
      ) +
      ggplot2::facet_wrap(
        ~Metric,
        ncol = 1,
        scales = "free_y"
      ) +
      ggplot2::scale_x_date(
        date_labels = "%b %Y"
      ) +
      ggplot2::labs(
        x = "Month",
        y = NULL,
        caption = if (any(monthly$Missing_Effort > 0)) {
          "Note: Some fishing records have missing hook-hours; reported effort is incomplete."
        } else {
          NULL
        }
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        ),
        plot.caption = ggplot2::element_text(
          hjust = 0,
          colour = "firebrick"
        )
      )
  })
  
  
  
  ## diel patterns 
  
  
  output$diel_capture_plot <- shiny::renderPlot({
    
    plot_data <- filtered_animals() |>
      dplyr::mutate(
        Hour = suppressWarnings(
          as.integer(
            substr(as.character(Catch_Time), 1, 2)
          )
        )
      ) |>
      dplyr::filter(
        !is.na(Hour),
        Hour >= 0,
        Hour <= 23
      ) |>
      dplyr::count(Hour, name = "Catches") |>
      tidyr::complete(
        Hour = 0:23,
        fill = list(Catches = 0L)
      )
    
    shiny::validate(
      shiny::need(
        sum(plot_data$Catches) > 0,
        "No shark capture times available for this selection."
      )
    )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = Hour,
        y = Catches
      )
    ) +
      ggplot2::geom_col(
        fill = "#527A86",
        width = 0.85
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 24, 3),
        limits = c(-0.5, 24),
        labels = function(x) sprintf("%02d:00", x)
      ) +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_width(1),
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::labs(
        x = "Time of day",
        y = "Number of sharks caught"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  
  
  
  ## species compisition 
  
  output$species_river_plot <- shiny::renderPlot({
    
    plot_data <- filtered_animals() |>
      dplyr::filter(
        !is.na(River),
        !is.na(Common_Name)
      ) |>
      dplyr::count(
        River,
        Common_Name,
        name = "Caught"
      )
    
    plot_data <- plot_data |>
      dplyr::mutate(
        River = factor(River, levels = river_order)
      )
    
    shiny::req(nrow(plot_data) > 0)
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = River,
        y = Caught,
        fill = Common_Name
      )
    ) +
      ggplot2::geom_col(
        width = 0.7,
        colour = "white",
        linewidth = 0.3
      ) +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_width(1)
      ) +
      ggplot2::labs(
        x = NULL,
        y = "Number of sharks caught",
        fill = "Species"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          angle = 30,
          hjust = 1
        ),
        legend.position = "right"
      )
  })
  
  
  
  ## size distribution plot 
  
  output$size_distribution_plot <- shiny::renderPlot({
    
    plot_data <- filtered_animals() |>
      dplyr::mutate(
        TL_cm = suppressWarnings(as.numeric(TL_cm)),
        Sex = dplyr::case_when(
          toupper(trimws(as.character(Sex))) %in%
            c("F", "FEMALE") ~ "Female",
          toupper(trimws(as.character(Sex))) %in%
            c("M", "MALE") ~ "Male",
          TRUE ~ "Unknown"
        )
      ) |>
      dplyr::filter(
        !is.na(Common_Name),
        !is.na(TL_cm),
        TL_cm > 0
      )
    
    shiny::validate(
      shiny::need(
        nrow(plot_data) > 0,
        "No total length measurements available for this selection."
      )
    )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = stats::reorder(Common_Name, TL_cm, FUN = stats::median),
        y = TL_cm
      )
    ) +
      ggplot2::geom_boxplot(
        width = 0.5,
        fill = "grey90",
        colour = "grey40",
        outlier.shape = NA,
        na.rm = TRUE
      ) +
      ggplot2::geom_jitter(
        ggplot2::aes(colour = Sex),
        width = 0.15,
        height = 0,
        size = 2.8,
        alpha = 0.85
      ) +
      # ggplot2::coord_flip() +
      ggplot2::scale_colour_manual(
        values = c(
          "Female" = "#D55E00",
          "Male" = "#0072B2",
          "Unknown" = "grey50"
        )
      ) +
      ggplot2::labs(
        x = NULL,
        y = "Total length (cm)",
        colour = "Sex"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "right"
      )
  })
  
  
  
  ## CPUE by river and gear type
  
  
  cpue_river_gear_data <- shiny::reactive({
    
    fishing_data <- dpi_fishing |>
      dplyr::left_join(river_lookup, by = "Location") |>
      dplyr::filter(
        River %in% river_order,
        Gear %in% c("Drumline", "Longline")
      )
    
    catch_data <- dpi_sharks_sites |>
      dplyr::mutate(
        Gear = dplyr::case_when(
          grepl("Drumline", Catch_Method, ignore.case = TRUE) ~ "Drumline",
          grepl("Longline", Catch_Method, ignore.case = TRUE) ~ "Longline",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::filter(
        River %in% river_order,
        Gear %in% c("Drumline", "Longline")
      )
    
    if (!is.null(selected_river())) {
      fishing_data <- fishing_data |>
        dplyr::filter(River == selected_river())
      
      catch_data <- catch_data |>
        dplyr::filter(River == selected_river())
    }
    
    effort <- fishing_data |>
      dplyr::group_by(River, Gear) |>
      dplyr::summarise(
        Hook_Hours = sum(Hook_Hours, na.rm = TRUE),
        Missing_Effort = sum(is.na(Hook_Hours)),
        .groups = "drop"
      )
    
    catches <- catch_data |>
      dplyr::count(River, Gear, name = "Caught")
    
    all_combinations <- expand.grid(
      River = unique(fishing_data$River),
      Gear = c("Drumline", "Longline"),
      stringsAsFactors = FALSE
    ) |>
      tibble::as_tibble()
    
    all_combinations |>
      dplyr::left_join(effort, by = c("River", "Gear")) |>
      dplyr::left_join(catches, by = c("River", "Gear")) |>
      dplyr::mutate(
        Hook_Hours = dplyr::coalesce(Hook_Hours, 0),
        Missing_Effort = dplyr::coalesce(Missing_Effort, 0L),
        Caught = dplyr::coalesce(Caught, 0L),
        CPUE = dplyr::if_else(
          Missing_Effort == 0 & Hook_Hours > 0,
          Caught / Hook_Hours,
          NA_real_
        ),
        River = factor(River, levels = river_order),
        Gear = factor(Gear, levels = c("Drumline", "Longline"))
      )
  })
  
  
  
  
  output$cpue_river_gear_plot <- shiny::renderPlot({
    
    plot_data <- cpue_river_gear_data()
    
    shiny::validate(
      shiny::need(
        nrow(plot_data) > 0,
        "No CPUE data available for this selection."
      )
    )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = River,
        y = CPUE,
        fill = Gear
      )
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_dodge(width = 0.75),
        width = 0.65,
        na.rm = TRUE
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "Drumline" = "#64748B",
          "Longline" = "#287C78"
        )
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.08)),
        labels = scales::label_number(accuracy = 0.0001)
      ) +
      ggplot2::labs(
        x = NULL,
        y = "CPUE (sharks hook⁻¹ h⁻¹)",
        fill = "Gear"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        ),
        legend.position = "top"
      )
  })
  
  
  ## heat map of effort
  
  effort_heatmap_data <- shiny::reactive({
    
    plot_data <- dpi_fishing |>
      dplyr::left_join(river_lookup, by = "Location") |>
      dplyr::filter(River %in% river_order) |>
      dplyr::mutate(
        Month = as.Date(format(as.Date(Date), "%Y-%m-01")),
        River = factor(River, levels = rev(river_order))
      )
    
    if (!is.null(selected_river())) {
      plot_data <- plot_data |>
        dplyr::filter(River == selected_river())
    }
    
    plot_data |>
      dplyr::count(River, Month, name = "Deployments")
  })
  
  output$effort_heatmap_plot <- shiny::renderPlot({
    
    plot_data <- effort_heatmap_data()
    
    shiny::validate(
      shiny::need(
        nrow(plot_data) > 0,
        "No fishing-effort data available for this selection."
      )
    )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = Month,
        y = River,
        fill = Deployments
      )
    ) +
      ggplot2::geom_tile(
        colour = "white",
        linewidth = 0.4
      ) +
      ggplot2::scale_fill_gradient(
        low = "#E8EEF2",
        high = "#1F4E5F"
      ) +
      ggplot2::scale_x_date(
        date_labels = "%b %Y",
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        x = "Month",
        y = NULL,
        fill = "Deployments"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        ),
        legend.position = "right"
      )
  })
  
  
  
  
  
  ## gear type comparison 
  
  # Shared data for all three plots
  gear_comparison <- shiny::reactive({
    
    effort <- filtered_fishing() |>
      dplyr::filter(Gear %in% c("Drumline", "Longline")) |>
      dplyr::group_by(Gear) |>
      dplyr::summarise(
        Hook_Hours = sum(Hook_Hours, na.rm = TRUE),
        Missing_Effort = sum(is.na(Hook_Hours)),
        .groups = "drop"
      )
    
    catches <- filtered_animals() |>
      dplyr::mutate(
        Gear = dplyr::case_when(
          grepl("Drumline", Catch_Method, ignore.case = TRUE) ~ "Drumline",
          grepl("Longline", Catch_Method, ignore.case = TRUE) ~ "Longline",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::filter(!is.na(Gear)) |>
      dplyr::count(Gear, name = "Caught")
    
    effort |>
      dplyr::left_join(catches, by = "Gear") |>
      dplyr::mutate(
        Caught = dplyr::coalesce(Caught, 0L),
        CPUE = dplyr::if_else(
          Missing_Effort == 0 & Hook_Hours > 0,
          Caught / Hook_Hours,
          NA_real_
        ),
        Gear = factor(Gear, levels = c("Drumline", "Longline"))
      )
  })
  
  
  # Common plotting function
  gear_plot <- function(dat, variable, y_label) {
    
    ggplot2::ggplot(
      dat,
      ggplot2::aes(
        x = Gear,
        y = .data[[variable]],
        fill = Gear
      )
    ) +
      ggplot2::geom_col(
        width = 0.55,
        na.rm = TRUE
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "Drumline" = "#64748B",
          "Longline" = "#287C78"
        )
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.1)),
        labels = scales::label_number(
          accuracy = if (variable == "CPUE") 0.0001 else 1
        )
      ) +
      ggplot2::labs(
        x = NULL,
        y = y_label
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(
          margin = ggplot2::margin(r = 10)
        )
      )
  }
  
  
  # Fishing effort
  output$gear_effort_plot <- shiny::renderPlot({
    
    gear_plot(
      gear_comparison(),
      "Hook_Hours",
      "Fishing effort (hook-hours)"
    )
  })
  
  
  # Shark catches
  output$gear_catches_plot <- shiny::renderPlot({
    
    gear_plot(
      gear_comparison(),
      "Caught",
      "Number of sharks caught"
    )
  })
  
  
  # CPUE
  output$gear_cpue_plot <- shiny::renderPlot({
    
    gear_plot(
      gear_comparison(),
      "CPUE",
      "CPUE (sharks hook⁻¹ h⁻¹)"
    )
  })
  
  
  output$receiver_map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        group = "Satellite"
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$OpenStreetMap,
        group = "OpenStreetMap"
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldTopoMap,
        group = "Topographic"
      ) |>
      leaflet::addTiles(
        urlTemplate = paste0(
          "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png?key=",
          carto_key
        ),
        group = "Light",
        attribution = "&copy; OpenStreetMap contributors &copy; CARTO"
      ) |>
      leaflet::addTiles(
        urlTemplate = paste0(
          "https://basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png?key=",
          carto_key
        ),
        group = "Dark",
        attribution = "&copy; OpenStreetMap contributors &copy; CARTO"
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldStreetMap,
        group = "Street Map"
      ) |>
      leaflet::addPolygons(
        data = river_sites.sf,
        group = "Study rivers",
        layerId = ~River,
        label = ~River,
        color = ~river_pal(River),
        fillColor = ~river_pal(River),
        weight = 2, fillOpacity = 0.3
      ) |>
      leaflet::addMarkers(
        data = dpi_rcr.sf,
        group = "Acoustic receivers",
        icon = receiver_icons,
        layerId = ~as.character(`Receiver ID`),
        label = ~paste0(Station, " | ", `Receiver ID`),
        
        popup = ~paste0(
          "<b>River:</b> ", River,
          "<br><b>Station:</b> ", Station,
          "<br><b>Receiver ID:</b> ", `Receiver ID`,
          "<br><b>Model:</b> ", Model,
          "<br><b>Deployment date:</b> ",
          format(`Date deployed`, "%d %b %Y"),
          "<br><b>Depth:</b> ",
          sprintf("%.1f", suppressWarnings(as.numeric(Depth))), " m"
        )
      ) |> 
      
      
      
      leaflet::addCircleMarkers(
        data = dpi_fishing.sf,
        group = "Fishing locations",
        radius = 3,
        color = "white",
        weight = 0.5,
        fillColor = "blue",
        fillOpacity = 0.9,
        popup = ~paste0(
          "<b>Location:</b> ", Location,
          "<br><b>River:</b> ", River,
          "<br><b>Date:</b> ", format(Date, "%d %b %Y"),
          "<br><b>Gear:</b> ", Gear_Type,
          "<br><b>Depth:</b> ", Depth
        )
      ) |> 
      
      
      leaflet::addMarkers(
        data = dpi_catches.sf,
        group = "Shark catches",
        icon = catch_icon,
        popup = ~paste0(
          "<b>Species:</b> ", Common_Name,
          "<br><b>Location:</b> ", Location,
          "<br><b>Date:</b> ", format(Date, "%d %b %Y"),
          "<br><b>TL:</b> ", TL_cm, " cm",
          "<br><b>Sex:</b> ", Sex,
          "<br><b>Acoustic ID:</b> ",
          dplyr::case_when(
            Acoustic_Tag %in% FALSE ~ "Not tagged",
            Acoustic_Tag %in% TRUE &
              !is.na(Acoustic_ID) &
              Acoustic_ID != "" ~ as.character(Acoustic_ID),
            Acoustic_Tag %in% TRUE ~ "Tagged (ID missing)",
            TRUE ~ "Tagging status unknown"
          )
        )
      ) |>
      
      
      leaflet::addLabelOnlyMarkers(
        data = river_labels.sf,
        group = "River labels",
        label = ~River,
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = "#123B5D",
            "font-weight" = "bold",
            "font-size" = "13px",
            "text-shadow" = "1px 1px 3px white, -1px -1px 3px white"
          )
        )
      ) |>
      
      
      leaflet::addLayersControl(
        baseGroups = c(
          "Satellite",
          "OpenStreetMap",
          "Topographic",
          "Light",
          "Dark",
          "Street Map"
        ),
        overlayGroups = c(
          "Study rivers",
          "River labels",
          "Fishing locations",
          "Acoustic receivers",
          "Shark catches"
        ),
        options = leaflet::layersControlOptions(
          collapsed = FALSE
        )
      ) |> 
      leaflet::addScaleBar(position = "bottomleft") |> 
      htmlwidgets::onRender("
  function(el, x) {

    const symbols = {
      'Fishing locations':
        '<svg width=\"16\" height=\"16\" viewBox=\"0 0 20 20\" style=\"vertical-align:middle; margin-right:6px;\">' +
        '<circle cx=\"10\" cy=\"10\" r=\"5\" fill=\"#0000FF\"/></svg>',

      'Acoustic receivers':
        '<svg width=\"16\" height=\"16\" viewBox=\"0 0 20 20\" style=\"vertical-align:middle; margin-right:6px;\">' +
        '<path d=\"M10 2 L18 18 L2 18 Z\" fill=\"#16A6B6\" stroke=\"white\" stroke-width=\"1\"/></svg>',

      'Shark catches':
        '<svg width=\"16\" height=\"16\" viewBox=\"0 0 20 20\" style=\"vertical-align:middle; margin-right:6px;\">' +
        '<path d=\"M4 4 L16 16 M16 4 L4 16\" stroke=\"#FF0000\" stroke-width=\"3\" stroke-linecap=\"round\"/></svg>'
    };

    function addSymbols() {

      el.querySelectorAll(
        '.leaflet-control-layers-overlays label'
      ).forEach(function(label) {

        const input = label.querySelector('input');
        if (!input) return;

        const nameSpan = input.nextElementSibling;
        if (!nameSpan) return;

        if (nameSpan.querySelector('.layer-symbol')) return;

        const name = nameSpan.textContent.trim();

        if (symbols[name]) {
          nameSpan.insertAdjacentHTML(
            'afterbegin',
            '<span class=\"layer-symbol\">' +
            symbols[name] +
            '</span>'
          );
        }
      });
    }

    this.whenReady(function() {
      setTimeout(addSymbols, 100);

      const observer = new MutationObserver(addSymbols);
      observer.observe(el, {
        childList: true,
        subtree: true
      });
    });
  }
")
  })
}

shiny::shinyApp(ui, server)
