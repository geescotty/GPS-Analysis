### DATE FUNCTION - names file nicely ###

format_date_pretty <- function(date = Sys.Date()) {
  day_num <- as.numeric(format(date, "%d"))
  
  suffix <- ifelse(day_num %in% c(1, 21, 31), "st",
                   ifelse(day_num %in% c(2, 22), "nd",
                          ifelse(day_num %in% c(3, 23), "rd", "th")))
  
  paste0(
    format(date, "%A "),   # Day name
    day_num,               # Day number (no leading zero)
    suffix,
    format(date, " %B %Y") # Month and year
  )
}

format_date_pretty()

### select your season start date to count weeks in season. 
start_date <- as.Date("2026-07-20")
week_no <- floor(as.numeric(Sys.Date() - start_date) / 7) + 1


#### GPS TARGET REPORTS ###
file_name <- paste0("Week ",week_no," - " , format_date_pretty()," - Targets", ".html")


rmarkdown::render(
  "Filelocation/TargetAnalysis.Rmd",
  output_dir = "Outputlocation",
  envir = new.env(),
  output_file = file_name
)

#### TRAINING GPS REPORT 
file_name <- paste0("Week ",week_no," - ", format_date_pretty(), " - GPS Report", ".html")

rmarkdown::render(
  "Filelocation/NewReport2.Rmd",
  output_dir = "Outputlocation",
  envir = new.env(),
  output_file = file_name
)


#### MATCH REPORT 
file_name <- paste0("Week ",week_no," - ", " Game Vs Opposition (A)", ".html")

rmarkdown::render(
  "FileLocationNewMatchReport2.Rmd",
  output_dir = "OutputLocation",
  envir = new.env(),
  output_file = file_name
)
