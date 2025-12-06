# This script downloads your life list, identifies common missing species in
# your target region, downloads images, and generates Anki flashcards.


# User configuration ------------------------------------------------------

# eBird regions for frequency (modify as needed)
my_locations <- c("CO")

# Name of your project folder inside Documents
project_name <- "colombia_birds"

# Max number of species to include in your study set
n_species <- 250 

# Month range you'll be visiting
my_startmonth <- 1
my_endmonth <- 12

# Optional: override Anki media directory (otherwise auto-detected)
# Sys.setenv(ANKI_MEDIA = "C:/path/to/collection.media")


# Detect file directories -------------------------------------------------

# This function will determine where your documents files live
get_documents_dir <- function() {
  # 1) Prefer USERPROFILE/Documents
  up <- Sys.getenv("USERPROFILE", unset = "")
  if (nzchar(up)) {
    d1 <- file.path(up, "Documents")
    if (dir.exists(d1)) {
      return(normalizePath(d1, winslash = "/", mustWork = FALSE))
    }
  }

  # 2) OneDrive redirect
  od <- Sys.getenv("OneDrive", unset = "")
  if (nzchar(od)) {
    d2 <- file.path(od, "Documents")
    if (dir.exists(d2)) {
      return(normalizePath(d2, winslash = "/", mustWork = FALSE))
    }
  }

  # 3) Fallback to ~/Documents, collapse Documents/Documents if needed
  d3 <- normalizePath(
    path.expand("~/Documents"), winslash = "/", mustWork = FALSE)
  d3 <- sub("(.*Documents)/+Documents$", "\\1/Documents", d3)
  return(d3)
}

# Use the function above to find your Documents
documents_dir <- get_documents_dir()

# Find your Downloads folder
downloads_dir <- normalizePath(
  file.path(Sys.getenv("USERPROFILE"), "Downloads"),
  winslash = "/", 
  mustWork = FALSE
)

# Detect Anki media path (if you overwrote it above)
anki_media <- Sys.getenv("ANKI_MEDIA", unset = "")

# If you didn't overwrite it, get the default Anki path
if (!nzchar(anki_media)) {
  anki_media <- file.path(Sys.getenv("APPDATA"), "Anki2", "User 1", "collection.media")
}

# Complain if it can't find that default Anki path
if (!dir.exists(anki_media)) {
  stop("Could not find Anki media directory:\n", anki_media,
       "\nSet ANKI_MEDIA environment variable if needed.")
}

# Create a new folder (directory) for this flashcard project
project_dir <- file.path(documents_dir, project_name)

# Only create it if it doesn't already exist
if (!dir.exists(project_dir)) {
  dir.create(project_dir, recursive = TRUE)
  message("Created: ", project_dir)
}


# Load required packages --------------------------------------------------

library(tidyverse) # For data manipulation
library(rebird) # For eBird API
library(rvest) # For web scraping
library(glue) # For better pasting of text
library(readr) # For better file import


# Download life list ------------------------------------------------------

message("Please save life list to your project folder with default name!")
utils::browseURL("https://ebird.org/lifelist?r=world&time=life&fmt=csv")

# Import your life list to the project folder
life_dest <- file.path(project_dir, "ebird_world_life_list.csv")

# Import it as a vector of common names only
my_lifelist <- 
  read_csv(life_dest, show_col_types = FALSE) |> 
  pull(`Common Name`)


# Download eBird frequency data -------------------------------------------

# First we have to build a function that opens a browser window and
# gets the file.

# Browser-triggered download with polling
download_via_browser <- 
  function(url, expected_filename, downloads = downloads_dir, destfile, max_wait = 60, interval = 5) {
  
  utils::browseURL(url) # Need to use this rather than download.file
  message("Opened browser to download: ", expected_filename)
  
  src <- file.path(downloads, expected_filename)
  
  waited <- 0
  
  # Wait until the file downloads, checking every [interval] seconds
  repeat {
    if (file.exists(src)) {
      message("  Found file after ", waited, " seconds.")
      break
    }
    
    # Give up if waited too long
    if (waited >= max_wait) {
      stop(
        sprintf(
          "Timed out after %d seconds. '%s' not found in Downloads.",
          max_wait, expected_filename)
      )
    }
    
    Sys.sleep(interval)
    waited <- waited + interval
    message("  Waiting... ", waited, " seconds elapsed.")
  }
  
  ok <- file.rename(src, destfile)
  if (!ok) stop("Failed to move file to: ", destfile)
  
  message("Saved to: ", destfile)
  destfile
}



get_freq <- function(
    location,
    startyear = lubridate::year(Sys.Date()) - 10,
    endyear = lubridate::year(Sys.Date()),
    startmonth = 1, endmonth = 12,
    targetdir = project_dir) {
  
  # Construct URL
  url <- glue(
    "https://ebird.org/barchartData?r={location}&bmo={startmonth}",
    "&emo={endmonth}&byr={startyear}&eyr={endyear}&fmt=tsv"
  )
  
  # Expected resulting filename in Downloads
  filename <- glue(
    "ebird_{location}__{startyear}_{endyear}_{startmonth}_{endmonth}_barchart.txt"
  )
  
  # Destination path inside project
  destfile <- file.path(targetdir, filename)

  # Skip if already present
  if (file.exists(destfile)) {
    message("Using cached frequency file: ", filename)
  } else {
    message("Downloading frequency file for ", location)
    
    # Browser-triggered download → move from Downloads
    download_via_browser(
      url = url,
      expected_filename = filename,
      destfile = destfile,
      max_wait = 30,
      interval = 5
    )
  }
  
  # Prepare for column names
  
  
  # Import
  read_tsv(
    destfile,
    skip = 15,
    col_names = FALSE,
    show_col_types = FALSE
  ) |>
    mutate(location = location, .before = 1)
}

# Download all locations' frequency data
birds <- map(
  my_locations, 
  ~ get_freq(
    .x, 
    startmonth = my_startmonth, 
    endmonth = my_endmonth)
  )


# Prepare species list ----------------------------------------------------

# Load in eBird taxonomy
tax <- ebirdtaxonomy() |> 
  select(comName, sciName, speciesCode, familySciName, category)

# Determine which columns we need based on your chosen time period
cols_for_months <- function(startmonth, endmonth) {
  # offsets: Jan starts at column 2
  start_col <- (startmonth - 1) * 4 + 2
  end_col   <- (endmonth   - 1) * 4 + 5
  
  paste0("X", start_col:end_col)
}


my_birds <-
  bind_rows(birds) |>
  rowwise() |> # Needed to properly compute frequency
   mutate(
    # Clunky function to take mean only from columns in your month range
    freq = mean(
      c_across(
        all_of(
          cols_for_months(my_startmonth, my_endmonth)
          )
        ), 
      na.rm = TRUE),
    # Remove sci name if present (we will add it back later)
    comName = str_remove(X1, " \\(<em class=\"sci\">.*") 
  ) |>
  ungroup() |>
  select(location, comName, freq) |>
  left_join(tax, by = "comName") # Add taxonomy table info

# Save full list as a table
write_csv(
  my_birds, 
  file.path(
    project_dir, 
    glue("birds_{my_startmonth}-{my_endmonth}.csv")
    )
  )

# Create list of focal species to learn
my_birds_focal <-
  my_birds |> 
  filter(
    category == "species", # Drop hybrids, spuhs, domestics
    # Comment out the below line if you want to study non-lifers too!
    !comName %in% my_lifelist # Only include new birds to you
  ) |> 
  # For each sp, only take the max freq (prefer location where most common)
  group_by(comName, sciName) |> 
  slice_max(order_by = freq, n = 1) |> 
  ungroup() |> 
  # Sort by most to least common
  arrange(desc(freq)) |> 
  # Only take, at most, the number of species you asked for
  slice_head(n = n_species)


# Download images ---------------------------------------------------------

# Now loop over every focal species and do the following:
for (i in seq_len(nrow(my_birds_focal))) {
  
  # Print start message
  message(i, " of ", nrow(my_birds_focal))
  
  # Create file fetching info for photos
  species <- my_birds_focal$speciesCode[[i]] # Code used in eBird URLs
  img_name <- paste0(species, ".jpg") # What we'll call it
  img_dest <- file.path(anki_media, img_name) # Where we'll save it
  
  # Read in the HTML of that species' webpage
  page <- tryCatch(
    read_html(paste0("https://ebird.org/species/", species)),
    error = function(e) return(NULL)
  )
  
  # If we don't get a page, give up
  if (is.null(page)) next
  
  # If we do get a page, look for images
  imgs <- html_nodes(page, "img")
  
  # If there aren't at least two images (eBird logo + species image), give up
  if (length(imgs) < 2) next
  
  # Otherwise, take the second image (first species image)
  img_url <- html_attr(imgs[2], "src")
  
  # If we don't get a URL for this image, give up
  if (is.na(img_url)) next
  
  # By default we get a dinky 320 px image 
  # but they are hiding the 1200px one so we'll ask for it instead
  full_img <- str_replace(img_url, "/320$", "/1200")
  
  # Fetch image and save to Anki folder
  download.file(full_img, img_dest, mode = "wb")
}


# Make Anki csv -----------------------------------------------------------

# This CSV will be imported into Anki to create flashcards that
# have the image path on the front and the species common+sci name on the back

anki_csv <- 
  my_birds_focal |>
  mutate(
    image = paste0("<img src='", speciesCode, ".jpg'>"),
    tags = familySciName, # Tags let you e.g. only study a single family
    species = str_c(comName, "\n<i>(", sciName, ")</i>")
  ) |>
  select(image, species, tags)

write_excel_csv(
  anki_csv,
  file = file.path(project_dir, "my_anki_cards.csv"),
  append = FALSE,
  col_names = FALSE
)

message("Done. Your Anki CSV and resources are in: ", project_dir)
