setwd("~/Library/CloudStorage/OneDrive-UGent/motivation-democracy-hub/")
library(stringr); library(stringdist)


dff <- readxl::read_xlsx("~/Library/CloudStorage/OneDrive-UGent/motivation-democracy-hub/Publications.xlsx")
dff <- as.data.frame(dff)

titles <- dff$title

normalize_title <- function(x) {
  x <- tolower(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9 ]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# --- Detect duplicates based on similarity ---
detect_similar_titles <- function(titles, threshold = 0.9) {
  titles_clean <- normalize_title(titles)
  dmat <- stringdistmatrix(titles_clean, method = "jw", p = 0.1)
  sim <- 1 - as.matrix(dmat)
  diag(sim) <- 1
  
  idx <- which(sim >= threshold & upper.tri(sim), arr.ind = TRUE)
  if (nrow(idx) == 0) {
    message("No near-duplicate titles found above similarity threshold.")
    return(NULL)
  }
  
  data.frame(
    i = idx[,1],
    j = idx[,2],
    title_i = titles[idx[,1]],
    title_j = titles[idx[,2]],
    similarity = round(sim[idx], 3),
    stringsAsFactors = FALSE
  )
}

# --- Run ---
similar_titles <- detect_similar_titles(titles, threshold = 0.9)
print(similar_titles)





dff <- dff[order(dff$year,decreasing = TRUE),]
df <- as.data.frame(dff)
dff$id <- 1:dim(dff)[1]

df[is.na(df$Abstract),'Abstract'] <- ""

df$title<- gsub('"', "'", df$title)
df$title<- gsub(' :', ":", df$title)

## --- Helpers ---
trim_ws <- function(x) gsub("^\\s+|\\s+$", "", x)

col_to_excel_letter <- function(n) {
  # 1 -> A, 26 -> Z, 27 -> AA, etc.
  out <- character()
  while (n > 0) {
    r <- (n - 1) %% 26
    out <- c(LETTERS[r + 1], out)
    n <- (n - r - 1) %/% 26
  }
  paste0(out, collapse = "")
}

split_items <- function(x) {
  # Accept comma or semicolon separated; remove surrounding quotes/spaces
  if (is.na(x) || trim_ws(x) == "") return(character(0))
  parts <- unlist(strsplit(x, "[,;]"))
  parts <- trim_ws(gsub('^"(.*)"$', "\\1", parts)) # strip outer quotes
  parts[nzchar(parts)]
}

is_integerish <- function(x) {
  suppressWarnings(!is.na(x) & !is.na(as.numeric(x)) & as.numeric(x) %% 1 == 0)
}

valid_year <- function(x, min_year = 1900, max_year = as.integer(format(Sys.Date(), "%Y")) + 1) {
  suppressWarnings({
    y <- as.numeric(x)
    !is.na(y) && y %% 1 == 0 && y >= min_year && y <= max_year && nchar(as.character(abs(y))) == 4
  })
}

valid_url <- function(x) {
  if (is.na(x)) return(FALSE)
  grepl("^https?://", x, ignore.case = TRUE)
}

non_empty <- function(x) {
  !is.na(x) && trim_ws(as.character(x)) != ""
}

all_non_empty <- function(vec) {
  length(vec) > 0 && all(nzchar(trim_ws(vec)))
}

## --- Allowed values (edit to your needs) ---
ALLOWED_TYPE      <- c("Artikel","Boek","Hoofdstuk","Preprint","Rapport","Thesis","Conference","Dataset","Tool","Doctoraat")
ALLOWED_LANGUAGE  <- c("Nederlands","Engels","Frans","Duits","Spaans","Italiaans","Portugees","Fins","Zweeds","Noors")
ALLOWED_GROUP     <- c("Algemeen","Baby's en peuters","Kinderen",
                       "Jongeren","Jongvolwassenen","Studenten","Volwassenen","Ouderen","Leerkrachten","Professionals")

## --- Updated authors validator: every author must be in double quotes ---
authors_validator <- function(x) {
  if (is.na(x) || trim_ws(x) == "") {
    return("authors must contain one or more names in double quotes")
  }
  raw_items <- trim_ws(unlist(strsplit(x, "[,;]")))
  if (length(raw_items) == 0) {
    return("authors must contain one or more names in double quotes")
  }
  bad <- !grepl('^".+?"$', raw_items)  # must start & end with double quotes, non-empty inside
  if (any(bad)) {
    bad_vals <- paste(raw_items[bad], collapse = "; ")
    return(paste0("each author must be enclosed in double quotes: ", bad_vals))
  }
  # Also check that after stripping quotes, items are non-empty
  stripped <- gsub('^"(.*)"$', "\\1", raw_items)
  if (!all_non_empty(stripped)) {
    return("authors contains empty items after quotes are removed")
  }
  NA
}

## --- Core validator factory ---
make_validators <- function(df) {
  # Normalize misspelling before validating
  
  
  validators <- list(
    id = function(x) if (is_integerish(x) && as.numeric(x) > 0) NA else "id must be a positive integer",
    type = function(x) if (x %in% ALLOWED_TYPE) NA else paste0("type must be one of: ", paste(ALLOWED_TYPE, collapse = ", ")),
    year = function(x) if (valid_year(x)) NA else "year must be a 4-digit integer within a sensible range",
    title = function(x) if (non_empty(x)) NA else "title cannot be empty",
    url = function(x) if (valid_url(x)) NA else "url must start with http:// or https://",
    language = function(x) if (x %in% ALLOWED_LANGUAGE) NA else paste0("language must be one of: ", paste(ALLOWED_LANGUAGE, collapse = ", ")),
    journal = function(x) if (non_empty(x)) NA else "journal cannot be empty",
    group = function(x) if (x %in% ALLOWED_GROUP) NA else paste0("group must be one of: ", paste(ALLOWED_GROUP, collapse = ", ")),
    authors = authors_validator,   # <-- stricter rule here
    HOOFDDOMEINEN = function(x) if (non_empty(x)) NA else "HOOFDDOMEINEN cannot be empty",
    SUBDOMEINEN = function(x) {
      items <- split_items(x)
      if (length(items) == 0) NA else if (all_non_empty(items)) NA else "SUBDOMEINEN contains empty items"
    },
    TOPICS = function(x) {
      items <- split_items(x)
      if (all_non_empty(items)) NA else "TOPICS must contain one or more non-empty items separated by , or ;"
    },
    SUBTOPICS = function(x) {
      items <- split_items(x)
      if (length(items) == 0) NA else if (all_non_empty(items)) NA else "SUBTOPICS contains empty items"
    }
  )
  
  # Only keep validators for columns present; ignore extras
  validators[names(validators) %in% names(df)]
}

fix_missing_commas <- function(s) {
  if (is.na(s)) return(s)
  s <- as.character(s)
  
  # Case 1: adjacent quoted items without delimiter
  #   "A" "B"   -> "A","B"
  #   "A""B"    -> "A","B"
  s <- gsub('"\\s*"', '","', s, perl = TRUE)
  
  # (Optional) If someone wrote ["A" "B"], the rule above fixes the interior too.
  # Keep original whitespace tidy (purely cosmetic)
  s <- gsub('\\s+,\\s+', ', ', s, perl = TRUE)
  
  s
}

cols_to_fix <- intersect(
  c("authors","HOOFDDOMEINEN","SUBDOMEINEN","HOOFDTOPICS","SUBTOPICS","group"),
  names(dff)
)

# dff[cols_to_fix] <- lapply(dff[cols_to_fix], function(col)
#   vapply(col, fix_missing_commas, character(1))
# )

cols_to_fix <- intersect(
  c("authors","HOOFDDOMEINEN","SUBDOMEINEN","HOOFDTOPICS","SUBTOPICS","group"),
  names(df)
)

# df[cols_to_fix] <- lapply(df[cols_to_fix], function(col)
#   vapply(col, fix_missing_commas, character(1))
# )


## --- Main check function ---
check_table <- function(df) {
  # Normalize misspelling before validating
 
  validators <- make_validators(df)
  errs <- list()
  
  for (col_name in names(validators)) {
    validator <- validators[[col_name]]
    col_idx <- match(col_name, names(df))
    for (i in seq_len(nrow(df))) {
      val <- df[[col_name]][i]
      msg <- tryCatch(validator(val), error = function(e) paste0("validation error: ", e$message))
      if (!is.na(msg) && nzchar(msg)) {
        # Excel cell: header row is 1, data starts at row 2
        cell <- paste0(col_to_excel_letter(col_idx), i + 1)
        errs[[length(errs) + 1]] <- data.frame(
          row = i,
          column = col_name,
          value = as.character(if (is.na(val)) NA else val),
          error = msg,
          cell = cell,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(errs) == 0) {
    out <- data.frame(row = integer(0), column = character(0), value = character(0), error = character(0), cell = character(0))
  } else {
    out <- do.call(rbind, errs)
    # nice sorting if dplyr available
    if (requireNamespace("dplyr", quietly = TRUE)) {
      out <- dplyr::arrange(out, row, cell)
    }
  }
  out
}

errors <- check_table(df)
View(errors)







results <- list()

# --- Case helpers ---
cap_first_only <- function(x) {
  x <- trimws(as.character(x))
  x <- ifelse(
    nzchar(x),
    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))),
    x
  )
  x
}

normalize_parts <- function(parts) {
  parts <- parts[!is.na(parts) & trimws(parts) != "" & parts != "NA"]
  if (!length(parts)) return(character(0))
  cap_first_only(parts)
}


format_field <- function(x) {
  # Accept bare string, comma/semicolon list, or ["A","B"]
  if (is.na(x) || trimws(x) == "") return('""')
  s <- as.character(x)
  
  # If it looks like ["A","B"], strip brackets and reuse the same path
  if (grepl("^\\s*\\[.*\\]\\s*$", s)) {
    s <- gsub("^\\s*\\[|\\]\\s*$", "", s)
  }
  
  # Split on comma/semicolon, clean tokens
  parts <- unlist(strsplit(s, "[,;]"))
  parts <- gsub('^"|"$', '', parts)
  parts <- normalize_parts(parts)
  if (!length(parts)) return('""')
  
  if (length(parts) == 1) {
    sprintf('"%s"', parts)
  } else {
    sprintf('[%s]', paste(sprintf('"%s"', parts), collapse = ", "))
  }
}


format_agegroup <- function(x) {
  # NA/empty -> ""
  if (length(x) == 0 || is.na(x) || trimws(x) == "" || x %in% c("NA", "[NA]")) return('""')
  
  s <- trimws(x)
  
  # If it already looks like ["A","B"], keep as is
  if (grepl("^\\s*\\[.*\\]\\s*$", s)) return(s)
  
  # Strip outer stray quotes
  s <- gsub('^"|"$', '', s)
  
  # Split on commas, clean tokens
  parts <- unlist(strsplit(s, "\\s*,\\s*"))
  parts <- gsub('^"|"$', '', parts)
  parts <- parts[!is.na(parts) & trimws(parts) != "" & parts != "NA"]
  
  if (!length(parts)) return('""')
  if (length(parts) == 1) return(sprintf('"%s"', parts))
  
  # More than one -> ["A","B","C"] (no spaces)
  paste0("[", paste(sprintf('"%s"', parts), collapse = ","), "]")
}

names <- basename(df$PDF)
names_clean <- tolower(names)
names_clean <- gsub("[ _]+", "-", names_clean)
names_clean <- iconv(names_clean, from = "UTF-8", to = "ASCII//TRANSLIT")  # remove accents
names_clean <- gsub("[^a-z0-9\\.-]", "", names_clean)
df$PDF <- names_clean


format_field_list <- function(x) {
  # Always return a JSON-like list [] and normalize case of each item
  if (length(x) == 0 || is.na(x) || trimws(x) == "" || x %in% c("NA","[NA]")) return("[]")
  s <- as.character(x)
  
  # If already bracketed like ["A","B"], strip brackets to normalize items uniformly
  if (grepl("^\\s*\\[.*\\]\\s*$", s)) {
    s <- gsub("^\\s*\\[|\\]\\s*$", "", s)
  }
  
  # Remove stray outer quotes, then split on comma or semicolon
  s <- gsub('^"|"$', '', s)
  parts <- unlist(strsplit(s, "\\s*[,;]\\s*"))
  parts <- gsub('^"|"$', '', parts)
  
  # Reuse your normalizer to "Cap first, lower rest"
  parts <- normalize_parts(parts)
  
  if (!length(parts)) return("[]")
  paste0("[", paste(sprintf('"%s"', parts), collapse=","), "]")
}

results <- vector("list", nrow(df))

for (i in seq_len(nrow(df))) {
  journal_str <- if (is.na(df$journal[i]) || trimws(df$journal[i]) == "" || df$journal[i] %in% c("NA", "[NA]")) '""' 
  else paste0('"', df$journal[i], '"')
  age_str <- format_agegroup(df$group[i])
  a <- df$authors[i]
  authors_str <-
    if (length(a) == 0 || is.na(a) || trimws(a) == "" || a %in% c("NA", "[NA]")) {
      '[""]'                      # -> when authors is NA/empty/[NA]
    } else if (grepl("^\\s*\\[.*\\]\\s*$", a)) {
      a                           # already like ["A","B"] -> keep as is
    } else {
      paste0("[", a, "]")         # wrap e.g. "A","B" -> ["A","B"]
    }
  
 

  pdf_str <- if (is.na(df$PDF[i]) || df$PDF[i] %in% c("NA", "na", "[NA]", "")) {
    '""'
  } else {
    paste0('"http://https://watjoa.github.io/psyco-datahub/PDFARTIKELS/', df$PDF[i], '"')
  }
  
  results[[i]] <- paste(
    "{id:", dff$id[i],
    ',title:"', df$title[i], '"',
    ',project:"', df$project[i], '"',
     ",year:", df$year[i],
     ",authors:", authors_str,
     ',language:"', df$language[i], '"',
     ",journal:",  journal_str,
     ",ageGroup:", age_str,
     ',format:"', df$type[i], '"',
     ",domain:",      format_field_list(dff$HOOFDDOMEINEN[i]),
     ",subdomain:",   format_field_list(dff$SUBDOMEINEN[i]),
     ",mainsubjects:",    format_field_list(dff$HOOFDTOPICS[i]),
     ",subsubject:", format_field_list(dff$SUBTOPICS[i]),   
    ",pdf:", pdf_str,
    ',abstract:"', df$Abstract[i], '"',
    #",contactEmail:", df$email[i],  
     ',url:"', df$url[i], '"},',
    sep = ""
  )
}


# Path to your folder
path <- "~/Library/CloudStorage/OneDrive-UGent/motivation-democracy-hub/PDFARTIKELS"
files <- list.files(path, full.names = TRUE)
names <- basename(files)
names_clean <- tolower(names)
names_clean <- gsub("[ _]+", "-", names_clean)
names_clean <- iconv(names_clean, from = "UTF-8", to = "ASCII//TRANSLIT")  # remove accents
names_clean <- gsub("[^a-z0-9\\.-]", "", names_clean)
new_files <- file.path(path, names_clean)
file.rename(from = files, to = new_files)

wow <- do.call(rbind, results)
wow <- as.data.frame(wow)

writexl::write_xlsx(wow,'wow.xlsx')







