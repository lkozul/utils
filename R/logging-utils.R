
# --------------------------------------------------------------------------- #
# 
# Logging utilities for analysis pipelines
#
# PURPOSE
# -------
# These helper functions are meant to make long analysis scripts easier to read
# both in the source code and in the console output.
#
# Rather than sprinkling cat() statements throughout your code, you call small
# helper functions with consistent formatting.
#
# Example:
#
# section("Normalization")
#
# step("Running SCTransform")
# obj <- SCTransform(obj)
# step_done()
#
# subsection("Integration")
#
# step_timer("Finding anchors")
# anchors <- FindIntegrationAnchors(...)
# step_done()
#
# --------------------------------------------------------------------------- #


# ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
# ####  CONFIGURATION  ####
# ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄ 

# Width used for pretty alignment.
# Default is 80, but I keep the console smaller than the source pane 
# (also, the output can more easily fit to narrow notepad windows :D)

LOG_WIDTH <- 60
options(width = LOG_WIDTH)



# ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
# ####  SECTION HEADERS  ####
# ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄ 

# ----  HEADER_1 / SECTION_1  ------------------------------------------------- 
#
# Highest level.
#
# Use for major analysis phases.
#
# Example:
#
# section("Load data")
# section("Normalization")
# section("Integration")
# --------------------------------------------------------------------------- #

header_1 <- function(title) {
  
  cat(
    "\n", 
    strrep("═", LOG_WIDTH), "\n",
    "  ", title, "\n",
    strrep("═", LOG_WIDTH), "\n",
    sep = ""
  )
  
}

section_1 <- header_1

# ####  HEADER_2 / SECTION_2  -------------------------------------------------  
#
# Medium level.
#
# Example:
#
# section_2("Read matrices")
# section_2("Quality control")
# --------------------------------------------------------------------------- #

header_2 <- function(title) {
  
  cat(
    "\n", 
    strrep("─", LOG_WIDTH), "\n",
    "  ", title, "\n",
    strrep("─", LOG_WIDTH), "\n",
    sep = ""
  )
  
}

section_2 <- header_2

# ####  HEADER_3 / SECTION_3  -------------------------------------------------
# section_3()
#
# Lowest level.
#
# Only use when a subsection becomes too large.
# In practice this will probably be fairly rare.
# --------------------------------------------------------------------------- #

header_3 <- function(title) {
  
  cat(
    "\n",
    "--- ", title, " ",
    strrep("-", max(1, LOG_WIDTH - nchar(title) - 5)),
    "\n",
    sep = ""
  )
  
}

section_3 <- header_3

# ####  Examples:  ------------------------------------------------------------  
# 
# cat("\n\n",
#   "    ╭───────────────────────────────╮\n",
#   "    │        Section Headers        │\n",
#   "    ╰───────────────────────────────╯\n", sep = "")
# 
# header_1("SECTION HEADER 1")
# header_2("Section Header 2")
# header_3("Section Header 3")
# 
# Output:
# 
# ════════════════════════════════════════════════════════════
# Section / Header 1
# ════════════════════════════════════════════════════════════
# 
# ────────────────────────────────────────────────────────────
# Section / Header 2
# ────────────────────────────────────────────────────────────
# 
# --- Section / Header 3 ---------------------------------------




# ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
# ####  STEP TRACKING  ####
# ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄ 

# These functions work together.
#
# step("Loading metadata")
#
# ...
#
# step_done() ....... prints a simple checkmark
# OR
# step_elapsed() .... prints checkmark and elapsed time in seconds
#
#
# They remember what the previous step was so you don't need to repeat yourself.


.current_step <- NULL
.step_start   <- NULL

# ####  STEP()  --------------------------------------------------------------- 
# step()
#
# Announces what the script is doing.
#
# Also stores the current time internally (global variable) so that step_done() 
# can later compute the elapsed time.
# --------------------------------------------------------------------------- #

step <- function(message) {
  
  .current_step <<- message
  .step_start   <<- Sys.time()
  
  dots <- strrep(
    ".",
    max(1, LOG_WIDTH - nchar(message))
  )
  
  cat(message, dots, " ", sep = "")
  
}


# ####  STEP_DONE()  ----------------------------------------------------------
# step_done()
#
# Finishes the previous step.
#
# By default it prints a check mark.
#
# Optionally it can also report the elapsed time.
# --------------------------------------------------------------------------- #

step_done <- function(show_time = TRUE) {
  
  if (show_time) {
    
    elapsed <- difftime(
      Sys.time(),
      .step_start,
      units = "secs"
    )
    
    cat(
      sprintf(
        "✓ (%.1f s)\n",
        as.numeric(elapsed)
      )
    )
    
  } else {
    
    cat("✓\n")
    
  }
  
}


# ####  Examples:  ------------------------------------------------------------  
# 
# Simple finish marker:
# 
# step("QC Started")
# Sys.sleep(3.6)
# step_done(show_time = FALSE)
# 
# Output:
# QC Started.................................................. ✓
# 
#
#
# Add duration of the step (default behavior):
#
# step("Generating QC report for 4 samples")
# Sys.sleep(12.3)
# step_done(show_time = TRUE)
# 
# Output:
# Generating QC report for 4 samples.......................... ✓ (12.5 s)




# ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ 
# ####  STATUS MESSAGES  ####
# ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄ 

# These are simple shortcuts for more intuitive cat() calls.
#
# They are useful for emphasizing important information without interrupting
# the flow of the analysis.
# 
# Coerce multiple arguments (strings, integers, vectors) into a string.


# ####  OK()  -----------------------------------------------------------------
#
# Successful completion
# --------------------------------------------------------------------------- #

ok <- function(...) {
  
  cat("✓ ", ..., "\n", sep = " ")
  
}

# ####  WARN()  ---------------------------------------------------------------
#
# Non-critical but important information
# (unlike native warning() command, it doesn't highlight in console)
# --------------------------------------------------------------------------- # 

warn <- function(...) {
  
  cat("⚠ ", ..., "\n", sep = " ")
  
}


# ####  Examples:  ------------------------------------------------------------ 
# 
# ok("Samples loaded")
#  
# warn("Removed", 4831, "low quality cells")
#  
# qc_pass_pct <- 12
# if (qc_pass_pct <= 20) {
#   warn("Less than 20% of cells passed QC.")
# } else {
#   fail("More than 20% of cells passed QC.")
# }
#
#
# Output:
# ✓  Samples loaded 
# ⚠  Removed 4831 low quality cells 
# ⚠  Less than 20% of cells passed QC.



# ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
# ####  FIXED WIDTH SUMMARY  ####
# ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
#
# Nicely aligned summaries.
#
# Width and fill character can be specified!
#

summary_line <- function(name,
                         value,
                         width = 30,
                         fill_char = ".") {
  
  
  fill_line <- strrep(
    fill_char,
    max(1, width - nchar(name))
  )
  
  cat(
    name, 
    " ",
    fill_line,
    " ",
    value,
    "\n",
    sep = ""
  )
  
}

# ####  Examples:  -------------------------------------------------------------
# 
# summary_line("Cells:", 98214)
# summary_line("Genes:", 18342)
# summary_line("Samples:", 58)
# 
# Output:
#
# Cells: ........................ 98214
# Genes: ........................ 18342
# Samples: ...................... 58



# ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
# ####  EXAMPLE PLAYGROUND  ####
# ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄ 

# Uncomment and play with these.
# 
# 
# section_1("Normalization")
# 
# section_2("Input")
# 
# step("Reading metadata")
# Sys.sleep(1.3)
# step_done()
# 
# ok("Metadata successfully loaded")
# 
# 
# step("Filtering cells")
# Sys.sleep(2)
# step_done()
# 
# warn("Removed 183 low-quality cells")
# 
# section_3("Summary")
# 
# summary_line("Cells", 98214)
# summary_line("Genes", 18342)
# summary_line("Samples", 58)
 


