# # source("renv/activate.R")
# 
# # .Rprofile for RStudio-like VS Code experience
# options(
#   repos = c(CRAN = "https://cloud.r-project.org/"),
#   browserNLdisabled = TRUE,
#   deparse.max.lines = 2,
#   max.print = 1000,
#   scipen = 999,
#   stringsAsFactors = FALSE,
#   
#   # Console appearance
#   width = 120,
#   prompt = "R> ",
#   continue = "+ ",
#   
#   # Graphics
#   device = "httpgd",
#   httpgd.host = "0.0.0.0",  # Important for Docker
#   httpgd.port = 0,
#   
#   # Error handling
#   error = function() {
#     calls <- sys.calls()
#     if (length(calls) >= 2L) {
#       sink(stderr())
#       on.exit(sink(NULL))
#       cat("Backtrace:\n")
#       calls <- rev(calls[-length(calls)])
#       for (i in seq_along(calls)) {
#         cat(format(i), ": ", deparse(calls[[i]], width.cutoff = 50L), 
#             "\n", sep = "")
#       }
#     }
#   }
# )
# 
# # VS Code specific initialization
# if (Sys.getenv("TERM_PROGRAM") == "vscode") {
#   # Load colorout for colored output (similar to RStudio)
#   if (require("colorout", quietly = TRUE)) {
#     colorout::setOutputColors256(
#       normal = 40,      # black
#       negnum = 209,     # orange  
#       zero = 226,       # yellow
#       number = 214,     # orange
#       date = 179,       # light brown
#       string = 85,      # light green
#       const = 35,       # green
#       false = 203,      # light red
#       true = 35,        # green
#       infinite = 39,    # blue
#       stderror = 213,   # pink
#       warn = c(1, 0, 1) # magenta
#     )
#   }
#   
#   # Auto-set console width
#   if (require("setwidth", quietly = TRUE)) {
#     setwidth::setwidth()
#   }
#   
#   # Enhanced prompt (optional)
#   if (require("prompt", quietly = TRUE)) {
#     prompt::set_prompt(prompt::prompt_memuse)
#   }
# }
# 
# # Startup message
# cat("R configured for VS Code in Docker environment\n")
# cat("Graphics device:", getOption("device"), "\n")
# cat("Working directory:", getwd(), "\n")
# 
# # Load frequently used packages silently
# invisible(suppressMessages(suppressWarnings({
#   library(graphics)
#   library(grDevices) 
#   library(stats)
#   library(utils)
# })))
# 
# # Custom functions for RStudio-like behavior
# View <- function(x, title = deparse(substitute(x))) {
#   if (require("jsonlite", quietly = TRUE)) {
#     # Create a simple data viewer
#     if (is.data.frame(x) || is.matrix(x)) {
#       cat("Data preview for:", title, "\n")
#       print(head(x, 10))
#       cat("Dimensions:", paste(dim(x), collapse = " x "), "\n")
#       if (is.data.frame(x)) {
#         cat("Column types:\n")
#         print(sapply(x, class))
#       }
#     } else {
#       print(x)
#     }
#   } else {
#     print(x)
#   }
# }
# 
# # Environment listing function
# ls.str <- function(...) {
#   objs <- ls(...)
#   if (length(objs) > 0) {
#     for (obj in objs) {
#       cat(obj, ":\n")
#       str(get(obj), max.level = 1)
#       cat("\n")
#     }
#   } else {
#     cat("No objects in environment\n")
#   }
# }
