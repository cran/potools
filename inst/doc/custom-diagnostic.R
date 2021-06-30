## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----check_spelling-----------------------------------------------------------
check_spelling = function(message_data) {
  # if aspell isn't installed, this won't work; be sure to return an object with the right schema anyway
  if (!nzchar(Sys.which("aspell"))) {
    warning("'aspell' is not installed; returning nothing")
    return(message_data[0, .(call, file, line_number)])
  }

  # aspell() works on files, so we'll write the msgid to files
  aspell_dir <- file.path(tempdir(), 'aspell')
  dir.create(aspell_dir)
  original_dir <- setwd(aspell_dir)
  on.exit({unlink(aspell_dir, recursive = TRUE); setwd(original_dir)})
  
  # (!is_repeat) makes sure we only check duplicate messages once
  # plural messages are in a list, so handle them separately
  message_data[(!is_repeat), by = .(file, type), {
    if (.BY$type == "singular") {
      cat(msgid, file = .BY$file, sep = "\n")
      # aspell() results has 5 columns: Original, File, Line, Column, Suggestions; we only need 1 & 5
      results = utils::aspell(.BY$file)
      unlink(.BY$file)
      
      typo_idx <- sapply(results$Original, grep, msgid)
      # take the first suggestion
      replacement = sapply(
        seq_along(results$Suggestions),
        function(typo_i) {
          # take the identified typo & replace it with aspell's 1st suggestion in the original `call`
          gsub(
            results$Original[typo_i], results$Suggestions[[typo_i]][1L],
            call[typo_idx[typo_i]], fixed = TRUE
          )
        }
      )
      
      .(
        call = call[typo_idx],
        file = file[typo_idx],
        line_number = line_number[typo_idx],
        replacement = replacement
      )
    } else {
      # unlist() to write both the n=1 and n!=1 messages to the file side-by-side
      all_msgid <- unlist(msgid_plural)
      cat(all_msgid, file = .BY$file, sep = "\n")
      results = utils::aspell(.BY$file)
      unlink(.BY$file)
      
      # odd numbers in grep output --> first entry for each plural_msgid; even numbers --> second entry.
      # do this arithmetic trick to re-map that to the original entry number in msgid_plural
      typo_idx <- ((sapply(results$Original, grep, all_msgid) - 1L) %/% 2L) + 1L
      # potentially overwrite each call >1 time if both messages have a typo
      replacement = call
      for (typo_i in seq_along(results$Suggestions)) {
        replacement[typo_idx[typo_i]] <- gsub(
          results$Original[typo_i], results$Suggestions[[typo_i]][1L],
          replacement[typo_idx[typo_i]], fixed = TRUE
        )
      }
      typo_idx <- unique(typo_idx)

      .(
        call = call[typo_idx],
        file = file[typo_idx],
        line_number = line_number[typo_idx],
        replacement = replacement[typo_idx]
      )
    }
  }]
}

## ----GreatSpelling------------------------------------------------------------
library(potools)
great_spelling_messages = get_message_data("GreatSpelling")

# showing the structure of the messagedata for this package
great_spelling_messages

# running our diagnostic
check_spelling(great_spelling_messages)

