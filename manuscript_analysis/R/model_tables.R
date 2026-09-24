# Multi-model coefficient tables that render in both HTML and Word.
#
# These replace sjPlot::tab_model(), which emits HTML only. Raw HTML survives
# an HTML render but is stripped when pandoc converts to docx, leaving the
# caption behind and spilling the coefficients into the surrounding prose as
# an unformatted run of digits. Writing a plain data frame and rendering it
# with knitr::kable() works in every output format.
#
# Layout is one column per model holding "b [95% CI]" with significance stars,
# rather than sjPlot's estimate/CI/p triple per model. With three models the
# triple form needs ten columns, which is unreadable at Word page width. Exact
# p values for the focal terms are reported in the Results prose.

star <- function(p) {
  if (is.na(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}

# models : named list of lm objects; names become the column headers
# labels : named character vector, term name -> display label, in display order
format_model_table <- function(models, labels, digits = 2) {
  fmt <- function(x) formatC(x, format = "f", digits = digits)

  # Estimate and interval share one cell as "b [LL, UL]". sjPlot's separate CI
  # column renders as "-3.350 - -0.100" for negative bounds, which is hard to
  # read. p keeps its own column, with the leading zero dropped per APA 7.
  cell <- function(model, term) {
    co <- summary(model)$coefficients
    if (!term %in% rownames(co)) return("--")
    ci <- suppressMessages(stats::confint(model)[term, ])
    paste0(fmt(co[term, "Estimate"]), " [", fmt(ci[1]), ", ", fmt(ci[2]), "]")
  }
  p_cell <- function(model, term) {
    co <- summary(model)$coefficients
    if (!term %in% rownames(co)) return("--")
    p <- co[term, "Pr(>|t|)"]
    if (p < .001) "<.001" else sub("^0", "", formatC(p, format = "f", digits = 3))
  }

  body <- data.frame(Predictor = unname(labels), stringsAsFactors = FALSE)
  for (nm in names(models)) {
    body[[paste0(nm, "__b [95% CI]")]] <-
      vapply(names(labels), function(t) cell(models[[nm]], t), character(1))
    body[[paste0(nm, "__p")]] <-
      vapply(names(labels), function(t) p_cell(models[[nm]], t), character(1))
  }

  # Fit statistics, appended as labelled rows so the table stays a single
  # rectangular object that kable can render without extra packing.
  fit_row <- function(label, f) {
    row <- data.frame(Predictor = label, stringsAsFactors = FALSE)
    for (nm in names(models)) {
      row[[paste0(nm, "__b [95% CI]")]] <- f(models[[nm]])
      row[[paste0(nm, "__p")]] <- ""
    }
    row
  }

  rbind(
    body,
    fit_row("Observations", function(m) as.character(stats::nobs(m))),
    fit_row("R2", function(m) sub("^0", "", fmt(summary(m)$r.squared))),
    fit_row("Adjusted R2", function(m) sub("^0", "", fmt(summary(m)$adj.r.squared))),
    fit_row("AIC", function(m) fmt(stats::AIC(m)))
  )
}

# Convert a formatted model-table data frame to a gt table and bold the
# coefficient/CI and p-value cells for statistically significant coefficients.
# Predictor names, nonsignificant model cells, and fit statistics remain
# unbolded.
style_model_table <- function(model_table, alpha = .05) {
  p_columns <- grepl("__p$", names(model_table))
  stopifnot(any(p_columns))

  parse_p <- function(x) {
    if (is.na(x) || x %in% c("", "--")) return(NA_real_)
    suppressWarnings(as.numeric(sub("^<", "", x)))
  }
  table_gt <- gt::tab_spanner_delim(gt::gt(model_table), delim = "__")
  for (column_index in which(p_columns)) {
    p_values <- vapply(model_table[[column_index]], parse_p, numeric(1))
    significant_rows <- which(!is.na(p_values) & p_values < alpha)
    if (length(significant_rows) > 0L) {
      table_gt <- gt::tab_style(
        table_gt,
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
          columns = c(column_index - 1L, column_index),
          rows = significant_rows
        )
      )
    }
  }
  table_gt
}


# APA 7: omit the leading zero only where a value cannot exceed 1 -- p values
# and R-squared -- while estimates, confidence bounds and AIC keep theirs.
# sjPlot has no option for this, so the emitted HTML is post-processed here.
# Columns are located by reading the header row rather than by fixed index, so
# the function survives changes to show.ci / show.stat in the tab_model call.
# NB: sjPlot puts each cell on its own line, so every pattern here needs the
# (?s) dotall flag -- R's `.` does not match newlines even with perl = TRUE.
strip_apa_leading_zeros <- function(path) {
  lines <- readLines(path, warn = FALSE)

  # Content of a cell line, tags stripped.
  content <- function(x) sub("(?s)^<t[hd][^>]*>(.*)</t[hd]>\\s*$", "\\1", x, perl = TRUE)
  is_cell <- function(x) grepl("^\\s*<t[hd][ >]", x)
  is_row  <- function(x) grepl("<tr>", x, fixed = TRUE)

  # Pass 1: find which column indices hold p values, from the header row.
  p_cols <- integer(0)
  col <- 0L
  in_header <- FALSE
  for (ln in lines) {
    if (is_row(ln)) { col <- 0L; in_header <- FALSE }
    if (!is_cell(ln)) next
    col <- col + 1L
    txt <- content(ln)
    if (identical(txt, "Predictors")) in_header <- TRUE
    if (in_header && identical(txt, "p")) p_cols <- c(p_cols, col)
  }
  stopifnot(length(p_cols) > 0L)

  # Pass 2: strip the leading zero in those columns, and in every value cell of
  # the R-squared row. Only the cell's text is touched, never its style
  # attribute, which contains lengths such as "0.2em".
  col <- 0L
  in_r2 <- FALSE
  for (i in seq_along(lines)) {
    if (is_row(lines[i])) { col <- 0L; in_r2 <- FALSE }
    if (!is_cell(lines[i])) next
    col <- col + 1L
    txt <- content(lines[i])
    # The label is markup: "R<sup>2</sup> / R<sup>2</sup> adjusted".
    if (col == 1L) in_r2 <- grepl("^R2 ", gsub("<[^>]+>", "", txt))
    if (!(col %in% p_cols || (in_r2 && col > 1L))) next
    stripped <- gsub("(^|[^0-9])0\\.", "\\1.", txt)
    if (!identical(stripped, txt)) {
      lines[i] <- sub(txt, stripped, lines[i], fixed = TRUE)
    }
  }

  writeLines(lines, path)
  invisible(path)
}


# gt::as_word() emits an empty <w:tblGrid/>, and gt's HTML colgroup declares a
# single column; either way pandoc/Word sees a grid that disagrees with the
# number of cells per row, which is what makes Word report "unreadable content".
# This injects a grid matching the widest row (counting gridSpan), so the table
# validates. Page width is 9360 twips (letter minus 1in margins).
gt_as_word <- function(gt_tbl, page_width = 9360L) {
  ooxml <- gt::as_word(gt_tbl)

  rows <- regmatches(ooxml, gregexpr("(?s)<w:tr>.*?</w:tr>", ooxml, perl = TRUE))[[1]]
  width_of <- function(row) {
    cells <- regmatches(row, gregexpr("(?s)<w:tc>.*?</w:tc>", row, perl = TRUE))[[1]]
    sum(vapply(cells, function(cell) {
      m <- regmatches(cell, regexpr('w:gridSpan w:val="\\d+"', cell, perl = TRUE))
      if (length(m)) as.integer(gsub("\\D", "", m)) else 1L
    }, integer(1)))
  }
  n_col <- max(vapply(rows, width_of, integer(1)))
  stopifnot(n_col >= 1L)

  col_w <- floor(page_width / n_col)
  grid <- paste0(
    "<w:tblGrid>",
    paste(sprintf('<w:gridCol w:w="%d"/>', rep(col_w, n_col)), collapse = ""),
    "</w:tblGrid>"
  )
  # gt omits <w:tblGrid> entirely, so insert it where the schema requires it:
  # immediately after </w:tblPr>. (If a future gt emits one, replace instead.)
  if (grepl("<w:tblGrid", ooxml, fixed = TRUE)) {
    sub("(?s)<w:tblGrid>.*?</w:tblGrid>|<w:tblGrid\\s*/>", grid, ooxml, perl = TRUE)
  } else {
    sub("</w:tblPr>", paste0("</w:tblPr>", grid), ooxml, fixed = TRUE)
  }
}
