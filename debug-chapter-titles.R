#!/usr/bin/env Rscript

root <- normalizePath(".")
chapter <- "00. Notes from a Moving Silence.md"
build_dir <- tempfile("notes-title-debug-")
font_dir <- file.path(root, "fonts")
crimson_regular <- file.path(font_dir, "CrimsonPro-Regular.ttf")
crimson_italic <- file.path(font_dir, "CrimsonPro-Italic.ttf")

escape_yaml <- function(path) {
  gsub("\\\\", "\\\\\\\\", gsub("\"", "\\\\\"", path))
}

extract_language <- function(path, language) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  document_title <- sub("^#\\s+", "", lines[grep("^#\\s+", lines)[1]])
  start <- which(lines == sprintf('<div class="col %s">', language))[1]
  if (is.na(start)) stop("Cannot find ", language, " content in ", path)
  end <- which(lines[(start + 1):length(lines)] == "</div>")[1] + start
  if (is.na(end)) stop("Cannot find closing language block in ", path)

  content <- lines[(start + 1):(end - 1)]
  heading <- grep("^#\\s+", content)[1]
  if (is.na(heading)) {
    if (language == "jp") stop("Cannot find Japanese title in ", path)
    title <- document_title
  } else {
    title <- sub("^#\\s+", "", content[heading])
    content <- content[-heading]
  }
  list(title = title, content = content)
}

write_debug_document <- function(language) {
  extracted <- extract_language(file.path(root, chapter), language)
  language_dir <- file.path(build_dir, language)
  dir.create(language_dir, recursive = TRUE)
  font_header <- file.path(language_dir, "english-font.tex")
  if (language == "en") {
    writeLines(c(
      "\\setmainfont{CrimsonPro-Regular.ttf}[",
      sprintf("  Path=%s/,", font_dir),
      "  ItalicFont=CrimsonPro-Italic.ttf",
      "]"
    ), font_header)
  }

  yaml <- c(
    "---",
    "format:",
    "  pdf:",
    "    pdf-engine: xelatex",
    "    mainfont: Noto Serif CJK JP",
    "    documentclass: article",
    "    titlepage: false",
    "    toc: false",
    "    number-sections: false",
    "    include-in-header:",
    sprintf("      - \"%s\"", escape_yaml(file.path(root, "preamble.tex"))),
    if (language == "en") sprintf("      - \"%s\"", escape_yaml(font_header)),
    "    geometry: margin=25mm",
    "---",
    "",
    paste0("# ", extracted$title),
    "",
    extracted$content
  )
  source_path <- file.path(language_dir, "chapter.qmd")
  writeLines(yaml, source_path, useBytes = TRUE)
  message("\n", toupper(language), " title: ", extracted$title)
  message("Source: ", source_path)
  source_path
}

dir.create(build_dir, recursive = TRUE)
message("Debug output: ", build_dir)

for (language in c("en", "jp")) {
  source_path <- write_debug_document(language)
  status <- system2("quarto", c("render", source_path, "--to", "pdf"))
  if (status != 0) stop("Render failed for ", language)
  pdf_path <- sub("\\.qmd$", ".pdf", source_path)
  message("PDF: ", pdf_path)
  system2("pdftotext", c("-f", "1", "-l", "1", "-layout", pdf_path, "-"))
}