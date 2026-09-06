#!/usr/bin/env Rscript

root <- normalizePath(".")
build_dir <- tempfile("notes-pdf-build-")
output_dir <- file.path(root, "_book_pdf")
font_dir <- file.path(root, "fonts")
crimson_regular <- file.path(font_dir, "CrimsonPro[wght].ttf")
crimson_italic <- file.path(font_dir, "CrimsonPro[ital,wght].ttf")
chapters <- c(
  "index.qmd",
  "00. Notes from a Moving Silence.md",
  "02. Aluminium Nest.md",
  "06. Sunny Coral.md",
  "04. Bon Voyage Chef!.md",
  "05. Closing time.md",
  "07. The Sea - Umi.md",
  "07b. The Smell of Wet Towels.md",
  "07c. Matsuri season.md",
  "01. Warabya Kids.md",
  "01a. Morino-san.md",
  "01c. Kobayashi-san.md",
  "08. Aka jima.md",
  "09. Yonaguni.md",
  "10. Myazaki.md",
  "11. Naha.md"#,
  #"XX. Misc.md"
)

xelatex <- Sys.which("xelatex")
if (!nzchar(xelatex)) {
  tinytex_xelatex <- file.path(path.expand("~"), ".TinyTeX", "bin", "x86_64-linux", "xelatex")
  if (file.exists(tinytex_xelatex)) xelatex <- tinytex_xelatex
}

required_commands <- c("quarto", "mutool", "pdfinfo")
missing_commands <- required_commands[!nzchar(Sys.which(required_commands))]
if (length(missing_commands) || !nzchar(xelatex)) {
  missing <- c(missing_commands, if (!nzchar(xelatex)) "xelatex")
  stop("Missing required command(s): ", paste(missing, collapse = ", "))
}
if (!file.exists(crimson_regular) || !file.exists(crimson_italic)) {
  stop("Missing bundled Crimson Pro font files in ", font_dir)
}

dir.create(build_dir, recursive = TRUE)
on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

escape_yaml <- function(path) {
  gsub("\\\\", "\\\\\\\\", gsub("\"", "\\\\\"", path))
}

split_title <- function(title) {
  parts <- regexec("^(.*?)\\s+-\\s+(.*)$", title, perl = TRUE)
  match <- regmatches(title, parts)[[1]]
  if (length(match) == 3) match[2:3] else c(title, title)
}

extract_language <- function(path, language) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  title <- sub("^#\\s+", "", lines[grep("^#\\s+", lines)[1]])
  language_title <- split_title(title)[if (language == "en") 1 else 2]
  start_marker <- sprintf('<div class="col %s">', language)
  start <- which(lines == start_marker)[1]
  if (is.na(start)) stop("Cannot find ", language, " content in ", path)
  end <- which(lines[(start + 1):length(lines)] == "</div>")[1] + start
  if (is.na(end)) stop("Cannot find closing language block in ", path)
  list(title = language_title, content = lines[(start + 1):(end - 1)])
}

write_chapter <- function(chapter, language, index) {
  extracted <- extract_language(file.path(root, chapter), language)
  language_dir <- file.path(build_dir, language)
  dir.create(language_dir, recursive = TRUE, showWarnings = FALSE)
  source_path <- file.path(language_dir, sprintf("%02d.qmd", index))
  preamble <- escape_yaml(file.path(root, "preamble.tex"))
  font_header <- file.path(language_dir, "english-font.tex")
  if (language == "en" && !file.exists(font_header)) {
    writeLines(c(
      "\\setmainfont{CrimsonPro[wght].ttf}[",
      sprintf("  Path=%s/,", font_dir),
      "  ItalicFont=CrimsonPro[ital,wght].ttf",
      "]"
    ), font_header, useBytes = TRUE)
  }
  yaml <- c(
    "format:",
    "  pdf:",
    "    documentclass: scrbook",
    "    pdf-engine: xelatex",
    sprintf("    mainfont: %s", if (language == "en") "Crimson Pro" else "Noto Serif CJK JP"),
    "    classoption: twoside,openany",
    "    titlepage: false",
    "    toc: false",
    "    number-sections: false",
    "    include-in-header:",
    sprintf("      - \"%s\"", preamble),
    if (language == "en") sprintf("      - \"%s\"", escape_yaml(font_header)),
    "    geometry: inner=25mm,outer=20mm,top=25mm,bottom=25mm",
    "---",
    ""
  )
  writeLines(c("---", yaml, paste0("# ", extracted$title), "", extracted$content), source_path, useBytes = TRUE)
  source_path
}

render_chapter <- function(source_path) {
  status <- system2("quarto", c("render", source_path, "--to", "pdf"))
  if (status != 0) stop("Quarto failed to render ", source_path)
  sub("\\.qmd$", ".pdf", source_path)
}

page_count <- function(pdf_path) {
  output <- system2("pdfinfo", pdf_path, stdout = TRUE, stderr = TRUE)
  as.integer(sub("^Pages:\\s+", "", output[grep("^Pages:", output)]))
}

blank_pdf <- file.path(build_dir, "blank.pdf")
blank_tex <- file.path(build_dir, "blank.tex")
writeLines(c(
  "\\documentclass[letterpaper]{article}",
  "\\usepackage[margin=0pt]{geometry}",
  "\\pagestyle{empty}",
  "\\begin{document}",
  "\\null",
  "\\end{document}"
), blank_tex)
if (system2(xelatex, c("-interaction=batchmode", "-output-directory", build_dir, blank_tex)) != 0) {
  stop("Could not create a blank PDF page")
}

merged_chapters <- character(length(chapters))
for (index in seq_along(chapters)) {
  message(sprintf("[%d/%d] %s", index, length(chapters), chapters[index]))
  english_pdf <- render_chapter(write_chapter(chapters[index], "en", index))
  japanese_pdf <- render_chapter(write_chapter(chapters[index], "jp", index))
  english_pages <- page_count(english_pdf)
  japanese_pages <- page_count(japanese_pdf)
  arguments <- c("merge", "-o", file.path(build_dir, sprintf("%02d.pdf", index)))
  for (page in seq_len(max(english_pages, japanese_pages))) {
    arguments <- c(arguments,
      if (page <= english_pages) c(english_pdf, page) else blank_pdf,
      if (page <= japanese_pages) c(japanese_pdf, page) else blank_pdf
    )
  }
  if (system2("mutool", arguments) != 0) stop("Could not merge ", chapters[index])
  merged_chapters[index] <- file.path(build_dir, sprintf("%02d.pdf", index))
}

book_pdf <- file.path(output_dir, "Notes-from-a-Moving-Silence.pdf")
if (system2("mutool", c("merge", "-o", book_pdf, merged_chapters)) != 0) {
  stop("Could not assemble the book PDF")
}
message("Created ", book_pdf)