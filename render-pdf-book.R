#!/usr/bin/env Rscript

# PDF typography (points)
body_size <- 14
title_size <- 24
section_title_size <- 18
subtitle_size <- 16
page_number_size <- 11
margin_cm <- 2
line_spacing <- 1.15
paragraph_spacing_pt <- 6
first_line_indent_cm <- 0.5
opening_body_offset_cm <- 5

root <- normalizePath(".")
output_dir <- file.path(root, "_book_pdf")
build_dir <- tempfile("notes-pdf-build-")
font_dir <- file.path(root, "fonts")
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
  "11. Naha.md"
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
if (!file.exists(file.path(font_dir, "CrimsonPro-Regular.ttf")) ||
    !file.exists(file.path(font_dir, "CrimsonPro-Italic.ttf"))) {
  stop("Missing bundled Crimson Pro font files in ", font_dir)
}

dir.create(build_dir, recursive = TRUE)
on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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
    #if (language == "jp") stop("Cannot find Japanese title in ", path)
    title <- document_title
  } else {
    title <- sub("^#\\s+", "", content[heading])
    content <- content[-heading]
  }
  list(title = title, content = content)
}

write_chapter <- function(chapter, language, index) {
  extracted <- extract_language(file.path(root, chapter), language)
  language_dir <- file.path(build_dir, sprintf("%02d", index), language)
  dir.create(language_dir, recursive = TRUE)
  layout_header <- file.path(language_dir, "layout.tex")
  writeLines(c(
    "\\usepackage{titlesec}",
    "\\usepackage{setspace}",
    "\\newfontfamily\\HeaderFont{Noto Serif CJK JP}",
    sprintf("\\fontsize{%d}{%d}\\selectfont", body_size, ceiling(body_size * 1.4)),
    sprintf("\\setstretch{%.2f}", line_spacing),
    sprintf("\\setlength{\\parskip}{%dpt}", paragraph_spacing_pt),
    sprintf("\\setlength{\\parindent}{%.1fcm}", first_line_indent_cm),
    "\\raggedright",
    sprintf("\\titleformat{\\section}[block]{\\centering\\HeaderFont\\bfseries\\fontsize{%d}{%d}\\selectfont}{}{0pt}{}[\\vspace{%dcm}]", title_size, ceiling(title_size * 1.2), opening_body_offset_cm),
    sprintf("\\titleformat{\\subsection}[block]{\\HeaderFont\\bfseries\\fontsize{%d}{%d}\\selectfont}{}{0pt}{}", section_title_size, ceiling(section_title_size * 1.2)),
    sprintf("\\titleformat{\\subsubsection}[block]{\\HeaderFont\\bfseries\\fontsize{%d}{%d}\\selectfont}{}{0pt}{}", subtitle_size, ceiling(subtitle_size * 1.2)),
    sprintf("\\fancyfoot[LE,RO]{\\fontsize{%d}{%d}\\selectfont\\thepage}", page_number_size, ceiling(page_number_size * 1.2)),
    sprintf("\\fancypagestyle{plain}{\\fancyhf{}\\fancyfoot[LE,RO]{\\fontsize{%d}{%d}\\selectfont\\thepage}\\renewcommand{\\headrulewidth}{0pt}}", page_number_size, ceiling(page_number_size * 1.2))
  ), layout_header)
  font_header <- file.path(language_dir, "english-font.tex")
  if (language == "en") {
    writeLines(c(
      "\\setmainfont{CrimsonPro-Regular.ttf}[",
      sprintf("  Path=%s/,", font_dir),
      "  ItalicFont=CrimsonPro-Italic.ttf",
      "]"
    ), font_header)
  }

  source_path <- file.path(language_dir, "chapter.qmd")
  writeLines(c(
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
    sprintf("      - \"%s\"", escape_yaml(layout_header)),
    sprintf("    geometry: margin=%dcm", margin_cm),
    "---",
    "",
    paste0("# ", extracted$title),
    "",
    extracted$content
  ), source_path, useBytes = TRUE)
  source_path
}

render_chapter <- function(source_path) {
  if (system2("quarto", c("render", source_path, "--to", "pdf")) != 0) {
    stop("Quarto failed to render ", source_path)
  }
  sub("\\.qmd$", ".pdf", source_path)
}

page_count <- function(pdf_path) {
  output <- system2("pdfinfo", pdf_path, stdout = TRUE, stderr = TRUE)
  as.integer(sub("^Pages:\\s+", "", output[grep("^Pages:", output)]))
}

blank_tex <- file.path(build_dir, "blank.tex")
blank_pdf <- file.path(build_dir, "blank.pdf")
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
  chapter <- chapters[index]
  message(sprintf("[%d/%d] %s", index, length(chapters), chapter))
  english_pdf <- render_chapter(write_chapter(chapter, "en", index))
  japanese_pdf <- render_chapter(write_chapter(chapter, "jp", index))
  english_pages <- page_count(english_pdf)
  japanese_pages <- page_count(japanese_pdf)
  merged_pdf <- file.path(build_dir, sprintf("%02d.pdf", index))
  arguments <- c("merge", "-o", merged_pdf)
  for (page in seq_len(max(english_pages, japanese_pages))) {
    arguments <- c(arguments,
      if (page <= english_pages) c(english_pdf, page) else blank_pdf,
      if (page <= japanese_pages) c(japanese_pdf, page) else blank_pdf
    )
  }
  if (system2("mutool", arguments) != 0) stop("Could not merge ", chapter)
  merged_chapters[index] <- merged_pdf
}

book_pdf <- file.path(output_dir, "Notes-from-a-Moving-Silence.pdf")
if (system2("mutool", c("merge", "-o", book_pdf, merged_chapters)) != 0) {
  stop("Could not assemble the book PDF")
}
message("Created ", book_pdf)