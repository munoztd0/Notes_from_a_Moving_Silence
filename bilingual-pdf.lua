-- Convert the existing HTML bilingual blocks into facing PDF pages only.
local function has_class(element, class)
  for _, value in ipairs(element.classes) do
    if value == class then
      return true
    end
  end
  return false
end

local function split_title(header)
  local title = pandoc.utils.stringify(header.content)
  local english, japanese = title:match("^(.-)%s+%-%s+(.+)$")
  return english or title, japanese or title
end

local function latex_escape(text)
  local replacements = {
    ["\\"] = "\\textbackslash{}",
    ["{"] = "\\{",
    ["}"] = "\\}",
    ["#"] = "\\#",
    ["$"] = "\\$",
    ["%"] = "\\%",
    ["&"] = "\\&",
    ["_"] = "\\_",
    ["~"] = "\\textasciitilde{}",
    ["^"] = "\\textasciicircum{}",
  }
  return (text:gsub("[\\{}#$%%&_~^]", replacements))
end

function Pandoc(document)
  if not FORMAT:match("latex") then
    return document
  end

  local output = {}
  local block_index = 1
  while block_index <= #document.blocks do
    local block = document.blocks[block_index]
    local bilingual = document.blocks[block_index + 1]

    if block.t == "Header" and block.level == 1 and bilingual
      and bilingual.t == "Div" and has_class(bilingual, "bilingual") then
      local english_title, japanese_title = split_title(block)
      local english, japanese

      for _, column in ipairs(bilingual.content) do
        if column.t == "Div" and has_class(column, "en") then
          english = column.content
        elseif column.t == "Div" and has_class(column, "jp") then
          japanese = column.content
        end
      end

      if english and japanese then
        table.insert(output, pandoc.RawBlock("latex", "\\BilingualEnglishStart"))
        table.insert(output, pandoc.RawBlock("latex", "\\BilingualEnglishTitle{" .. latex_escape(english_title) .. "}"))
        for _, content_block in ipairs(english) do
          table.insert(output, content_block)
        end
        table.insert(output, pandoc.RawBlock("latex", "\\BilingualJapaneseStart"))
        table.insert(output, pandoc.RawBlock("latex", "\\BilingualJapaneseTitle{" .. latex_escape(japanese_title) .. "}"))
        for _, content_block in ipairs(japanese) do
          table.insert(output, content_block)
        end
        block_index = block_index + 2
      else
        table.insert(output, block)
        block_index = block_index + 1
      end
    else
      table.insert(output, block)
      block_index = block_index + 1
    end
  end

  document.blocks = output
  return document
end