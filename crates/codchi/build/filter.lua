local logging = require 'build.logging'

local text = pandoc.text

local inside_name_section = 0
function Header(el) 
  if el.level == 2 then
    el = el:walk {
      Str = function(el)
          return pandoc.Str(text.upper(text.sub(el.text, 1, 1)) .. text.lower(text.sub(el.text, 2)))
      end
    }
    if text.lower(pandoc.utils.stringify(el.content[1])) == "name" then
      inside_name_section = 1
      el = {}
    end
    return el
  end
end

function Para(el)
  if inside_name_section == 1 then
    inside_name_section = 2

    local text = el.content[1].text:gsub("%-", " ")
    local header = pandoc.Header(1, pandoc.Str(text))

    for _ = 1, 4 do
      table.remove(el.content, 1)
    end
    return { header, el }
  end
end

function DefinitionList(el)
  local result = {}
  for _, item in pairs(el.content) do
    local content = {}

    local heading = {}
    for _, inline in ipairs(item[1]) do
      table.insert(heading, inline)
    end
    table.insert(content, pandoc.Para(heading))

    for _, block in ipairs(item[2][1]) do
      table.insert(content, pandoc.Para(block.content))
    end

    table.insert(result, content)
  end
  return pandoc.BulletList(result)
end

function Pandoc(pandoc)
    -- logging.warning('pandoc', pandoc)
end
