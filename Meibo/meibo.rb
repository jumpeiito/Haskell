# coding: utf-8
require 'win32ole'
require 'pp'

Bunkai = { 1 => "石田",
           2 => "日野",
           3 => "小栗栖",
           4 => "一言寺",
           5 => "三宝院",
           6 => "点在"
         }

INITFILE = "s:/馬場フォルダ/組合員名簿/組合員名簿.xlsm"
TESTFILE = "f:/組合員名簿/組合員名簿.xlsm"
TEXTFILE = "c:/Users/Jumpei/Haskell/Meibo/.meibo"

def file()
  File.exists?(INITFILE) ? INITFILE : TESTFILE
end

def float_to_int(line)
  line.map {|el| case el
                 when Float; el.to_i
                 else el
                 end }
end

def filter_blank(lines)
  lines.keep_if {|line| line.compact != [] }
end

def with_excel()
  begin
    ex = WIN32OLE.new('Excel.Application')
    ex.Visible = false;
    ex.DisplayAlerts = false;
    bk = ex.Workbooks.Open(file())
    yield(bk)
  ensure
    bk.Close()
    ex.Quit()
  end
end

def excel_contents 
  ary = Array.new
  with_excel {|book|
    1.upto(6) {|index|
      sheet = book.Worksheets.Item(index)
      value = filter_blank(sheet.Range("A6:J300").Value)
      lines = value.map {|line| [Bunkai[index]] + line }
        .map {|line| float_to_int(line) }
        .map {|line| line.join(",") }
      ary   = ary + lines 
    }
  }
  return ary.join("\n").encode('utf-8')
end

def textfile_writable_p
  not(File.exists?(TEXTFILE)) ||
    File.size(TEXTFILE) == 0  ||
    File.mtime(TEXTFILE) < File.mtime(file())
end

def write_textfile()
  File.open(TEXTFILE, 'w') {|f|
    # excel_contents().each {|line| f.puts(line) }
    f.puts excel_contents()
  }
end

write_textfile() if textfile_writable_p
# puts File.read(TEXTFILE)
# File.readlines(TEXTFILE).each {|line| puts line }
puts ""
