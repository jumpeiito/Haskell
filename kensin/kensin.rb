# coding: cp932
require 'win32ole'
require 'pp'

# FILE = "f:/Haskell/kensin/16春の健診受付名簿.xlsx"
FILE = ARGV[0]
SHEET_INDEX = [1,2]

bunkai = { 1 => "石田",
           2 => "日野",
           3 => "小栗栖",
           4 => "一言寺",
           5 => "三宝院",
           6 => "点在"
         }

def float_to_int line
  line.map {|i|
    if i.kind_of?(Float)
    then i.to_i
    else i
    end
  }
end

def filter_blank lines
  lines.keep_if {|line| line[0] }
end

begin
  ex = WIN32OLE.new('Excel.Application')
  ex.Visible = false;
  ex.DisplayAlerts = false;

  bk = ex.Workbooks.Open(FILE)
  ary = Array.new
  SHEET_INDEX.each do |sh_index|
    sh = bk.Worksheets.Item(sh_index)
    val = filter_blank sh.Range("B2:AC2000").Value
    ary = ary + val
  end

  ary.each do |lines|
    lines = float_to_int lines
    puts lines.join (",")
  end

ensure
  bk.Close()
  ex.Quit()
end
