# coding: cp932
require 'win32ole'
require 'pp'

bunkai = { 1 => "�Γc",
           2 => "����",
           3 => "���I��",
           4 => "�ꌾ��",
           5 => "�O��@",
           6 => "�_��"
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
  lines.keep_if {|line| line.compact != [] }
end

begin
  ex = WIN32OLE.new('Excel.Application')
  ex.Visible = false;
  ex.DisplayAlerts = false;
  # if File.exists?("s:/�n��t�H���_/�g��������/�g��������.xlsm")
  #   bk = ex.Workbooks.Open("s:/�n��t�H���_/�g��������/�g��������.xlsm")
  # else
  #   bk = ex.Workbooks.Open("f:/�g��������/�g��������.xlsm")
  # end
  bk = ex.Workbooks.Open("f:/�g��������.xlsm")
  ary = Array.new
  1.upto(6) do |i|
    sh = bk.Worksheets.Item(i)
    val = filter_blank sh.Range("A6:J300").Value
    ary = ary + val.map {|line| [bunkai[i]] + line }
  end

  ary.each do |lines|
    lines = float_to_int lines
    puts lines.join(",")
  end

ensure
  bk.Close()
  ex.Quit()
end
