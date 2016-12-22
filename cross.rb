# -*- coding: utf-8 -*-
NKF = "d:/home/nkf.exe -Ws"
SKK = "d:/home/.test-skk"

# "2=あ&3=い" => [[2, "あ"], [3, "い"]]
def _separate(string)
  string.split(/&/)
  .map {|term| term.split(/=/) }
  .map {|list| [list[0].to_i, list[1]] }
end

def make_predicate(header, char)
  if header == 100
    Proc.new {|str| str.length == char.to_i }
  elsif header == 200
    Proc.new {|str| str.include?(char) }
  else
    Proc.new {|str| str[header] == char }
  end
end
# [[0, "4"],[2, "あ"], [3, "い"]] から"全体が4文字で、2文字目が'あ'、3
# 文字目が'い'かどうか判定する関数"のリストを返す。
def make_predicate_list(string)
  _separate(string)
  .map {|pair| make_predicate(pair[0], pair[1])}
end

def matchP(string, predicates)
  predicates.reduce(true) { |bool, f|
    bool && f.call(string)
  }
end

def read_dictionary(predicates)
  File.open(SKK) {|f|
    f.each_line do |line|
      key = line.split(/ /)[0]
      puts line if matchP(key, predicates)
    end
  }
end

def main(arg)
  key = arg
  predicates = make_predicate_list(key)
  read_dictionary(predicates)
end

# main(ARGV[0])
