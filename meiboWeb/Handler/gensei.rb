# -*- coding: utf-8 -*-

require 'win32ole'
require 'pp'
require 'date'


Bunkai = { 1 => "石",
           2 => "日",
           3 => "小",
           4 => "一",
           5 => "三",
           6 => "点"
}
BunkaiLong = { 1 => "石田",
  2 => "日野",
  3 => "小栗栖",
  4 => "一言寺",
  5 => "三宝院",
  6 => "点在"
}

VerseBunkai = Bunkai.invert

Visible = true
Alerts  = false

class Integer
  def toSheetIndex
    return self.to_s + "月"
  end
end

class Date
  def toJP
    return "#{self.year}年#{self.month}月度"
  end
end

class String
  def toCode
    regexp = /([石日小一三点])([①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳㉑㉒])/
    if self =~ regexp
      match  = self.match(regexp)
      bunkai = VerseBunkai[match[1]]
      maru   = match[2].ord
      han    = maru - 9311
      return bunkai * 100 + han
    else
      return 0
    end
  end
end

class Array
  def ranking()
    self.inject(Hash.new(0)) {|r, i| r[i] += 1; r}
  end
end

module Personal
  class Personal::Person
    def initialize(dummy)
      @name    = ""
      @reason  = ""
      @belongs = 0
      @bunkai  = ""
      @han     = ""
    end
    private
    def regularize(val)
      case val
      when Float;  val.to_i.to_s
      when String; val.gsub(/\+/, ",")
      end
    end
  end

  class Enter < Personal::Person
    attr_reader :kind
    attr_reader :belongs
    attr_reader :work
    def initialize(ary)
      @kind       = ary[0].to_s
      @month1     = ary[1].to_i
      @month2     = ary[2].to_i
      @type       = ary[3]
      @name       = ary[4]
      @work       = ary[5]
      @reason     = regularize(ary[6])
      @motivation = regularize(ary[7])
      @belongs    = ary[8].to_s.toCode
      @bunkai     = BunkaiLong[@belongs / 100]
      @han        = @belongs.modulo(100).to_s
    end
    def value
      type = @type == "〆切後" ? "〆切後" : ""
      return [@name, "", "", @bunkai, @han, @reason, @motivation, type]
    end
  end

  class Leave < Personal::Person
    attr_reader :month
    def initialize(ary)
      @name, _, _, @bunkai, @reason, @lastmonth, month = ary
      @month = month.to_i
    end
    def value
      return [@name, "", "", @bunkai, @reason, @lastmonth]
    end
  end

  class Danketsu < Personal::Person
    def initialize(ary)
      @month1, @month2, @name, @bunkai, @matchmaker = ary
    end
    def value
      return [@name, @bunkai, @matchmaker, @month2]
    end
  end
end

module Excel
  class Sheet
    attr_reader :file
    attr_reader :this_month
    def initialize ()
      @file       = ""
      @this_month = Date.today.month
      @data       = { :sheet => "", :range => [] }
      @output_range = ""
    end
    # protected
    def init_fetch(app)
      filter(self.get(app)[0])
    end
    def init_person(app)
      self.init_fetch(app).map(&:value)
    end
    def filter(list)              # to be overlapped.
      return list
    end
    def get(app)
      sheetName, range = @data.values
      with_excel(app, sheetName) {|book, sheet|
        return range.map {|r| sheet.Range(r).Value }
      }
    end
    def put(app, sheetName, range, value)
      with_excel(app, sheetName) {|book, sheet|
        sheet.Range(range).Select
        sheet.Range(range).Value = value
        book.Save();
      }
    end
    def output_range(value)
      endrow = 13 + value.length - 1
      return "#{@output_range}#{endrow}"
    end
    private
    def with_excel(app, sheetName)
      begin
        book = app.Workbooks.Open(@file)
        sheet = book.Worksheets.Item(sheetName)
        yield(book, sheet)
      ensure
        book.Close()
      end
    end
  end
  module Init
    FILE = "s:/馬場フォルダ/新加入/2016年度加入手続状況一覧.xlsx"
    # FILE = "f:/2016年度加入手続状況一覧.xlsx"
    class Enter < Excel::Sheet
      def initialize()
        super()
        @file = Excel::Init::FILE
        @data = {:sheet => "加入", :range => ["B3:AE138"] }
        @output_range = "G13:N"
        @column = [0, 2, 3, 4, 7, 9, 11, 12, 17]
      end
      def filter(list)
        list
          .keep_if {|row| row[2].to_i == @this_month }                      # 今月分のデータだけ抜き出し
          .map     {|row| Personal::Enter.new(@column.map {|col| row[col]})}     # オブジェクトの作成
          .keep_if {|eperson| eperson.kind != "団結" }                      # 団結を除外
          .sort    {|x, y| x.belongs <=> y.belongs }                        # 分会・班で並べ替え
      end
      def work_output(app)
        work_rank(app)
        # self.put(app, "加入台帳", "AH2:AI#{2 + list.length - 1}", list)
      end
      def work_filter(list)
        list
          .map     {|row| Personal::Enter.new(@column.map {|col| row[col]})}     # オブジェクトの作成
          .keep_if {|eperson| eperson.kind != "団結" && eperson.kind != "既労" }                      # 団結を除外
      end
      def work_rank(app)
        work_filter(self.get(app)[0])
          .map(&:work)
          .delete_if(&:nil?)
          .ranking
          .to_a
          .sort {|x, y| y[1] <=> x[1] }
          .map  {|x| [x[0], x[1].to_s]}
      end
    end

    class Danketsu < Excel::Init::Enter
      def initialize()
        super()
        @output_range = "AF6:AI"
      end
      def filter(list)
        column = [2, 3, 7, 15, 16]
        list
          .keep_if {|row| row[2].to_i == @this_month && row[0] == "団結" }
          .map     {|row| Personal::Danketsu.new(column.map {|col| row[col]})}
      end
      def output_range(value)
        return "#{@output_range}#{6 + value.length - 1}"
      end
    end
    class Leave < Excel::Sheet
      def initialize
        super()
        @file = Excel::Init::FILE
        @data = {:sheet => "脱退", :range => ["A2:G93"]}
        @output_range = "V13:AB"
      end
      private
      def filter(list)
        list
          .map     {|row| Personal::Leave.new(row) }
          .keep_if {|lp| lp.month == @this_month }
      end
    end
  end

  class GenseiFile < Excel::Sheet
    def initialize
      super
      # @file       = "s:/原田フォルダ/2016年度/年間資料/2016年度　現勢報告書.xlsx"
      @file       = "f:/2016年度　現勢報告書.xlsx"
      @range      = ["A8", "AB6:AB9"]
      @sheetName  = @this_month.toSheetIndex
    end
    def output(app, excelSheet)
      value = excelSheet.init_person(app)
      range = excelSheet.output_range(value)
      self.put(app, @sheetName, range, value)
    end
    private
    def prev_fetch(app)
      prev_sheet = (@this_month - 1).toSheetIndex
      self.get(app, prev_sheet).flatten.map(&:to_i)
    end
  end
end

begin
  app               = WIN32OLE.new('Excel.Application')
  app.Visible       = Visible
  app.DisplayAlerts = true
  ie                = Excel::Init::Enter.new()
  # il                = Excel::Init::Leave.new()
  # id                = Excel::Init::Danketsu.new()
  # g                 = Excel::GenseiFile.new()
  # [ie, il, id].each {|sheet| g.output(app, sheet)}
  # pp ie.work_output(app)
  pp ie.work_rank(app)
ensure
  app.Quit()
end

