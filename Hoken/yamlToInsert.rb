require 'yaml'
require 'pp'

str = File.open("./test.yaml").read()
data = YAML.load(str)

class Array
  def split_by(num)
    small = Array.new
    big   = self
    while (big.length > num)
      small << big.take(num)
      big = big.drop(num)
    end
    small << big
    return small
  end
end

def page_print(line)
  puts "\\begin{picture}(0,0)(36,255)"
  line.each {|person|
  post = person["post"]
  ad1  = person["ad1"]
  ad2  = person["ad2"]
  name = person["name"]
  puts " \\personallabel{#{post}}{#{ad1}}{#{ad2}}{#{name}}"
  }
  puts "\\end{picture}\\newpage\\setcounter{width}{0}\\setcounter{height}{0}"
end

data.split_by(21).each {|page| page_print(page)}
