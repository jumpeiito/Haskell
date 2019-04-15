# coding: utf-8
require "socket"
require "kconv"
#usernameの取得
# puts "usernameを入力してください"
# user = gets.to_s

def mainSystem()
  #送信内容の入力
  puts "メッセージを入力してください"
  msg = gets.to_s
  if msg == "exit"
    exit!
  end
  port.puts "#{msg}"
  puts "Send: #{Time.now}"

  print port.gets

  puts "Receive: #{Time.now}"

  port.close
end


begin
  port = TCPSocket.open("localhost",8080)
rescue
  puts "TCPSocket.open failed :#$!"
else
  while true
    #送信内容の入力
    puts "メッセージを入力してください"
    msg = gets.to_s
    if msg == "exit"
      exit!
    end
    # port.puts "#{user}:#{msg}"
    port.puts msg.toutf8
    puts "Send: #{Time.now}"
    puts port.gets.toutf8
    puts "Receive: #{Time.now}"
  end
  port.close
end
