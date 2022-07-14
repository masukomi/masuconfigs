require 'irb'

######################################
# m_lang development
def lextest
  require './lexer'
  lexer = MLang.new()
  lexer.load_file("spec/files/test.m")
  lexer.tokenize
end
def show_tokens(tokens)
  tokens.each{|t| puts t.inspect}
  nil
end

# end m_lang dev
################
# ######################################
def backtrace_lines(exception, lines=5)
  git_root = `git rev-parse --show-toplevel`.strip
  puts exception.message
  puts "-" * exception.message.length
  count = 0
  exception.backtrace.each do | line |
    next if line.include?("/ruby/")
# \e[32m \e[0m
    line.sub!(/(.*)\/(.*?):(\d+):(in .*)/, "\\1/\e[32m\\2\e[0m:\\3 #\\4")
    line.sub!("`", "'")
    if git_root != ''
      puts line.sub(git_root, "")
    else
      puts line
    end
    count += 1
    break if count == lines
  end
  nil
end


# IRB.conf[:SAVE_HISTORY] = 1000

# def require_dir(relative_dir_path)
#   cleaned_relative_dir_path =relative_dir_path.sub(/\/$/, '')
#   Dir["#{cleaned_relative_dir_path}/**/*.rb"].each do |file|
#     puts "requiring #{file}"
#     begin
#       require file
#     rescue Exception => e
#       puts "Couldn't require #{file}: #{e.message}"
#     end
#   end
#   return true
# end

# def show_pos_in_array(pos, array)
#   raise "don't be silly" if pos < 0 or pos > (array.length -1)
#   response = array.map{|item| item.to_s}
#   if pos == 0
#     puts response.join(',')
#     puts '^'
#     return
#   end
#   # guaranteed pos is > 0
#   starting_char = 0
#   (0..(pos -1)).each do |index|
#     starting_char += 1 unless starting_char == 0 # for the comma
#     starting_char += response[index].length
#   end
#   puts response.join(',')
#   puts "#{' ' * starting_char } ^"
# end

# module R # for rails
#   class Debug
#     class << self

#       def path(url_string, method='GET')
#         puts "running: Rails.application.routes.recognize_path(\"#{url_string}\", {:method=>'#{method}'})"
#         Rails.application.routes.recognize_path(url_string, {:method=>method})
#       end
#     end
#   end
# end

# def sql(myarg)
#   ap ActiveRecord::Base.connection.execute(myarg).map{|x| x}
# end
