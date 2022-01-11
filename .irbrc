IRB.conf[:SAVE_HISTORY] = 1000

def require_dir(relative_dir_path)
  cleaned_relative_dir_path =relative_dir_path.sub(/\/$/, '')
  Dir["#{cleaned_relative_dir_path}/**/*.rb"].each do |file|
    puts "requiring #{file}"
    begin
      require file
    rescue Exception => e
      puts "Couldn't require #{file}: #{e.message}"
    end
  end
  return true
end

def show_pos_in_array(pos, array)
  raise "don't be silly" if pos < 0 or pos > (array.length -1)
  response = array.map{|item| item.to_s}
  if pos == 0
    puts response.join(',')
    puts '^'
    return
  end
  # guaranteed pos is > 0
  starting_char = 0
  (0..(pos -1)).each do |index|
    starting_char += 1 unless starting_char == 0 # for the comma
    starting_char += response[index].length
  end
  puts response.join(',')
  puts "#{' ' * starting_char } ^"
end

module R # for rails
  class Debug
    class << self

      def path(url_string, method='GET')
        puts "running: Rails.application.routes.recognize_path(\"#{url_string}\", {:method=>'#{method}'})"
        Rails.application.routes.recognize_path(url_string, {:method=>method})
      end
    end
  end
end

def sql(myarg)
  ap ActiveRecord::Base.connection.execute(myarg).map{|x| x}
end

