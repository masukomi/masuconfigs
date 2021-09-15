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
module RBM
  class Tools
    class << self
      def generate_attr_columns(from, to)
        (from..to).each do | n |
          num = n < 100 ? "0#{n}" : "#{n}"
          puts "add_column :contents, :attr#{num}, :text unless column_exists? :contents, :attr${num}"
        end
      end
    end
  end
  class Debug
    class << self
      def get_spawned_report_file(id)
        return SpawnedReport.find(id).binary_file.sha2_binary_path()
      end

      def display_attrs_for(klass, sort_by = :name) #name or :attr
        map_display_name_to_column_name = klass.attribute_categories\
          .map{|c|[c.name, c.props[:column]]}
        if sort_by == :name
          map_display_name_to_column_name.sort!{|a,b| a.first <=> b.first}
        else :attr
          map_display_name_to_column_name.sort!{|a,b| a.last <=> b.last}
        end
        max_display = map_display_name_to_column_name.map{|elt| elt.first}\
          .max{|a, b| a.length <=> b.length}.length
        map_display_name_to_column_name.each_with_index do |a,i|
          puts "\n****************************************************************" if i == 0
          answer = ' '*200
          answer.insert(0,a.first.to_s.strip)
          answer.insert(max_display+1, "<=>")
          answer.insert(max_display + 5,a.last.to_s.strip)
          puts "\t#{answer.strip}"
        end;nil
      end
    end

  end
end

def sql(myarg)
  ap ActiveRecord::Base.connection.execute(myarg).map{|x| x}
end

