require 'irb'

######################################
# Temporary Crap
def hello
  puts "howdy"
end

def ets_stuff
    puts "inner loop c"
    puts "e_and_ts_by_source_id & ach_ids.map(&:source_id) intersection size"
    puts e_and_ts_by_source_id.slice(*ach_entries.map(&:source_id)).size
    puts "e_and_ts.. source_ids"
    puts e_and_ts_by_source_id.keys.sort.inspect
    puts "ach_entries source ids"
    puts ach_entries.map(&:source_id).sort.inspect
end
def details
  puts "not defined e_and_ts_by_source_id"
  if defined? payment_entry_details
    puts "payment_entry_details-----------"
    puts "size"
    puts [
            payment_entry_details.entries_and_transactions.size,
            payment_entry_details.ach_entry_ids.size
        ].inspect
    puts "nils?"
    puts payment_entry_details.entries_and_transactions.map{|e_and_t|
      e_and_t.entry_detail.nil?
    }.inspect

    puts "contents match:"
    puts PaymentProcessing::ACHEntry
      .where(id: payment_entry_details.ach_entry_ids)
      .map{|a| a.source_id}
      .sort == payment_entry_details
                .entries_and_transactions
                .map{|e|e.transaction.id}
                .sort
    puts "transaction class(es)"
    puts payment_entry_details.entries_and_transactions.map{ |et| et.transaction.class.name }.uniq
  end
  if defined? settlement_entry_details
    puts "settlement_entry_details-----------"
    puts "size"
    puts [
            settlement_entry_details.entries_and_transactions.size,
            settlement_entry_details.ach_entry_ids.size
        ].inspect
    puts "nils?"
    puts settlement_entry_details.entries_and_transactions.map{|e_and_t|
      e_and_t.entry_detail.nil?
    }.inspect
    puts "contents match:"
    puts PaymentProcessing::ACHEntry
      .where(id: settlement_entry_details.ach_entry_ids)
      .map{|a| a.source_id}
      .sort == settlement_entry_details
                .entries_and_transactions
                .map{|e|e.transaction.id}
                .sort
    puts "transaction class(es)"
    puts settlement_entry_details.entries_and_transactions.map{ |et| et.transaction.class.name }.uniq
  end
  "DONE"

end
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
