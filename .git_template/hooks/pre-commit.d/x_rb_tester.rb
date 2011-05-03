#!/usr/bin/env ruby
require 'set'

files = `git diff --cached --name-only --diff-filter=ACM`.split("\n")
tested_files = Set.new()
# tracking tested files because there may be changes to
# foo.rb and foo_spec.rb which would result in
# foo_spec.rb being invoked twice.
files.each do | file |
  filename=file
  if ! file.end_with? '_spec.rb'
    filename="spec/#{file.sub(/^app\//, '').sub(/\.rb$/, '_spec.rb')}"
  end
  if File.exist?(filename) && ! tested_files.include?(filename)
    tested_files.add filename

    response = system("bundle exec rspec #{filename}")
    if ! response
      puts "spec failed: #{filename}"
      exit 1
    end
  end
end
exit 0
