#!/usr/bin/env ruby
require 'set'

files = `git diff --cached --name-only --diff-filter=ACM`.split("\n")
tested_files = Set.new()
# tracking tested files because there may be changes to
# foo.rb and foo_spec.rb which would result in
# foo_spec.rb being invoked twice.
def get_spec_filename(file)
  return file if file.end_with? '_spec.rb'
  "spec/#{file.sub(/^app\//, '').sub(/\.rb$/, '_spec.rb')}"
end

files.each do | file |
  filename=file
  if file.end_with? '.rb'
    valid_code = system("ruby -c #{filename}")
    exit 2 unless valid_code

    filename = get_spec_filename(filename)
    if File.exist?(filename) && ! tested_files.include?(filename)
      tested_files.add filename

      # normally....
      # response = system("bundle exec rspec #{filename}")
      response = system("/usr/local/bin/docker-compose exec -T api-server bin/bundle exec rspec #{filename}")
      if ! response
        puts "spec failed: #{filename}"
        exit 1
      end
    end
  end
end
exit 0
