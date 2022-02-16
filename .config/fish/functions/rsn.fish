# run spec named...
function rsn --argument-names "filename" "path" -d "finds by name and then runs with rspec"
  # can't figure out how to call fbn
  # and have it work without changing fbn to use echo which kills line
  # breaks...
  set -l ex_path (fbn "$filename" "$path")
  if test $status -eq 0
    bundle exec rspec --format=documentation "$ex_path"
  else
    echo "Not found: $filename"
  end
end

