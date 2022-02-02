# run spec named...
function rsn --argument-names "filename" -d "finds by name and then runs with rspec"
  set -l path (find . -name "$filename")
  if test $status -eq 0
    echo "$path"
    bundle exec rspec --format=documentation "$path"
  else
    echo "Not found: $filename"
  end
end

