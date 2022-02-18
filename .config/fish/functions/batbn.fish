function batbn --argument-names 'filename' 'path' -d "find by name and runs it through bat"
    set -l find_result (fbn "$filename" "$path")

    bat "$find_result"
end
