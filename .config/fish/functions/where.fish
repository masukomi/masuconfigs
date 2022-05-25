function where --argument-names 'exe_or_function'

  set exe_path (command -v $exe_or_function)
  if test $status -eq 0
    echo $exe_path
    return 0
  else
    set function_names (functions --names | sd ", " "\n" | grep "^$exe_or_function")
    for x in $function_names
      set function_path (functions -D $x)
      string match -r "/$exe_or_function.fish\$" "$function_path" > /dev/null
      if test $status -eq 0
        echo $function_path
        return 0
      end
    end
  end
  return 1
end

