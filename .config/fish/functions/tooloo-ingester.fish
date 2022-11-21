function tooloo-ingester
  set -l all 0
  for file in (find . -name "*.meta.toml")
      if test $all -eq 1
        tooloo add $file
        sleep 0.5
      else
        echo "include $file ?"
        read -P "[n/a/q/enter] " yn
        if test "$yn" = ""
            tooloo add $file
        else if test "$yn" = "q"
            echo "quitting"
            break
        else if test "$yn" = "a"
            set all 1
            tooloo add $file
        else
            echo "skipping..."
        end
      end
  end
end

