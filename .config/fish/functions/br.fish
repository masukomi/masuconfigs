function br
    set f (mktemp)
    broot --outcmd $f $argv
    if test $status -ne 0
        rm -f "$f"
        return "$code"
    end
    set d (cat "$f")
    rm -f "$f"
    eval "$d"
end
