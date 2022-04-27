# from https://github.com/houhoulis/dotfiles/blob/main/fish/functions/desc.fish
#
# Based on https://github.com/jonmarkprice/fish-functions/blob/master/desc.fish
# # Based on comment by Adam Brenecki (https://github.com/adambrenecki) on Fish
# # issue #597 (https://github.com/fish-shell/fish-shell/issues/597).
function desc --description 'Print the description of one or more fish functions by name, or all functions, or all functions in ~/.config/fish/functions/'

    # Check that $argv is not empty
    if test (count $argv) -eq 0
        desc desc
        printf 'Usage: desc { -a | --all | -m | --mine | <function> [<function> ...] }\n'
        return 1
    end

    # Do we want to print *all* descriptions?
    if test "$argv" = '--all' -o "$argv" = '-a'
        for f in (functions | sed -E 's/(.*), /\1\n/g')
            desc $f
        end
        return 0
    end

    # Do we want to print the descriptions of all user-defined functions?
    if test "$argv" = '--mine' -o "$argv" = '-m'
        for f in ~/.config/fish/functions/*.fish
            string match -q -r '.*/(?<function_name>[a-zA-Z0-9_-]+)\.fish\Z' $f
            desc $function_name
        end
        return 0
    end

    for arg in $argv
        # Check that $arg is indeed a fish function
        if not functions -q $arg
            printf '"%s" is not a function.\n' $arg

        # Check that the function has a description
        else if not functions $arg | grep -q -e 'function '$arg' .*--description'
            printf 'The function "%s" has no description.\n' $arg

        else
            # Print description
            printf '%s\t - %s\n' (string pad -r -w 20 $arg) (functions $arg | \
                grep 'function '$arg'.*--description' | sed -E "s|.*'(.*)'.*|\1|")
        end
    end
end

