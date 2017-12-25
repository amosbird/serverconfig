function gitignore -d "Create .gitignore easily using gitignore.io"
    function __gitignore_run -a msg command
        if not spin "$command" -f "\r @ $msg\r\n" --error=/dev/null
            printf " × $msg\n"
            return 1
        end
        printf " ✓ $msg\n"
    end

    set -l output ".gitignore"
    set -l templates
    set -l update 0
    set -l cache_home $XDG_CACHE_HOME
    set -l gitignoreio_url "https://www.gitignore.io/api/"

    if test -z "$cache_home"
        set cache_home ~/.cache
    end

    set -l cache_templates "$cache_home/gitignore_templates"

    getopts $argv | while read -l 1 2
        switch "$1"
            case _
                set templates $templates $2

            case u update
                set update 1

            case o outout
                set output $2

            case h help
                printf "Usage: gitignore [--update] template [templates...] \n"
                printf "                 [--output=<file>] [--help]\n\n"

                printf "     -u --update              Update templates file\n"
                printf "     -o --output=<file>       Write output to <file>\n"
                printf "     -h --help                Show this help\n"
                return

            case \*
                printf "gitignore: '%s' is not a valid option\n" $1 >& 2
                gitignore --help >& 2
                return 1
        end
    end

    if test ! -s $cache_templates -o $update -ne 0
        if not __gitignore_run "Update templates" "
            curl --max-time 10 -sS '$gitignoreio_url/list' | tr ',' ' ' | tr '\n' ' ' > $cache_templates
            "
            echo "gitignore: can not fetch templates list from gitignore.io." > /dev/stderr
            return 1
        end
    end

    if test ! -s $cache_templates
        echo "gitignore: can not read templates list." > /dev/stderr
        return 1
    end

    if not set -q templates[1]
        return
    end

    set templates (printf ',%s' $templates | cut -c2-)

    if not __gitignore_run "Fetch template" "
        curl --max-time 10 -sS '$gitignoreio_url/$templates' > $output
        "
        echo "gitignore: can not fetch template from gitignore.io." > /dev/stderr
        return 1
    end
end
