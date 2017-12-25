function spin -d "Background job spinner"
    set -l format "  @\r"
    set -l commands
    set -l spinners "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
    set -l error /dev/stderr

    getopts $argv | while read -l 1 2
        switch "$1"
            case _
                set commands $commands ";$2"

            case s style
                set spinners $2

            case f format
                set format $2

            case error
                set error $2

            case h help
                printf "Usage: spin COMMANDS [(-s | --style STYLE)] [(-f | --format FORMAT)] \n"
                printf "                     [--error FILE] [(-h | --help)]\n\n"

                printf "    -s --style STRING   String to slice the spinner characters\n"
                printf "    -f --format FORMAT  Customize the spinner display\n"
                printf "        --error FILE    Write errors to FILE\n"
                printf "     -h --help          Show usage help\n"
                return

            case \*
                printf "spin: '%s' is not a valid option\n" $1 > /dev/stderr
                spin -h > /dev/stderr
                return 1
        end
    end

    if not set -q commands[1]
        return 1
    end

    set spinners (printf "%s\n" "$spinners" | grep -o .)

    set -l tmp (mktemp -t spin.XXX)
    set -l job_id

    fish -c "$commands" > /dev/stdout ^ $tmp &

    set job_id (jobs -l | awk -v FS=\t '
        /[0-9]+\t/{
            jobs[++job_count] = $1
        }
        END {
            for (i = 1; i <= job_count; i++) {
                print(jobs[i])
            }
            exit job_count == 0
        }
    ')

    while contains -- $job_id (jobs | cut -d\t -f1 ^ /dev/null)
        if status --is-interactive
            for i in $spinners
                printf "$format" | awk -v i=(printf "%s\n" $i | sed 's/=/\\\=/') '
                {
                    gsub("@", i)
                    printf("%s", $0)
                }
                ' > /dev/stderr

                sleep 0.01
            end
        end
    end

    if test -s $tmp
        command cat $tmp > $error
        command rm -f $tmp
        return 1
    end

    command rm -f $tmp
end
