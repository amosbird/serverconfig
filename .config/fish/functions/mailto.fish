function mailto -d "Mail to url" --argument-names 'url'
    function _mailto -d "Mail to url" --argument-names 'url'
        if test (count $argv) -ne 1
            echo "Usage: $_ <mailto url>"
            return 1
        end
        set -x EMACS_SERVER_NAME mu4e
        emacsclient -s $EMACS_SERVER_NAME -n --display $DISPLAY -e "(browse-url-mail \"$url\")"
    end
    if _mailto $argv
        i3-msg workspace ""
    else
        notify-send -t "mail" "mu4e" "Bad url!"
    end
end
