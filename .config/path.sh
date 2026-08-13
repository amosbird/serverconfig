case ${1:-remote} in
gui)
    PATH="$HOME/scripts:$HOME/.npm-packages/bin"
    PATH="$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
    export PATH="$PATH:$HOME/.local/bin:$HOME/.mambatools/bin"
    ;;
remote)
    PATH="$HOME/scripts:$HOME/.mambatools/bin:$HOME/.npm-packages/bin"
    PATH="$PATH:$HOME/.local/bin"
    export PATH="$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
    ;;
android)
    PATH="$HOME/scripts:/data/adb/modules/ssh/usr/bin:$HOME/.npm-packages/bin"
    export PATH="$PATH:$HOME/.local/bin:/system/bin:/system/xbin:/system/sbin"
    ;;
*)
    printf 'unknown PATH profile: %s\n' "$1" >&2
    return 1
    ;;
esac
