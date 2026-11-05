#!/usr/bin/env bash

usage() {
    cat >&2 <<'EOF'
usage:
  tstart.sh workspace local
  tstart.sh workspace remote --display DISPLAY --ssh-control PATH [--ssh-port PORT] (--abush-web-forward-port PORT | --without-abush)
  tstart.sh app emacs|htop|lvim|crush
  tstart.sh app abush (--local | --remote-web-port PORT)
  tstart.sh android

Legacy aliases for local and app names remain supported. The legacy remote forms are:
  tstart.sh remote DISPLAY SSH_CONTROL
  tstart.sh remote DISPLAY SSH_CONTROL ABUSH_WEB_FORWARD_PORT
EOF
}

has_active_x11_session() {
    local session type active

    command -v loginctl >/dev/null 2>&1 || return 1
    while read -r session _ user _; do
        [[ $user == "${USER:-$(id -un)}" ]] || continue
        type=$(loginctl show-session "$session" --property=Type --value 2>/dev/null)
        active=$(loginctl show-session "$session" --property=Active --value 2>/dev/null)
        [[ $type == x11 && $active == yes ]] && return 0
    done < <(loginctl list-sessions --no-legend --no-pager 2>/dev/null)
    return 1
}

require_value() {
    if [[ $# -lt 2 || -z $2 ]]; then
        printf 'tstart.sh: %s requires a value\n' "$1" >&2
        usage
        exit 1
    fi
}

validate_port() {
    if [[ ! $1 =~ ^[0-9]+$ ]] || (( $1 < 1 || $1 > 65535 )); then
        printf 'tstart.sh: invalid port: %s\n' "$1" >&2
        exit 1
    fi
}

if [[ ${GUI:-} != t ]]; then
    # shellcheck source=/dev/null
    source "$HOME/.config/environment.sh"
    if has_active_x11_session; then
        # shellcheck source=/dev/null
        source "$HOME/.config/path.sh" gui
    else
        # shellcheck source=/dev/null
        source "$HOME/.config/path.sh" remote
    fi
fi

command_name=${1:-}
shift || true
# Launch order is the kitty OS-window id: 1 is the amos window that falls
# through below, then emacs, htop, abush. alt+0/+/=/+9 follow that order.
apps=(emacs htop abush)

# Compatibility with the old top-level modes. New callers should use the
# explicit workspace/app scopes above.
case "$command_name" in
local)
    command_name=workspace
    set -- local "$@"
    ;;
remote)
    if [[ $# -ne 2 && $# -ne 3 ]]; then
        printf 'tstart.sh: legacy remote requires DISPLAY SSH_CONTROL [ABUSH_WEB_FORWARD_PORT]\n' >&2
        usage
        exit 1
    fi
    command_name=workspace
    if [[ $# -eq 2 ]]; then
        set -- remote --display "$1" --ssh-control "$2" --without-abush
    else
        set -- remote --display "$1" --ssh-control "$2" --abush-web-forward-port "$3"
    fi
    ;;
emacs|htop|lvim|crush|abush)
    set -- "$command_name" "$@"
    command_name=app
    ;;
esac

case "$command_name" in
android)
    [[ $# -eq 0 ]] || { usage; exit 1; }
    # shellcheck source=/dev/null
    source "$HOME/.config/path.sh" android
    exec tmux -u new-session -A -s amos
    ;;
workspace)
    workspace_mode=${1:-}
    shift || true
    case "$workspace_mode" in
    local)
        [[ $# -eq 0 ]] || { usage; exit 1; }
        for app in "${apps[@]}"; do
            app_args=(app "$app")
            if [[ $app == abush ]]; then
                app_args+=(--local)
            fi
            kitten @ launch --allow-remote-control --keep-focus "$0" "${app_args[@]}"
        done
        ;;
    remote)
        display=
        ssh_control=
        ssh_port=
        abush_web_forward_port=
        without_abush=false
        while [[ $# -gt 0 ]]; do
            case "$1" in
            --display)
                require_value "$@"
                [[ -z $display ]] || { printf 'tstart.sh: --display specified more than once\n' >&2; exit 1; }
                display=$2
                shift 2
                ;;
            --ssh-control)
                require_value "$@"
                [[ -z $ssh_control ]] || { printf 'tstart.sh: --ssh-control specified more than once\n' >&2; exit 1; }
                ssh_control=$2
                shift 2
                ;;
            --ssh-port)
                require_value "$@"
                [[ -z $ssh_port ]] || { printf 'tstart.sh: --ssh-port specified more than once\n' >&2; exit 1; }
                ssh_port=$2
                shift 2
                ;;
            --abush-web-forward-port)
                require_value "$@"
                [[ -z $abush_web_forward_port && $without_abush == false ]] || {
                    printf 'tstart.sh: choose exactly one of --abush-web-forward-port and --without-abush\n' >&2
                    exit 1
                }
                abush_web_forward_port=$2
                shift 2
                ;;
            --without-abush)
                [[ -z $abush_web_forward_port && $without_abush == false ]] || {
                    printf 'tstart.sh: choose exactly one of --abush-web-forward-port and --without-abush\n' >&2
                    exit 1
                }
                without_abush=true
                shift
                ;;
            *)
                printf 'tstart.sh: unknown workspace remote option: %s\n' "$1" >&2
                usage
                exit 1
                ;;
            esac
        done
        if [[ -z $display || -z $ssh_control ]]; then
            printf 'tstart.sh: workspace remote requires --display and --ssh-control\n' >&2
            exit 1
        fi
        if [[ -z $abush_web_forward_port && $without_abush == false ]]; then
            printf 'tstart.sh: workspace remote requires one of --abush-web-forward-port and --without-abush\n' >&2
            exit 1
        fi
        if [[ -n $ssh_port ]]; then
            validate_port "$ssh_port"
        fi
        if [[ -n $abush_web_forward_port ]]; then
            validate_port "$abush_web_forward_port"
        fi
        ssh_args=(-S "$ssh_control")
        if [[ -n $ssh_port ]]; then
            ssh_args+=(-p "$ssh_port")
        fi
        for app in "${apps[@]}"; do
            if [[ $app == abush && $without_abush == true ]]; then
                continue
            fi
            remote_app_args=("$0" app "$app")
            if [[ $app == abush ]]; then
                remote_app_args+=(--remote-web-port "$abush_web_forward_port")
            fi
            printf -v remote_app_command '%q ' "${remote_app_args[@]}"
            kitten @ launch --allow-remote-control --keep-focus \
                ssh "${ssh_args[@]}" -tt "$display" "$remote_app_command"
        done
        ;;
    *)
        usage
        exit 1
        ;;
    esac
    ;;
app)
    app=${1:-}
    shift || true
    case "$app" in
    emacs)
        [[ $# -eq 0 ]] || { usage; exit 1; }
        exec startemacs
        ;;
    htop)
        [[ $# -eq 0 ]] || { usage; exit 1; }
        exec starthtop
        ;;
    lvim)
        [[ $# -eq 0 ]] || { usage; exit 1; }
        exec startlvim
        ;;
    crush)
        [[ $# -eq 0 ]] || { usage; exit 1; }
        exec startcrush-tmux
        ;;
    abush)
        case "${1:-}" in
        --local)
            [[ $# -eq 1 ]] || { usage; exit 1; }
            exec startabush --local
            ;;
        --remote-web-port)
            require_value "$@"
            [[ $# -eq 2 ]] || { usage; exit 1; }
            validate_port "$2"
            exec startabush --remote-web-port "$2"
            ;;
        '')
            # Preserve the old direct `tstart.sh abush` behavior as a local launch.
            exec startabush --local
            ;;
        *)
            usage
            exit 1
            ;;
        esac
        ;;
    *)
        usage
        exit 1
        ;;
    esac
    ;;
*)
    usage
    exit 1
    ;;
esac

export TMUX=/tmp/tmux-amos
tmux -u new-session -A -s amos
