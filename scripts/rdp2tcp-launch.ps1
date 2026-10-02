#
# Keep rdp2tcp.exe running in the interactive session.
#
# Run by the "clientname" scheduled task. The task had a logon trigger only, and
# that is not enough: an RDP reconnect re-attaches to a session that already
# exists (LocalSessionManager event 25, not 21), so nothing fires and a helper
# that is not running stays that way. That is the 2026-10-02 failure -- the exe
# was replaced, the session was reconnected, and the task's Last Run Time was
# still 09:35 that morning.
#
# This is a guard, not a launcher: it starts rdp2tcp only if it is not already
# there, and otherwise does nothing. Two reasons not to restart it every time.
#
# A running copy recovers on its own. server/main.c keeps the whole body in a
# do/while, so when the channel goes away channel_init() fails, it sleeps a
# second, and it tries again -- a process that outlived a disconnect picks the
# channel back up when the session returns. This was measured rather than
# assumed: PID 11672 on TX-PW0FNXY8 sat through the 13:21 disconnect and was
# still healthy afterwards, consuming ~16 ms of CPU per 10 s while disconnected.
#
# A restart is also not free. channel_kill() calls CancelIo and CloseHandle on
# handles the server has already torn down, with a standing "TODO why does it
# throw invalid handle exception?" against that line in the source. Avoiding the
# teardown path entirely is better than exercising it on every reconnect.
#
# A named mutex serialises the runs. The logon and reconnect triggers can fire
# within milliseconds of each other, and two guards checking "is it running?"
# at the same moment would both find nothing and both start a copy.

$ErrorActionPreference = 'Stop'

$exe     = 'C:\Users\Administrator\Desktop\rdp2tcp.exe'
$logDir  = Join-Path $env:LOCALAPPDATA 'rdp2tcp'
$logFile = Join-Path $logDir 'launch.log'

function Write-Log([string]$msg) {
    try {
        if (-not (Test-Path $logDir)) { New-Item -ItemType Directory -Force -Path $logDir | Out-Null }

        # Keep the log bounded. It only matters for the few runs after something
        # goes wrong, and the interesting part is always at the end.
        if ((Test-Path $logFile) -and (Get-Item $logFile).Length -gt 64KB) {
            $tail = Get-Content $logFile -Tail 200
            Set-Content -Path $logFile -Value $tail
        }

        $stamp = Get-Date -Format 'yyyy-MM-dd HH:mm:ss'
        Add-Content -Path $logFile -Value "$stamp  $msg"
    } catch {
        # Logging must never be the reason the helper fails to start.
    }
}

$mutex = New-Object System.Threading.Mutex($false, 'Local\rdp2tcp-guard')
if (-not $mutex.WaitOne(0)) {
    Write-Log 'another guard holds the mutex; nothing to do'
    exit 0
}

try {
    # The task's principal is InteractiveToken, so this process normally runs in
    # the interactive session and that is the session whose channel matters. The
    # TimeTrigger is the exception: it fires whether or not anyone is logged in,
    # and a copy started where there is no desktop would sit there with no
    # channel to open. The presence of the shell process is what distinguishes a
    # real session from session 0, where services (and this guard, when run over
    # ssh) do their work.
    $session = (Get-Process -Id $PID).SessionId

    $shell = @(Get-Process -Name explorer -ErrorAction SilentlyContinue |
               Where-Object { $_.SessionId -eq $session })

    if ($shell.Count -eq 0) {
        Write-Log "no interactive shell in session $session; not starting anything"
        exit 0
    }

    $running = @(Get-Process -Name rdp2tcp -ErrorAction SilentlyContinue |
                 Where-Object { $_.SessionId -eq $session })

    if ($running.Count -gt 0) {
        Write-Log ("already running in session {0}: {1}" -f $session, (($running | ForEach-Object { $_.Id }) -join ','))
        exit 0
    }

    if (-not (Test-Path $exe)) {
        Write-Log "NOT starting: $exe is missing"
        exit 1
    }

    # -WindowStyle Hidden because this is a console program and the task is
    # started at logon; without it a console window flashes on every reconnect.
    Start-Process -FilePath $exe -WindowStyle Hidden
    Write-Log "started $exe in session $session"
}
finally {
    $mutex.ReleaseMutex()
}
