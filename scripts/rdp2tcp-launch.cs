using System;
using System.Runtime.InteropServices;

// Windows-subsystem launcher for the rdp2tcp guard. The scheduled task runs
// this instead of powershell.exe. A console program started by the scheduler
// gets a visible console before -WindowStyle Hidden can hide it, which
// flashed a window on every five-minute run.
//
// CREATE_NO_WINDOW is not usable here. The guard then starts rdp2tcp.exe,
// and that exe aborts in ucrtbase (0xc0000409) when its parent has no
// console. CREATE_NEW_CONSOLE plus SW_HIDE gives PowerShell a real console
// that is hidden before it is shown.
class Program {
    const string Script =
        @"C:\Users\Administrator\AppData\Local\rdp2tcp\rdp2tcp-launch.ps1";
    const uint CreateNewConsole = 0x10;
    const int StartfUseShowWindow = 0x1;
    const uint WaitObject0 = 0;
    const uint WaitTimeout = 0x102;

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    struct StartupInfo {
        public int cb;
        public string lpReserved;
        public string lpDesktop;
        public string lpTitle;
        public int dwX, dwY, dwXSize, dwYSize;
        public int dwXCountChars, dwYCountChars, dwFillAttribute, dwFlags;
        public short wShowWindow, cbReserved2;
        public IntPtr lpReserved2;
        public IntPtr hStdInput, hStdOutput, hStdError;
    }

    [StructLayout(LayoutKind.Sequential)]
    struct ProcessInformation {
        public IntPtr hProcess, hThread;
        public int dwProcessId, dwThreadId;
    }

    [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
    static extern bool CreateProcess(
        string app, string cmd, IntPtr pa, IntPtr ta, bool inherit,
        uint flags, IntPtr env, string dir,
        ref StartupInfo si, out ProcessInformation pi);

    [DllImport("kernel32.dll", SetLastError = true)]
    static extern bool CloseHandle(IntPtr handle);

    [DllImport("kernel32.dll", SetLastError = true)]
    static extern uint WaitForSingleObject(IntPtr handle, uint ms);

    [DllImport("kernel32.dll", SetLastError = true)]
    static extern bool GetExitCodeProcess(IntPtr handle, out uint code);

    [DllImport("kernel32.dll", SetLastError = true)]
    static extern bool TerminateProcess(IntPtr handle, uint code);

    static int Main() {
        string cmd =
            "powershell.exe -NoProfile -NonInteractive " +
            "-ExecutionPolicy Bypass -File \"" + Script + "\"";
        var si = new StartupInfo();
        si.cb = Marshal.SizeOf(typeof(StartupInfo));
        si.dwFlags = StartfUseShowWindow;
        si.wShowWindow = 0;

        ProcessInformation pi;
        if (!CreateProcess(
                null, cmd, IntPtr.Zero, IntPtr.Zero, false,
                CreateNewConsole, IntPtr.Zero, null, ref si, out pi))
            return 1;

        CloseHandle(pi.hThread);
        uint waited = WaitForSingleObject(pi.hProcess, 120000);
        if (waited != WaitObject0) {
            if (waited == WaitTimeout)
                TerminateProcess(pi.hProcess, 1);
            CloseHandle(pi.hProcess);
            return 1;
        }

        uint code;
        if (!GetExitCodeProcess(pi.hProcess, out code))
            code = 1;
        CloseHandle(pi.hProcess);
        return (int)code;
    }
}
