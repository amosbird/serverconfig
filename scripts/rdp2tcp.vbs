' Manual fallback. The scheduled task runs rdp2tcp-launch.exe directly.
Set ws = CreateObject("WScript.Shell")
ws.Run """C:\Users\Administrator\AppData\Local\rdp2tcp\rdp2tcp-launch.exe""", 0, True
