Set objShell = CreateObject("WScript.Shell")
Set objEnv = objShell.Environment("USER")

objEnv("CLIENTNAME") = "100.125.197.99:22"
objEnv("CLIENTNAME") = "100.93.135.69:22"
