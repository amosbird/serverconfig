#Persistent
#SingleInstance force

Loop, %0%  ; For each parameter:
  {
    param := %A_Index%  ; Fetch the contents of the variable whose name is contained in A_Index.
    params .= A_ralt . param
  }
ShellExecute := A_IsUnicode ? "shell32\ShellExecute":"shell32\ShellExecuteA"

if not A_IsAdmin
{
    If A_IsCompiled
       DllCall(ShellExecute, uint, 0, str, "RunAs", str, A_ScriptFullPath, str, params , str, A_WorkingDir, int, 1)
    ExitApp
}

;SetTimer, adskill, 1000
;
;GroupAdd,ads,京东
;GroupAdd,ads,系统通知
;GroupAdd,ads,淘金币活动消息
;GroupAdd,ads,腾讯游戏嘉年华

;adskill:
;    loop, {
;        WinWait, ahk_group ads
;        WinClose, ahk_group ads
;    }
;return

:*:a@::amosbird@gmail.com
^,::send, ^+{tab}
^.::send, ^{tab}

SetTitleMatchMode, 2
#WinActivateForce
ToggleWinMinimize(WindowExe)
{
	DetectHiddenWindows, Off
    ifWinActive, %WindowExe%
        WinMinimize
	Else
        IfWinExist, %WindowExe%
        {
            WinActivate
        }
	Return
}
ToggleWinMinimizeQQ(WindowExe)
{
	SetTitleMatchMode, 2
	DetectHiddenWindows, Off
    WinGetTitle, qq, A
    if (qq = "QQ")
    {
        send, !m
        Return
    }
    ifWinActive, %WindowExe% ,, QQ
        WinMinimize
	Else
        IfWinExist, %WindowExe% ,, QQ
        {
            WinActivate
        }
	Return
}

!g::ToggleWinMinimize("ahk_class CabinetWClass")
!s::ToggleWinMinimize("ahk_class tSkMainForm")
!e::ToggleWinMinimize("ahk_class Vim")
!o::ToggleWinMinimize("ahk_class CabinetWClass")
!p::ToggleWinMinimize("ahk_exe powerpnt.exe")
!t::ToggleWinMinimize("ahk_exe EXCEL.EXE")
!w::ToggleWinMinimize("ahk_exe winword.exe")
!i::ToggleWinMinimize("ahk_exe VISIO.exe")
!u::ToggleWinMinimize("ahk_exe Sumatrapdf.exe")
!y::ToggleWinMinimize("ahk_exe xmind.exe")


+Space::send, {space}
$`::send, #{space}
SC079::
    return
RAlt::
    return

SC079 & q::send, {.}
SC079 & a::send, {~}
SC079 & z::send, {``}
SC079 & x::send, {:}
SC079 & c::send, {;}
SC079 & s::send, {?}
SC079 & w::send, {/}
SC079 & e::send, {{}
SC079 & r::send, {}}
SC079 & d::send, {[}
SC079 & f::send, {]}
SC079 & t::send, {"}
SC079 & g::send, {'}
SC079 & b::send, {\}
SC079 & v::send, {|}
SC079 & 1::send, {6}
SC079 & 2::send, {&}
SC079 & 3::send, {(}
SC079 & 4::send, {)}
SC079 & 5::send, {^}
$[::send, {"}
$+9::send, {+}
$+0::send, {=}
$`;::send, {_}
$+`;::send, {`-}
$_::send, {!}
!r::Reload

; readline key mapping
#If !WinActive("ahk_class Vim") && !WinActive("ahk_class VirtualConsoleClass") &&
    !WinActive("ahk_class mintty") && !WinActive("ahk_exe devenv.exe") &&
    !WinActive("ahk_exe Code.exe") && !WinActive("ahk_exe XShell.exe") &&
    !WinActive("ahk_exe VISIO.EXE")
    ^a::send, {Home}
    ~!a::send, ^a
    ^e::send, {End}
    ^d::send, {Delete}
    ^u::send, +{Home}{Delete}
    ^i::send, {tab}
    ^o::send, +{End}{Delete}
    ^j::send, {Down}
    ^k::send, {Up}
    ^+i::send, +{tab}
    !f::send, ^{Right}
    !b::send, ^{Left}
    !Backspace::send, ^+{Left}{Delete}
    !d::send, ^+{Right}{Delete}
#If

+LButton::
    CoordMode, Mouse  ; Switch to screen/absolute coordinates.
    MouseGetPos, EWD_MouseStartX, EWD_MouseStartY, EWD_MouseWin
    WinGetPos, EWD_OriginalPosX, EWD_OriginalPosY,,, ahk_id %EWD_MouseWin%
    WinGet, EWD_WinState, MinMax, ahk_id %EWD_MouseWin%
    if EWD_WinState = 0  ; Only if the window isn't maximized
        SetTimer, EWD_WatchMouse, 10 ; Track the mouse as the user drags it.
return

EWD_WatchMouse:
    GetKeyState, EWD_LButtonState, LButton, P
    if EWD_LButtonState = U  ; Button has been released, so drag is complete.
    {
        SetTimer, EWD_WatchMouse, off
        return
    }
    GetKeyState, EWD_EscapeState, Escape, P
    if EWD_EscapeState = D  ; Escape has been pressed, so drag is cancelled.
    {
        SetTimer, EWD_WatchMouse, off
        WinMove, ahk_id %EWD_MouseWin%,, %EWD_OriginalPosX%, %EWD_OriginalPosY%
        return
    }
    ; Otherwise, reposition the window to match the change in mouse coordinates
    ; caused by the user having dragged the mouse:
    CoordMode, Mouse
    MouseGetPos, EWD_MouseX, EWD_MouseY
    WinGetPos, EWD_WinX, EWD_WinY,,, ahk_id %EWD_MouseWin%
    SetWinDelay, -1   ; Makes the below move faster/smoother.
    WinMove, ahk_id %EWD_MouseWin%,, EWD_WinX + EWD_MouseX - EWD_MouseStartX, EWD_WinY + EWD_MouseY - EWD_MouseStartY
    EWD_MouseStartX := EWD_MouseX  ; Update for the next timer-call to this subroutine.
    EWD_MouseStartY := EWD_MouseY
return

+RButton::
    CoordMode, Mouse ; Switch to screen/absolute coordinates.
    MouseGetPos, EWR_MouseStartX, EWR_MouseStartY, EWR_MouseWin
    WinGetPos, EWR_OriginalPosX, EWR_OriginalPosY,,, ahk_id %EWR_MouseWin%
    WinGet, EWR_WinState, MinMax, ahk_id %EWR_MouseWin%
    if EWR_WinState = 0 ; Only if the window isn't maximized
    SetTimer, EWR_WatchMouse, 10 ; Track the mouse as the user drags it.
return

EWR_WatchMouse:
    GetKeyState, EWR_RButtonState, RButton, P
    if EWR_RButtonState = U ; Button has been released, so drag is complete.
    {
        SetTimer, EWR_WatchMouse, off
        return
    }
    GetKeyState, EWR_EscapeState, Escape, P
    if EWR_EscapeState = D ; Escape has been pressed, so drag is cancelled.
    {
        SetTimer, EWR_WatchMouse, off
        WinMove, ahk_id %EWR_MouseWin%,, %EWR_OriginalPosX%, %EWR_OriginalPosY%
        return
    }
    ; Otherwise, reposition the window to match the change in mouse coordinates
    ; caused by the user having dragged the mouse:
    CoordMode, Mouse
    MouseGetPos, EWR_MouseX, EWR_MouseY
    WinGetPos, EWR_WinX, EWR_WinY, EWR_WinW, EWR_WinH, ahk_id %EWR_MouseWin%
    SetWinDelay, -1 ; Makes the below move faster/smoother.
    if (EWR_MouseX-EWR_WinX) <= (EWR_WinW/3)
    {
        if (EWR_MouseY-EWR_WinY) <= (EWR_WinH/3)
        {
            WinMove, ahk_id %EWR_MouseWin%,, EWR_WinX + EWR_MouseX - EWR_MouseStartX, EWR_WinY + EWR_MouseY - EWR_MouseStartY, EWR_WinW + EWR_MouseStartX - EWR_MouseX, EWR_WinH + EWR_MouseStartY - EWR_MouseY
        }
        else if (EWR_MouseY-EWR_WinY) <= (EWR_WinH/3)*2
        {
            WinMove, ahk_id %EWR_MouseWin%,, EWR_WinX + EWR_MouseX - EWR_MouseStartX, , EWR_WinW + EWR_MouseStartX - EWR_MouseX,
        }
        else
        {
            WinMove, ahk_id %EWR_MouseWin%,, EWR_WinX + EWR_MouseX - EWR_MouseStartX, , EWR_WinW + EWR_MouseStartX - EWR_MouseX, EWR_WinH + EWR_MouseY - EWR_MouseStartY
        }
    }
    else if (EWR_MouseX-EWR_WinX) <= (EWR_WinW/3)*2
    {
        if (EWR_MouseY-EWR_WinY) <= (EWR_WinH/2)
        {
            WinMove, ahk_id %EWR_MouseWin%,,, EWR_WinY + EWR_MouseY - EWR_MouseStartY,, EWR_WinH + EWR_MouseStartY - EWR_MouseY
        }
        else
        {
            WinMove, ahk_id %EWR_MouseWin%,,,,, EWR_WinH + EWR_MouseY - EWR_MouseStartY
        }
    }
    else
    {
        if (EWR_MouseY-EWR_WinY) <= (EWR_WinH/3)
        {
            WinMove, ahk_id %EWR_MouseWin%,,, EWR_WinY + EWR_MouseY - EWR_MouseStartY, EWR_WinW + EWR_MouseX - EWR_MouseStartX, EWR_WinH + EWR_MouseStartY - EWR_MouseY
        }
        else if (EWR_MouseY-EWR_WinY) <= (EWR_WinH/3)*2
        {
            WinMove, ahk_id %EWR_MouseWin%,,, , EWR_WinW + EWR_MouseX - EWR_MouseStartX,
        }
        else
        {
            WinMove, ahk_id %EWR_MouseWin%,,, , EWR_WinW + EWR_MouseX - EWR_MouseStartX, EWR_WinH + EWR_MouseY - EWR_MouseStartY
        }
    }
    EWR_MouseStartX := EWR_MouseX ; Update for the next timer-call to this subroutine.
    EWR_MouseStartY := EWR_MouseY
return
