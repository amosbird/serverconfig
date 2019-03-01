#!/usr/bin/env bash

pkill xcape
#xcape -e 'Alt_L:#65=Return;#101=Escape;#101:#65=Return;Mode_switch:#65=Shift_R|Return;#65=Return;#108=Shift_R|Insert;#100=BackSpace;Alt_L:#100=BackSpace

if (($#)); then
    DEBUG="-d"
fi
xcape $DEBUG -e 'Alt_L:#62=Return;#37=Escape;#62:#37=Escape;#37:#62=Return;#100:#62=Shift_L|Return;#62=Return;#108=Shift_L|Insert;#100=BackSpace'
