#!/usr/bin/env bash

pkill xcape
xcape -e 'Alt_L:#65=Return;#101=Escape;#101:#65=Return;Mode_switch:#65=Shift_R|Return;#65=Return;#108=Shift_R|Insert;#100=BackSpace;Alt_L:#100=BackSpace'
