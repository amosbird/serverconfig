#!/usr/bin/env bash

pkill xcape
#xcape -e 'Alt_L:#65=Return;#101=Escape;#101:#65=Return;Mode_switch:#65=Shift_R|Return;#65=Return;#108=Shift_R|Insert;#100=BackSpace;Alt_L:#100=BackSpace'
xcape -e 'Alt_L:#50=Return;#37=Escape;#37:#50=Return;#100:#50=Shift_R|Return;#50=Return;#108=Shift_R|Insert;#100=BackSpace;Alt_L:#100=BackSpace'
