use strict;
use utf8;
use MIME::Base64;
use Encode;

if (!weechat::register ("cut", "Amos Bird", "0.1", "WTFPL", "Cut command buffer", "", ""))
{
    weechat::print ("", "\tCut is already loaded");
    return weechat::WEECHAT_RC_OK;
}
else
{
    weechat::hook_command("Cut", "Cut the buffer string into system's clipboard", "", "", "", "command_cb", "");
}

sub command_cb {
    my ($data, $buffer, $args) = @_;
    my $line = decode("utf8", weechat::buffer_get_string($buffer, "input"));
    my $input_pos = weechat::buffer_get_integer($buffer, "input_pos");
    my $input_line = substr($line, 0, $input_pos);
    my $s = encode_base64(encode("utf8", $input_line), "");
    my $osc52 = "\033]52;c;".$s."\07";
    my $esc = "\033Ptmux;\033".$osc52."\033\\"; # tmux
    open(TTY, "> /dev/stdout");
    *STDOUT = *TTY;
    print $esc;
    close(TTY);
    weechat::buffer_set($buffer, "input", encode("utf8", substr($line, $input_pos)));
    weechat::buffer_set($buffer, "input_pos", "0");
    return weechat::WEECHAT_RC_OK
}
