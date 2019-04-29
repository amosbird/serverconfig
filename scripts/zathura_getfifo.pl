#!/usr/bin/env perl

use IPC::Run3;

sub fuser {
    my $line;
    run3 [ 'fuser', $_[0] ], \undef, \$line, \$line;
    return split( " ", substr( $line, rindex( $line, ": " ) + 2 ) );
}

$file  = $ARGV[0];
@pids  = fuser $file;
@fifos = glob "/tmp/llpp/*";
for my $pid (@pids) {
    for my $fifo (@fifos) {
        if ( grep { $_ eq $pid } fuser($fifo) ) {
            print $fifo;
            exit 0;
        }
    }
}
