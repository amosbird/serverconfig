#!/usr/bin/env perl

# Determine if input is stdin or file
if ( @ARGV == 0 ) {

    # No file specified, assume stdin
    $INFILE = "<&0";
    $|      = 1;       # Don't buffer input
}
else {
    $INFILE = $ARGV[0];
}

unless ( open( INPUT, $INFILE ) ) { die("File open error\n"); }

while (1) {

    &next_interval;

    my @lines = ();

    while (&read_next) {
        if ( $input eq "" ) { last; }
        push @lines, [ split( /\s+/, $input ) ];
    }

    $~ = "LINE";
    write;
    $~ = "HEADER";
    write;
    $~ = "LINE";
    write;
    $~ = "COLUMNS";

    for my $cols ( sort { $a->[0] cmp $b->[0] } @lines ) {
        $device = $cols->[0];
        $rrqm   = $cols->[1];
        $wrqm   = $cols->[2];
        $rmb    = $cols->[5];
        $wmb    = $cols->[6];
        $reqsz  = $cols->[7];
        $quesz  = $cols->[8];
        $await  = $cols->[9];
        $svctm  = $cols->[12];
        $util   = $cols->[13];
        write;
    }
}

close(INPUT);

sub next_interval {
    do {
        &read_next;
    } until ( $input =~ /^Device:/ );
}

sub read_next {
    $input = <INPUT>;
    if (eof) {
        exit;
    }
    chop($input);
}

format HEADER =
Device    rqm    wqm    rMB    wMB   reqsz  quesz  await  svctm  util
.

format COLUMNS =
@||||  @####  @####  @####  @####  @####  @####  @####  @####  @###%
$device, $rrqm, $wrqm, $rmb, $wmb, $reqsz, $quesz, $await, $svctm, $util
.

format LINE =
----------------------------------------------------------------------
.
