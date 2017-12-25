#!/usr/bin/env perl

# -w
use strict;
use warnings;

# (1) quit unless we have the correct number of command-line args
my $num_args = @ARGV;
    if ($num_args != 1) {
        print "\nUsage: xxx.pl <maybe-url-string>\n";
        exit;
}
my $text = $ARGV[0];
my $pattern = qr{
    (?:https?://|ftp://|news://|mailto:|file://|\bwww\.)
    [\w\-\@;\/?:&=%\$.+!*\x27,~#]*
    (
    \([\w\-\@;\/?:&=%\$.+!*\x27,~#]*\)   # Allow a pair of matched parentheses
    |                                    #
    [\w\-\@;\/?:&=%\$+*~]                # exclude some trailing characters (heuristic)
    )+
}x;

if ($text =~ /^$pattern$/g) {
    exit 0;
} else {
    exit 1;
};
