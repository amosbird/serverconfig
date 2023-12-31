#!/usr/bin/env perl

use MIME::Base64;

my $script = <<'END_SCRIPT';
use IO::Socket::INET;
use IO::Select;
my $port = $ARGV[0];
my $host = '127.0.0.1';
my $client = IO::Socket::INET->new(
    PeerAddr => $host,
    PeerPort => $port,
    Proto    => 'tcp',
);
my $select = IO::Select->new(\*STDIN, $client);
while (1) {
    foreach my $fh ($select->can_read) {
        my $data;
        my $bytes = sysread($fh, $data, 1024);
        if (!defined $bytes || $bytes == 0) {
            close $client if $fh == $client;
            exit;
        }
        if ($fh == \*STDIN) {
            syswrite($client, $data);
        } elsif ($fh == $client) {
            syswrite(\*STDOUT, $data);
        }
    }
}
END_SCRIPT

my $encoded_script = encode_base64($script);
$encoded_script =~ s/\s+//g;
print "perl -MMIME::Base64 -e \"'eval(decode_base64(q(".$encoded_script.")))'\""
