#!/usr/bin/env perl
# vim:ts=4:sw=4:expandtab
# © 2012 Michael Stapelberg
# Licensed under BSD license, see http://code.i3wm.org/i3/tree/LICENSE
#
# Append this line to your i3 config file:
#     exec_always ~/per-workspace-layout.pl
#
# Then, change the %layouts hash like you want your workspaces to be set up.
# This script requires i3 >= v4.4 for the extended workspace event.

use strict;
use warnings;
use AnyEvent;
use AnyEvent::I3;
use v5.10;

my %layouts = (
    '3: ' => 'default',
);

my $i3 = i3();

die "Could not connect to i3: $!" unless $i3->connect->recv();

die "Could not subscribe to the workspace event: $!" unless
    $i3->subscribe({
        workspace => sub {
            my ($msg) = @_;
            return unless $msg->{change} eq 'focus';
            die "Your version of i3 is too old. You need >= v4.4"
                unless exists($msg->{current});
            my $ws = $msg->{current};

            my $name = substr $ws->{name}, 0, 3;
            my $con_id = $ws->{id};

            print $name;
            print "\n";
            return unless exists $layouts{$name};
            print qq|[con_id="$con_id"] layout | . $layouts{$name};
            print "\n";
            $i3->command(qq|[con_id="$con_id"] layout | . $layouts{$name});
        },
        _error => sub {
            my ($msg) = @_;
            say "AnyEvent::I3 error: $msg";
            say "Exiting.";
            exit 1;
        },
    })->recv->{success};

# Run forever.
AnyEvent->condvar->recv
