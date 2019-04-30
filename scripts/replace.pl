#!/usr/bin/env perl

use strict;
use warnings;

# my @output = `realpath ~/mysub/*`;
# chomp @output;

# foreach my $line (@output)
# {
#     my ($number) = $line =~ /.*mysub\/([0-9]+).*/;
#     local $/;
#     open(my $file, '<:encoding(UTF-8)', $line) or die "Could not open file '$line' $!";
#     my $doc = <$file>;

#     my @fname = glob "~/cc/playground/leetcode-$number--*/snippet.cpp";
#     print "$fname[0]\n";
#     open(my $nf, "<$fname[0]") || die "File not found";

#     $doc =~ s/.*(^class Solution.*)$/$1/sm;
#     my $ndoc = <$nf>;
#     local $/;
#     $ndoc =~ s/class Solution.*int mymain/$doc\nint mymain/sm;
#     open(my $nf2, ">$fname[0]") || die "File not found";
#     print $nf2 "$ndoc";
# }

my @output = `realpath ~/cc/playground/leetcode-*/snippet.cpp`;
chomp @output;
foreach my $line (@output) {
    local $/;
    open( my $file, "<$line" ) or die "Could not open file '$line' $!";
    my $doc = <$file>;
    $doc =~
s/\/\*\*.*Definition for a binary tree node.*\*\//\n#ifdef CC_PLAYGROUND\nstruct TreeNode {\nint val;\n TreeNode* left;\n TreeNode* right;\n TreeNode(int x)\n : val(x)\n , left(NULL)\n , right(NULL) {}\n};\n#endif\n/sm;
    open( my $nf2, ">$line" ) || die "File not found";
    print $nf2 "$doc";
}
