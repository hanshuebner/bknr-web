#!/usr/bin/perl -w

use strict;

my $ripper = "streamripper";
my $url = (shift @ARGV) || "http://64.236.34.196:80/stream/1012";
my $spool_dir = "/backup/mp3-spool";
my $lisp_fifo_path = (shift @ARGV) || "/var/run/bknr-import-mp3";

open(RIPPER, "streamripper $url -s -d $spool_dir -t 2>&1 | cat -u |")
    or die "$0: can't popen streamripper: $!\n";
open(FIFO, ">$lisp_fifo_path")
    or die "$0: can't open fifo to lisp: $!\n";
select(FIFO); $| = 1; select(STDOUT);

my $first_file = 1;

while (<RIPPER>) {
    s/.*\r(....)/$1/;
    s/[\r\n]*$//;
    unless (/^\[ripping...\s*\] (.+) \[\s*\d+\S*\]$/) {
	warn "ignored: $_\n";
	next;
    }
    my $name = $1;
    $name =~ tr/./-/;
    my $file = "$spool_dir/$name.mp3";;
    if ($first_file) {
	warn "first file $file skipped\n";
	$first_file = 0;
    } else {
	if (-f $file) {
	    warn "got file: $file\n";
	    print FIFO "$file\n";
	} else {
	    warn "streamripper announced file $file, but it was not found\n";
	}
    }
}
    
