#!./perl

use strict;
use lib '.';
use Net::Telnet ();

## Global variables.
use vars qw($T);

## Local variables.
my(
   $count,
   $test,
   $tests,
   $total,
   );

## Get the tests to run.
$tests = &init_tests();

## Print total number of tests.
$total = @$tests;
print "1..$total\n";

## Do each test.
$count = 1;
foreach $test (@$tests) {
    if (&$test) {
	print "ok $count\n";
    }
    else {
	print "not ok $count\n";
    }

    $count++;
}
	
exit;


sub init_tests {
    [
     sub {
	 my($nfound, $r);

	 ## Create a TCP socket.
	 use Socket qw(AF_INET SOCK_STREAM);
	 socket S, AF_INET, SOCK_STREAM, 0
	     or return;

	 ## Check for input on socket - it should return 0.
	 vec($r='', fileno(S), 1) = 1;
	 eval { $nfound = select($r, '', '', 0) };

	 ## Check for failure.
	 return if $@;
	 return unless defined($nfound) and $nfound == 0;

	 1;
     },
    ];
}
