#!/usr/bin/perl

use HTTP::Daemon;
use HTTP::Status;

my $max_waits = 5;
my $pid_file = "/usr/local/squid/logs/squid.pid";

sub error {
    print join(" ", @_);
    exit;
}

sub restart_squid {
    print "restarting squid\n";
    
    if (-f $pid_file) {
    	my $pid = int(`cat $pid_file`);

    	error("can't open pid file $pid_file ($!)") unless ($pid);

    	error("bad pid $pid read from pid file") unless ($pid > 1);

    	!kill(0, $pid) or kill(15, $pid) or error("can't send SIGTERM to squid process (pid $pid): $!");

    	my $waits = 0;
    	for (; kill(0, $pid) && $waits < $max_waits; $i++) {
	    sleep(2);
    	}

        error("squid did not die, can't continue") if ($waits == $max_waits);
    }

    system("rm -rf /usr/local/squid/cache/*");
    system("squid -z");
    system("squid");

    print "squid restarted at ", `date`;
}

if ($>) {
    die "must be run as root\n";
}

my $d = HTTP::Daemon->new(LocalPort => 19921) || die;
print "Please contact me at: <URL:", $d->url, ">\n";
while (my $c = $d->accept) {
    if (my $r = $c->get_request) {
	if ($r->method eq 'GET' and $r->url->path eq "/mcc4rthy") {
	    restart_squid();
	    $c->send_response(RC_OK);
	    print $c "<html><body><h2>squid has been restarted</h2></body></html>";
	} else {
	    $c->send_error(RC_FORBIDDEN);
	}
    }
    $c->close;
    undef($c);
}
