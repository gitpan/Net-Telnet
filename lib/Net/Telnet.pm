package Net::Telnet;
require 5.002;

## User documentation in POD format at bottom of file.  Search for =head

use strict;

## Module import. 
use Exporter ();
use Socket qw(AF_INET SOCK_STREAM inet_aton sockaddr_in);

## Base classes.
use vars qw(@ISA);
@ISA = qw(Exporter);
if (eval 'require IO::Socket') {
    push @ISA, 'IO::Socket::INET';
}
else {
    require FileHandle;
    push @ISA, 'FileHandle';
}

## Global variables.
use vars qw($VERSION $Default_blksize);
$VERSION = "3.00";
$Default_blksize = 8192;


########################### Public Methods ###########################


sub new {
    my($class) = @_;
    my(
       $fh_open,
       $host,
       $self,
       %args,
       );

    ## Create a new object with defaults.
    $self = $class->SUPER::new;
    $ {*$self}{net_telnet} = {
	bin_mode     => '',
	blksize      => $Default_blksize,
	buf          => '',
	cmd_prompt   => '/[$%#>] $/',
	eofile       => 1,
	errormode    => 'die',
	errormsg     => '',
	fdmask       => '',
	host         => 'localhost',
	inputlog     => '',
	last_line    => '',
	maxbufsize   => 1024 * 1024,
	dumplog      => '',
	num_wrote    => 0,
	ofs          => '',
	opened       => '',
	ors          => "\n",
	outputlog    => '',
	port         => 23,
	pushback_buf => '',
	rs           => "\n",
	telnet_mode  => 1,
	time_out     => 10,
	timedout     => '',
	unsent_opts  => '',
    };

    ## Parse the args.
    if (@_ == 2) {  # one positional arg given
	$host = $_[1];
    }
    elsif (@_ > 2) {  # named args given
	## Get the named args.
	(undef, %args) = @_;

	## Parse the errmode named arg first.
	foreach (keys %args) {
	    $self->errmode($args{$_})
		if /^-?errmode$/i;
	}
	
	## Parse all other named args.
	foreach (keys %args) {
	    if (/^-?binmode$/i) {
		$self->binmode($args{$_});
	    }
	    elsif (/^-?dump_log$/i) {
		$self->dump_log($args{$_});
	    }
	    elsif (/^-?errmode$/i) {
		next;
	    }
	    elsif (/^-?fhopen$/i) {
		$fh_open = $args{$_};
	    }
	    elsif (/^-?host$/i) {
		$host = $args{$_};
	    }
	    elsif (/^-?input_log$/i) {
		$self->input_log($args{$_});
	    }
	    elsif (/^-?input_record_separator$/i) {
		$self->input_record_separator($args{$_});
	    }
	    elsif (/^-?output_log$/i) {
		$self->output_log($args{$_});
	    }
	    elsif (/^-?output_record_separator$/i) {
		$self->output_record_separator($args{$_});
	    }
	    elsif (/^-?port$/i) {
		$self->port($args{$_})
		    or return;
	    }
	    elsif (/^-?prompt$/i) {
		$self->prompt($args{$_})
		    or return;
	    }
	    elsif (/^-?telnetmode$/i) {
		$self->telnetmode($args{$_});
	    }
	    elsif (/^-?timeout$/i) {
		$self->timeout($args{$_});
	    }
	    else {
		$self->error('usage: Net::Telnet->new(' .
			     '[Binmode => $mode,] ' .
			     '[Dump_Log => $filename,] ' .
			     '[Errmode => $errmode,] ' .
			     '[Fhopen => $filehandle,] ' .
			     '[Host => $host,] ' .
			     '[Input_log => $file,] ' .
			     '[Input_record_separator => $char,] ' .
			     '[Output_log => $file,] ' .
			     '[Output_record_separator => $char,] '.
			     '[Port => $port,] [Prompt => $matchop,] ' .
			     '[Telnetmode => $mode,] ' .
			     '[Timeout => $secs,])');
	    }
	}
    }

    if (defined $fh_open) {  # user wants us to attach to existing filehandle
	$self->fhopen($fh_open)
	    or return;
    }
    elsif (defined $host) {  # user want us to open a connection to host
	$self->host($host)
	    or return;
	$self->open
	    or return;
    }

    $self;
} # end sub new


sub DESTROY {
} # end sub DESTROY


sub binmode {
    my($self, $mode) = @_;
    my(
       $prev,
       $stream,
       );

    ## With no args, turn on binary mode.
    if (@_ < 2) {
	$mode = 1;
    }
    else {
	defined $mode or $mode = '';
    }

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{bin_mode};
    $stream->{bin_mode} = $mode;
    $prev;
} # end sub binmode


sub break {
    my($self) = @_;
    my $stream = $ {*$self}{net_telnet};
    $stream->{timedout} = '';
    return if $stream->{eofile};
    local $stream->{rs} = '';

    $self->print("\xff\xf3");
} # end sub break


sub close {
    my($self) = @_;
    my $stream = $ {*$self}{net_telnet};

    $stream->{eofile} = 1;
    $stream->{opened} = '';
    close $self
	if defined fileno($self);

    1;
} # end sub close


sub cmd {
    my($self, @args) = @_;
    my(
       $arg,
       $firstpos,
       $lastpos,
       $lines,
       $orig_errmode,
       $orig_prompt,
       $orig_timeout,
       $output,
       $output_ref,
       $prompt,
       $rs,
       $rs_len,
       $timeout,
       @cmd,
       );

    ## Init vars.
    $output = [];
    $timeout = $self->timeout;
    $self->timed_out('');
    return if $self->eof;

    ## Parse args.
    if (@_ == 2) {  # one positional arg given
	push @cmd, $_[1];
    }
    elsif (@_ > 2) {  # named args given
	## Parse the named args.
	while (($_, $arg) = splice @args, 0, 2) {
	    if (/^-?output$/i) {
		$output_ref = $arg;
		if (defined($output_ref) and ref($output_ref) eq "ARRAY") {
		    $output = $output_ref;
		}
	    }
	    elsif (/^-?prompt$/i) {
		$prompt = $arg;
	    }
	    elsif (/^-?string$/i) {
		push @cmd, $arg;
	    }
	    elsif (/^-?timeout$/i) {
		$timeout = &_parse_timeout($arg);
	    }
	    else {
		return $self->error('usage: $obj->cmd(',
				    '[Output => $ref,] ',
				    '[Prompt => $match,] ',
				    '[String => $string,] ',
				    '[Timeout => $secs,])');
	    }
	}
    }

    ## Override some user settings.
    $orig_errmode = $self->errmode('return');
    $orig_timeout = $self->timeout(&_endtime($timeout));
    $orig_prompt  = $self->prompt($prompt) if defined $prompt;
    $self->errmsg('');

    ## Send command and wait for the prompt.
    $self->print(@cmd)
	and ($lines) = $self->waitfor($self->prompt);

    ## Restore user settings.
    $self->errmode($orig_errmode);
    $self->timeout($orig_timeout);
    $self->prompt($orig_prompt) if defined $orig_prompt;

    ## Check for failure.
    return $self->error("command timed-out") if $self->timed_out;
    return $self->error($self->errmsg) if $self->errmsg ne '';
    return if $self->eof;

    ## Split on record terminator while maintaining terminator in output.
    $firstpos = 0;
    $rs = $self->input_record_separator;
    $rs_len = length $rs;
    while (($lastpos = index($lines, $rs, $firstpos)) > -1) {
	push(@$output,
	     substr($lines, $firstpos, $lastpos - $firstpos + $rs_len));
	$firstpos = $lastpos + $rs_len;
    }

    if ($firstpos < length $lines) {
	push @$output, substr($lines, $firstpos);
    }

    ## Get rid of echo back command.
    shift @$output;
    unless (@$output) {
	@$output = ('');
    }

    ## Return command output via named arg, if requested.
    if (defined $output_ref) {
	if (ref($output_ref) eq "SCALAR") {
	    $$output_ref = join '', @$output;
	}
	elsif (ref($output_ref) eq "HASH") {
	    %$output_ref = @$output;
	}
    }
    
    wantarray ? @$output : 1;
} # end sub cmd


sub dump_log {
    my($self, $name) = @_;
    my(
       $fh,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $fh = $stream->{dumplog};

    if (@_ >= 2) {
	$fh = &_fname_to_handle($self, $name);
	$stream->{dumplog} = $fh;
    }

    $fh;
} # end sub dump_log


sub eof {
    my($self) = @_;

    $ {*$self}{net_telnet}{eofile};
} # end sub eof


sub errmode {
    my($self, $mode) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{errormode};

    if (@_ >= 2) {
	## Set the error mode.
	defined $mode or $mode = '';
	if (ref($mode) eq 'CODE') {
	    $stream->{errormode} = $mode;
	}
	elsif (ref($mode) eq 'ARRAY') {
	    unless (ref($mode->[0]) eq 'CODE') {
		&_carp($self,
		       'bad errmode: first item in list must be a code ref');
		$mode = 'die';
	    }
	    $stream->{errormode} = $mode;
	}
	elsif ($mode =~ /^return$/i) {
	    $stream->{errormode} = 'return';
	}
	else {
	    $stream->{errormode} = 'die';
	}
    }

    $prev;
} # end sub errmode


sub errmsg {
    my($self, @errmsgs) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{errormsg};

    if (@_ >= 2) {
	$stream->{errormsg} = join '', @errmsgs;
    }

    $prev;
} # end sub errmsg


sub error {
    my($self, @errmsg) = @_;
    my(
       $errmsg,
       $func,
       $mode,
       $stream,
       @args,
       );

    $stream = $ {*$self}{net_telnet};

    if (@_ >= 2) {
	## Put error message in the object.
	$errmsg = join '', @errmsg;
	$stream->{errormsg} = $errmsg;

	## Do the error action as described by error mode.
	$mode = $stream->{errormode};
	if (ref($mode) eq 'CODE') {
	    &$mode($errmsg);
	    return;
	}
	elsif (ref($mode) eq 'ARRAY') {
	    ($func, @args) = @$mode;
	    &$func(@args);
	    return;
	}
	elsif ($mode =~ /^return$/i) {
	    return;
	}
	else {  # die
	    if ($errmsg =~ /\n$/) {
		die $errmsg;
	    }
	    else {
		## Die and append caller's line number to message.
		&_croak($self, $errmsg);
	    }
	}
    }
    else {
	return $stream->{errormsg} ne '';
    }
} # end sub error


sub fhopen {
    my($self, $fh) = @_;
    my(
       $blksize,
       $fd,
       $stream,
       );

    {
	no strict 'refs';
	$fd = fileno $fh;
    }

    ## Ensure associated filehandle is already open.
    return $self->error("fhopen filehandle isn't already open")
	unless defined $fd;

    ## Ensure object is closed.
    $self->close;

    ## Associate the object with already open filehandle.
    open $self, "+<&=$fd"
	or return $self->error("problem attaching to fhopen filehandle: $!");
    $self->autoflush;

    ## Re-initialize the object.
    $stream = $ {*$self}{net_telnet};
    $blksize = (stat $self)[11];
    $stream->{blksize} = $blksize || $Default_blksize;
    $stream->{buf} = '';
    $stream->{eofile} = '';
    vec($stream->{fdmask}='', fileno($self), 1) = 1;
    $stream->{host} = '';
    $stream->{last_line} = '';
    $stream->{num_wrote} = '';
    $stream->{opened} = 1;
    $stream->{pushback_buf} = '';
    $stream->{timedout} = '';
    $stream->{unsent_opts} = '';
    1;
} # end sub fhopen


sub get {
    my($self, %args) = @_;
    my(
       $endtime,
       $line,
       $stream,
       $timeout,
       );

    ## Init vars.
    $stream = $ {*$self}{net_telnet};
    $timeout = $stream->{time_out};
    $stream->{timedout} = '';
    return if $stream->{eofile};

    ## Parse the named args.
    foreach (keys %args) {
	if (/^-?timeout$/i) {
	    $timeout = &_parse_timeout($args{$_});
	}
	else {
	    return $self->error('usage: $obj->get([Timeout => $secs,])');
	}
    }

    ## Set wall time when we time out.
    $endtime = &_endtime($timeout);

    ## Try to send any waiting option negotiation.
    if (length $stream->{unsent_opts}) {
	&_flush_opts($self, $stream);
    }

    ## Try to read just the waiting data using return error mode.
    {
	local $stream->{errormode} = 'return';
	$stream->{errormsg} = '';
	&_fillbuf($self, $stream, 0);
    }

    ## We're done if we timed-out and timeout value is set to "poll".
    return if $stream->{timedout} and defined($timeout) and $timeout == 0;

    ## We're done if we hit an error other than timing out.
    return $self->error($stream->{errormsg})
	if $stream->{errormsg} and ! $stream->{timedout};


    ## If buffer is still empty, try to read according to user's timeout.
    if (! length $stream->{buf}) {
	&_fillbuf($self, $stream, $endtime)
	    or do {
		return if $stream->{timedout};

		## We've reached end-of-file.
		$self->close;
		return;
	    };
    }

    ## Extract chars from buffer.
    $line = $stream->{buf};
    $stream->{buf} = '';

    $line;
} # end sub get


sub getline {
    my($self, %args) = @_;
    my(
       $endtime,
       $len,
       $line,
       $offset,
       $pos,
       $stream,
       $timeout,
       );

    ## Init vars.
    $stream = $ {*$self}{net_telnet};
    $timeout = $stream->{time_out};
    $stream->{timedout} = '';
    return if $stream->{eofile};

    ## Parse the named args.
    foreach (keys %args) {
	if (/^-?timeout$/i) {
	    $timeout = &_parse_timeout($args{$_});
	}
	else {
	    return $self->error('usage: $obj->getline([Timeout => $secs,])');
	}
    }

    ## Set wall time when we time out.
    $endtime = &_endtime($timeout);

    ## Try to send any waiting option negotiation.
    if (length $stream->{unsent_opts}) {
	&_flush_opts($self, $stream);
    }

    ## Keep reading into buffer until end-of-line is read.
    $offset = 0;
    while (($pos = index($stream->{buf}, $stream->{rs}, $offset)) == -1) {
	$offset = length $stream->{buf};
	&_fillbuf($self, $stream, $endtime)
	    or do {
		return if $stream->{timedout};

		## We've reached end-of-file.
		$self->close;
		if (length $stream->{buf}) {
		    return $stream->{buf};
		}
		else {
		    return;
		}
	    };
    }

    ## Extract line from buffer.
    $len = $pos + length $stream->{rs};
    $line = substr($stream->{buf}, 0, $len);
    substr($stream->{buf}, 0, $len) = '';

    $line;
} # end sub getline


sub getlines {
    my($self) = @_;
    my(
       $len,
       $line,
       $pos,
       $stream,
       @lines,
       );

    $stream = $ {*$self}{net_telnet};

    ## Fill buffer and get first line.
    $line = getline(@_)
	or return;
    push @lines, $line;
    
    ## Extract subsequent lines from buffer.
    while (($pos = index($stream->{buf}, $stream->{rs})) != -1) {
	$len = $pos + length $stream->{rs};
	push @lines, substr($stream->{buf}, 0, $len);
	substr($stream->{buf}, 0, $len) = '';
    }

    @lines;
} # end sub getlines


sub host {
    my($self, $host) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{host};

    if (@_ >= 2) {
	unless (defined $host and length $host) {
	    $host = 'localhost';
	}
	$stream->{host} = $host;
    }

    $prev;
} # end sub host


sub input_log {
    my($self, $name) = @_;
    my(
       $fh,
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{inputlog};

    if (@_ >= 2) {
	$fh = &_fname_to_handle($self, $name);
	$stream->{inputlog} = $fh;
    }

    $prev;
} # end sub input_log


sub input_record_separator {
    my($self, $rs) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{rs};

    if (@_ >= 2) {
	defined $rs or $rs = '';
	$stream->{rs} = $rs;
    }

    $prev;
} # end sub input_record_separator


sub lastline {
    my($self, $line) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{last_line};

    if (@_ >= 2) {
	defined $line or $line = '';
	$stream->{last_line} = $line;
    }

    $prev;
} # end sub lastline


sub login {
    my($self) = @_;
    my(
       $cmd_prompt,
       $endtime,
       $error,
       $match,
       $orig_errmode,
       $orig_timeout,
       $passwd,
       $prematch,
       $reset,
       $timeout,
       $usage,
       $username,
       %args,
       );

    ## Init vars.
    $timeout = $self->timeout;
    $self->timed_out('');
    return if $self->eof;
    $cmd_prompt = $self->prompt;
    $usage = 'usage: $obj->login(Name => $name, Password => $password, ' .
	'[Prompt => $match,] [Timeout => $secs,])';

    if (@_ == 3) {  # just username and passwd given
	$username = $_[1];
	$passwd = $_[2];
    }
    else {  # named args given
	## Get the named args.
	(undef, %args) = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?name$/i) {
		$username = $args{$_};
		defined($username)
		    or $username = "";
	    }
	    elsif (/^-?pass/i) {
		$passwd = $args{$_};
		defined($passwd)
		    or $passwd = "";
	    }
	    elsif (/^-?prompt$/i) {
		$cmd_prompt = $args{$_};
		defined $cmd_prompt
		    or $cmd_prompt = '';
		return $self->error("bad match operator: ",
				    "opening delimiter missing: $cmd_prompt")
		    unless ($cmd_prompt =~ m(^\s*/)
			    or $cmd_prompt =~ m(^\s*m\s*\W));
	    }
	    elsif (/^-?timeout$/i) {
		$timeout = &_parse_timeout($args{$_});
	    }
	    else {
		return $self->error($usage);
	    }
	}
    }

    return $self->error($usage)
	unless defined($username) and defined($passwd);

    ## Override these user set-able values.
    $endtime = &_endtime($timeout);
    $orig_timeout = $self->timeout($endtime);
    $orig_errmode = $self->errmode('return');
    
    ## Create a subroutine to reset to original values.
    $reset
	= sub {
	    $self->errmode($orig_errmode);
	    $self->timeout($orig_timeout);
	    1;
	};

    ## Create a subroutine to generate an error for user.
    $error
	= sub {
	    my($errmsg) = @_;

	    &$reset;
	    if ($self->timed_out) {
		return $self->error($errmsg);
	    }
	    elsif ($self->eof) {
		return $self->error($errmsg, ": ", $self->lastline);
	    }
	    else {
		return $self->error($self->errmsg);
	    }
	};

    ## Wait for login prompt.
    $self->waitfor(-match => '/login[: ]*$/i',
		   -match => '/username[: ]*$/i')
	or return &$error("login timed-out waiting for login prompt");

    ## Send login name.
    $self->print($username)
	or return &$error("login disconnected");

    ## Wait for password prompt.
    $self->waitfor(-match => '/password[: ]*$/i')
	or return &$error("login timed-out waiting for password prompt");

    ## Send password.
    $self->print($passwd)
	or return &$error("login disconnected");

    ## Wait for command prompt or another login prompt.
    ($prematch, $match) = $self->waitfor(-match => $cmd_prompt,
					 -match => '/login[: ]*$/i')
	or return &$error("login timed-out waiting for command prompt");
    
    ## Reset to orig values.
    &$reset;

    ## It's a bad login if we got another login prompt.
    return $self->error("login failed: bad name or password")
	if $match =~ /login[: ]*$/i or $match =~ '/username[: ]*$/i';

    1;
} # end sub login


sub max_buffer_length {
    my($self, $maxbufsize) = @_;
    my(
       $minbufsize,
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{maxbufsize};
    $minbufsize = 512;

    if (@_ >= 2) {
	## Ensure a valid max length.
	unless (defined $maxbufsize) {
	    $maxbufsize = $minbufsize;
	}

	## Test for non-numeric or negative values.
	eval {
	    local $^W = 1;
	    local $SIG{'__WARN__'} = sub { die "non-numeric\n" };
	    $maxbufsize *= 1;
	};
	if ($@ or $maxbufsize < $minbufsize) {
	    $maxbufsize = $minbufsize;
	}
	
	$stream->{maxbufsize} = $maxbufsize;
    }

    $prev;
} # end sub max_buffer_length


sub open {
    my($self) = @_;
    my(
       $blksize,
       $connected,
       $errno,
       $host,
       $ip_addr,
       $port,
       $stream,
       $timeout,
       %args,
       );

    ## Init vars.
    $stream = $ {*$self}{net_telnet};
    $timeout = $stream->{time_out};
    $stream->{timedout} = '';

    if (@_ == 2) {  # one positional arg given
	$self->host($_[1]);
    }
    elsif (@_ > 2) {  # named args given
	## Get the named args.
	(undef, %args) = @_;

	## Parse the named args.
	foreach (keys %args) {
	    if (/^-?host$/i) {
		$self->host($args{$_});
	    }
	    elsif (/^-?port$/i) {
		$self->port($args{$_})
		    or return;
	    }
	    elsif (/^-?timeout$/i) {
		$timeout = &_parse_timeout($args{$_});
	    }
	    else {
		return $self->error('usage: $obj->open([Host => $host,] ',
				    '[Port => $service,] ',
				    '[Timeout => secs,])');
	    }
	}
    }

    ## Get host and port.
    $host = $self->host;
    $port = $self->port;

    ## Ensure object is already closed.
    $self->close;

    ## Don't use a timeout if we can't use the alarm signal.
    unless (&_have_alarm) {
	$timeout = undef;
    }

    if (defined $timeout) {  # use a timeout
	## Ensure a valid timeout value for alarm.
	if ($timeout < 1) {
	    $timeout = 1;
	}
	$timeout = int($timeout + 1.5);
	
	## Connect to server, timing out if it takes too long.
	eval {
	    ## Turn on timer.
	    local $SIG{ALRM} = sub { die "timed-out\n" };
	    alarm $timeout;

	    ## Lookup server's IP address.
	    $ip_addr = inet_aton $host
		or die "unknown remote host: $host\n";

	    ## Create a socket and attach the filehandle to it.
	    socket $self, AF_INET, SOCK_STREAM, 0
		or die "problem creating socket: $!\n";

	    ## Open connection to server.
	    $connected = connect $self, sockaddr_in($port, $ip_addr)
		or die "problem connecting to \"$host\", port $port: $!\n";
	};
	alarm 0;

	## Check for error.
	if ($@ =~ /^timed-out$/) {  # time out failure
	    $stream->{timedout} = 1;
	    $self->close;
	    if (! $ip_addr) {
		return $self->error("unknown remote host: $host: ",
				    "name lookup timed-out");
	    }
	    else {
		return $self->error("problem connecting to \"$host\", ",
				    "port $port: connection timed-out");
	    }
	}
	elsif ($@) {  # hostname lookup or connect failure
	    $self->close;
	    chomp $@;
	    return $self->error($@);
	}
    }
    else {  # don't use a timeout
	## Lookup server's IP address.
	$ip_addr = inet_aton $host
	    or return $self->error("unknown remote host: $host");

	## Create a socket and attach the filehandle to it.
	socket $self, AF_INET, SOCK_STREAM, 0
	    or return $self->error("problem creating socket: $!");

	## Open connection to server.
	connect $self, sockaddr_in($port, $ip_addr)
	    or do {
		$errno = "$!";
		$self->close;
		return $self->error("problem connecting to \"$host\", ",
				    "port $port: $errno");
	    };
    }

    $self->autoflush;
    $blksize = (stat $self)[11];
    $stream->{blksize} = $blksize || $Default_blksize;
    $stream->{buf} = '';
    $stream->{eofile} = '';
    vec($stream->{fdmask}='', fileno($self), 1) = 1;
    $stream->{last_line} = '';
    $stream->{num_wrote} = '';
    $stream->{opened} = 1;
    $stream->{pushback_buf} = '';
    $stream->{timedout} = '';
    $stream->{unsent_opts} = '';
    1;
} # end sub open


sub output_field_separator {
    my($self, $ofs) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{ofs};

    if (@_ >= 2) {
	defined $ofs or $ofs = '';
	$stream->{ofs} = $ofs;
    }

    $prev;
} # end sub output_field_separator


sub output_log {
    my($self, $name) = @_;
    my(
       $fh,
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{outputlog};

    if (@_ >= 2) {
	$fh = &_fname_to_handle($self, $name);
	$stream->{outputlog} = $fh;
    }

    $prev;
} # end sub output_log


sub output_record_separator {
    my($self, $ors) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{ors};

    if (@_ >= 2) {
	defined $ors or $ors = '';
	$stream->{ors} = $ors;
    }

    $prev;
} # end sub output_record_separator


sub port {
    my($self, $port) = @_;
    my(
       $prev,
       $service,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{port};

    if (@_ >= 2) {
	return $self->error("bad port number: 0")
	    unless defined $port and $port;

	## Convert service to a port number.
	if ($port !~ /^\d+$/) {  # port isn't all digits
	    $service = $port;
	    $port = getservbyname($service, 'tcp')
		or return $self->error("unknown TCP service: $service");
	}

	$stream->{port} = $port;
    }

    $prev;
} # end sub port


sub print {
    my($self) = shift;
    my(
       $data,
       $endtime,
       $fh,
       $len,
       $nfound,
       $nwrote,
       $offset,
       $ready,
       $stream,
       $timed_out,
       $timeout,
       );

    $stream = $ {*$self}{net_telnet};
    $stream->{timedout} = '';
    $stream->{num_wrote} = 0;
    return $self->error("print failed: handle is closed")
	unless $stream->{opened};

    ## Try to send any waiting option negotiation.
    if (length $stream->{unsent_opts}) {
	&_flush_opts($self, $stream);
    }

    ## Add field and record separators.
    $data = join($stream->{ofs}, @_) . $stream->{ors};

    ## If requested, log the output.
    if ($stream->{outputlog}) {
	local $\ = '';
	$fh = $stream->{outputlog};
	$fh->print($data);
    }

    ## Convert newlines to carriage-return and newline.
    $data =~ s(\n)(\r\n)g
	unless $stream->{bin_mode};

    $offset = 0;
    $len = length $data;
    $endtime = &_endtime($stream->{time_out});
    while ($len) {
	## Set how long to wait for output ready.
	($timed_out, $timeout) = &_timeout_interval($endtime);
	if ($timed_out) {
	    $stream->{timedout} = 1;
	    return $self->error("print timed-out");
	}

	## Wait for output ready.
	$nfound = select '', $ready=$stream->{fdmask}, '', $timeout;
	if ($nfound > 0) {  # data can be written
	    if ($nwrote = syswrite $self, $data, $len, $offset) {
		## If requested, display network traffic.
		($stream->{dumplog})
		    and &_dump_data('>', $stream->{dumplog},
				    \$data, $offset, $nwrote);

		$stream->{num_wrote} += $nwrote;
		$offset += $nwrote;
		$len -= $nwrote;
		next;
	    }
	    elsif (! defined $nwrote) {  # write failed
		next if $! =~ /^Interrupted/;
		
		$stream->{opened} = '';
		return $self->error("unexpected write error: $!");
	    }
	    else {  # zero chars written
		$stream->{opened} = '';
		return $self->error("unexpected zero length write error: $!");
	    }
	}
	elsif ($nfound < 0) {  # select failure
	    next if $! =~ /^Interrupted/;

	    ## Failure equates to eof.
	    $stream->{opened} = '';
	    return $self->error("unexpected write error: $!");
	}
	else {  # timed-out
	    $stream->{timedout} = 1;
	    return $self->error("print timed-out");
	}
    }

    1;
} # end sub print


sub print_length {
    my($self) = @_;

    $ {*$self}{net_telnet}{num_wrote};
} # end sub print_length


sub prompt {
    my($self, $prompt) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{cmd_prompt};

    ## Parse args.
    if (@_ == 2) {
	defined $prompt or $prompt = '';
	return $self->error("bad match operator: ",
			    "opening delimiter missing: $prompt")
	    unless $prompt =~ m(^\s*/) or $prompt =~ m(^\s*m\s*\W);

	$stream->{cmd_prompt} = $prompt;
    }
    elsif (@_ > 2) {
	return $self->error('usage: $obj->prompt($match_op)');
    }

    $prev;
} # end sub prompt


sub telnetmode {
    my($self, $mode) = @_;
    my(
       $prev,
       $stream,
       );

    ## With no args, turn on telnet mode.
    if (@_ < 2) {
	$mode = 1;
    }
    else {
	defined $mode or $mode = '';
    }

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{telnet_mode};
    $stream->{telnet_mode} = $mode;
    $prev;
} # end sub telnetmode


sub timed_out {
    my($self, $value) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{timedout};

    if (@_ >= 2) {
	defined $value or $value = '';
	$stream->{timedout} = $value;
    }

    $prev;
} # end sub timed_out


sub timeout {
    my($self, $timeout) = @_;
    my(
       $prev,
       $stream,
       );

    $stream = $ {*$self}{net_telnet};
    $prev = $stream->{time_out};

    if (@_ >= 2) {
	$stream->{time_out} = &_parse_timeout($timeout);
    }

    $prev;
} # end sub timeout


sub waitfor {
    my($self, @args) = @_;
    my(
       $arg,
       $endtime,
       $len,
       $match,
       $match_op,
       $pos,
       $prematch,
       $search,
       $search_cond,
       $stream,
       $string,
       $timeout,
       @match_cond,
       @match_ops,
       @search_cond,
       @string_cond,
       @warns,
       );

    ## Init vars.
    $stream = $ {*$self}{net_telnet};
    $timeout = $stream->{time_out};
    $stream->{timedout} = '';
    return if $stream->{eofile};
    return unless @args;

    ## Code template used build conditional to match a string.
    ## Values between array elements must be supplied later.
    @string_cond =
	('if (($pos = index $stream->{buf}, ', ') > -1) {
	    $len = ', ';
	    $prematch = substr $stream->{buf}, 0, $pos;
	    $match = substr $stream->{buf}, $pos, $len;
	    substr($stream->{buf}, 0, $pos + $len) = "";
	    last;
	}');

    ## Code template used build conditional to match a pattern.
    ## Values between array elements must be supplied later.
    @match_cond =
	('if ($stream->{buf} =~ ', ') {
	    $prematch = $`;
	    $match = $&;
	    substr($stream->{buf}, 0, length($`) + length($&)) = "";
	    last;
	}');

    ## Parse args.
    if (@_ == 2) {  # one positional arg given
	$arg = $_[1];
	return $self->error("bad match operator: ",
			    "opening delimiter missing: $arg")
	    unless $arg =~ m(^\s*/) or $arg =~ m(^\s*m\s*\W);

	## Fill in the blanks in the code template.
	push @match_ops, $arg;
	push @search_cond, join('', $match_cond[0], $arg, $match_cond[1]);
    }
    elsif (@_ > 2) {  # named args given
	## Parse the named args.
	while (($_, $arg) = splice @args, 0, 2) {
	    if (/^-?timeout$/i) {
		$timeout = &_parse_timeout($arg);
	    }
	    elsif (/^-?match$/i) {
		return $self->error("bad match operator: ",
				    "opening delimiter missing: $arg")
		    unless $arg =~ m(^\s*/) or $arg =~ m(^\s*m\s*\W);

		## Fill in the blanks in the code template.
		push @match_ops, $arg;
		push @search_cond, join('',
					$match_cond[0], $arg, $match_cond[1]);
	    }
	    elsif (/^-?string$/i) {
		## Fill in the blanks in the code template.
		$arg =~ s/'/\\'/g;  # quote ticks
		push @search_cond, join('',
					$string_cond[0], "'$arg'",
					$string_cond[1], length($arg),
					$string_cond[2]);
	    }
	    else {
		return $self->error('usage: $obj->waitfor([Match => ',
				    '$match_op,] [String => $string,] ',
				    '[Timeout => $secs,])');
	    }
	}
    }

    ## Construct conditional to check for requested string and pattern matches.
    ## Turn subsequent "if"s into "elsif".
    $search_cond = join "\n\tels", @search_cond;

    ## Construct loop to fill buffer until string/pattern, timeout, or eof.
    $search = join '', "
    while (1) {\n\t",
	$search_cond, '
	&_fillbuf($self, $stream, $endtime)
	    or do {
		last if $stream->{timedout};
		$self->close;
		last;
	    };
    }';

    ## Set wall time when we timeout.
    $endtime = &_endtime($timeout);

    ## Run the loop.
    {
	local $^W = 1;
	local $SIG{'__WARN__'} = sub { push @warns, @_ };
	local $stream->{errormode} = 'return';
	$stream->{errormsg} = '';
	eval $search;
    }

    ## Check for failure.
    return $self->error("pattern timed-out") if $stream->{timedout};
    return $self->error($stream->{errormsg}) if $stream->{errormsg} ne '';
    return if $stream->{eofile};

    ## Check for Perl syntax errors or warnings.
    if ($@ or @warns) {
	foreach $match_op (@match_ops) {
	    &_match_check($self, $match_op)
		or return;
	}
	return $self->error($@) if $@;
	return $self->error(@warns) if @warns;
    }

    wantarray ? ($prematch, $match) : 1;
} # end sub waitfor


######################## Private Subroutines #########################


sub _append_lineno {
    my($obj, @msgs) = @_;
    my(
       $class,
       $curr_pkg,
       $file,
       $i,
       $line,
       $pkg,
       %isa,
       @isa,
       );


    ## Create a boolean hash to test for isa.  Make sure current
    ## package and the object's class are members.
    $class = ref $obj;
    ($curr_pkg) = caller 1;
    @isa = eval "\@${class}::ISA";
    push @isa, $class, $curr_pkg;
    %isa = map {$_ => 1} @isa;

    ## Search back in call frames for a package that's not in isa.
    $i = 1;
    while (($pkg, $file , $line) = caller ++$i) {
	next if $isa{$pkg};

	return join('', @msgs, " at ", $file, " line ", $line, "\n");
    }

    ## If not found, choose outer most call frame.
    ($pkg, $file , $line) = caller --$i;
    join('', @msgs, " at ", $file, " line ", $line, "\n");
} # end sub _append_lineno


sub _carp {
    warn &_append_lineno(@_);
} # end sub _carp


sub _croak {
    die &_append_lineno(@_);
} # end sub _croak


sub _dump_data {
    my($direction, $fh, $data, $offset, $len) = @_;
    my(
       $addr,
       $hexvals,
       $line,
       );

    $addr = 0;
    $len = length($$data) - $offset
	unless defined $len;
    
    ## Print data in dump format.
    while ($len > 0) {
	## Convert up to the next 16 chars to hex, padding w/ spaces.
	if ($len >= 16) {
	    $line = substr $$data, $offset, 16;
	}
	else {
	    $line = substr $$data, $offset, $len;
	}
	$hexvals = unpack('H*', $line);
	$hexvals .= ' ' x (32 - length $hexvals);

	## Place in 16 columns, each containing two hex digits.
	$hexvals = sprintf("%s %s %s %s  " x 4,
			   unpack('a2' x 16, $hexvals));
	
	## For the ASCII column, change unprintable chars to a period.
	$line =~ s/[\000-\037,\177-\237]/./g;

	## Print the line in dump format.
	printf($fh "%s 0x%5.5lx: %s%s\n", $direction, $addr, $hexvals, $line);

	$addr += 16;
	$offset += 16;
	$len -= 16;
    }

    1;
} # end sub _dump_data


sub _endtime {
    my($interval) = @_;

    ## Compute wall time when timeout occurs.
    if (defined $interval) {
	if ($interval >= $^T) {  # it's already an absolute time
	    return $interval;
	}
	elsif ($interval > 0) {  # it's relative to the current time
	    return int(time + 1.5 + $interval);
	}
	else {  # it's a one time poll
	    return 0;
	}
    }
    else {  # there's no timeout
	return undef;
    }
} # end sub _endtime


sub _fillbuf {
    my($self, $s, $endtime) = @_;
    my(
       $fh,
       $firstpos,
       $lastpos,
       $len_w_sep,
       $len_wo_sep,
       $nextchar,
       $nfound,
       $nread,
       $offset,
       $pos,
       $pushback_len,
       $ready,
       $timed_out,
       $timeout,
       );

    return unless $s->{opened};

    while (1) {
	## Ensure we haven't exceeded maximum buffer size.
	return $self->error("maximum input buffer length exceeded: ",
			    $s->{maxbufsize}, " bytes")
	    unless length($s->{buf}) <= $s->{maxbufsize};

	## Set how long to wait for input ready.
	($timed_out, $timeout) = &_timeout_interval($endtime);
	if ($timed_out) {
	    $s->{timedout} = 1;
	    return $self->error("read timed-out");
	}

	## Wait for input ready.
	$nfound = select $ready=$s->{fdmask}, '', '', $timeout;
	if ($nfound > 0) {  # data can be read
	    ## Append any partially read telnet char sequence.
	    $pushback_len = length $s->{pushback_buf};
	    if ($pushback_len) {
		$s->{buf} .= $s->{pushback_buf};
		$s->{pushback_buf} = '';
	    }

	    ## Do the read.
	    $offset = length $s->{buf};
	    if ($nread = sysread $self, $s->{buf}, $s->{blksize}, $offset) {
		## If requested, display network traffic.
		($s->{dumplog})
		    and &_dump_data('<', $s->{dumplog}, \$s->{buf}, $offset);
		
		## Process any telnet commands in the data stream.
		if ($s->{telnet_mode}
		    and index($s->{buf}, "\377", $offset - $pushback_len) > -1)
		{
		    &_interpret_cmd($self, $s, $offset - $pushback_len);
		}

		## Process carriage-return sequences in the data stream.
		$pos = $offset - $pushback_len;
		while (($pos = index($s->{buf}, "\r", $pos)) > -1) {
		    $nextchar = substr($s->{buf}, $pos + 1, 1);
		    if ($nextchar eq "\0") {
			## Convert \r\0 to \r
			substr($s->{buf}, $pos + 1, 1) = '';
		    }
		    elsif ($nextchar eq "\n") {
			## Convert \r\n to \n when not in binary mode.
			substr($s->{buf}, $pos, 1) = ''
			    unless $s->{bin_mode};
		    }
		    elsif (! length $nextchar) {
			$s->{pushback_buf} .= "\r";
			chop $s->{buf};
		    }

		    $pos++;
		}
		
		next if length $s->{buf} <= $offset;

		## If requested, log the input.
		if ($s->{inputlog}) {
		    local $\ = '';
		    $fh = $s->{inputlog};
		    $fh->print(substr $s->{buf}, $offset);
		}

		## Save last line in the buffer.
		if (($lastpos = rindex $s->{buf}, $s->{rs}) > -1) {
		    while (1) {
			## Find beginning of line.
			$firstpos = rindex $s->{buf}, $s->{rs}, $lastpos - 1;
			if ($firstpos == -1) {
			    $offset = 0;
			}
			else {
			    $offset = $firstpos + length $s->{rs};
			}

			## Determine length of line with and without separator.
			$len_wo_sep = $lastpos - $offset;
			$len_w_sep = $len_wo_sep + length $s->{rs};

			## Save line if it's not blank.
			if (substr($s->{buf}, $offset, $len_wo_sep)
			    !~ /^\s*$/)
			{
			    $s->{last_line} = substr($s->{buf},
						     $offset,
						     $len_w_sep);
			    last;
			}

			last if $firstpos == -1;

			$lastpos = $firstpos;
		    }
		}

		return 1;
	    }
	    elsif (! defined $nread) {  # read failed
		next if $! =~ /^Interrupted/;
		
		$s->{opened} = '';
		return $self->error("unexpected read error: $!");
	    }
	    else {  # read end-of-file
		$s->{opened} = '';
		return;
	    }
	}
	elsif ($nfound < 0) {  # select failure
	    next if $! =~ /^Interrupted/;

	    ## Failure equates to eof.
	    $s->{opened} = '';
	    return $self->error("unexpected read error: $!");
	}
	else {  # timed-out
	    $s->{timedout} = 1;
	    return $self->error("read timed-out");
	}
    }
} # end sub _fillbuf


sub _flush_opts {
    my($self, $s) = @_;
    my(
       $option_chars,
       );

    ## Get option and clear the output buf.
    $option_chars = $s->{unsent_opts};
    $s->{unsent_opts} = '';

    ## Try to send options without waiting.
    {
	local $s->{errormode} = 'return';
	local $s->{time_out} = 0;
	local $s->{ors} = '';
	$self->print($option_chars)
	    or do {
		## Save chars not printed for later.
		substr($option_chars, 0, $self->print_length) = '';
		$s->{unsent_opts} .= $option_chars;
	    };
    }

    1;
} # end sub _flush_opts


sub _fname_to_handle {
    my($self, $fh) = @_;
    my(
       $filename,
       );

    ## Default is off.
    if (!defined $fh or !length $fh) {
	return '';
    }

    ## Assume arg is a filename if it's not an open filehandle.
    no strict 'refs';
    if (!defined fileno($fh)) {
	$filename = $fh;
	$fh = &_new_handle();
	open $fh, ">$filename"
	    or do {
		&_carp($self, "problem creating $filename: $!");
		return '';
	    };
    }

    $fh->autoflush;
    $fh;
} # end sub _fname_to_handle


sub _have_alarm {
    eval {
	alarm 0;
	local $SIG{ALRM} = sub { die };
    };

    ! $@;
} # end sub _have_alarm


sub _interpret_cmd {
    my($self, $s, $offset) = @_;
    my(
       $endpos,
       $nextchar,
       $option,
       $pos,
       );

    ## Parse telnet commands in the data stream.
    $pos = $offset;
    while (($pos = index $s->{buf}, "\377", $pos) > -1) {  # unprocessed IAC
	$nextchar = substr $s->{buf}, $pos + 1, 1;

	## Save command if it's only partially read.
	if (! length $nextchar) {
	    $s->{pushback_buf} .= "\377";
	    chop $s->{buf};
	    last;
	}

	if ($nextchar eq "\377") {  # IAC is escaping "\377" char
	    ## Remove escape char from data stream.
	    substr($s->{buf}, $pos, 1) = '';
	    $pos++;
	}
	elsif ($nextchar eq "\375" or $nextchar eq "\373" or
	       $nextchar eq "\374" or $nextchar eq "\376") {  # opt negotiation
	    $option = substr $s->{buf}, $pos + 2, 1;

	    ## Save command if it's only partially read.
	    if (! length $option) {
		$s->{pushback_buf} .= "\377" . $nextchar;
		chop $s->{buf};
		chop $s->{buf};
		last;
	    }

	    ## Remove command from data stream.
	    substr($s->{buf}, $pos, 3) = '';

	    ## Ignore all options except "DO" and "WILL".
	    if ($nextchar eq "\375") {  # DO
		## Indicate we "won't" do this option request.
		$s->{unsent_opts} .= "\377\374$option";
	    }
	    elsif ($nextchar eq "\373") {  # WILL
		## Indicate we "don't" do this option request.
		$s->{unsent_opts} .= "\377\376$option";
	    }
	}
	elsif ($nextchar eq "\372") {  # start of subnegotiation parameters
	    ## Save command if it's only partially read.
	    $endpos = index $s->{buf}, "\360", $pos;
	    if ($endpos == -1) {
		$s->{pushback_buf} .= substr $s->{buf}, $pos;
		substr($s->{buf}, $pos) = '';
		last;
	    }
	    
	    ## Ignore subnegotiation cmd.
	    substr($s->{buf}, $pos, $endpos - $pos + 1) = '';
	}
	else {  # various two char telnet commands
	    ## Ignore and remove command from data stream.
	    substr($s->{buf}, $pos, 2) = '';
	}
    }

    ## Try to send any waiting option negotiation.
    if (length $s->{unsent_opts}) {
	&_flush_opts($self, $s);
    }

    1;
} # end sub _interpret_cmd


sub _match_check {
    my($self, $code) = @_;
    my $error;
    my @warns = ();

    ## Use eval to check for syntax errors or warnings.
    {
	local $^W = 1;
	local $SIG{'__WARN__'} = sub { push @warns, @_ };
	local $_ = '';
	eval "\$_ =~ $code;";
    }
    if ($@) {
	## Remove useless lines numbers from message.
	($error = $@) =~ s/ at \(eval \d+\) line \d+.?//;
	chomp $error;
	return $self->error("bad match operator: $error");
    }
    elsif (@warns) {
	## Remove useless lines numbers from message.
	($error = shift @warns) =~ s/ at \(eval \d+\) line \d+.?//;
	$error =~ s/ while "strict subs" in use//;
	chomp $error;
	return $self->error("bad match operator: $error");
    }

    1;
} # end sub _match_check


sub _new_handle {
    if ($INC{'IO/Handle.pm'}) {
	return IO::Handle->new;
    }
    else {
	require FileHandle;
	return FileHandle->new;
    }
} # end sub _new_handle


sub _parse_timeout {
    my($timeout) = @_;

    ## Ensure valid timeout.
    if (defined $timeout) {
	## Test for non-numeric or negative values.
	eval {
	    local $^W = 1;
	    local $SIG{'__WARN__'} = sub { die "non-numeric\n" };
	    $timeout *= 1;
	};
	if ($@) {  # timeout arg is non-numeric
	    $timeout = undef;
	}
	elsif ($timeout < 0) {
	    $timeout = undef;
	}
    }

    $timeout;
} # end sub _parse_timeout


sub _timeout_interval {
    my($endtime) = @_;
    my(
       $timeout,
       );

    ## Return timed-out boolean and timeout interval.
    if (defined $endtime) {
	## Is it a one-time poll.
	return ('', 0) if $endtime == 0;

	## Calculate the timeout interval.
	$timeout = $endtime - time;

	## Did we already timeout.
	return (1, 0) unless $timeout > 0;

	return ('', $timeout);
    }
    else {  # there is no timeout
	return ('', undef);
    }
} # end sub _timeout_interval


######################## Exported Constants ##########################


use vars qw(@EXPORT_OK);
@EXPORT_OK = qw(TELNET_IAC TELNET_DONT TELNET_DO TELNET_WONT
		TELNET_WILL TELNET_SB TELNET_GA TELNET_EL TELNET_EC
		TELNET_AYT TELNET_AO TELNET_IP TELNET_BREAK TELNET_DM
		TELNET_NOP TELNET_SE TELNET_EOR TELNET_ABORT
		TELNET_SUSP TELNET_EOF TELNET_SYNCH TELOPT_BINARY
		TELOPT_ECHO TELOPT_RCP TELOPT_SGA TELOPT_NAMS
		TELOPT_STATUS TELOPT_TM TELOPT_RCTE TELOPT_NAOL
		TELOPT_NAOP TELOPT_NAOCRD TELOPT_NAOHTS TELOPT_NAOHTD
		TELOPT_NAOFFD TELOPT_NAOVTS TELOPT_NAOVTD
		TELOPT_NAOLFD TELOPT_XASCII TELOPT_LOGOUT TELOPT_BM
		TELOPT_DET TELOPT_SUPDUP TELOPT_SUPDUPOUTPUT
		TELOPT_SNDLOC TELOPT_TTYPE TELOPT_EOR TELOPT_TUID
		TELOPT_OUTMRK TELOPT_TTYLOC TELOPT_3270REGIME
		TELOPT_X3PAD TELOPT_NAWS TELOPT_TSPEED TELOPT_LFLOW
		TELOPT_LINEMODE TELOPT_XDISPLOC TELOPT_OLD_ENVIRON
		TELOPT_AUTHENTICATION TELOPT_ENCRYPT
		TELOPT_NEW_ENVIRON TELOPT_EXOPL SLC_SYNCH SLC_BRK
		SLC_IP SLC_AO SLC_AYT SLC_EOR SLC_ABORT SLC_EOF
		SLC_SUSP SLC_EC SLC_EL SLC_EW SLC_RP SLC_LNEXT SLC_XON
		SLC_XOFF SLC_FORW1 SLC_FORW2
		);

sub TELNET_IAC		{255};	# interpret as command:
sub TELNET_DONT		{254};	# you are not to use option
sub TELNET_DO		{253};	# please, you use option
sub TELNET_WONT		{252};	# I won't use option
sub TELNET_WILL		{251};	# I will use option
sub TELNET_SB		{250};	# interpret as subnegotiation
sub TELNET_GA		{249};	# you may reverse the line
sub TELNET_EL		{248};	# erase the current line
sub TELNET_EC		{247};	# erase the current character
sub TELNET_AYT		{246};	# are you there
sub TELNET_AO		{245};	# abort output--but let prog finish
sub TELNET_IP		{244};	# interrupt process--permanently
sub TELNET_BREAK	{243};	# break
sub TELNET_DM		{242};	# data mark--for connect. cleaning
sub TELNET_NOP		{241};	# nop
sub TELNET_SE		{240};	# end sub negotiation
sub TELNET_EOR		{239};	# end of record (transparent mode)
sub TELNET_ABORT	{238};	# Abort process
sub TELNET_SUSP		{237};	# Suspend process
sub TELNET_EOF		{236};	# End of file: EOF is already used...
sub TELNET_SYNCH	{242};	# for telfunc calls

sub TELOPT_BINARY	  {0};	# 8-bit data path
sub TELOPT_ECHO		  {1};	# echo
sub TELOPT_RCP		  {2};	# prepare to reconnect
sub TELOPT_SGA		  {3};	# suppress go ahead
sub TELOPT_NAMS		  {4};	# approximate message size
sub TELOPT_STATUS	  {5};	# give status
sub TELOPT_TM		  {6};	# timing mark
sub TELOPT_RCTE		  {7};	# remote controlled transmission and echo
sub TELOPT_NAOL		  {8};	# negotiate about output line width
sub TELOPT_NAOP		  {9};	# negotiate about output page size
sub TELOPT_NAOCRD	  {10}; # negotiate about CR disposition
sub TELOPT_NAOHTS	  {11}; # negotiate about horizontal tabstops
sub TELOPT_NAOHTD	  {12}; # negotiate about horizontal tab disposition
sub TELOPT_NAOFFD	  {13}; # negotiate about formfeed disposition
sub TELOPT_NAOVTS	  {14}; # negotiate about vertical tab stops
sub TELOPT_NAOVTD	  {15}; # negotiate about vertical tab disposition
sub TELOPT_NAOLFD	  {16}; # negotiate about output LF disposition
sub TELOPT_XASCII	  {17}; # extended ascic character set
sub TELOPT_LOGOUT	  {18}; # force logout
sub TELOPT_BM		  {19}; # byte macro
sub TELOPT_DET		  {20}; # data entry terminal
sub TELOPT_SUPDUP	  {21}; # supdup protocol
sub TELOPT_SUPDUPOUTPUT	  {22}; # supdup output
sub TELOPT_SNDLOC	  {23}; # send location
sub TELOPT_TTYPE	  {24}; # terminal type
sub TELOPT_EOR		  {25}; # end or record
sub TELOPT_TUID		  {26}; # TACACS user identification
sub TELOPT_OUTMRK	  {27}; # output marking
sub TELOPT_TTYLOC	  {28}; # terminal location number
sub TELOPT_3270REGIME	  {29}; # 3270 regime
sub TELOPT_X3PAD	  {30}; # X.3 PAD
sub TELOPT_NAWS		  {31}; # window size
sub TELOPT_TSPEED	  {32}; # terminal speed
sub TELOPT_LFLOW	  {33}; # remote flow control
sub TELOPT_LINEMODE	  {34}; # Linemode option
sub TELOPT_XDISPLOC	  {35}; # X Display Location
sub TELOPT_OLD_ENVIRON	  {36}; # Old - Environment variables
sub TELOPT_AUTHENTICATION {37}; # Authenticate
sub TELOPT_ENCRYPT	  {38}; # Encryption option
sub TELOPT_NEW_ENVIRON	  {39}; # New - Environment variables
sub TELOPT_EXOPL	 {255}; # extended-options-list

sub SLC_SYNCH		   {1};
sub SLC_BRK		   {2};
sub SLC_IP		   {3};
sub SLC_AO		   {4};
sub SLC_AYT		   {5};
sub SLC_EOR		   {6};
sub SLC_ABORT		   {7};
sub SLC_EOF		   {8};
sub SLC_SUSP		   {9};
sub SLC_EC		  {10};
sub SLC_EL		  {11};
sub SLC_EW		  {12};
sub SLC_RP		  {13};
sub SLC_LNEXT		  {14};
sub SLC_XON		  {15};
sub SLC_XOFF		  {16};
sub SLC_FORW1		  {17};
sub SLC_FORW2		  {18};

1;
__END__;


########################### Documentation ############################


=head1 NAME

Net::Telnet - interact with TELNET port or other TCP ports

=head1 SYNOPSIS

    use Net::Telnet ();
    see METHODS section below

=head1 DESCRIPTION

Net::Telnet allows you to make client connections to a TCP port and do
network I/O, especially with a port using the TELNET protocol.  Simple
I/O methods such as print, get, and getline are provided.  More
sophisticated interactive features are provided because connecting to
a TELNET port ultimately means communicating with a program designed
for human interaction.  Some interactive features include the ability
to specify a timeout and to wait for patterns to appear in the input
stream, such as the prompt from a command interpreter.

This example prints who's logged-on to the remote host sparky:

    $sparky = new Net::Telnet (Host => "sparky",
                               Timeout => 10,
                               Prompt => '/[$%#>] $/');
    $sparky->login($username, $passwd);
    @lines = $sparky->cmd("/usr/bin/who");
    print @lines;
    $sparky->close;

Methods B<login()> and B<cmd()> use the prompt setting in the object
to determine when a login or command is complete.  If the prompt
doesn't match, it's likely those commands will timeout.

Other reasons to use this class than strictly with a TELNET port are:

=over 2

=item *

You're not familiar with sockets and you want a simple way to make
client connections to TCP services.

=item *

You want to be able to specify your own time-out while connecting,
reading, or writing.

=item *

You're communicating with an interactive program at the other end of
some socket or pipe and you want to wait for certain patterns to
appear.

=back

B<Please note> some important differences with most other Perl I/O
calls.  All input is buffered, while all output is flushed.  The
output record separator for B<print()> is set to B<\n> by default, so
there's no need to append all your commands with a newline.  See
B<output_record_separator()> to change the default.  In the input
stream, each sequence of B<\r\n> is converted to B<\n>.  In the output
stream, each occurrence of B<\n> is converted to a sequence of
B<\r\n>.  See B<binmode()> to change the default.  TCP protocols
typically use the ASCII sequence I<carriage-return> I<newline> to
designate a newline.

You'll need to be running at least Perl version 5.002 to use this
module.  This module does not require any libraries that don't already
come with the standard Perl distribution.  If you have the IO::
libraries then methods are inherited from the class IO::Socket::INET,
otherwise FileHandle is used as a base class.

Special methods are provided to handle errors.  Normally when an error
or timeout is encountered using a telnet object, the program dies with
an error message printed to standard error.  You may arrange for the
methods to return with an undefined value instead by using
B<errmode()> or the B<errmode> option to B<new()>.  See B<errmode()>
for other sophisticated error mode settings.  The error message itself
may be obtained using the B<errmsg()>.

Note that I<eof> is not considered an error while I<timing-out> is.

While debugging your program use B<input_log()> or B<dump_log()> to
see what's actually being received and sent.

Two different styles of named arguments are supported.  This document
only shows the IO:: style:

    Net::Telnet->new(Timeout => 20);

however the dash-option style is also allowed:

    Net::Telnet->new(-timeout => 20);

For more help, see the B<EXAMPLES> section below.

This is an alpha version - meaning that the interface may change in
future versions.  Contact me, Jay Rogers <jay@rgrs.com>, if you find
any bugs or have suggestions for improvement.


=head1 METHODS


=head2 new - create a new Net::Telnet object

    $obj = Net::Telnet->new([Binmode    => $mode,]
                            [Dump_Log   => $filename,]
                            [Errmode    => $errmode,]
                            [Fhopen     => $filehandle,]
                            [Host       => $host,]
                            [Input_log  => $file,]
                            [Input_record_separator => $char,]
                            [Output_log => $file,]
                            [Output_record_separator => $char,]
                            [Port       => $port,]
                            [Prompt     => $matchop,]
                            [Telnetmode => $mode,]
                            [Timeout    => $secs,]);

This is the constructor for Net::Telnet objects.  A new object is
returned on success, the I<$errmode> action is performed on failure -
see B<errmode()>.  The arguments are short-cuts to methods of the same
name.

If the I<$host> argument is given then the object is opened by
connecting to TCP I<$port> on I<$host>.  Also see B<open()>.  The new
object returned is given the following defaults in the absence of
corresponding named arguments:

=over 2

=item *

The default B<host> is B<"localhost">

=item *

The default B<port> is B<23>

=item *

The default B<prompt> is B<'/[$%#>>B<] $/'>

=item *

The default B<timeout> is B<10>

=item *

The default B<errmode> is B<'die'>

=item *

The default B<output_record_separator> is B<"\n">

=item *

The default B<input_record_separator> is B<"\n">

=item *

The default B<binmode> is B<0>, which means do newline translations

=back

=head2 binmode - turn off/on newline translation

    $prev = $obj->binmode($mode);

This method controls whether or not sequences of B<\r\n> are
translated.  By default they are translated (i.e. binmode is I<off>).

If I<$mode> is missing or B<1> then binmode is I<on> and newline
translation is not done.

If I<$mode> is B<0> then binmode is I<off> and newline translation is
done.  In the input stream, each sequence of B<\r\n> is converted to
B<\n> and in the output stream, each occurrence of B<\n> is converted
to a sequence of B<\r\n>.

Note that input is always buffered.  Changing binmode doesn't effect
what's already been read into the buffer.  Output is not buffered and
changing binmode will have an immediate effect.


=head2 break - send TELNET break character

    $ok = $obj->break;

This method sends the TELNET break character.  This character is
provided because it's a signal outside the USASCII set which is
currently given local meaning within many systems.  It's intended to
indicate that the Break Key or the Attention Key was hit.


=head2 close - close object

    $ok = $obj->close;

This method closes the socket, file, or pipe associated with the
object.


=head2 cmd - issue command and retrieve output

    $ok = $obj->cmd($string);
    $ok = $obj->cmd(String   => $string,
                    [Output  => $ref,]
                    [Prompt  => $match,]
                    [Timeout => $secs,]);

    @output = $obj->cmd($string);
    @output = $obj->cmd(String  => $string,
                        [Output  => $ref,]
                        [Prompt  => $match,]
                        [Timeout => $secs,]);

This method sends the command I<$string>, and reads the characters
sent back by the command up until and including the matching prompt.
It's assumed that the program to which you're sending is some kind of
command prompting interpreter such as a shell.

In a scalar context the characters read are discarded and a boolean is
returned indicating the success or failure of sending the command
string and reading the prompt.  Note that in order to return on error,
B<errmode()> must not be set to I<die>.

In an array context, just the output generated by the command is
returned, one line per element.  In other words, all the characters in
between the echoed back command string and the prompt are returned.
If the command happens to return no output, an array containing one
element, the null string is returned.  This is so the array will
indicate I<true> in a boolean context.

Optional named arguments are provided to override the current settings
of prompt and timeout.

The B<output> named argument provides an alternative method of
receiving command output.  If you pass a scalar reference, the output
is returned in the referenced scalar.  If you pass an array or hash
reference, the lines of output are returned in the referenced array or
hash.


=head2 dump_log - log all I/O in dump format

    $fh = $obj->dump_log;

    $fh = $obj->dump_log($fh);

    $fh = $obj->dump_log($filename);

This method starts or stops dump format logging of all the object's
input and output.  The dump format shows the blocks read and written
in a hexadecimal and printable character format.  This method is
useful when debugging, however you might want to first try
B<input_log()> as it's more readable.

If no argument is given, the current log filehandle is returned.  A
null string indicates logging is off.

To stop logging, use a null string as an argument.

If an open filehandle is given, it is used for logging and returned.
Otherwise, the argument is assumed to be the name of a file, the file
is opened and a filehandle to it is returned.


=head2 eof - end of file read indicator

    $eof = $obj->eof;

This method indicates if end of file has been read.  Because the input
is buffered this isn't the same thing as I<$obj> has closed.  In other
words I<$obj> can be closed but there still can be stuff in the buffer
to be read.  Under this condition you can still read but you won't be
able to write.


=head2 errmode - set action to perform on error

    $mode = $obj->errmode;

    $prev = $obj->errmode($mode);

This method gets or sets the action used when errors are encountered
using the object.  The first calling sequence returns the current
error mode.  The second calling sequence sets it to I<$mode> and
returns the previous mode.  Valid values for I<$mode> are B<die> (the
default), B<return>, a I<coderef>, or an I<arrayref>.

When mode is B<die> then when an error is encountered using the
object, the program dies and an error message is printed on standard
error.

When mode is B<return> then the method generating the error places an
error message in the object and returns the undefined value in a
scalar context and a null list in list context.  The error message may
be obtained using B<errmsg()>.

When mode is a I<coderef>, then when an error is encountered
I<coderef> is called with the error message as its first argument.
Using this mode you may have your own subroutine handle errors.  If
I<coderef> itself returns then the method generating the error returns
undefined or a null list depending on context.

When mode is an I<arrayref>, the first element of the array must be a
I<coderef>.  Any elements that follow are the arguments to I<coderef>.
When an error is encountered, the I<coderef> is called with its
arguments.  Using this mode you may have your own subroutine handle
errors.  If the I<coderef> itself returns then the method generating
the error returns undefined or a null list depending on context.


=head2 errmsg - most recent error message

    $msg = $obj->errmsg;

    $prev = $obj->errmsg(@msgs);

The first calling sequence returns the error message associated with
the object.  The null string is returned if no error has been
encountered yet.  The second calling sequence sets the error message
for the object to the concatenation of I<@msgs> and returns the
previous error message.  Normally, error messages are set internally
by a method when an error is encountered.


=head2 error - perform the error mode action

    $obj->error(@msgs);

This method concatenates I<@msgs> into a string and places it in the
object as the error message.  Also see B<errmsg()>.  It then performs
the error mode.  Also see B<errmode()>.

If the error mode doesn't cause the program to die then the undefined
value or a null list is returned depending on context.

This method is primarily used by this class or a sub-class to perform
the user requested action when an error is encountered.


=head2 fhopen - use an existing open filehandle

    $ok = $obj->fhopen($fh);

This method associates the open filehandle I<$fh> with the object for
further I/O.

This method provides a way to use this module with a filehandle that's
already opened.  Suppose you want to use the features of this module
to do I/O to something other than a TCP port.  Instead of opening the
object for I/O to a TCP port by passing a B<host> arg to B<new()> or
invoking B<open()>, call this method instead.


=head2 get - read block of data

    $data = $obj->get([Timeout => $secs,]);

This method reads a block of data from the object and returns it along
with any buffered data.  If no buffered data is available to return,
it will wait for data to read using the timeout specified in the
object.  You can override that timeout using I<$secs>.  Also see
B<timeout()>.  If buffered data is available to return, it also checks
for a block of data that can be immediately read.

On eof an undefined value is returned.  On timeout or other errors the
error mode action is performed.


=head2 getline - read next line

    $line = $obj->getline([Timeout => $secs,]);

This method reads and returns the next line of data from the object.
You can use B<input_record_separator()> to change the notion of what
separates a line.  The default is B<\n>.

If a line isn't immediately available, this method blocks waiting for
a line or the timeout.  You can override the object's timeout for this
method using I<$secs>.  Also see B<timeout()>.

On eof an undefined value is returned.  On timeout or other errors the
error mode action is performed.


=head2 getlines - read next lines

    @lines = $obj->getlines([Timeout => $secs,]);

This method reads and returns the next available lines of data from
the object.  You can use B<input_record_separator()> to change the
notion of what separates a line.  The default is B<\n>.

If a line isn't immediately available, this method blocks waiting for
one or more lines, or the timeout.  You can override the object's
timeout for this method using I<$secs>.  Also see B<timeout()>.

On eof a null array is returned.  On timeout or other errors the error
mode action is performed.


=head2 host - name of remote host

    $host = $obj->host;

    $prev = $obj->host($host);

This method designates the remote host.  With no argument this method
returns the current host name set in the object.  With an argument it
sets the current host name to I<$host> and returns the previous host
name.  You may indicate the remote host using either a hostname or an
IP address.


=head2 input_log - log all input

    $fh = $obj->input_log;

    $fh = $obj->input_log($fh);

    $fh = $obj->input_log($filename);

This method starts or stops logging of input.  This is useful when
debugging.  Also see B<dump_log()>.  Because most command interpreters
echo back commands received, its likely all your output will also be
in this log.  Note that input logging occurs after newline
translation.  See B<binmode()> for details on newline translation.

If no argument is given, the log filehandle is returned.  A null
string indicates logging is off.

To stop logging, use a null string as an argument.

If an open filehandle is given, it is used for logging and returned.
Otherwise, the argument is assumed to be the name of a file, the file
is opened for logging and a filehandle to it is returned.


=head2 input_record_separator - input line delimiter

    $rs = $obj->input_record_separator;

    $prev = $obj->input_record_separator($rs);

This method designates the line delimiter for input.  It's used with
B<getline()>, B<getlines()>, and B<cmd()> to determine lines in the
input.

With no argument this method returns the current input record
separator set in the object.  With an argument it sets the input
record separator to I<$rs> and returns the previous value.


=head2 lastline - the lastline read

    $line = $obj->lastline;

    $prev = $obj->lastline($line);

This method saves the last line read from the object.  This may be a
useful error message when the remote side abnormally closes the
connection.  Typically the remote side will print an error message
before closing.

With no argument this method returns the last line read from the
object.  With an argument it sets the last line read to I<$line> and
returns the previous value.  Normally, only internal methods set the
last line.


=head2 login - perform standard login

    $ok = $obj->login($username, $password);

    $ok = $obj->login(Name     => $username,
                      Password => $password,
                      [Prompt  => $match,]
                      [Timeout => $secs,]);

This method performs a standard login by waiting for a login prompt and
responding with I<$username>, then waiting for the password prompt and
responding with I<$password>, and then waiting for the command
interpreter prompt.  If any of the prompts sent don't match what's
expected, the method will timeout - unless timeout is turned off.

Login prompts must match either of the patterns:

    /login[: ]*$/i
    /username[: ]*$/i

Password prompts must match the pattern:

    /password[: ]*$/i

The command interpreter prompt must match the current value of
B<prompt()>.

Optional named arguments are provided to override the current settings
of prompt and timeout.


=head2 max_buffer_length - maximum size of input buffer

    $len = $obj->max_buffer_length;

    $prev = $obj->max_buffer_length($len);

This method designates the maximum size of the input buffer.  An error
is generated when a read causes the buffer to exceed this limit.  The
default value is 1,048,576 bytes (1MB).  The input buffer can grow
much larger than the block size when you read using B<getline()> or
B<waitfor()> and the data stream contains no newlines or matching
waitfor patterns.

With no argument this method returns the current maximum buffer length
set in the object.  With an argument it sets the maximum buffer length
to I<$len> and returns the previous value.


=head2 open - connect to host and port

    $ok = $obj->open($host);

    $ok = $obj->open([Host    => $host,]
                     [Port    => $port,]
                     [Timeout => $secs,]);

This method opens a TCP connection to I<$port> on I<$host>.  If either
argument is missing then the current value of B<host()> or B<port()>
is used.

An optional named argument is provided to override the current setting
of timeout.

Timeouts don't work for this method on machines that don't implement
SIGALRM.  For those machines, an error is returned when the system
reaches its own time-out while trying to connect.

A side effect of this method is to reset the alarm interval associated
with SIGALRM.


=head2 output_field_separator - field separator for print

    $ofs = $obj->output_field_separator;

    $prev = $obj->output_field_separator($ofs);

This method designates the output field separator for B<print()>.
Ordinarily the print method simply prints out the comma separated
fields you specify.  Set this to specify what's printed between
fields.

With no argument this method returns the current output field
separator set in the object.  With an argument it sets the output
field separator to I<$ofs> and returns the previous value.


=head2 output_log - log all output

    $fh = $obj->output_log;

    $fh = $obj->output_log($fh);

    $fh = $obj->output_log($filename);

This method starts or stops logging of output.  This is useful when
debugging.  Also see B<dump_log()>.  Because most command interpreters
echo back commands received, its likely all your output would also be
in an input log.  See B<input_log()>.  Note that output logging occurs
before newline translation.  See B<binmode()> for details on newline
translation.

If no argument is given, the log filehandle is returned.  A null
string indicates logging is off.

To stop logging, use a null string as an argument.

If an open filehandle is given, it is used for logging and returned.
Otherwise, the argument is assumed to be the name of a file, the file
is opened for logging and a filehandle to it is returned.


=head2 output_record_separator - output line delimiter

    $ors = $obj->output_record_separator;

    $prev = $obj->output_record_separator($ors);

This method designates the output record separator for B<print()>.
Ordinarily the print operator simply prints out the comma separated
fields you specify, with no trailing newline or record separator
assumed.  Set this variable to specify what's printed at the end of
the print.

Note: the output record separator is set to B<\n> by default, so
there's no need to append all your commands with a newline.

With no argument this method returns the current output record
separator set in the object.  With an argument it sets the output
record separator to I<$ors> and returns the previous value.


=head2 port - remote port

    $port = $obj->port;

    $prev = $obj->port($port);

This method designates the remote TCP port.  With no argument this
method returns the current port number.  With an argument it sets the
current port number to I<$port> and returns the previous port.  If
I<$port> is a service name, then first it's converted to a port number
using the perl function B<getservbyname()>.


=head2 print - write to object

    $ok = $obj->print(@list);

This method prints a string or a comma-separated list of strings to
the opened object and returns non-zero if all data was successfully
written.

By default, the B<output_record_separator()> is set to B<\n> in order
to have your commands automatically end with a newline.  In most cases
your output is being read by a command interpreter which won't accept
a command until newline is read.  This is similar to someone typing a
command and hitting the return key.

On failure, it's possible that some data was written.  If you choose
to try and recover from a print timing-out, use B<print_length()> to
determine how much was written before timeout occurred.


=head2 print_length - number of bytes written by print

    $num = $obj->print_length;

This returns the number of bytes successfully written by the most
recent B<print()>.


=head2 prompt - pattern to match a prompt

    $matchop = $obj->prompt;

    $prev = $obj->prompt($matchop);

This method sets the pattern used to find a prompt in the input
stream.  It must be a string representing a valid perl pattern match
operator.  The methods B<login()> and B<cmd()> try to read until
matching the prompt.  If the pattern chosen doesn't match what's
sent, then it's likely those commands will timeout.

With no argument this method returns the prompt set in the object.
With an argument it sets the prompt to I<$matchop> and returns the
previous value.

The default prompt is '/[$%#>] $/'

Always use single quotes to construct I<$matchop> to avoid unintended
backslash interpretation.  Using single quotes, you only need add
extra backslashes to quote patterns containing B<\'> or B<\\>.


=head2 telnetmode - turn off/on telnet command interpretation

    $prev = $obj->telnet($mode);

This method controls whether or not telnet commands in the data stream
are recognized and handled.  The telnet protocol uses certain
character sequences sent in the data stream to control the session.
If the port you're connecting to isn't using the telnet protocol, then
you should turn this mode off.  The default is I<on>.

If I<$mode> is B<0> then telnet mode is off.  If I<$mode> is missing
or B<1> then telnet mode is on.


=head2 timed_out - timeout indicator

    $boolean = $obj->timed_out;

    $prev = $obj->timed_out($boolean);

This method indicates if a previous read or write method timed-out.

With no argument this method returns true if a previous method
timed-out.  With an argument it sets the indicator.  Generally this is
used by internal methods to clear it.


=head2 timeout - I/O timeout interval

    $secs = $obj->timeout;

    $prev = $obj->timeout($secs);

This method sets the timeout interval that's used when performing I/O
or connecting to a port.  When a method doesn't complete within the
timeout interval then it's an error and the error mode action is
performed.

The timeout may be expressed as a relative or absolute value.  If
I<$secs> is greater than or equal to the time the program was started,
as determined by $^T, then it's the absolute time when timeout occurs.
Also see the perl function B<time()>.  A relative timeout happens
I<$secs> from when the I/O method begins.

If I<$secs> is B<0> then timeout occurs if the data cannot be
immediately read or written.  Use the undefined value to turn off
timing-out.

With no argument this method returns the timeout set in the object.
With an argument it sets the timeout to I<$secs> and returns the
previous value.


=head2 watchfor - wait for pattern in the input

    $ok = $obj->waitfor($matchop);
    $ok = $obj->waitfor([Match   => $matchop,]
                        [String  => $string,]
                        [Timeout => $secs,]);

    ($prematch, $match) = $obj->waitfor($matchop);
    ($prematch, $match) = $obj->waitfor([Match   => $matchop,]
                                        [String  => $string,]
                                        [Timeout => $secs,]);

This method reads until a pattern match or string is found in the
input stream.  All the characters before and including the match are
removed from the input stream.  On eof an undefined value is returned.
On timeout or other errors the error mode action is performed.

In an array context the characters before the match and the matched
characters are returned in I<$prematch> and I<$match>.

You can specify more than one pattern or string by simply providing
multiple B<Match> and/or B<String> named arguments.  A I<$matchop>
must be a string representing a valid perl pattern match operator.
The I<$string> is just a substring to find in the input stream.

An optional named argument is provided to override the current setting
of timeout.

Always use single quotes to construct I<$matchop> to avoid unintended
backslash interpretation.  Using single quotes, you only need add
extra backslashes to quote patterns containing B<\'> or B<\\>.


=head1 SEE ALSO

=over 2

=item *

RFC 854 - TELNET Protocol Specification

=item *

RFC 1143 - The Q Method of Implementing TELNET Option Negotiation

=item *

TELNET Options

=back


=head1 EXAMPLES

This example gets the current weather forecast for Brainerd, Minnesota.

    use Net::Telnet ();
    my($forecast, $t);

    $t = new Net::Telnet (-host => "rainmaker.wunderground.com");

    ## Wait for first prompt and "hit return".
    $t->waitfor('/continue:.*$/');
    $t->print("");

    ## Wait for second prompt and respond with city code.
    $t->waitfor('/city code:.*$/');
    $t->print("BRD");

    ## Read and print the first page of forecast.
    ($forecast) = $t->waitfor('/[ \t]+press return to continue/i');
    print $forecast;

    exit;


This example checks a POP server to see if you have mail.

    use Net::Telnet ();
    my($hostname, $line, $passwd, $pop, $username);

    $hostname = "your_destination_host_here";
    $username = "your_username_here";
    $passwd = "your_password_here";

    $pop = new Net::Telnet (-host => $hostname,
			    -port => 110,
			    -telnetmode => '');

    ## Read connection message.
    $line = $pop->getline;
    die $line unless $line =~ /^\+OK/;

    ## Send user name.
    $pop->print("user $username");
    $line = $pop->getline;
    die $line unless $line =~ /^\+OK/;

    ## Send password.
    $pop->print("pass $passwd");
    $line = $pop->getline;
    die $line unless $line =~ /^\+OK/;

    ## Request status of messages.
    $pop->print("list");
    $line = $pop->getline;
    print $line;

    exit;


Here's an example you can use to down load a file of any type.  The
file is read from the remote host's standard output using cat.  To
prevent any output processing, the remote host's standard output is
put in raw mode using the Bourne shell.  The Bourne shell is used
because some shells, notably tcsh, prevent changing tty modes.  Upon
completion, FTP style statistics are printed to stderr.

    use Net::Telnet;
    my($block, $filename, $host, $hostname, $k_per_sec, $line,
       $num_read, $passwd, $prevblock, $prompt, $size, $size_bsd,
       $size_sysv, $start_time, $total_time, $username);

    $hostname = "your_destination_host_here";
    $username = "your_username_here";
    $passwd = "your_password_here";
    $filename = "your_download_file_here";

    ## Connect and login.
    $host = new Net::Telnet (Host => $hostname,
                             Timeout => 30,
                             Prompt => '/[%#>] $/');
    $host->login($username, $passwd);

    ## Make sure prompt won't match anything in send data.
    $prompt = '_funkyPrompt_';
    $host->prompt("/$prompt\$/");
    $host->cmd("set prompt = '$prompt'");

    ## Get size of file.
    ($line) = $host->cmd("/usr/bin/ls -l $filename");
    ($size_bsd, $size_sysv) = (split ' ', $line)[3,4];
    if ($size_sysv =~ /^\d+$/) {
        $size = $size_sysv;
    }
    elsif ($size_bsd =~ /^\d+$/) {
        $size = $size_bsd;
    }
    else {
        die "$filename: no such file on $hostname";
    }

    ## Start sending the file.
    binmode STDOUT;
    $host->binmode;
    $host->print("/usr/bin/sh -c 'stty raw; cat $filename'");
    $host->getline;    # discard echoed back line

    ## Read file a block at a time.
    $num_read = 0;
    $prevblock = '';
    $start_time = time;
    while (($block = $host->get) and ($block !~ /$prompt$/o)) {
        if (length $block >= length $prompt) {
            print $prevblock;
            $num_read += length $prevblock;
            $prevblock = $block;
        }
        else {
            $prevblock .= $block;
        }

    }
    $host->close;

    ## Print last block without trailing prompt.
    $prevblock .= $block;
    $prevblock =~ s/$prompt$//;
    print $prevblock;
    $num_read += length $prevblock;
    die "error: expected size $size, received size $num_read\n"
        unless $num_read == $size;

    ## Print totals.
    $total_time = (time - $start_time) || 1;
    $k_per_sec = ($size / 1024) / $total_time;
    $k_per_sec = sprintf "%3.1f", $k_per_sec;
    warn("$num_read bytes received in $total_time seconds ",
         "($k_per_sec Kbytes/s)\n");

    exit;


Here's an example that shows how to talk to a program that
must communicate via a terminal.  In this case we're talking
to the telnet program via a pseudo-terminal.  We use the
Comm package to start the telnet program and return a
filehandle to the pseudo-terminal.  This example sends some
initial commands and then allows the user to type commands
to the telnet session.

    use Net::Telnet;
    my($comm_pty, $host, $hostname, $passwd, $pty,
       $username, @lines);

    $hostname = "your_host_here";
    $username = "your_name_here";
    $passwd = "your_passwd_here";

    ## Start the telnet program so we can talk to it via a
    ## pseudo-terminal.
    {
        local $^W = 0;  # Comm.pl isn't warning clean

        require "Comm.pl";
        &Comm::init("close_it", "interact",
                    "open_proc", "stty_raw", "stty_sane");
        $comm_pty = &open_proc("telnet $hostname")
            or die "open_proc failed";

        ## Unfortunately the Comm package doesn't
        ## return us a fully qualified filehandle.  We
        ## must keep the filehandle Comm returned for
        ## its use and we must build another filehandle
        ## qualified with the current package for our
        ## use.
        $pty = "main::" . $comm_pty;
    }

    ## Obtain a new Net::Telnet object that does I/O to the
    ## pseudo-terminal attached to the running telnet
    ## program.  The "Telnetmode" is "off" because we're
    ## not talking directly to a telnet port as we normally
    ## do, we're talking to a pseudo-terminal.  The
    ## "Output_record_separator" is now a carriage-return
    ## because that's what you'd normally hit when you get
    ## done typing a line at a terminal.
    $host = new Net::Telnet (Fhopen => $pty,
                             Timeout => 10,
                             Prompt => '/[%#>] $/',
                             Telnetmode => 0,
                             Output_record_separator => "\r");

    ## Issue some commands.
    $host->login($username, $passwd);
    $host->cmd("setenv DISPLAY $ENV{DISPLAY}");
    print $host->cmd("who");

    ## Allow the user to interact with telnet program until
    ## they exit.
    {
        no strict 'subs';  # so we can refer to STDIN
        local $^W = 0;     # Comm.pl isn't warning clean

        &stty_raw(STDIN);
        &interact($comm_pty);
        &stty_sane(STDIN);
        &close_it($comm_pty);
    }

    print "Exited telnet\n";
    exit;


=head1 AUTHOR

Jay Rogers <jay@rgrs.com>


=head1 COPYRIGHT

Copyright (c) 1997 Jay Rogers. All rights reserved.  This program is
free software; you can redistribute it and/or modify it under the same
terms as Perl itself.
