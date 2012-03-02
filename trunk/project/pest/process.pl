#! /usr/bin/perl

my %years;
my %tries;
my %vars;
my %data;

my @years;
my @tries;
my @vars;

my $try = "---";
my $year = -1;

for (<>)
{
    if ( /^\*\* sims\[[0-9]+\]: (...)$/ )
    {
        $try = $1;

        if (!$tries{$try})
        {
            $tries{$try} = 1;
            push (otries, $try);
        }
    }
    if ( /LOGFILE: ...-(....)/ )
    {
        $year = $1;

        if (!$years{$year})
        {
            $years{$year} = 1;
            push (oyears, $year);
        }
    }
    if ( /^ *([^ ]+) = +([0-9.-]+) \[.*\]$/)
    {
        my $var = $1;
        my $value = $2;
        if (!$vars{$var})
        {
            $vars{$var} = 1;
            push (ovars, $var);
        }

        $data{"$try!$year!$var"} = $value;
    }
}

print "Mean";

for (@otries)
{
    my $try = $_;
    print "\t$try";
}
print "\n";

for (@ovars)
{
    my $var = $_;

    print "$var";

    for (@otries)
    {
        my $try = $_;

        my $sum = 0.0;
        my $count = 0;
        for (@oyears)
        {
            my $year = $_;
            $sum += $data{"$try!$year!$var"};
            $count++;
        }
        my $average = $sum / ($count + 0.0);
        print "\t$average";
    }
    print "\n";
}

print "\nMax";

for (@otries)
{
    my $try = $_;
    print "\t$try";
}
print "\n";

for (@ovars)
{
    my $var = $_;

    print "$var";

    for (@otries)
    {
        my $try = $_;

        my $max = -1e99;
        for (@oyears)
        {
            my $year = $_;
            my $value = $data{"$try!$year!$var"};
            $max = $value if ($value > $max);
        }
        print "\t$max";
    }
    print "\n";
}

print "\nMin";

for (@otries)
{
    my $try = $_;
    print "\t$try";
}
print "\n";

for (@ovars)
{
    my $var = $_;

    print "$var";

    for (@otries)
    {
        my $try = $_;

        my $min = 1e99;
        for (@oyears)
        {
            my $year = $_;
            my $value = $data{"$try!$year!$var"};
            $min = $value if ($value < $min);
        }
        print "\t$min";
    }
    print "\n";
}

print "\nStddev";

for (@otries)
{
    my $try = $_;
    print "\t$try";
}
print "\n";

for (@ovars)
{
    my $var = $_;

    print "$var";

    for (@otries)
    {
        my $try = $_;

        my $sum = 0.0;
        my $count = 0;
        for (@oyears)
        {
            my $year = $_;
            $sum += $data{"$try!$year!$var"};
            $count++;
        }

        my $average = $sum / ($count + 0.0);

        my $total = 0.0;
        for (@oyears)
        {
            my $year = $_;
            my $diff = $data{"$try!$year!$var"} - $average;
            $total += $diff * $diff;
        }
        my $stddev = sqrt ($total);

        print "\t$stddev";
    }
    print "\n";
}

