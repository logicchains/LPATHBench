#!env perl

use warnings;
use strict;

my (@nodesDst, @nodesCost, @visited);

sub readPlaces {
	open my $AGRAPH, '<', "agraph";
	my $numnodes = <$AGRAPH>;
	for my $i (0 .. $numnodes - 1) {
		($nodesDst[$i], $nodesCost[$i]) = ([], []);
	}
	for (<$AGRAPH>) {
		chomp;
		my ($node, $dest, $cost) = split;
		push @{$nodesDst[$node]}, $dest+0;
		push @{$nodesCost[$node]}, $cost+0;
	}
}

sub getLongestPath {
	my ($nodeid) = @_;
	$visited[$nodeid] = 1;
	my $neighboursDst = $nodesDst[$nodeid];
	my $max = 0;
	for my $i (0 .. $#{$neighboursDst}) {
		if (!$visited[$neighboursDst->[$i]]) {
			my $dist = $nodesCost[$nodeid]->[$i] + getLongestPath($neighboursDst->[$i]);
			if ($dist > $max) {
				$max = $dist;
			}
		}
	}
	$visited[$nodeid] = 0;
	
	return $max;
}

readPlaces();

my $start = time;
my $length = getLongestPath(0);
my $duration = time - $start;
print "$length LANGUAGE Perl " . $duration * 1000 . "\n";
