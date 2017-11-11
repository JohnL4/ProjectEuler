package com.how_hard_can_it_be.primes;

import org.kohsuke.args4j.Option;

public class PrimesOptions {
	
	@Option( name="--ceil", usage="The biggest prime number to find")
	public Integer ceiling = 100;
	
	@Option( name="--noprint", usage="Suppress printing of found primes")
	public Boolean noPrint = false; // Print by default
	
	@Option( name="--toFile", usage="Name of a binary file to which to write primes")
	public String toFile;
	
	@Option( name="--fromFile", usage="Name of a binary file from which to read primes")
	public String fromFile;
	
	@Option( name="--factor", usage="Give prime factors of the given number")
	public long numberToFactor;
	
	@Option( name="--sumPrimes", usage="Sum all primes found.  Implies --noprint")
	public boolean sumPrimes;
	
	@Option( name="--triangularNumberFactors", usage="Print the first triangular number (sum(i, i, 1, n)) whose number of factors is over the given number")
	public int triangularNumberFactorCount;
}
