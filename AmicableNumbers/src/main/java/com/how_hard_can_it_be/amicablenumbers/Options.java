package com.how_hard_can_it_be.amicablenumbers;

import org.kohsuke.args4j.Option;

public class Options
{
    @Option( name="--lessThan", usage="Find all amicable numbers below this number")
    public int ceiling;
    
    @Option( name="--factor", usage="Factor this number")
    public int numberToFactor;
}
