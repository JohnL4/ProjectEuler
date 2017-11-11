package com.how_hard_can_it_be.amicablenumbers;

import org.kohsuke.args4j.Option;

public class AmicableNumbersOptions
{
    @Option( name="--lessThan", usage="Find all amicable numbers below this number")
    public int ceiling;
    
    @Option( name="--factor", usage="Factor this number")
    public int numberToFactor;
    
    @Option( name="--numberType", usage="Determine what type of number (deficient, perfect, abundant) this is")
    public int numberToType;
    
    @Option( name="--numberTypeLow", usage="Low end of range of numbers to determine type(s) of")
    public int numberToTypeLow;
    
    @Option( name="--numberTypeHigh", usage="High end of range of numbers to determine type(s) of")
    public int numberToTypeHigh;
    
    @Option( name="--perfectHigh", usage="Find all perfect numbers less than or equal to this number")
    public int perfectHigh;
    
    @Option( name="--abundantHigh", usage="Find all abundant numbers less than or equal to this number")
    public int abundantHigh;
    
    @Option( name="--notSumOfTwoAbundant", usage="Find all numbers less than or equal to this number which cannot be written as the sum of two abundant numbers")
    public boolean notSumOfTwoAbundant;
}
