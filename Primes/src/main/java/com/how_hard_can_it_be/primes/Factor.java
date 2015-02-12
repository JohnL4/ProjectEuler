package com.how_hard_can_it_be.primes;

/**
 * A factor of a number.
 * @author John Lusk
 *
 */
public class Factor
{
    private long factor;
    private int count;
    
    /**
     * 
     * @param aRemainingToFactor The factor
     * @param aCount The number of times the factor occurs in the number being factored.
     */
    public Factor( long aRemainingToFactor, int aCount)
    {
        factor = aRemainingToFactor;
        count = aCount;
    }
    
    public long getFactor()
    {
        return factor;
    }
    public int getCount()
    {
        return count;
    }
    
    
    
}
