package com.how_hard_can_it_be.amicablenumbers;

public enum NumberType
{
    /**
     * Sum of proper divisors of number is less than the number itself.
     */
    DEFICIENT,
    
    /**
     * Sum of proper divisors of number is equal to the number itself.
     */
    PERFECT,
    
    /**
     * Sum of proper divisors of number is greater than the number itself.
     */
    ABUNDANT
}
